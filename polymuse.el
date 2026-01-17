;;; polymuse.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Niten
;;
;; Author: Niten <niten@fudo.org>
;; Maintainer: Niten <niten@fudo.org>
;; Created: November 15, 2025
;; Modified: November 15, 2025
;; Version: 0.0.1
;; Keywords: docs outlines processes terminals text tools
;; Homepage: https://github.com/niten/polymuse
;; Package-Requires: ((emacs "29.3") (gptel "0.9.0") (markdown-mode "2.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Polymuse is an AI-powered "over-the-shoulder" coding assistant for Emacs.
;; It provides continuous, context-aware code review and suggestions while
;; you write code or prose.
;;
;; Features:
;; - Automatic periodic reviews of your work
;; - Context-aware suggestions based on surrounding code
;; - Support for multiple LLM backends (Ollama, OpenAI)
;; - Customizable review intervals and instructions
;; - Typewriter-style animated output for a pleasant UX
;; - Tool profiles that give the LLM access to project-specific information
;; - Integration with canon.el for character/location tracking in prose
;;
;; Quick Start:
;; 1. Set up a backend: M-x polymuse-define-default-backend
;; 2. Enable polymuse-mode in a buffer
;; 3. Add a reviewer: M-x polymuse-add-reviewer
;; 4. Customize instructions: C-c C-r e
;;
;; The reviewer will automatically provide feedback based on your cursor
;; position and the surrounding context. Reviews appear in a side window
;; and are updated periodically as you work.
;;
;; Tool Profiles:
;; Polymuse uses a layered tool system to give the LLM context-specific
;; capabilities:
;; - Built-in tools are auto-registered based on active modes
;;   (e.g., canon-mode provides entity lookup tools)
;; - Profiles organize tools for different workflows
;;   (prose-writing, code-review, architecture, debugging)
;; - File-local tools can be defined in .polymuse-tools.el
;;
;; When canon-mode is active, Polymuse automatically provides tools for:
;; - Looking up characters, locations, and other entities
;; - Searching entities by properties
;; - Suggesting modifications (appended safely, never clobbering)
;;
;; Use M-x polymuse-switch-profile to change profiles.
;; Use M-x polymuse-show-active-tools to see available tools.
;;
;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'gptel)
(require 'gptel-ollama)
(require 'gptel-org)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'typewrite)
(require 'markdown-mode)

(defvar polymuse-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r e") #'polymuse-edit-instructions)
    (define-key map (kbd "C-c C-r r") #'polymuse-run-review)
    (define-key map (kbd "C-c C-r d") #'polymuse-kill-reviewer)
    (define-key map (kbd "C-c C-r D") #'polymuse-kill-all-reviewers)
    (define-key map (kbd "C-c C-r o") #'polymuse-open-review)
    (define-key map (kbd "C-c C-r a") #'polymuse-add-reviewer)
    (define-key map (kbd "C-c C-r z") #'polymuse-reset-output)
    map))

(define-minor-mode polymuse-mode
  "LLM over-the-shoulder assistant."
  :init-value nil
  :lighter " Muse"
  :keymap polymuse-keymap
  (if polymuse-mode
      (polymuse--enable)
    (polymuse--disable)))

(defgroup polymuse nil
  "LLM over-the-shoulder assistant."
  :group 'applications)

(defcustom polymuse-backend nil
  "`gptel' backend used by polymuse."
  :type 'symbol)

(defcustom polymuse-ollama-hostname nil
  "Ollama backend host for use with polymuse."
  :type 'string)

(defcustom polymuse-ollama-protocol "https"
  "Protocol with which to interact with polymuse."
  :type 'string)

(defcustom polymuse-max-prompt-characters 10000
  "Maximum length of a prompt, in characters.

For Ollama models with 8K context (~6K usable tokens):
- Set to 6000-8000 (assuming ~4 chars per token)
- Leaves room for model response and overhead

The actual tokens used will be lower than character count,
but this provides a safe margin."
  :type 'integer)

(defcustom polymuse-context-backward-ratio 0.60
  "Ratio of context budget to allocate to backward context.

The remaining context space (after response reserve) goes to forward context.
Default 0.60 means 60% backward, 40% forward.

Backward context (code/text before cursor) is typically more relevant for
understanding what the user is working on. Increase this if you find the
model lacks enough prior context. Decrease if you need more forward lookahead."
  :type 'float)

(defcustom polymuse-response-reserve-ratio 0.25
  "Ratio of available context to reserve for model response.

Default 0.25 means 25% of available space is reserved for the model's output,
preventing response truncation. For models with small context windows, you may
want to increase this. For larger models, you can decrease it to send more context."
  :type 'float)

(defcustom polymuse-default-review-context-lines 20
  "Number of previous lines of review to include with the prompt."
  :type 'integer)

(defcustom polymuse-include-previous-review nil
  "Whether to include previous review content in the prompt.

When non-nil, the prompt will include the last
`polymuse-default-review-context-lines' lines of the previous review
as context, with instructions not to repeat previous points.

When nil (the default), previous reviews are not included. This is useful
for simpler models that tend to revise previous reviews rather than
generate new ones."
  :type 'boolean)

(defcustom polymuse-default-buffer-size-limit 10000
  "Default maximum size for Polymuse review buffers.

When the buffer grows larger than this, the beginning will be truncated."
  :type 'integer)

(defcustom polymuse-require-buffer-visible t
  "Only review buffers visible in a window.

When non-nil, reviews will only run for buffers that are currently
visible in at least one window."
  :type 'boolean)

(defcustom polymuse-review-timeout 300
  "Maximum seconds before marking a review as stale.

If a review request takes longer than this, it will be reset to idle
status so new reviews can be requested. Default is 5 minutes (300 seconds)."
  :type 'integer)

(defcustom polymuse-buffer-activity-timeout 60
  "Maximum seconds of inactivity before pausing reviews.

Reviews will only run if the buffer was active since the last review,
or was active within this many seconds. This prevents generating reviews
when the user is away from their desk. Default is 60 seconds."
  :type 'integer)

;;;;
;; MODEL CONFIGURATION PRESETS
;;;;

(cl-defstruct polymuse-config
  "Configuration preset for a specific LLM model.

Each preset encapsulates the optimal settings for a particular model,
including context limits, allocation ratios, and whether to include
previous review history."
  name                      ; Symbol identifier (e.g., 'llama-3.1-8b)
  description               ; Human-readable description
  max-prompt-characters     ; Maximum prompt length in characters
  response-reserve-ratio    ; Ratio of space reserved for response
  context-backward-ratio    ; Ratio of context allocated backward
  include-previous-review   ; Whether to include review history
  mode-prompts)             ; Optional custom mode-specific prompts

(defvar polymuse-config-presets
  `((default
     . ,(make-polymuse-config
         :name 'default
         :description "Conservative defaults for 8K context models"
         :max-prompt-characters 7000
         :response-reserve-ratio 0.28
         :context-backward-ratio 0.60
         :include-previous-review nil
         :mode-prompts nil))

    (llama-3.2-3b
     . ,(make-polymuse-config
         :name 'llama-3.2-3b
         :description "Llama 3.2 3B - Fast, lightweight, good for quick feedback"
         :max-prompt-characters 5500
         :response-reserve-ratio 0.35
         :context-backward-ratio 0.55
         :include-previous-review nil
         :mode-prompts nil))

    (llama-3.1-8b
     . ,(make-polymuse-config
         :name 'llama-3.1-8b
         :description "Llama 3.1 8B - Great balance of speed and capability (recommended)"
         :max-prompt-characters 7000
         :response-reserve-ratio 0.28
         :context-backward-ratio 0.60
         :include-previous-review nil
         :mode-prompts nil))

    (mistral-7b
     . ,(make-polymuse-config
         :name 'mistral-7b
         :description "Mistral 7B v0.3 - Excellent for code, concise responses"
         :max-prompt-characters 6800
         :response-reserve-ratio 0.25
         :context-backward-ratio 0.62
         :include-previous-review nil
         :mode-prompts nil))

    (qwen-2.5-coder-7b
     . ,(make-polymuse-config
         :name 'qwen-2.5-coder-7b
         :description "Qwen 2.5 Coder 7B - Code-specialized, excellent pattern recognition"
         :max-prompt-characters 7200
         :response-reserve-ratio 0.27
         :context-backward-ratio 0.68
         :include-previous-review nil
         :mode-prompts '((prog-mode . "Review code in `focus-region`. Priority: (1) bugs/logic errors; (2) security issues (injection, validation); (3) performance problems; (4) better standard library usage. Suggest specific functions/patterns.")
                         (text-mode . "Read the prose in `focus-region` and share your thoughts. What's working? What could be more engaging? Think about characters, pacing, emotional impact, voice. Share ideas and reactions that come to mind, not just corrections."))))

    (deepseek-coder-6.7b
     . ,(make-polymuse-config
         :name 'deepseek-coder-6.7b
         :description "DeepSeek Coder 6.7B - Excellent code understanding, good at explaining complex logic"
         :max-prompt-characters 6500
         :response-reserve-ratio 0.32
         :context-backward-ratio 0.65
         :include-previous-review nil
         :mode-prompts nil))

    (gemma-2-9b
     . ,(make-polymuse-config
         :name 'gemma-2-9b
         :description "Gemma 2 9B - Strong general-purpose model for code and prose"
         :max-prompt-characters 7500
         :response-reserve-ratio 0.28
         :context-backward-ratio 0.58
         :include-previous-review t
         :mode-prompts nil))

    (phi-3-mini
     . ,(make-polymuse-config
         :name 'phi-3-mini
         :description "Phi-3 Mini (3.8B) - Efficient small model with strong capabilities"
         :max-prompt-characters 5000
         :response-reserve-ratio 0.35
         :context-backward-ratio 0.58
         :include-previous-review nil
         :mode-prompts nil))

    (codellama-7b
     . ,(make-polymuse-config
         :name 'codellama-7b
         :description "CodeLlama 7B - Meta's code-specialized Llama variant"
         :max-prompt-characters 6800
         :response-reserve-ratio 0.30
         :context-backward-ratio 0.70
         :include-previous-review nil
         :mode-prompts nil)))
  "Alist of predefined configuration presets for popular Ollama models.

Each preset is optimized for a specific model's context window size,
response patterns, and typical use cases. Use `polymuse-use-config'
to apply a preset.")

(defvar polymuse-current-config 'default
  "Currently active configuration preset (symbol).")

(defun polymuse-get-config (config-name)
  "Get configuration preset CONFIG-NAME from `polymuse-config-presets'.

Returns a `polymuse-config' struct or nil if not found."
  (cdr (assq config-name polymuse-config-presets)))

(defun polymuse-apply-config (config &optional buffer)
  "Apply CONFIG (a `polymuse-config' struct) to BUFFER or current buffer.

Sets buffer-local variables according to the configuration preset.
If BUFFER is nil, applies to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local polymuse-max-prompt-characters
                (polymuse-config-max-prompt-characters config))
    (setq-local polymuse-response-reserve-ratio
                (polymuse-config-response-reserve-ratio config))
    (setq-local polymuse-context-backward-ratio
                (polymuse-config-context-backward-ratio config))
    (setq-local polymuse-include-previous-review
                (polymuse-config-include-previous-review config))
    (when (polymuse-config-mode-prompts config)
      (setq-local polymuse-mode-prompts
                  (polymuse-config-mode-prompts config)))
    (setq-local polymuse-current-config
                (polymuse-config-name config))))

(defun polymuse-use-config (config-name)
  "Switch to configuration preset CONFIG-NAME.

CONFIG-NAME should be a symbol like 'llama-3.1-8b or 'mistral-7b.
See `polymuse-config-presets' for available presets.

This sets buffer-local variables, so different buffers can use
different configurations."
  (interactive
   (list (intern (completing-read
                  "Polymuse config: "
                  (mapcar (lambda (entry)
                            (let* ((config (cdr entry))
                                   (name (symbol-name (polymuse-config-name config)))
                                   (desc (polymuse-config-description config)))
                              (cons (format "%-20s - %s" name desc) (car entry))))
                          polymuse-config-presets)
                  nil t))))
  (let ((config (polymuse-get-config config-name)))
    (unless config
      (user-error "Unknown configuration preset: %s" config-name))
    (polymuse-apply-config config)
    (message "Polymuse config: %s - %s"
             (polymuse-config-name config)
             (polymuse-config-description config))))

(defun polymuse-show-current-config ()
  "Display the current Polymuse configuration settings."
  (interactive)
  (let ((config-name (if (boundp 'polymuse-current-config)
                         polymuse-current-config
                       'default)))
    (message "Polymuse config: %s | prompt-chars=%d | response-reserve=%.2f | backward-ratio=%.2f | prev-review=%s"
             config-name
             polymuse-max-prompt-characters
             polymuse-response-reserve-ratio
             polymuse-context-backward-ratio
             polymuse-include-previous-review)))

(defun polymuse-list-configs ()
  "Display all available configuration presets."
  (interactive)
  (let ((buf (get-buffer-create "*Polymuse Configurations*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Polymuse Configuration Presets\n\n")
      (insert "Use M-x polymuse-use-config to switch between these presets:\n\n")
      (dolist (entry polymuse-config-presets)
        (let* ((config (cdr entry))
               (current-p (eq (polymuse-config-name config) polymuse-current-config)))
          (insert (format "%s%-20s - %s\n"
                          (if current-p "* " "  ")
                          (polymuse-config-name config)
                          (polymuse-config-description config)))
          (insert (format "  max-chars: %d | response-reserve: %.2f | backward: %.2f | prev-review: %s\n\n"
                          (polymuse-config-max-prompt-characters config)
                          (polymuse-config-response-reserve-ratio config)
                          (polymuse-config-context-backward-ratio config)
                          (polymuse-config-include-previous-review config)))))
      (goto-char (point-min))
      (help-mode))
    (display-buffer buf)))

;;;;
;; EXAMPLE CONFIGURATIONS FOR POPULAR OLLAMA MODELS
;;;;

;; NOTE: The configuration examples below are now available as presets!
;; Instead of copying these settings manually, use:
;;
;;   M-x polymuse-use-config RET llama-3.1-8b RET
;;
;; Or in your init.el:
;;
;;   (add-hook 'polymuse-mode-hook
;;             (lambda () (polymuse-use-config 'llama-3.1-8b)))
;;
;; Use M-x polymuse-list-configs to see all available presets.
;;
;; The examples below are kept for reference and for understanding
;; how to create custom configurations.

;; The settings below are optimized for models with ~8K context windows.
;; Adjust based on your hardware and quality preferences.
;;
;; General guidelines:
;; - Smaller models (3B-7B): Need more response space, less context
;; - Larger models (8B+): Can handle more context, better instruction following
;; - Code-specialized models: May benefit from more backward context (0.65-0.70)
;; - General models: Balanced allocation (0.55-0.60)
;;
;; To use a configuration, add it to your init.el BEFORE enabling polymuse-mode.

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ Llama 3.2 3B - Fast, lightweight, good for quick feedback           │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Very fast responses, low resource usage
;; Cons: Less sophisticated reasoning, may miss subtle issues
;; Best for: Quick syntax checks, obvious bugs, simple refactoring suggestions
;;
;; (setq polymuse-max-prompt-characters 5500
;;       polymuse-response-reserve-ratio 0.35  ; Needs more space to formulate response
;;       polymuse-context-backward-ratio 0.55  ; Balanced context
;;       polymuse-include-previous-review nil) ; Keep it simple

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ Llama 3.1 8B - Great balance of speed and capability                │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Good reasoning, fast enough, handles instructions well
;; Cons: May still miss complex architectural issues
;; Best for: Most use cases, daily coding, prose editing
;;
;; (setq polymuse-max-prompt-characters 7000
;;       polymuse-response-reserve-ratio 0.28
;;       polymuse-context-backward-ratio 0.60
;;       polymuse-include-previous-review nil) ; Optional: set to t if model performs well

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ Mistral 7B v0.3 - Excellent for code, concise responses             │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Very good at code understanding, naturally concise
;; Cons: Sometimes too brief, may need prompting for detail
;; Best for: Code review, refactoring suggestions, API design
;;
;; (setq polymuse-max-prompt-characters 6800
;;       polymuse-response-reserve-ratio 0.25  ; Concise by nature
;;       polymuse-context-backward-ratio 0.62
;;       polymuse-include-previous-review nil)

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ Qwen 2.5 Coder 7B - Code-specialized, excellent pattern recognition │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Trained on code, understands patterns, good library knowledge
;; Cons: Less useful for prose, may over-engineer solutions
;; Best for: Code review, finding bugs, suggesting standard libraries
;;
;; (setq polymuse-max-prompt-characters 7200
;;       polymuse-response-reserve-ratio 0.27
;;       polymuse-context-backward-ratio 0.68  ; Benefits from seeing more prior code
;;       polymuse-include-previous-review nil)
;;
;; ;; Override prompts for more code-specific guidance:
;; (setq polymuse-mode-prompts
;;       '((prog-mode . "Review code in `focus-region`. Priority: (1) bugs/logic errors; (2) security issues (injection, validation); (3) performance problems; (4) better standard library usage. Suggest specific functions/patterns.")
;;         (text-mode . "Review prose in `focus-region` for clarity and correctness. Keep feedback brief.")))

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ DeepSeek Coder 6.7B - Another strong code specialist                │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Excellent code understanding, good at explaining complex logic
;; Cons: Can be verbose, may need shorter prompt to fit context
;; Best for: Understanding complex code, architecture suggestions
;;
;; (setq polymuse-max-prompt-characters 6500
;;       polymuse-response-reserve-ratio 0.32  ; Tends toward longer responses
;;       polymuse-context-backward-ratio 0.65
;;       polymuse-include-previous-review nil)

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ Gemma 2 9B - Strong general-purpose model                           │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Good at both code and prose, balanced feedback
;; Cons: Slower than 7B models, higher resource usage
;; Best for: Mixed workflows (code + documentation), thoughtful analysis
;;
;; (setq polymuse-max-prompt-characters 7500
;;       polymuse-response-reserve-ratio 0.28
;;       polymuse-context-backward-ratio 0.58
;;       polymuse-include-previous-review t)  ; Handles conversation history well

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ Phi-3 Mini (3.8B) - Efficient small model with strong capabilities  │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Punches above its weight class, very efficient
;; Cons: Smaller context window, may struggle with very large functions
;; Best for: Resource-constrained systems, quick feedback loops
;;
;; (setq polymuse-max-prompt-characters 5000
;;       polymuse-response-reserve-ratio 0.35
;;       polymuse-context-backward-ratio 0.58
;;       polymuse-include-previous-review nil)

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ CodeLlama 7B - Meta's code-specialized Llama variant                │
;; └─────────────────────────────────────────────────────────────────────┘
;; Pros: Good code completion context, understands patterns
;; Cons: Older model, may miss modern library conventions
;; Best for: Legacy code review, learning from existing patterns
;;
;; (setq polymuse-max-prompt-characters 6800
;;       polymuse-response-reserve-ratio 0.30
;;       polymuse-context-backward-ratio 0.70  ; Really benefits from prior context
;;       polymuse-include-previous-review nil)

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ CUSTOM CONFIGURATION TEMPLATE                                       │
;; └─────────────────────────────────────────────────────────────────────┘
;; Use this template to tune settings for your specific model:
;;
;; (setq polymuse-max-prompt-characters 7000    ; Start here, adjust down if responses truncate
;;       polymuse-response-reserve-ratio 0.28    ; Increase if responses are cut off
;;       polymuse-context-backward-ratio 0.60    ; Increase if model seems to lack prior context
;;       polymuse-include-previous-review nil)   ; Try 't' if model can handle it
;;
;; Debug your settings with:
;; (setq polymuse-debug t)
;; Then check the *polymuse-debug* buffer to see actual prompt sizes.

;; ┌─────────────────────────────────────────────────────────────────────┐
;; │ ADVANCED: Per-mode configurations                                   │
;; └─────────────────────────────────────────────────────────────────────┘
;; You can set different limits for different major modes:
;;
;; (defun my-polymuse-config-hook ()
;;   "Configure Polymuse based on major mode."
;;   (pcase major-mode
;;     ;; Emacs Lisp: smaller sexps, can afford more context
;;     ('emacs-lisp-mode
;;      (setq-local polymuse-max-prompt-characters 8000
;;                  polymuse-context-backward-ratio 0.65))
;;     ;; JavaScript: larger functions, need more for current focus
;;     ('js-mode
;;      (setq-local polymuse-max-prompt-characters 6500
;;                  polymuse-context-backward-ratio 0.55))
;;     ;; Prose: balanced approach
;;     ('markdown-mode
;;      (setq-local polymuse-max-prompt-characters 7500
;;                  polymuse-context-backward-ratio 0.50))))
;;
;; (add-hook 'polymuse-mode-hook #'my-polymuse-config-hook)

(cl-defstruct polymuse-scheduler
  "Protocol for scheduling review ticks.

This allows tests to use a synchronous scheduler instead of real timers."
  (start-fn nil)  ; (lambda (interval callback) ...) -> timer object
  (stop-fn nil))  ; (lambda (timer) ...) -> void

(defun polymuse--create-timer-scheduler ()
  "Create the default timer-based scheduler for reviews."
  (make-polymuse-scheduler
   :start-fn (lambda (interval callback)
               (run-with-timer interval interval callback))
   :stop-fn (lambda (timer)
              (when (timerp timer)
                (cancel-timer timer)))))

(defun polymuse--create-immediate-scheduler ()
  "Create a scheduler for immediate synchronous execution (for testing).

This scheduler immediately calls the callback without using timers."
  (make-polymuse-scheduler
   :start-fn (lambda (_interval callback)
               (funcall callback)
               'immediate-scheduler-token)
   :stop-fn (lambda (_timer) nil)))

(cl-defstruct polymuse-global-state
  "Global state container for Polymuse.

This allows tests to isolate timer state by binding `polymuse--test-state'."
  (timer nil)
  (scheduler nil)) ; polymuse-scheduler instance

(defvar polymuse--default-global-state
  (make-polymuse-global-state :scheduler (polymuse--create-timer-scheduler))
  "Default global state for Polymuse.")

(defvar polymuse--test-global-state nil
  "When bound in tests, overrides the default global state.

Tests should bind this to a fresh `polymuse-global-state' to isolate
themselves from global state and other tests.")

(defun polymuse--get-global-state ()
  "Get the current Polymuse global state.

Returns `polymuse--test-global-state' if bound (for testing), otherwise
returns the global `polymuse--default-global-state'."
  (or polymuse--test-global-state polymuse--default-global-state))

(defvar-local polymuse--last-activity-time nil
  "Timestamp of the last buffer activity (for tracking user presence).")

(defcustom polymuse-debug nil
  "Enable Polymuse debug mode."
  :type 'boolean)

(defcustom polymuse-debug-buffer "*polymuse-debug*"
  "Buffer used to capture Polymuse debug input and output."
  :type 'string)

(defvar-local polymuse-tools-file nil
  "Project-local tools file for the current buffer.

This should be a file name, absolute or relative to the current file,
that defines any project-specific tools for Polymuse to use.")

;;;;
;; BACKENDS
;;;;

(defvar polymuse-backends '()
  "Alist of (ID . BACKEND) pairs of Polymuse backends.")

(defun polymuse-list-backends ()
  "Return a list of all available Polymuse backends."
  (interactive)
  (mapcar #'car polymuse-backends))

(cl-defstruct polymuse-backend-spec
  id
  model
  temperature)

(cl-defstruct (polymuse-ollama-backend-spec (:include polymuse-backend-spec))
  host
  protocol)

(cl-defstruct (polymuse-openai-backend-spec (:include polymuse-backend-spec)))

(cl-defstruct polymuse-backend
  id            ;; unique id for this backend
  model         ;; model name string
  temperature)  ;; model temperature

(cl-defstruct (polymuse-mock-backend (:include polymuse-backend))
  "Mock backend for testing that returns predefined responses.

The response-fn should be a function that takes a request and returns
a response string. If nil, returns a default test response."
  response-fn)

(cl-defstruct (polymuse-gptel-backend (:include polymuse-backend))
  executor)

(cl-defgeneric polymuse--initialize-backend (spec)
  "Given a Polymuse backend SPEC, initialize it.")

(defun polymuse-create-ollama-executor (name host model &optional protocol)
  "Set up Polymuse Ollama backend NAME for HOST with MODEL over PROTOCOL."
  (gptel-make-ollama name
    :host     host
    :protocol (or protocol "https")
    :key      nil
    :models   (list model)))

(defun polymuse-create-openai-executor (name model)
  "Set up Polymuse Ollama backend NAME for HOST with MODEL over PROTOCOL."
  (gptel-make-openai name :models (list model)))

(cl-defmethod polymuse--initialize-backend ((spec polymuse-ollama-backend-spec))
  "Instantiate ollama gptel backend from SPEC."
  (let* ((id       (polymuse-backend-spec-id spec))
         (existing (alist-get id polymuse-backends)))
    (if existing
        existing
      (let* ((model    (polymuse-backend-spec-model spec))
             (temperature (polymuse-backend-spec-temperature spec))
             (host     (polymuse-ollama-backend-spec-host spec))
             (protocol (polymuse-ollama-backend-spec-protocol spec))
             (backend  (make-polymuse-gptel-backend
                        :id          id
                        :model       model
                        :temperature temperature
                        :executor (polymuse-create-ollama-executor id host model protocol))))
        (setf (alist-get id polymuse-backends)
              backend)
        backend))))

(cl-defmethod polymuse--initialize-backend ((spec polymuse-openai-backend-spec))
  "Instantiate openai gptel backend from SPEC."
  (let* ((id       (polymuse-backend-spec-id spec))
         (existing (alist-get id polymuse-backends)))
    (if existing
        existing
      (let* ((model    (polymuse-backend-spec-model spec))
             (backend  (make-polymuse-gptel-backend
                        :id       id
                        :model    model
                        :executor (polymuse-create-openai-executor id model))))
        (setf (alist-get id polymuse-backends)
              backend)
        backend))))

(defcustom polymuse-default-backend-id nil
  "ID of default backend for Polymuse."
  :type 'symbol)

(defun polymuse--interactive-setup-backend ()
  "Define a new backend."
  (let* ((backend-type (completing-read
                        "Polymuse backend type: "
                        '("Ollama" "OpenAI")
                        nil t nil nil "Ollama")))
    (pcase backend-type
      ("Ollama" (polymuse--setup-ollama-backend))
      ("OpenAI" (polymuse--setup-openai-backend))
      (_ (user-error "Unsupported backend type: %s" backend-type)))))

(defun polymuse--interactive-setup-default-backend ()
  "Interactively create a default Polymuse backend, returning it."
  (setq polymuse-default-backend-id (polymuse--interactive-setup-backend)))

(defun polymuse-define-default-backend ()
  "Interactively define the default Polymuse backend."
  (interactive)
  (let ((id (polymuse--interactive-setup-default-backend)))
    (if id
        (message "Set default Polymuse backend to %s" id)
      (user-error "No backend configured"))))

(defun polymuse--delete-backend (backend-id)
  "Remove BACKEND-ID from Polymuse backends."
  (cl-remove backend-id
             polymuse-backends
             :key  #'polymuse-backend-id
             :test #'eq))

(defun polymuse-delete-backend ()
  "Delete backend specified by user from Polymuse."
  (interactive)
  (let* ((choice-id (completing-read "Remove backend: "
                                     (mapcar #'car polymuse-backends) nil t))
         (backend   (cdr (alist-get choice-id polymuse-backends))))
    (unless backend (user-error "No review found for id: %s" choice-id))
    (polymuse--delete-backend choice-id)))

(defun polymuse-ollama-list-models (host &optional protocol)
  "Return a list of available Ollama model names from HOST over PROTOCOL.

HOST should be like \"localhost:11434\".

WARNING: This function blocks Emacs for up to 5 seconds while fetching models.
If the server is unreachable, you may experience UI freezing."
  (let* ((url-request-method "GET")
         (url (format "%s://%s/api/tags" (or protocol "https") host))
         (buf (url-retrieve-synchronously url t t 5))) ;; 5s timeout
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let* ((json (json-parse-buffer
                            :object-type  'alist
                            :array-type   'list
                            :null-object  nil
                            :false-object nil))
                     (models (alist-get 'models json)))
                (delq nil
                      (mapcar (lambda (m) (alist-get 'name m))
                              models)))))
        (kill-buffer buf)))))

(defun polymuse--format-json (json-string)
  "Given a JSON string JSON-STRING, return a pretty-printed version."
  (with-temp-buffer
    (insert json-string)
    (json-pretty-print-buffer)
    (buffer-string)))

(defun polymuse--setup-openai-backend ()
  "Interactively create an OpenAI polymuse backend using gptel."
  (let* ((model (read-string "OpenAI model (e.g. gpt-4o-mini): " "gpt-4o-mini"))
         (gptel-backend (gptel-make-openai "polymuse-openai"))
         (id (intern (format "openai-%s" model)))
         (spec (make-polymuse-openai-backend-spec :id id :model model)))
    (polymuse--initialize-backend spec)
    id))

(defun polymuse--setup-ollama-backend ()
  "Interactively create an Ollama polymuse backend using gptel."
  (let* ((host   (read-string "Ollama host (host:port): " "localhost:11434"))
         (protocol (completing-read "Ollama protocol: "
                                    '("http" "https")
                                    nil t nil nil "https"))
         (models (or (polymuse-ollama-list-models host protocol)
                     (list (read-string "Model name: "))))
         (model  (completing-read "Ollama model: " models nil t))
         (temperature (read-number "Model temperature (0.0 - 2.0): " 0.2))
         (id     (intern (format "ollama-%s" model)))
         (spec   (make-polymuse-ollama-backend-spec :id          id
                                                    :host        host
                                                    :protocol    protocol
                                                    :model       model
                                                    :temperature temperature)))
    (polymuse--initialize-backend spec)
    id))

(cl-defstruct polymuse-review-state
  id
  interval          ;; seconds between runs
  last-run-time     ;; last time this review was run
  last-hash         ;; string hash of buffer contents on last run
  buffer-size-limit ;; max length in chars of suggestion buffer
  output-buffer     ;; suggestion buffer
  instructions      ;; special instructions for this reviewer
  backend           ;; backend to which requests will be sent
  review-context    ;; lines of review context to include
  source-buffer     ;; The buffer under review
  status            ;; 'idle | 'running - tracks whether review is in progress
  request-started-time) ;; timestamp when current request was initiated

(define-error 'polymuse-json-error "Polymuse JSON parse error")

(defun polymuse--extract-json-review (response)
  "Parse RESPONSE as JSON and return the value of the `review' field.

  Signals `polymuse-json-error' if parsing fails or the field is missing."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type  'list)
             (obj              (json-read-from-string response))
             (review           (alist-get 'review obj)))
        (unless review
          (signal 'polymuse-json-error
                  (list "Missing `review' key in JSON response" obj)))
        review)
    (error (signal 'polymuse-json-error
                   (list "Failed to parse JSON from LLM response" err response)))))

(defun polymuse--format-response (response &optional width)
  "Wrap each paragraph in RESPONSE to WIDTH columns, preserving linebreaks."
  (let ((fill-column (or width 80)))
    (with-temp-buffer
      (insert response)
      (goto-char (point-min))
      (while (not (eobp))
        ;; Skip blank lines
        (skip-chars-forward "\n")
        (let ((start (point)))
          (forward-paragraph)
          (fill-region start (point))))
      (buffer-string))))

(defun polymuse-toggle-debug ()
  "Toggle Polymuse debug mode."
  (interactive)
  (setq polymuse-debug (not polymuse-debug)))

(defun polymuse-switch-profile (profile)
  "Switch to a different tool PROFILE."
  (interactive
   (list (intern (completing-read "Tool profile: "
                                  (mapcar (lambda (p) (symbol-name (car p)))
                                          polymuse-tool-profiles)
                                  nil t))))
  (setq polymuse-active-profile profile)
  (message "Switched to profile: %s" profile))

(defun polymuse-show-active-tools ()
  "Display the currently active tools for this buffer."
  (interactive)
  (let* ((tools (polymuse--collect-tools))
         (profile (or polymuse-active-profile
                      (polymuse--default-profile)))
         (buf (get-buffer-create "*Polymuse Active Tools*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Active Profile: %s\n\n" profile))
      (if tools
          (dolist (tool tools)
            (insert (format "- %s (%s)\n  %s\n\n"
                            (polymuse-tool-name tool)
                            (mapconcat #'symbol-name
                                       (polymuse-tool-arguments tool)
                                       ", ")
                            (polymuse-tool-description tool))))
        (insert "No tools active.\n"))
      (help-mode))
    (display-buffer buf)))

(cl-defgeneric polymuse-request-review (backend request &key system callback &allow-other-keys)
  "Execute REQUEST to the given BACKEND, calling CALLBACK upon completion.")

(cl-defmethod polymuse-request-review ((backend polymuse-gptel-backend) request &rest args)
  "Execute REQUEST to the given BACKEND."
  (let* ((system   (plist-get args :system))
         (callback (plist-get args :callback))
         (executor (polymuse-gptel-backend-executor backend))
         (model    (or (plist-get args :model)
                       (polymuse-backend-model backend)))
         (temperature (or (plist-get args :temperature)
                          (polymuse-backend-temperature backend)))
         (handler  (lambda (resp info)
                     (polymuse--debug-log "INFO: %s" info)
                     (polymuse--debug-log "\n\nRESPONSE:\n\n%s\n\nEND RESPONSE\n\n" resp)
                     (when callback (funcall callback resp info))))
         (request-json (json-serialize request)))
    (when polymuse-debug
      (polymuse--debug-log "\n\nREQUEST:\n\n%s\n\nEND REQUEST\n\n"
                           (polymuse--format-json request-json)))
    (let* ((gptel-backend     executor)
           (gptel-model       model)
           (gptel-temperature temperature)
           (result (gptel-request request-json
                     :system   system
                     :callback handler)))
      result)))

(cl-defmethod polymuse-request-review ((backend polymuse-mock-backend) request &rest args)
  "Execute REQUEST to the mock BACKEND, returning a predefined response.

This method is synchronous and immediately calls the callback with the
mock response, making it suitable for testing without network calls."
  (let* ((callback (plist-get args :callback))
         (response-fn (polymuse-mock-backend-response-fn backend))
         (response (if response-fn
                       (funcall response-fn request)
                     "Mock review response for testing.")))
    (when polymuse-debug
      (polymuse--debug-log "\n\nMOCK REQUEST:\n\n%s\n\nEND REQUEST\n\n"
                           (polymuse--format-json (json-serialize request)))
      (polymuse--debug-log "\n\nMOCK RESPONSE:\n\n%s\n\nEND RESPONSE\n\n" response))
    (when callback
      (funcall callback response nil))
    response))

(defun polymuse-find-backend (backend-id)
  "Given a BACKEND-ID, find the associated Polymuse backend."
  (alist-get backend-id polymuse-backends nil nil #'eq))

(defun polymuse-ensure-backend (&optional backend-id)
  "Return a backend for the current buffer, prompting on first use.

  If BACKEND-ID is set, try to find that backend. Otherwise, use the
  buffer-local default or global `polymuse-default-backend', initializing
  it interactively if necessary."
  (or (when backend-id
        (polymuse-find-backend backend-id))
      ;; Buffer-local default
      (when polymuse-default-backend-id
        (polymuse-find-backend polymuse-default-backend-id))
      (let ((id (polymuse--interactive-setup-default-backend)))
        (when id
          (polymuse-find-backend id)))))

(defun polymuse--truncate-buffer-to-length (buffer max-chars)
  "Truncate BUFFER from the top so it is at most MAX-CHARS characters long.

  Deletion is done in whole lines starting at `point-min`.

  If BUFFER is visible in one or more windows, their view is left
  unchanged *unless* we must delete text that is currently visible
  in order to satisfy MAX-CHARS. In that case, Emacs will naturally
  recenter those windows."
  (interactive
   (list (current-buffer)
         (prefix-numeric-value
          (read-number "Max buffer length (chars): " 10000))))
  (when (and (buffer-live-p buffer)
             (integerp max-chars)
             (>= max-chars 0))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (let ((inhibit-read-only t))
            (widen)
            (let* ((size   (buffer-size))
                   (excess (- size max-chars)))
              (when (> excess 0)
                (let ((delete-end (+ (point-min) excess)))
                  (goto-char delete-end)
                  (end-of-line)
                  (setq delete-end (min (point-max) (point)))
                  (when (> delete-end (point-min))
                    (delete-region (point-min) delete-end)))))))))))

(defcustom polymuse-system-prompt
  (string-join
   '("You are a creative assistant providing real-time feedback as the user writes."
     "Offer varied, interesting insights and suggestions. Don't be afraid to explore different angles or possibilities."
     "For prose, focus on what makes the content engaging - characters, plot, voice, emotional impact, and reader experience."
     "For code, balance between catching important issues and suggesting improvements that make things more elegant or clear."
     "Share observations, ideas, and reactions. If something sparks a thought or possibility, share it."
     "Keep responses conversational and under 200 words unless exploring something particularly interesting.")
   " ")
  "Base system prompt for polymuse."
  :type 'string)

(defcustom polymuse-mode-prompts
  '((prog-mode . "Review the code in `focus-region`. Check for: (1) bugs, edge cases, or incorrect logic; (2) potential errors or exceptions not handled; (3) unclear naming or confusing structure; (4) opportunities to simplify or use better patterns/libraries. Ignore trivial style issues.")
    (text-mode . "Read the prose in `focus-region` and share your thoughts. What's working? What could be more engaging? Consider the characters - are they compelling and consistent? Does the pacing feel right? Are there moments that could have more emotional impact? What about the voice and style - does it draw you in? Share ideas, reactions, or possibilities that come to mind. Skip grammar and mechanics unless something truly disrupts the reading experience."))
  "Mode-specific prompt for the polymuse over-the-shoulder assistant."
  :type '(alist :key-type symbol :value-type string))

(defun polymuse--code-mode-p ()
  "Return non-nil when the current major mode is derived from PROG-MODE."
  (derived-mode-p 'prog-mode))

(defun polymuse-get-mode-prompt ()
  "Return the appropriate prompt for the current major mode."
  (or (seq-some (lambda (pair)
                  (when (derived-mode-p (car pair))
                    (cdr pair)))
                polymuse-mode-prompts)
      (cdr (assq 'text-mode polymuse-mode-prompts))))

(defvar-local polymuse--unit-grabber nil
  "Function to grab the unit (paragraph or sexp) around the point.")

(defvar-local polymuse--next-context-grabber nil
  "Function to grab the context following the point, in units.")

(defvar-local polymuse--prev-context-grabber nil
  "Function to grab the context before the point, in units.")

(cl-defstruct polymuse-tool
  "Tool to expose to the LLM, allowing it to make calls back to Emacs."
  name
  function
  description
  arguments)

(defvar-local polymuse-local-tools '()
  "Tools to expose to the LLM, specific to the local buffer.

  Contains an alist of (FUN-NAME . FUN).")

(defvar polymuse-tool-profiles
  '((prose-writing . ())
    (code-review . ())
    (architecture . ())
    (debugging . ()))
  "Named tool profiles for different Polymuse sessions.

Each profile is an alist of (PROFILE-NAME . TOOL-LIST), where TOOL-LIST
is a list of tool symbols to be registered for that profile.")

(defvar-local polymuse-active-profile nil
  "Currently active tool profile for this buffer.

If nil, the profile will be auto-selected based on major mode and
active minor modes.")

(defvar-local polymuse--reviews '()
  "List of all `polymuse-review-state' structs associated with this buffer.")

(defvar-local polymuse-final-instructions ""
  "Instructions to pass at the very end of the prompt.")

(defun polymuse--collect-units-to-limit (limit start get-next)
  "Expand region from START by units until LIMIT chars, using GET-NEXT.

  START is a buffer position.

  GET-NEXT is a function that takes the current region as a cons (BEG . END) and
  should either:

  - Return a *larger* region (NEW-BEG . NEW-END) that strictly grows the old
  one, or
  - Return nil if no further unit is available.

  This returns the *largest* region whose length does not exceed LIMIT
  characters. If GET-NEXT never manages to grow the region without going
  over LIMIT, the result is NIL. Callers can detect this case and fall
  back to a raw substring if needed."
  (let* ((region (cons start start))
         (best   nil) ;; nil = no acceptable region yet
         (best-len 0)
         done)
    (while (not done)
      (let ((next (funcall get-next region)))
        (if (null next)
            (setq done t)
          (let ((beg (car next))
                (end (cdr next)))
            (let ((len (- end beg)))
              (cond
               ;; must strictly grow the previous best; if not, stop
               ((<= len best-len)
                (setq done t))
               ;; too big; stop and keep last best
               ((> len limit)
                (setq done t))
               (t
                (setq region   next
                      best     next
                      best-len len))))))))
    best))

(defun polymuse--make-get-prev-wrapper (move-back-fn)
  "Return a GET-NEXT function that grows a region backward using MOVE-BACK-FN.

  MOVE-BACK-FN should move point to the previous logical unit
  boundary, e.g. `backward-paragraph` or `backward-sexp`."
  (lambda (region)
    (let ((beg (car region))
          (end (cdr region)))
      (save-excursion
        (goto-char beg)
        (condition-case nil
            (progn
              (funcall move-back-fn)
              (let ((new-beg (max (point-min) (point))))
                (when (< new-beg beg)
                  (cons new-beg end))))
          (error nil))))))

(defun polymuse--make-get-next-wrapper (move-fwd-fn)
  "Return a GET-NEXT function that grows a region forward using MOVE-FWD-FN.

  MOVE-FWD-FN should move point to the next logical unit boundary,
  e.g. `forward-paragraph` or `forward-sexp`."
  (lambda (region)
    (let ((beg (car region))
          (end (cdr region)))
      (save-excursion
        (goto-char end)
        (condition-case nil
            (progn
              (funcall move-fwd-fn)
              (let ((new-end (min (point-max) (point))))
                (when (> new-end end)
                  (cons beg new-end))))
          (error nil))))))

(defun polymuse--get-context-before-point-paragraphs (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

  Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (polymuse--collect-units-to-limit limit
                                      (or pt (point))
                                      (polymuse--make-get-prev-wrapper #'backward-paragraph))))

(defun polymuse--get-context-after-point-paragraphs (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

  Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (polymuse--collect-units-to-limit limit
                                      (or pt (point))
                                      (polymuse--make-get-next-wrapper #'forward-paragraph))))

(defun polymuse--get-context-before-point-sexps (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

  Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (polymuse--collect-units-to-limit limit
                                      (or pt (point))
                                      (polymuse--make-get-prev-wrapper #'backward-sexp))))

(defun polymuse--get-context-after-point-sexps (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

  Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (polymuse--collect-units-to-limit limit
                                      (or pt (point))
                                      (polymuse--make-get-next-wrapper #'forward-sexp))))

(defun polymuse--get-unit-wrapper (go-back go-forward &optional buffer)
  "Pull the range of the current unit.

  GO-BACK is a function to go to the start of the current unit.
  GO-FORWARD is a function to go to the end of the current unit.
  If BUFFER is specified, the unit will be pulled from the point in that buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((start) (end))
      (save-excursion
        (funcall go-back)
        (setq start (point))
        (funcall go-forward)
        (setq end (point))
        (cons start end)))))

(defun polymuse--get-unit-sexp (&optional buffer)
  "Return the range of the current code block, from BUFFER or `(current-buffer)'."
  (polymuse--get-unit-wrapper #'beginning-of-defun #'end-of-defun buffer))

(defun polymuse--get-unit-paragraph (&optional buffer)
  "Return the range of the current paragraph, from BUFFER or `(current-buffer)'."
  (polymuse--get-unit-wrapper #'backward-paragraph #'forward-paragraph buffer))

(defun polymuse--handle-llm-response (review response)
  "On LLM response, display RESPONSE in output buffer, and update REVIEW state."
  (condition-case err
      (when (and response
                 (stringp response)
                 (> (length response) 0))
        (polymuse--display-review review response))
    (error (message "Polymuse: error handling response for review %s: %S"
                    (polymuse-review-state-id review) err))))

(defun polymuse--run-review (src-buffer review)
  "Submit review request for SRC-BUFFER to LLM per config in REVIEW."
  (let ((backend (polymuse-review-state-backend review)))
    (unless backend (error "Polymuse: no backend configured, skipping review"))
    (message "Requesting Polymuse review: %s" (polymuse-review-state-id review))
    (with-current-buffer src-buffer
      (let ((old-hash (polymuse-review-state-last-hash review))
            (new-hash (polymuse--buffer-hash)))
        (if (and old-hash (string= old-hash new-hash))
            (when polymuse-debug (message "skipping review, buffer is unchanged"))
          (let ((prompt (polymuse--compose-prompt review)))
            ;; Mark review as running BEFORE making the request
            (setf (polymuse-review-state-status review) 'running)
            (setf (polymuse-review-state-request-started-time review) (float-time))
            (setf (polymuse-review-state-last-run-time review) (float-time))
            (setf (polymuse-review-state-last-hash review) new-hash)
            (polymuse-request-review backend
                                     prompt
                                     :system   polymuse-system-prompt
                                     :callback (lambda (response info)
                                                 ;; Always reset status to idle when request completes
                                                 (unwind-protect
                                                     (polymuse--handle-llm-response review response)
                                                   (setf (polymuse-review-state-status review) 'idle)
                                                   (setf (polymuse-review-state-request-started-time review) nil))))))))))

(defun polymuse--buffer-recently-active-p (buffer review-state)
  "Return t if BUFFER was recently active.

A buffer is considered recently active if it was modified since the last
review, or if it was modified within `polymuse-buffer-activity-timeout' seconds."
  (let* ((last-activity (buffer-local-value 'polymuse--last-activity-time buffer))
         (last-review (polymuse-review-state-last-run-time review-state))
         (now (float-time)))
    (or (not last-activity)  ;; No activity recorded yet, allow review
        (and last-review (> last-activity last-review))  ;; Active since last review
        (< (- now last-activity) polymuse-buffer-activity-timeout))))  ;; Recently active

(defun polymuse--review-stale-p (review-state)
  "Return t if REVIEW-STATE represents a stale (timed-out) review request."
  (let ((status (polymuse-review-state-status review-state))
        (started (polymuse-review-state-request-started-time review-state))
        (now (float-time)))
    (and (eq status 'running)
         started
         (> (- now started) polymuse-review-timeout))))

(defun polymuse--should-review-p (buffer review-state)
  "Return t if BUFFER with REVIEW-STATE should be reviewed now."
  (let ((status (polymuse-review-state-status review-state)))
    (and
     ;; Buffer must be visible in some window (if configured)
     (or (not polymuse-require-buffer-visible)
         (get-buffer-window buffer 'visible))
     ;; Buffer must be recently active
     (polymuse--buffer-recently-active-p buffer review-state)
     ;; Review must not already be running, or must be stale
     (or (not (eq status 'running))
         (polymuse--review-stale-p review-state)))))

(defun polymuse--reset-stale-reviews ()
  "Reset any stale reviews across all buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p polymuse-mode)
          (dolist (review polymuse--reviews)
            (when (polymuse--review-stale-p review)
              (when polymuse-debug
                (message "Resetting stale review: %s" (polymuse-review-state-id review)))
              (setf (polymuse-review-state-status review) 'idle)
              (setf (polymuse-review-state-request-started-time review) nil))))))))

(defun polymuse--global-idle-tick ()
  "Check all muse-enabled buffers and run due reviews."
  (when polymuse-debug  (message "Polymuse: Tick!"))
  ;; First, reset any stale reviews
  (polymuse--reset-stale-reviews)
  ;; Then check for due reviews
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when polymuse-mode
          (dolist (review polymuse--reviews)
            (let* ((interval (polymuse-review-state-interval review))
                   (last-run (or (polymuse-review-state-last-run-time review) 0))
                   (now (float-time)))
              (if (and (> (- now last-run) interval)
                       (polymuse--should-review-p buf review))
                  (polymuse--run-review buf review)
                (when polymuse-debug
                  (message "skipping %s, review too recent or conditions not met"
                           (polymuse-review-state-id review)))))))))))

(defsubst polymuse--valid-region-p (region)
  "Return t if REGION is a valid region cons.

A valid region is a cons of (BEG . END) where both are integers
and BEG <= END."
  (and (consp region)
       (integerp (car region))
       (integerp (cdr region))
       (<= (car region) (cdr region))))

(defsubst polymuse--region-empty-p (region)
  "Return t if REGION represents an empty region.

A region is empty if it is nil or if BEG = END."
  (or (null region)
      (and (consp region)
           (= (car region) (cdr region)))))

(defsubst polymuse--region-length (region)
  "Return the length of REGION (a cons of BEG . END), or 0 if nil."
  (if (and region (consp region))
      (- (cdr region) (car region))
    0))

(defsubst polymuse--region-string (region)
  "Return the buffer substring for REGION, or an empty string if nil."
  (if (and region (consp region))
      (buffer-substring-no-properties (car region) (cdr region))
    ""))

(defun polymuse--valid-prompt-p (prompt)
  "Return t if PROMPT has the required structure.

A valid prompt should be an alist with at least the following keys:
  - review-request
  - environment
  - context
  - current
  - task"
  (and (listp prompt)
       (assq 'review-request prompt)
       (assq 'environment prompt)
       (assq 'context prompt)
       (assq 'current prompt)
       (assq 'task prompt)))

(defun polymuse--valid-context-params-p (params)
  "Return t if PARAMS is a valid context parameters plist.

A valid context params plist should have at least:
  - :mode-prompt
  - :current-unit
  - :major-mode"
  (and (listp params)
       (plist-member params :mode-prompt)
       (plist-member params :current-unit)
       (plist-member params :major-mode)))

(defun polymuse--grab-buffer-tail (buffer n)
  "Return the last N lines from BUFFER as a string."
  (with-current-buffer buffer
    (goto-char (point-max))
    (forward-line (- n))
    (buffer-substring-no-properties (point) (point-max))))

(defun polymuse--debug-log (format-string &rest args)
  "Log debug message to Polymuse debug buffer if debug mode is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when polymuse-debug
    (with-current-buffer (get-buffer-create polymuse-debug-buffer)
      (insert (apply #'format format-string args)))))

(defun polymuse--load-tools ()
  "Load project-specific tools from `polymuse-tools-file', if set."
  (when polymuse-tools-file
    (let* ((base-dir (or (and buffer-file-name
                              (file-name-directory buffer-file-name))
                         default-directory))
           (file (if (file-name-absolute-p polymuse-tools-file)
                     polymuse-tools-file
                   (expand-file-name polymuse-tools-file base-dir))))
      (when (file-readable-p file)
        (load file nil 'nomessage)))))

(defun polymuse--default-profile ()
  "Return the default profile based on current major mode and active modes."
  (cond
   ((and (bound-and-true-p canon-mode)
         (derived-mode-p 'prog-mode))
    'code-review)
   ((bound-and-true-p canon-mode)
    'prose-writing)
   ((derived-mode-p 'prog-mode)
    'code-review)
   ((derived-mode-p 'text-mode)
    'prose-writing)
   (t 'prose-writing)))

(defun polymuse--builtin-tools ()
  "Return built-in tools based on active modes.

Returns a list of `polymuse-tool' structs."
  (let (tools)
    ;; Canon mode provides entity lookup tools
    (when (bound-and-true-p canon-mode)
      (if (derived-mode-p 'prog-mode)
          (setq tools (append tools (polymuse--canon-code-tools)))
        (setq tools (append tools (polymuse--canon-prose-tools)))))
    tools))

(defun polymuse--profile-tools (profile)
  "Return tools for PROFILE.

Returns a list of `polymuse-tool' structs."
  (let ((tool-names (alist-get profile polymuse-tool-profiles)))
    ;; For now, profile tools are empty by default
    ;; Users can add custom tools to profiles
    (delq nil
          (mapcar (lambda (name)
                    (alist-get name polymuse-local-tools))
                  tool-names))))

(defun polymuse--merge-tool-lists (&rest tool-lists)
  "Merge TOOL-LISTS, with later lists taking precedence.

Tools are identified by their name. If multiple tools have the same
name, the last one wins."
  (let ((merged '()))
    (dolist (tool-list tool-lists)
      (dolist (tool tool-list)
        (when (polymuse-tool-p tool)
          (setf (alist-get (polymuse-tool-name tool) merged
                           nil nil #'string=)
                tool))))
    (mapcar #'cdr merged)))

(defun polymuse--collect-tools ()
  "Collect tools from all layers: built-ins, profile, and local.

Returns a list of `polymuse-tool' structs."
  (let* ((builtin-tools (polymuse--builtin-tools))
         (profile (or polymuse-active-profile
                      (polymuse--default-profile)))
         (profile-tools (polymuse--profile-tools profile))
         (local-tools (mapcar #'cdr polymuse-local-tools)))
    (polymuse--merge-tool-lists builtin-tools profile-tools local-tools)))

(defun polymuse--canon-prose-tools ()
  "Return Polymuse tools for prose writing with canon-mode.

These tools allow the LLM to lookup characters, locations, and other
entities, and suggest modifications without clobbering definitions."
  (require 'canon)
  (list
   (make-polymuse-tool
    :name "canon-lookup-entity"
    :function #'canon-tool-lookup-entity
    :description "Look up a character, location, or other entity from the story canon by ID. Returns the full entity definition."
    :arguments '(entity-id))
   (make-polymuse-tool
    :name "canon-list-entities"
    :function #'canon-tool-list-all-entities
    :description "List all entities in the canon organized by type (Characters, Locations, etc). Useful for getting an overview."
    :arguments '())
   (make-polymuse-tool
    :name "canon-search-by-property"
    :function #'canon-tool-search-by-property
    :description "Search for entities by property value. Example: find all characters with 'role=protagonist'. Returns matching entity IDs."
    :arguments '(property value))
   (make-polymuse-tool
    :name "canon-suggest-update"
    :function #'canon-tool-suggest-entity-update
    :description "Suggest a modification to an entity (character, location, etc). The suggestion will be appended to the entity's 'Suggestions' section for the user to review. This does NOT modify the entity directly."
    :arguments '(entity-id suggestion))))

(defun polymuse--canon-code-tools ()
  "Return Polymuse tools for code review with canon-mode.

These tools allow the LLM to access architecture docs, style guides, and
suggest improvements without modifying the canon directly."
  (require 'canon)
  (list
   (make-polymuse-tool
    :name "canon-lookup-doc"
    :function #'canon-tool-lookup-entity
    :description "Look up a documentation entity from the canon by ID (e.g., 'architecture', 'style-guide', 'api-design'). Returns the full document."
    :arguments '(doc-id))
   (make-polymuse-tool
    :name "canon-list-docs"
    :function #'canon-tool-list-all-entities
    :description "List all documentation entities in the canon (architecture, style guides, blueprints, etc)."
    :arguments '())
   (make-polymuse-tool
    :name "canon-suggest-doc-update"
    :function #'canon-tool-suggest-section-update
    :description "Suggest an update to a specific section of a documentation entity. Use this to note inaccuracies or suggest improvements. The suggestion will be added to the 'Suggestions' section for user review. This does NOT modify the document directly."
    :arguments '(doc-id section-name suggestion))))

(defun polymuse--format-tools-prompt ()
  "Format tools prompt for LLM from all tool sources."
  (let ((tools (polymuse--collect-tools)))
    (vconcat
     (mapcar (lambda (tool)
               `((tool-name        . ,(polymuse-tool-name tool))
                 (tool-args        . ,(vconcat (mapcar #'symbol-name (polymuse-tool-arguments tool))))
                 (tool-description . ,(polymuse-tool-description tool))))
             tools))))

(defun polymuse--calculate-context-regions (max-chars mode-prompt local-prompt current-unit)
  "Calculate forward and backward context regions within budget.

MAX-CHARS is the total character budget.
MODE-PROMPT and LOCAL-PROMPT are the base prompts.
CURRENT-UNIT is the cons (BEG . END) of the current focus region.

Returns a plist with :forward-context and :backward-context regions.

For small context models (~8K), this allocates context as:
- 25% for response space (critical!)
- 60% backward context (typically more relevant)
- 15% forward context (useful but less critical)"
  (let* ((prompt-size   (+ (length mode-prompt)
                           (length local-prompt)
                           200))  ; overhead for JSON structure
         (current-len   (polymuse--region-length current-unit))
         ;; Reserve space for model response to avoid truncation
         (available     (- max-chars prompt-size current-len))
         (response-reserve (floor (* available polymuse-response-reserve-ratio)))
         (context-space (max 0 (- available response-reserve)))
         ;; Allocate to backward context per configuration
         (backward-budget (max 0 (floor (* context-space polymuse-context-backward-ratio))))
         (backward-context (and (> backward-budget 0)
                                (funcall polymuse--prev-context-grabber
                                         backward-budget
                                         nil
                                         (and current-unit (car current-unit)))))
         (backward-len    (polymuse--region-length backward-context))
         ;; Give remaining space to forward context
         (forward-budget  (max 0 (- context-space backward-len)))
         (forward-context (and (> forward-budget 0)
                               (funcall polymuse--next-context-grabber
                                        forward-budget
                                        nil
                                        (and current-unit (cdr current-unit))))))
    (list :forward-context forward-context
          :backward-context backward-context)))

(defun polymuse--compose-prompt-from-context (review-params)
  "Generate a prompt from explicit REVIEW-PARAMS.

This is a pure function that constructs the prompt from explicit parameters,
making it easy to test without buffer-local state. REVIEW-PARAMS should be
a plist containing:
  :mode-prompt          Mode-specific prompt string
  :local-prompt         Buffer-specific instructions
  :tools-prompt         Vector of tool definitions
  :backward-context     String of context before point
  :forward-context      String of context after point
  :current-unit         String of the current focus region
  :previous-review      String of previous review (optional)
  :major-mode           Symbol of the major mode
  :include-previous-review Boolean
  :final-instructions   String of final instructions (optional)"
  (let ((mode-prompt (plist-get review-params :mode-prompt))
        (local-prompt (plist-get review-params :local-prompt))
        (tools-prompt (plist-get review-params :tools-prompt))
        (backward-context (plist-get review-params :backward-context))
        (forward-context (plist-get review-params :forward-context))
        (current-unit (plist-get review-params :current-unit))
        (previous-review (plist-get review-params :previous-review))
        (major-mode-sym (plist-get review-params :major-mode))
        (include-previous-review (plist-get review-params :include-previous-review))
        (final-instructions (plist-get review-params :final-instructions)))
    `((review-request
       . ((mode-prompt       . ,mode-prompt)
          (buffer-prompt     . ,local-prompt)
          ,@(when tools-prompt
              (list `(tools . ((instructions
                                . "If you need more info, respond with JSON ONLY: {\"action\":\"tool-call\",\"tool\":\"<name>\",\"arguments\":{\"arg\":\"value\"}}. You'll receive the result and can then provide your review.")
                               (tool-list . ,tools-prompt)))))))
      (environment
       . ((major_mode . ,(symbol-name major-mode-sym))))
      (context
       . ((backward-context . ,backward-context)
          (forward-context  . ,forward-context)
          ,@(when include-previous-review
              (list `(previous-review . ,previous-review)))))
      (current
       . ((focus-region . ,current-unit)))
      (task
       . ((focus   . "Review `focus-region` acording tho the instructions in `buffer-prompt`, use `backward-context` and `forward-context` for reference only.")
          ,@(when include-previous-review
              (list '(previous . "Don't repeat points from `previous-review`; offer new insights only.")))
          ,@(when final-instructions
              (list `(instructions . ,final-instructions))))))))

(defun polymuse--compose-prompt (review)
  "Generate the full prompt for REVIEW, for use with the PolyMuse backend LLM."
  (let* ((mode-prompt   (or (polymuse-get-mode-prompt) ""))
         (local-prompt  (or (polymuse-review-state-instructions review) ""))
         (max-chars     (or polymuse-max-prompt-characters most-positive-fixnum))
         (tools-prompt  (polymuse--format-tools-prompt))
         (current-unit  (funcall polymuse--unit-grabber))
         (context-regions (polymuse--calculate-context-regions
                           max-chars mode-prompt local-prompt current-unit))
         (forward-context  (plist-get context-regions :forward-context))
         (backward-context (plist-get context-regions :backward-context))
         (out-buffer            (polymuse-review-state-output-buffer review))
         (previous-review-lines (polymuse-review-state-review-context review))
         (previous-review       (polymuse--grab-buffer-tail out-buffer previous-review-lines)))
    (polymuse--compose-prompt-from-context
     (list :mode-prompt mode-prompt
           :local-prompt local-prompt
           :tools-prompt tools-prompt
           :backward-context (polymuse--region-string backward-context)
           :forward-context (polymuse--region-string forward-context)
           :current-unit (polymuse--region-string current-unit)
           :previous-review previous-review
           :major-mode major-mode
           :include-previous-review polymuse-include-previous-review
           :final-instructions polymuse-final-instructions))))

(defun polymuse--reviewed-buffer-p (&optional buffer)
  "Return t if a BUFFER is a buffer under review."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p polymuse-mode)))

(defun polymuse--review-output-buffer-p (&optional buffer)
  "Return t if a BUFFER is the output buffer of a Polymuse review."
  (with-current-buffer (or buffer (current-buffer))
    (eq major-mode 'polymuse-suggestions-mode)))

(defun polymuse--erase-buffer (buffer)
  "Clear the contents of BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun polymuse--select-review (buffer)
  "Given a BUFFER under review, interactively select and return the ID of a review."
  (with-current-buffer buffer
    (if (polymuse--review-output-buffer-p buffer)
        polymuse--buffer-review-state
      (let ((reviews polymuse--reviews))
        (pcase (length reviews)
          (0 (user-error "No reviewers configured for this buffer"))
          (1 (car reviews))
          (_ (let* ((choices   (mapcar (lambda (state)
                                         (cons (polymuse-review-state-id state) state))
                                       reviews))
                    (review-id (completing-read "Edit instructions for reviewer: "
                                                (mapcar #'car choices) nil t)))
               (cdr (assoc review-id choices)))))))))

(defun polymuse-reset-output (&optional buffer)
  "Reset the review output of BUFFER. Query for review if there's more than one."
  (interactive)
  (let* ((buff (or buffer (current-buffer)))
         (review (polymuse--select-review buff)))
    (polymuse--erase-buffer (polymuse-review-state-output-buffer review))))

(defun polymuse-run-review (&optional buffer)
  "Select and run a Polymuse review on BUFFER."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (review (polymuse--select-review buf))
         (target (polymuse-review-state-source-buffer review)))
    (polymuse--run-review target review)))

(defun polymuse--open-review (review)
  "Open Polymuse REVIEW in a window."
  (pop-to-buffer (polymuse-review-state-output-buffer review)))

(defun polymuse-open-review ()
  "Open Polymuse review in a window. If there's more than one, open selector."
  (interactive)
  (let* ((buf (current-buffer))
         (review (polymuse--select-review buf)))
    (polymuse--open-review review)))

(defun polymuse-edit-instructions ()
  "Edit the buffer-specific instructions for Polymuse reviewer.

  If called from a buffer under review, look at `polymuse-reviews':
  - If there's exactly one review, edit that one.
  - If there are more than one, prompt the user to select which to edit."
  (interactive)
  (let* ((buf (current-buffer))
         (review (polymuse--select-review buf)))
    (polymuse--edit-review-instructions review)))

(defvar-local polymuse--buffer-review-state nil
  "The `polymuse-review-state' struct associated with a review buffer.")

(defun polymuse--edit-review-instructions (review)
  "Given a `polymuse-review-state' REVIEW, let the user modify and save the review instructions."
  (let ((buf (get-buffer-create (format "*polymuse prompt:%s*"
                                        (polymuse-review-state-id review))))
        (instructions (or (polymuse-review-state-instructions review) "")))
    (with-current-buffer buf
      (erase-buffer)
      (polymuse-instructions-mode)
      (insert instructions)
      (goto-char (point-min))
      (setq-local polymuse--buffer-review-state review))
    (pop-to-buffer buf)))

(defun polymuse-save-instructions ()
  "Save an edited reviewer prompt to the associated `polymuse-review-state' struct."
  (interactive)
  (unless (eq major-mode 'polymuse-instructions-mode)
    (user-error "This command must be called from a polymuse instructions buffer"))
  (let* ((state        (buffer-local-value 'polymuse--buffer-review-state (current-buffer)))
         (reviewer-id  (polymuse-review-state-id state))
         (instructions (buffer-substring-no-properties (point-min) (point-max))))
    (setf (polymuse-review-state-instructions state) instructions)
    (quit-window 'kill)
    (message "Local prompt for reviewer %s updated." reviewer-id)))

(defun polymuse-close-instructions ()
  "Close an edited reviewer prompt without saving."
  (interactive)
  (unless (eq major-mode 'polymuse-instructions-mode)
    (user-error "This command must be called from a polymuse instructions buffer"))
  (quit-window 'kill))

(define-derived-mode polymuse-suggestions-mode markdown-mode " Review"
  "Mode for displaying AI suggestions from Polymuse."
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(define-key polymuse-suggestions-mode-map
            (kbd "C-c C-r e") #'polymuse-edit-instructions)
(define-key polymuse-suggestions-mode-map
            (kbd "C-c C-r t") #'polymuse-reset-output)

(define-derived-mode polymuse-instructions-mode markdown-mode " Prompt"
  "Mode for editing AI instruction prompt for Polymuse reviewer.")

(define-key polymuse-instructions-mode-map
            (kbd "C-c C-c") #'polymuse-save-instructions)
(define-key polymuse-instructions-mode-map
            (kbd "C-c C-d") #'polymuse-close-instructions)

(defcustom polymuse-default-interval 180
  "Default idle time (in seconds) between Polymuse reviews."
  :type 'number)

(defun polymuse--update-activity-time ()
  "Update the last activity timestamp for the current buffer."
  (setq polymuse--last-activity-time (float-time)))

(defun polymuse--enable ()
  "Enable the Polymuse LLM live review mode."
  (let* ((state (polymuse--get-global-state))
         (scheduler (or (polymuse-global-state-scheduler state)
                        (polymuse--create-timer-scheduler))))
    (unless (polymuse-global-state-timer state)
      (setf (polymuse-global-state-timer state)
            (funcall (polymuse-scheduler-start-fn scheduler)
                     polymuse-default-interval
                     #'polymuse--global-idle-tick))))
  (if (polymuse--code-mode-p)
      (setq polymuse--unit-grabber #'polymuse--get-unit-sexp
            polymuse--prev-context-grabber #'polymuse--get-context-before-point-sexps
            polymuse--next-context-grabber #'polymuse--get-context-after-point-sexps)
    (setq polymuse--unit-grabber #'polymuse--get-unit-paragraph
          polymuse--prev-context-grabber #'polymuse--get-context-before-point-paragraphs
          polymuse--next-context-grabber #'polymuse--get-context-after-point-paragraphs))
  ;; Track buffer activity for intelligent review scheduling
  (polymuse--update-activity-time)
  (add-hook 'after-change-functions
            (lambda (&rest _) (polymuse--update-activity-time))
            nil t)
  (polymuse--load-tools))

(defun polymuse--disable ()
  "Disable the Polymuse LLM live review mode."
  (let* ((state (polymuse--get-global-state))
         (scheduler (or (polymuse-global-state-scheduler state)
                        (polymuse--create-timer-scheduler))))
    (when (and (polymuse-global-state-timer state)
               (not (cl-some (lambda (b) (with-current-buffer b polymuse-mode))
                             (buffer-list))))
      (funcall (polymuse-scheduler-stop-fn scheduler)
               (polymuse-global-state-timer state))
      (setf (polymuse-global-state-timer state) nil))))

(defun polymuse--buffer-hash ()
  "Generate a hash for the current buffer, to detect differences."
  (secure-hash 'sha1 (current-buffer)))

(defun polymuse--format-review-for-display (response &optional timestamp)
  "Format RESPONSE for display with TIMESTAMP.

This is a pure function that returns the formatted string without
performing any I/O. If TIMESTAMP is nil, uses the current time.

Returns a string suitable for appending to a review buffer."
  (concat "\n\n>>> "
          (format-time-string "%H:%M:%S" timestamp)
          "\n\n"
          response))

(defun polymuse--display-review (review response)
  "Display the review in RESPONSE according to the state in REVIEW."
  (let ((outbuf (polymuse-review-state-output-buffer review)))
    (if (not (buffer-live-p outbuf))
        (error "Output buffer %s for review %s is not live"
               outbuf (polymuse-review-state-id review))
      (let ((formatted-response (polymuse--format-review-for-display response)))
        (with-current-buffer outbuf
          (let ((inhibit-read-only t))
            (polymuse-suggestions-mode)
            (polymuse--truncate-buffer-to-length
             (current-buffer)
             (polymuse-review-state-buffer-size-limit review)))
          (typewrite-enqueue-job formatted-response outbuf
                                 :inhibit-read-only t
                                 :follow t
                                 :cps 45))))))

(defun polymuse--kill-reviewer (buffer review)
  "Remove a REVIEW from BUFFER, and delete its output buffer."
  (with-current-buffer buffer
    (unless (and (boundp 'polymuse--reviews)
                 polymuse--reviews)
      (user-error "No active Polymuse reviewers configured for this buffer"))
    (kill-buffer (polymuse-review-state-output-buffer review))
    (setq polymuse--reviews
          (cl-remove (polymuse-review-state-id review)
                     polymuse--reviews
                     :key  #'polymuse-review-state-id
                     :test #'string=))))

(defun polymuse-kill-all-reviewers (&optional buffer)
  "Remove all reviewers from BUFFER, and delete their output buffers."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (and (boundp 'polymuse--reviews)
                 polymuse--reviews)
      (user-error "No active Polymuse reviewers configured for this buffer"))
    (dolist (review polymuse--reviews)
      (kill-buffer (polymuse-review-state-output-buffer review)))
    (setq polymuse--reviews '())
    (message "Killed all polymuse reviewers.")))

(defun polymuse-kill-reviewer (&optional buffer)
  "Select and kill a reviewer associated with BUFFER."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (review (polymuse--select-review buf))
         (parent (polymuse-review-state-source-buffer review)))
    (polymuse--kill-reviewer parent review)
    (message "Killed polymuse reviewer: %s" (polymuse-review-state-id review))))

(cl-defun polymuse-add-reviewer
    (&key id
          backend
          instructions
          (interval polymuse-default-interval)
          (buffer-size-limit polymuse-default-buffer-size-limit)
          (review-context polymuse-default-review-context-lines)
          out-buffer)
  "Create a new Polymuse review buffer for the current buffer.

  ID is the identifier for this review, and will be generated if not provided.
  BACKEND is the backend (LLM) to which requests will be sent for review.
  INSTRUCTIONS are instructions specifically for this reviewer.
  INTERVAL is the time in seconds between review requests. A default will be
    used if none is provided.
  BUFFER-SIZE-LIMIT is the size limit of the review buffer. It will be truncated
    if it grows too large. A default will be used if none in provided.
  OUT-BUFFER is the buffer where reviews will be posted. A new buffer will be
    created if none is provided.
  REVIEW-CONTEXT is the number of lines of earlier review to include in the
    prompt."
  (interactive)
  (let* ((id (or id (format "polymuse-%s-%d" (buffer-name) (float-time))))
         (backend (or backend (polymuse-find-backend (polymuse--interactive-setup-backend))))
         (suggestions-buf (or out-buffer
                              (generate-new-buffer (format "*polymuse:%s*" id))))
         (local-instructions (or instructions
                                 (read-string "Review instructions (empty for none): ")))
         (state (make-polymuse-review-state
                 :id                 id
                 :interval           interval
                 :last-run-time      nil
                 :last-hash          nil
                 :buffer-size-limit  buffer-size-limit
                 :output-buffer      suggestions-buf
                 :instructions       (unless (string-empty-p local-instructions)
                                       local-instructions)
                 :backend            backend
                 :review-context     review-context
                 :source-buffer      (current-buffer)
                 :status             'idle
                 :request-started-time nil)))
    (push state polymuse--reviews)
    (polymuse--enable)
    (message "Polymuse review %s created with backend %s; suggestions in %s"
             id (polymuse-backend-id backend) (buffer-name suggestions-buf))
    (display-buffer suggestions-buf
                    '((display-buffer-in-side-window)
                      (side . right)
                      (slot . 0)
                      (window-width . 0.33)))
    (polymuse--run-review (current-buffer) state)
    state))

(cl-defun polymuse-add-default-reviewer
    (&key id
          instructions
          (interval polymuse-default-interval)
          (buffer-size-limit polymuse-default-buffer-size-limit)
          (review-context polymuse-default-review-context-lines)
          out-buffer)
  "Create a new Polymuse review buffer for the current buffer.

ID is the identifier for this review, and will be generated if not provided.
BACKEND is the backend (LLM) to which requests will be sent for review.
INSTRUCTIONS are instructions specifically for this reviewer.
INTERVAL is the time in seconds between review requests. A default will be used
  if none is provided.
BUFFER-SIZE-LIMIT is the size limit of the review buffer. It will be truncated
  if it grows too large. A default will be used if none in provided.
OUT-BUFFER is the buffer where reviews will be posted. A new buffer will be
  created if none is provided.
REVIEW-CONTEXT is the number of lines of earlier review to include in the
  prompt."
  (interactive)
  (let* ((id (or id (format "polymuse-%s-%d" (buffer-name) (float-time))))
         (backend (polymuse-ensure-backend))
         (suggestions-buf (or out-buffer
                              (generate-new-buffer (format "*polymuse:%s*" id))))
         (local-instructions (or instructions
                                 (read-string "Review instructions (empty for none): ")))
         (state (make-polymuse-review-state
                 :id                 id
                 :interval           interval
                 :last-run-time      nil
                 :last-hash          nil
                 :buffer-size-limit  buffer-size-limit
                 :output-buffer      suggestions-buf
                 :instructions       (unless (string-empty-p local-instructions)
                                       local-instructions)
                 :backend            backend
                 :review-context     review-context
                 :source-buffer      (current-buffer)
                 :status             'idle
                 :request-started-time nil)))
    (push state polymuse--reviews)
    (polymuse--enable)
    (message "Polymuse review %s created with backend %s; suggestions in %s"
             id (polymuse-backend-id backend) (buffer-name suggestions-buf))
    (display-buffer suggestions-buf
                    '((display-buffer-in-side-window)
                      (side . right)
                      (slot . 0)
                      (window-width . 0.33)))
    (polymuse--run-review (current-buffer) state)
    state))

;;;;
;; Test Helpers
;;;;

(defun polymuse-create-mock-backend (&optional response-fn)
  "Create a mock backend for testing.

RESPONSE-FN, if provided, should be a function that takes a request
and returns a response string. If nil, a default test response is used.

Example:
  (let ((backend (polymuse-create-mock-backend
                   (lambda (req) \"Custom test response\"))))
    (polymuse-request-review backend
                             '((test . prompt))
                             :callback (lambda (resp info)
                                        (message \"Got: %s\" resp))))"
  (make-polymuse-mock-backend
   :id 'test-backend
   :model "mock-model"
   :temperature 0.0
   :response-fn response-fn))

(defmacro polymuse-test-with-isolated-state (&rest body)
  "Execute BODY with isolated Polymuse global state.

This creates a fresh state container for the duration of BODY,
preventing tests from interfering with each other or with global state.
Uses an immediate scheduler so tests don't depend on timers.

Example:
  (polymuse-test-with-isolated-state
    (with-temp-buffer
      (polymuse-mode 1)
      ;; Test polymuse functionality without affecting global state
      ...))"
  (declare (indent 0))
  `(let ((polymuse--test-global-state
          (make-polymuse-global-state
           :scheduler (polymuse--create-immediate-scheduler))))
     ,@body))

(defun polymuse-test-fixture-simple-context ()
  "Create a simple test context for prompt composition testing.

Returns a plist suitable for passing to
`polymuse--compose-prompt-from-context'."
  (list :mode-prompt "Provide code review feedback."
        :local-prompt "Be concise and actionable."
        :tools-prompt nil
        :backward-context "Previous code context here."
        :forward-context "Following code context here."
        :current-unit "(defun example-function (x)\n  (+ x 1))"
        :previous-review nil
        :major-mode 'emacs-lisp-mode
        :include-previous-review nil
        :final-instructions nil))

(defun polymuse-test-fixture-context-with-tools ()
  "Create a test context with tools for prompt composition testing.

Returns a plist with sample tool definitions included."
  (let ((base-context (polymuse-test-fixture-simple-context)))
    (plist-put base-context :tools-prompt
               (vector
                '((tool-name . "lookup-doc")
                  (tool-args . ["doc-id"])
                  (tool-description . "Look up documentation by ID"))))
    base-context))

(defun polymuse-test-create-mock-review-state (&optional backend)
  "Create a mock review state for testing.

BACKEND, if provided, should be a backend instance. Otherwise, creates
a mock backend with default responses.

Returns a `polymuse-review-state' struct suitable for testing.
The caller is responsible for cleaning up the output buffer."
  (let* ((test-backend (or backend (polymuse-create-mock-backend)))
         (output-buffer (generate-new-buffer " *polymuse-test-output*")))
    (make-polymuse-review-state
     :id "test-review"
     :interval 60
     :last-run-time nil
     :last-hash nil
     :buffer-size-limit 10000
     :output-buffer output-buffer
     :instructions "Test instructions"
     :backend test-backend
     :review-context 10
     :source-buffer (current-buffer)
     :status 'idle
     :request-started-time nil)))

(defmacro polymuse-test-with-mock-review (&rest body)
  "Execute BODY with a mock review state.

Within BODY, the variable `test-review' is bound to a mock review state
and `test-backend' is bound to a mock backend.

Example:
  (polymuse-test-with-mock-review
    (should (eq (polymuse-review-state-status test-review) 'idle))
    (polymuse-request-review test-backend
                             '((test . prompt))
                             :callback (lambda (resp info)
                                        (should (stringp resp)))))"
  (declare (indent 0))
  `(polymuse-test-with-isolated-state
     (with-temp-buffer
       (let* ((test-backend (polymuse-create-mock-backend))
              (test-review (polymuse-test-create-mock-review-state test-backend)))
         (unwind-protect
             (progn ,@body)
           (when-let ((buf (polymuse-review-state-output-buffer test-review)))
             (when (buffer-live-p buf)
               (kill-buffer buf))))))))

(provide 'polymuse)
;;; polymuse.el ends here
