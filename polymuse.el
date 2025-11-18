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
;; Package-Requires: ((emacs "29.3") (gptel "0.9"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'uri)
(require 'json)

(define-minor-mode polymuse-mode
  "LLM over-the-sholder assistant."
  :init-value nil
  :lighter "PolyMuse"
  :keymap (make-sparse-keymap)
  (if polymuse-mode
      (polymuse-mode--enable)
    (polymuse-mode--disable)))

(defgroup polymuse nil
  "LLM over-the-shoulder assistant."
  :group 'applications)

(defcustom polymuse-backend nil
  "`gptel' backend used by polymuse."
  :type 'symbol)

(defcustom polymuse-ollama-hostname nil
  "Ollama backend host for use with polymuse."
  :type 'string)

(defcustom polymuse-ollama-protocol "http"
  "Protocol with which to interact with polymuse."
  :type 'string)

(defcustom polymuse-ollama-models '()
  "Ollama models for use with polymuse."
  :type '(string))

(defcustom polymuse--max-prompt-characters 18000
  "Maximum length of a prompt, in characters."
  :type 'integer)

(defcustom polymuse-default-buffer-size-limit 8196
  "Default maximum size for Polymuse review buffers.

When the buffer grows larger than this, the beginning will be truncated."
  :type 'integer)

(defcustom polymuse-idle-seconds 60
  "Default delay in seconds between Polymuse review requests."
  :type 'number)

(defvar polymuse--idle-timer nil
  "Driver timer for active Polymuse reviewers.")

;;;;
;; BACKENDS
;;;;

(defvar polymuse-backends '()
  "Alist of (ID . BACKEND) pairs of Polymuse backends.")

(cl-defstruct polymuse-backend-spec
  id
  model)

(cl-defstruct (polymuse-gptel-backend-spec (:include polymuse-backend-spec))
  host
  protocol)

(cl-defstruct polymuse-backend
  id      ;; unique id for this backend
  model)  ;; model name string

(cl-defstruct (polymuse-gptel-backend (:include polymuse-backend))
  executor)

(cl-defgeneric polymuse--initialize-backend (backend)
  "Given a Polymuse BACKEND spec, initialize it.")

(defun polymuse-create-ollama-executor (name host model &optional protocol)
  "Set up Polymuse Ollama backend NAME for HOST with MODEL over PROTOCOL."
  (gptel-make-ollama name
                     :host     host
                     :protocol (or protocol "http")
                     :models   (list model)))

(cl-defmethod polymuse--initialize-backend ((backend-spec polymuse-gptel-backend-spec))
  "Instantiate gptel BACKEND-SPEC."
  (let ((id       (polymuse-backend-spec-id backend-spec))
        (existing (alist-get id polymuse-backends nil nil #'equal)))
    (if existing
        existing
      (let* ((model    (polymuse-backend-spec-model backend-spec))
             (host     (polymuse-gptel-backend-spec-host backend-spec))
             (protocol (polymuse-gptel-backend-spec-protocol backend-spec))
             (backend  (make-polymuse-gptel-backend
                        :id       id
                        :model    model
                        :executor (polymuse-create-ollama-executor id host model protocol))))
        (setf (alist-get id polymuse-backends nil nil #'equal)
              backend)
        backend))))

(defcustom polymuse-default-backend nil
  "ID of default backend for Polymuse."
  :type 'string)

(defun polymuse--interactive-setup-default-backend ()
  "Interactively create a default Polymuse backend, returning it.

Currently defaults to Ollama and prompts for host + model."
  (let* ((backend-type (completing-read
                        "Polymuse backend type: "
                        '("Ollama" "OpenAI")
                        nil t nil nil "Ollama")))
    (pcase backend-type
      ("Ollama" (setq polymuse-default-backend (polymuse--setup-ollama-backend)))
      ("OpenAI" (setq polymuse-default-backend (polymuse--setup-openai-backend))))))

(defun polymuse-ollama-list-models (host)
  "Return a list of available Ollama model names from HOST.

HOST should be like \"localhost:11434\"."
  (let* ((url-request-method "GET")
         (url (format "http://%s/api/tags" host))
         (buf (url-retrieve-synchronously url t t 5))) ;; 5s timeout
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let* ((json (json-parse-buffer
                            :object-type 'alist
                            :array-type 'list
                            :null-object nil
                            :false-object nil))
                     (models (alist-get 'models json)))
                (mapcar (lambda (m) (alist-get 'name m))
                        models)))
            (kill-buffer buf))))))

(defun polymuse--setup-ollama-backend ()
  "Interactively create an Ollama polymuse backend using gptel."
  (let* ((host   (read-string "Ollama host (host:port): " "localhost:11434"))
         (protocol (completing-read "Ollama protocol: "
                                    '("http" "https")
                                    nil t nil nil "http"))
         (models (or (polymuse-ollama-list-models host)
                     (list (read-string "Model name: "))))
         (model  (completing-read "Ollama model: " models nil t))
         (id     (format "ollama-%s" model))
         (spec   (make-polymuse-gptel-backend-spec :id       id
                                                   :host     host
                                                   :protocol protocol
                                                   :model    model)))
    (polymuse--initialize-backend spec)
    id))

(cl-defstruct polymuse-review-state
  id
  interval          ;; seconds between runs
  last-run-time     ;; last time this review was run
  last-hash         ;; string hash of buffer contents on last run
  buffer-size-limit ;; max length in chars of suggestion buffer
  output-buffer     ;; suggestion buffer
  backend)           ;; backend to which requests will be sent

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

(cl-defgeneric polymuse-request-review (backend request &key system callback &allow-other-keys)
  "Execute REQUEST to the given BACKEND, calling CALLBACK upon completion.")

(cl-defmethod polymuse-request-review ((backend polymuse-gptel-backend) request &rest args)
  "Execute REQUEST to the given BACKEND."
  (let* ((system   (plist-get args :system))
         (callback (plist-get args :callback))
         (executor (polymuse-gptel-backend-executor backend))
         (model    (or (plist-get args :model)
                       (polymuse-backend-model backend))))
    (gptel-request request
      :system   system
      :backend  executor
      :model    model
      :callback (lambda (resp info)
                  (let ((review (polymuse--extract-json-review resp)))
                    (when callback (funcall callback review info)))))))

(defun polymuse-ensure-backend (&optional backend-id)
  "Return a backend for the current buffer, prompting on first use.

If BACKEND-ID is set, try to find that backend. Otherwise, use the buffer-local
default or global `polymuse-default-backend', initializing it interactively if
necessary."
  (or (when backend-id
        (polymuse-find-backend backend-id))
      ;; Buffer-local default
      (when polymuse-default-backend-id
        (polymuse-find-backend polymuse-default-backend-id))
      polymuse-default-backend
      (let ((backend (polymuse--interactive-setup-default-backend)))
        (setq polymuse-default-backend backend)
        (push backend polymuse-backends)
        (setq polymuse-default-backend-id (polymuse-backend-id backend))
        backend)))

(defun polymuse-find-backend (backend-id)
  "Find backend with given BACKEND-ID among buffer-local backends or global default."
  (or (seq-find (lambda (b) (eq (polymuse-backend-id b) backend-id))
                polymuse-backends)
      (and polymuse-default-backend
           (eq (polymuse-backend-id polymuse-default-backend) backend-id)
           polymuse-default-backend)))

(defun truncate-buffer-to-length (buffer max-chars)
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
      (when buffer-read-only
        (user-error "Buffer %s is read-only" (buffer-name)))
      (save-excursion
        (save-restriction
          (widen)
          (let* ((size   (buffer-size))
                 (excess (- size max-chars)))
            (when (> excess 0)
              ;; Where are we allowed to delete without changing
              ;; what’s currently on-screen?
              (let* ((wins (get-buffer-window-list (current-buffer) nil t))
                     (min-start (when wins
                                  (apply #'min
                                         (mapcar (lambda (w)
                                                   (marker-position
                                                    (window-start w)))
                                                 wins))))
                     ;; Max buffer position we can delete up to without
                     ;; touching visible text.
                     (safe-limit (when min-start
                                   (1- min-start)))
                     ;; We *need* to delete EXCESS chars to meet the limit.
                     ;; First try to keep deletion entirely before the first
                     ;; visible char; if that’s impossible, we accept that
                     ;; visible text will be truncated.
                     (delete-end (if (and safe-limit
                                          (<= (+ (point-min) excess)
                                              safe-limit))
                                     (+ (point-min) excess)
                                   (+ (point-min) excess))))
                ;; Extend to end of line (we remove whole lines).
                (goto-char delete-end)
                (end-of-line)
                (setq delete-end (min (point-max) (point)))
                (when (> delete-end (point-min))
                  (delete-region (point-min) delete-end)))))))))))

;; (defun polymuse--setup-openai-backend ()
;;   "Interactively create an OpenAI polymuse backend using gptel."
;;   (require 'gptel)
;;   (let* ((model (read-string "OpenAI model (e.g. gpt-4.1-mini): " "gpt-4.1-mini"))
;;          (gptel-backend (gptel-make-openai "polymuse-openai")))
;;     (make-polymuse-backend
;;      :id :openai-default
;;      :provider :openai
;;      :gptel-backend gptel-backend
;;      :model model)))

(defcustom polymuse-model nil
  "Model to use with polymuse."
  :type 'string)

(defcustom polymuse-system-prompt
  (string-join " "
               '("You are an over-the-shoulder reviewer."
                 "Provide feedback on the content provided,"
                 "aimed to guide and advice the writer."
                 "Be friendly and polite. Give specific examples."
                 "Reply in STRICT JSON, providing the key `review',"
                 "like this:"
                 "{ \"review\": \"<your textual feedback here>\" }")
               "Base system prompt for polymuse."
               :type 'string))

(defcustom polymuse-mode-prompts
  '((prog-mode . "Provide feedback on the code below. Focus on clarity, reuse, improvements, library suggestions and general quality.")
    (text-mode . "Provide feedback on the prose content below. Suggest style improvements for clarity, layout suggestions, vocabulary improvements, plot suggetions, continuity fixes, character motivations, etc."))
  "Mode-specific prompt for the polymuse over-the-shoulder assistant."
  :type '(alist :key-type symbol :value-type string))

(defun polymuse-get-mode-prompt ()
  "Return the appropriate prompt for the current major mode."
  (or (seq-some (lambda (pair)
                  (when (derived-mode-p (car pair))
                    (cdr pair)))
                polymuse-mode-prompts)
      (cdr (assq 'text-mode polymuse-mode-prompts))))

(defvar-local polymuse-local-prompt nil
  "Optional buffer-local override of the prompt for polymuse.")

(defvar-local polymuse--unit-grabber nil
  "Function to grab the 'unit' (paragraph or sexp) around the point.")

(defvar-local polymuse--next-context-grabber nil
  "Function to grab the context following the point, in units.")

(defvar-local polymuse--prev-context-grabber nil
  "Function to grab the context before the point, in units.")

(cl-defstruct polymuse-tool
  "Tool to expose to the LLM, allowing it to make calls back to Emacs."
  function
  description
  arguments)

(defvar-local polymuse-local-tools '()
  "Tools to expose to the LLM, specific to the local buffer. Should be a list of POLYMUSE-TOOL.")

(defvar-local polymuse--reviews '()
  "List of all `polymuse-review-state' structs associated with this buffer.")

(defvar-local polymuse-final-instruction ""
  "Instructions to pass at the very end of the prompt.")

(defun polymuse--collect-units-to-limit (limit start get-next)
  "Expand region from START by units until LIMIT chars, using GET-NEXT.

START is a buffer position.

GET-NEXT is a function that takes the current region as a cons (BEG . END) and
should either:

  - Return a *larger* region (NEW-BEG . NEW-END) that strictly grows the old
    one, or
  - Return nil if no further unit is available.

This returns the *largest* region whose length does not exceed LIMIT characters.
If GET-NEXT never manages to grow the region without going over LIMIT, the result
is NIL. Callers can detect this case and fall back to a raw substring if needed."
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
    (when-let* ((range
                 (polymuse--collect-units-to-limit limit (or pt (point))
                                                   (polymuse--make-get-prev-wrapper #'backward-paragraph))))
      (buffer-substring-no-properties (car range) (cdr range)))))

(defun polymuse--get-context-after-point-paragraphs (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((range
                 (polymuse--collect-units-to-limit limit (or pt (point))
                                                   (polymuse--make-get-next-wrapper #'forward-paragraph))))
      (buffer-substring-no-properties (car range) (cdr range)))))

(defun polymuse--get-context-before-point-sexps (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((range
                 (polymuse--collect-units-to-limit limit (or pt (point))
                                                   (polymuse--make-get-prev-wrapper #'backward-sexp))))
      (buffer-substring-no-properties (car range) (cdr range)))))

(defun polymuse--get-context-after-point-sexps (limit &optional buffer pt)
  "Paragraph-based context puller, grabbing paragraphs up to LIMIT characters.

Pull from BUFFER buffer and PT point, or (current-buffer) & (point)."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((range
                 (polymuse--collect-units-to-limit limit (or pt (point))
                                                   (polymuse--make-get-next-wrapper #'forward-sexp))))
      (buffer-substring-no-properties (car range) (cdr range)))))

(print (polymuse--get-context-before-point-paragraphs 50))

(defun polymuse--handle-llm-response (src review response)
  ())

(defun polymuse--run-review (source-buffer review)
  "Submit a review request for SOURCE-BUFFER to the associated LLM according to the instructions in REVIEW."
  (let ((backend (or (polymuse-review-state-backend review)
                     polymuse-default-backend)))
    (unless backend
      (error "Polymuse: no backend configured, skipping review")
      (cl-return-from polymuse--run-review))
    (with-current-buffer source-buffer
      (let ((prompt (polymuse--compose-prompt)))
        (polymuse-request-review backend
                                 prompt
                                 :system   polymuse-system-prompt
                                 :callback (lambda (response info)
                                             (polymuse--handle-llm-response source-buffer review response)))))))

(defun polymuse--global-idle-tick ()
  "Check all muse-enabled buffers and run due reviews."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when polymuse-mode
        (dolist (review polymuse--reviews)
          (let* ((interval (polymuse-review-state-internal review))
                 (last-run (or (polymuse-review-state-last-run-time review) 0))
                 (now (float-time)))
            (when (> (- now last-run) interval)
              (polymuse--run-review buf review))))))))

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

(defun polymuse--compose-prompt ()
  "Generate the full prompt for use with the PolyMuse backend LLM."
  (let* ((mode-prompt   (or (polymuse-get-mode-prompt) ""))
         (local-prompt  (or polymuse-local-prompt ""))
         ;; total char budget for this prompt
         (max-chars     (or polymuse--max-prompt-characters
                            most-positive-fixnum))
         (tools-prompt (mapcar (lambda (tool) `(("tool-name"             . ,(symbol-name (polymuse-tool-function tool)))
                                                ("tool-args"        . ,(mapcar #'symbol-name (polymuse-tool-args tool)))
                                                ("tool-description" . ,(polymuse-tool-description tool))))
                               polymuse-local-tools))

         ;; Rough overhead for non-context text + headings/newlines.
         (prompt-size   (+ (length mode-prompt)
                           (length local-prompt)
                           80))

         ;; Ask the unit grabber to stay within the remaining budget.
         (current-unit  (funcall polymuse--unit-grabber
                                 (max 0 (- max-chars prompt-size))))
         (current-len   (polymuse--region-length current-unit))

         ;; Remaining budget for context.
         (context-space (max 0 (- max-chars prompt-size current-len)))

         ;; Use at most half available space for forward context.
         (forward-budget  (max 0 (floor context-space 2)))
         (forward-context (and (> forward-budget 0)
                               (funcall polymuse--next-context-grabber
                                        forward-budget
                                        nil
                                        (and current-unit (cdr current-unit)))))
         (forward-len     (polymuse--region-length forward-context))

         ;; Whatever is left goes to backward context.
         (backward-budget (max 0 (- context-space forward-len)))
         (backward-context (and (> backward-budget 0)
                                (funcall polymuse--prev-context-grabber
                                         backward-budget
                                         (and current-unit (car current-unit))))))
    `(("review-request"
       . (("mode-prompt"       . ,mode-prompt)
          ("buffer-prompt"     . ,local-prompt)
          ,@(when tools-prompt
              ("tools"             . ,(if tools-prompt
                                          `(("instructions"
                                             . ,(string-join '("You may call a tool for more information instead of replying directly."
                                                               "If you want more information, reply ONLY with a JSON request of the form:"
                                                               "`{"
                                                               "\"action\":\"tool-call\","
                                                               "\"tool\":\"<tool-name>\","
                                                               "\"arguments\": {"
                                                               "\"arg0\": \"<value>\","
                                                               "\"arg1\": \"<value>\""
                                                               "}"
                                                               "}`"
                                                               "The result of the tool call will be added to the request and"
                                                               "the resulting request will be passed back to you.")
                                                             " "))
                                            ("tool-list" . ,tools-prompt)))))))
      ("environment"
       . (("editor"     . "emacs")
          ("major_mode" . ,major-mode)))
      ("context"
       . (("backward-context" . ,(polymuse--region-string backward-context))
          ("forward-context"  . ,(polymuse--region-string forward-context))))
      ("current"
       . (("focus-region" . ,(polymuse--region-string current-unit))))
      ("task"
       . (("focus" . "Prioritize the text in the `current.focus-region' field.")
          ("details" . ,(string-join '("Analyze and provide feedback based primarily on"
                                       "the current editing focus-region, using the earlier"
                                       "and later context for reference.")
                                     " "))
          ("instructions" . ,(or polymuse-final-instructions "")))))
    :pretty t))

(defun polymuse-edit-prompt ()
  "Edit the buffer-specific prompt for polymuse."
  (interactive)
  (let ((buf (get-buffer-create "*Polymuse Buffer Prompt*"))
        (current (or polymuse-local-prompt
                     (polymuse--compose-prompt))))
    (with-current-buffer buf
      (erase-buffer)
      (insert current)
      (org-mode)
      (goto-char (point-min))
      (setq-local polymuse--source-buffer (current-buffer)))
    (display-buffer buf)))

(defun polymuse-save-prompt ()
  "Save the edited prompt from *Polymuse Buffer Prompt* to this buffer's local prompt variable."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "This command expects the *Polymuse Buffer Prompt* buffer"))
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (src  (buffer-local-value 'polymuse--source-buffer (current-buffer))))
    (with-current-buffer src
      (setq polymuse-local-prompt text))
    (message "polymuse local prompt updated.")))

(define-derived-mode polymuse-suggestions-mode special-mode "Polymuse"
  "Mode for displaying AI suggestions from Polymuse."
  (setq buffer-read-only t))

(defcustom polymuse-default-interval 60
  "Default idle time (in seconds) between Polymuse reviews."
  :type 'number)

(defun polymuse--enable ()
  "Enable the Polymuse LLM live review mode."
  (unless polymuse--idle-timer
    (setq polymuse--idle-timer
          (run-with-idle-timer polymuse-idle-seconds t
                               #'polymuse--global-idle-tick))))

(defun polymuse--disable ()
  "Disable the Polymuse LLM live review mode."
  (when (and polymuse--idle-timer
             (not (cl-some (lambda (b) (with-current-buffer b polymuse-mode))
                           (buffer-list))))
    (cancel-timer polymuse--idle-timer)
    (setq polymuse--idle-timer nil)))

(defun polymuse--buffer-hash ()
  "Generate a hash for the current buffer, to detect differences."
  (secure-hash 'sha1 (current-buffer)))

(defun polymuse--display-review (review response)
  "Display the review in RESPONSE according to the state in REVIEW."
  (let ((outbuf (polymuse-review-state-output-buffer review)))
    (if (not (buffer-live-p outbuf))
        (error "Output buffer %s for review %s is not live"
               outbuf (polymuse-review-state-id review))
      (with-current-buffer outbuf
        (polymuse-suggestions-mode 1)
        (let ((inhibit-read-only t))
          (truncate-buffer-to-length outbuf (polymuse-review-state-buffer-size-limit review))
          (typewrite-enqueue-job response buf))))))

(cl-defun polymuse-add-reviewer (&key id
                                      backend
                                      (interval polymuse-default-interval)
                                      (buffer-size-limit polymuse-default-buffer-size-limit)
                                      out-buffer)
  "Create a new Polymuse review buffer for the current buffer.

ID is the identifier for this review, and will be generated if not provided.
BACKEND is the backend (LLM) to which requests will be sent for review.
INTERVAL is the time in seconds between review requests. A default will be used
  if none is provided.
BUFFER-SIZE-LIMIT is the size limit of the review buffer. It will be truncated
  if it grows too large. A default will be used if none in provided.
OUT-BUFFER is the buffer where reviews will be posted. A new buffer will be
  created if none is provided."
  (interactive)
  (let* ((id (or id (format "polymuse-%s-%d" (buffer-name) (float-time))))
         (backend (or backend (polymuse-default-backend)))
         (suggestions-buf (or out-buffer
                              (generate-new-buffer (format "*polymuse:%s" id))))
         (state (make-polymuse-review-state
                 :id                 id
                 :interval           interval
                 :last-run-time      nil
                 :last-hash          nil
                 :buffer-size-limit  buffer-size-limit
                 :output-buffer      suggestions-buf
                 :backend            backend)))
    (push state polymuse--reviews)
    (polymuse--enable)
    (message "Polymuse review %s created with backend %s; suggestions in %s"
             id backend (buffer-name suggestions-buf))
    state))

(provide 'polymuse)
;;; polymuse.el ends here
