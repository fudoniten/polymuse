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
;;  Description
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

(defcustom polymuse-max-prompt-characters 12000
  "Maximum length of a prompt, in characters."
  :type 'integer)

(defcustom polymuse-default-review-context-lines 20
  "Number of previous lines of review to include with the prompt."
  :type 'integer)

(defcustom polymuse-default-buffer-size-limit 8196
  "Default maximum size for Polymuse review buffers.

When the buffer grows larger than this, the beginning will be truncated."
  :type 'integer)

(defvar polymuse--timer nil
  "Driver timer for active Polymuse reviewers.")

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
  source-buffer)    ;; The buffer under review

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
                     (when polymuse-debug
                       (with-current-buffer (get-buffer-create polymuse-debug-buffer)
                         (insert (format "INFO: %s" info))))
                     (when polymuse-debug
                       (with-current-buffer (get-buffer-create polymuse-debug-buffer)
                         (insert "\n\nRESPONSE:\n\n")
                         (insert resp)
                         (insert "\n\nEND RESPONSE\n\n")))
                     (when callback (funcall callback resp info)))))
    (let ((request (json-serialize request)))
      (let* ((gptel-backend     executor)
             (gptel-model       model)
             (gptel-temperature temperature)
             (result (gptel-request request
                       :system   system
                       :callback handler)))
        (when polymuse-debug
          (with-current-buffer (get-buffer-create polymuse-debug-buffer)
            (insert "\n\nREQUEST:\n\n")
            (insert (polymuse--format-json request))
            (insert "\n\nEND REQUEST\n\n")))
        result))))

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
   '("You are an over-the-shoulder assistant. Respond in markdown format or"
     "in plain text.")
   " ")
  "Base system prompt for polymuse."
  :type 'string)

(defcustom polymuse-mode-prompts
  '((prog-mode . "Provide feedback on the code in the `current.focus-region' field below. Focus on clarity, reuse, improvements, library suggestions and general quality. This is part of an ongoing stream of advice.")
    (text-mode . "Provide feedback or react to the prose content in the `current.focus-region' field. You are providing an ongoing stream of feedback."))
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
            (setf (polymuse-review-state-last-run-time review) (float-time))
            (setf (polymuse-review-state-last-hash review) new-hash)
            (polymuse-request-review backend
                                     prompt
                                     :system   polymuse-system-prompt
                                     :callback (lambda (response _)
                                                 (polymuse--handle-llm-response review response)))))))))

(defun polymuse--global-idle-tick ()
  "Check all muse-enabled buffers and run due reviews."
  (when polymuse-debug  (message "Polymuse: Tick!"))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when polymuse-mode
        (dolist (review polymuse--reviews)
          (let* ((interval (polymuse-review-state-interval review))
                 (last-run (or (polymuse-review-state-last-run-time review) 0))
                 (now (float-time)))
            (if (> (- now last-run) interval)
                (polymuse--run-review buf review)
              (message "skipping %s, review too recent"
                       (polymuse-review-state-id review)))))))))

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

(defun polymuse--grab-buffer-tail (buffer n)
  "Return the last N lines from BUFFER as a string."
  (with-current-buffer buffer
    (goto-char (point-max))
    (forward-line (- n))
    (buffer-substring-no-properties (point) (point-max))))

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

(defun polymuse--compose-prompt (review)
  "Generate the full prompt for REVIEW, for use with the PolyMuse backend LLM."
  (let* ((mode-prompt   (or (polymuse-get-mode-prompt) ""))
         (local-prompt  (or (polymuse-review-state-instructions review) ""))
         ;; total char budget for this prompt
         (max-chars     (or polymuse-max-prompt-characters
                            most-positive-fixnum))
         (tools-prompt (mapcar (lambda (tool) `(("tool-name"        . ,(polymuse-tool-name tool))
                                                ("tool-args"        . ,(mapcar #'symbol-name (polymuse-tool-arguments (cdr tool))))
                                                ("tool-description" . ,(polymuse-tool-description (cdr tool)))))
                               polymuse-local-tools))

         ;; Rough overhead for non-context text + headings/newlines.
         (prompt-size   (+ (length mode-prompt)
                           (length local-prompt)
                           80))

         (current-unit  (funcall polymuse--unit-grabber))
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
                                         nil
                                         (and current-unit (car current-unit)))))
         (out-buffer            (polymuse-review-state-output-buffer review))
         (existing-review-lines (polymuse-review-state-review-context review))
         (existing-review       (polymuse--grab-buffer-tail out-buffer existing-review-lines)))
    `((review-request
       . ((mode-prompt       . ,mode-prompt)
          (buffer-prompt     . ,local-prompt)
          ,@(when tools-prompt
              (list `(tools . ((instructions
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
                               (tool-list . ,tools-prompt)))))))
      (environment
       . ((editor     . "emacs")
          (major_mode . ,(symbol-name major-mode))))
      (context
       . ((backward-context . ,(polymuse--region-string backward-context))
          (forward-context  . ,(polymuse--region-string forward-context))
          (existing-review  . ,existing-review)))
      (current
       . ((focus-region . ,(polymuse--region-string current-unit))))
      (task
       . ((focus   . "Prioritize the text in the `current.focus-region' field.")
          (details . ,(string-join '("Analyze and provide feedback based primarily on"
                                     "the current editing focus-region, using the earlier"
                                     "and later context for reference.")
                                   " "))
          ,@(when polymuse-final-instructions
              (list `(instructions . ,polymuse-final-instructions))))))))

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

(defcustom polymuse-default-interval 60
  "Default idle time (in seconds) between Polymuse reviews."
  :type 'number)

(defun polymuse--enable ()
  "Enable the Polymuse LLM live review mode."
  (unless polymuse--timer
    (setq polymuse--timer
          (run-with-timer polymuse-default-interval ;; How long to wait before the first review
                          polymuse-default-interval ;; How long to pause between reviews
                          #'polymuse--global-idle-tick)))
  (if (polymuse--code-mode-p)
      (setq polymuse--unit-grabber #'polymuse--get-unit-sexp
            polymuse--prev-context-grabber #'polymuse--get-context-before-point-sexps
            polymuse--next-context-grabber #'polymuse--get-context-after-point-sexps)
    (setq polymuse--unit-grabber #'polymuse--get-unit-paragraph
          polymuse--prev-context-grabber #'polymuse--get-context-before-point-paragraphs
          polymuse--next-context-grabber #'polymuse--get-context-after-point-paragraphs))
  (polymuse--load-tools))

(defun polymuse--disable ()
  "Disable the Polymuse LLM live review mode."
  (when (and polymuse--timer
             (not (cl-some (lambda (b) (with-current-buffer b polymuse-mode))
                           (buffer-list))))
    (cancel-timer polymuse--timer)
    (setq polymuse--timer nil)))

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
        (let ((inhibit-read-only t)
              (full-response (concat "\n\n>>> "
                                     (format-time-string "%H:%M:%S")
                                     "\n\n"
                                     response)))
          (polymuse-suggestions-mode)
          (polymuse--truncate-buffer-to-length
           (current-buffer)
           (polymuse-review-state-buffer-size-limit review))
          (typewrite-enqueue-job full-response outbuf
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
                 :source-buffer      (current-buffer))))
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
                 :source-buffer      (current-buffer))))
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

(provide 'polymuse)
;;; polymuse.el ends here
