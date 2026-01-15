;;; typewrite.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Niten
;;
;; Author: Niten <niten@fudo.org>
;; Maintainer: Niten <niten@fudo.org>
;; Created: November 17, 2025
;; Modified: November 17, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/niten/typewrite
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Typewrite provides a typewriter effect for inserting text into Emacs buffers.
;; Instead of inserting text all at once, it gradually reveals the text
;; character by character at a configurable rate, creating a more dynamic
;; and pleasant visual experience.
;;
;; Features:
;; - Configurable typing speed (characters per second)
;; - Multiple simultaneous jobs in different buffers
;; - Auto-scrolling to follow the insertion point
;; - Budget-based character accumulation for smooth animation
;; - Proper timer cleanup when jobs complete
;;
;; Usage:
;;   (typewrite-enqueue-job "Hello, world!" (current-buffer)
;;                          :cps 50
;;                          :follow t)
;;
;; This is particularly useful for displaying LLM responses, log output,
;; or any other text where a gradual reveal enhances the user experience.
;;
;;; Code:

(require 'cl-lib)

(defgroup typewrite nil
  "Simulate incremental typing into buffers."
  :group 'applications)

(defcustom typewrite-default-cps 30.0
  "Default characters-per-second rate for newly enqueued jobs."
  :type 'number
  :group 'typewrite)

(defcustom typewrite-tick-interval 0.05
  "Default timer interval used to drive active jobs."
  :type 'number
  :group 'typewrite)

(defcustom typewrite-default-inhibit-read-only t
  "Whether `typewrite' should ignore read-only protections by default."
  :type 'boolean
  :group 'typewrite)

(cl-defstruct typewrite-state
  "State container for typewriter jobs and timer.

This allows tests to isolate state by binding `typewrite--test-state'."
  (jobs nil)
  (timer nil))

(defvar typewrite--default-state (make-typewrite-state)
  "Default global state for typewriter.")

(defvar typewrite--test-state nil
  "When bound in tests, overrides the default state.

Tests should bind this to a fresh `typewrite-state' to isolate themselves
from global state and other tests.")

(defun typewrite--get-state ()
  "Get the current typewriter state.

Returns `typewrite--test-state' if bound (for testing), otherwise
returns the global `typewrite--default-state'."
  (or typewrite--test-state typewrite--default-state))

(defvar typewrite-test-mode nil
  "When non-nil, execute typewriter jobs synchronously for testing.

In test mode, `typewrite-enqueue-job' will immediately insert all text
without using timers, making tests deterministic and fast.")

(cl-defstruct typewrite-job
  buffer
  marker
  string
  index
  cps
  last-time
  budget
  follow
  inhibit-read-only
  done-callback)

(defun typewrite--ensure-timer (&optional interval)
  "Ensure the driver timer is running, at INTERVAL seconds (or default)."
  (let* ((seconds (or interval typewrite-tick-interval))
         (state (typewrite--get-state)))
    (setq seconds (if (and (numberp seconds) (> seconds 0))
                      seconds
                    typewrite-tick-interval))
    (unless (typewrite-state-timer state)
      (setf (typewrite-state-timer state)
            (run-at-time 0 seconds #'typewrite--tick)))))

(defun typewrite--execute-job-synchronously (job)
  "Execute JOB immediately and synchronously, inserting all text at once.

This is used in test mode to avoid timer dependencies and make tests
deterministic. The job's done-callback will be called after insertion."
  (let* ((buf (typewrite-job-buffer job))
         (marker (typewrite-job-marker job))
         (str (typewrite-job-string job))
         (follow (typewrite-job-follow job))
         (inhibit-read-only-val (typewrite-job-inhibit-read-only job)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (let ((inhibit-read-only inhibit-read-only-val))
            (goto-char marker)
            (insert str)))
        (setf (typewrite-job-index job) (length str))
        (when follow
          (typewrite--follow-marker job marker))
        (typewrite--run-done-callback job)))))

(defun typewrite-enqueue-job (str buffer &rest config)
  "Enqueue a new typewriter job for STR into BUFFER, with CONFIG.

CONFIG is a plist which may contain:
  :cps            characters per second (default 30.0)
  :follow         non-nil => scroll windows showing the buffer
  :done-callback  function called with the job when finished
  :at-end         default t; when nil, use current point instead of end
  :newline-before default \n\n; when nil, don't insert a newline first.
  :inhibit-read-only non-nil => bind `inhibit-read-only' during writes."
  (unless (stringp str)
    (user-error "typewrite: STR must be a string"))
  (let* ((buf (or (get-buffer buffer)
                  (user-error "Buffer %S does not exist" buffer)))
         ;; Defaults: at-end = t, newline-before = t, unless explicitly set
         (at-end (if (plist-member config :at-end)
                     (plist-get config :at-end)
                   t))
         (newline-before (if (plist-member config :newline-before)
                             (plist-get config :newline-before)
                           "\n\n"))
         (cps (or (plist-get config :cps) typewrite-default-cps))
         (inhibit-read-only-setting (if (plist-member config :inhibit-read-only)
                                        (plist-get config :inhibit-read-only)
                                      typewrite-default-inhibit-read-only))
         marker)
    (unless (and (numberp cps) (> cps 0))
      (user-error "Typewrite: cps must be a positive number"))
    (with-current-buffer buf
      (let ((inhibit-read-only inhibit-read-only-setting))
        ;; Move to end of buffer if requested (default)
        (when at-end
          (goto-char (point-max)))
        ;; Optionally insert a newline before we start typing (default)
        (when newline-before
          ;; avoid double-blank-line if already at BOL
          (unless (bolp)
            (insert newline-before)))
        ;; Start marker at current point, insertion-type so it follows inserts
        (setq marker (copy-marker (point) t))))
    (let ((job (make-typewrite-job
                :buffer buf
                :marker marker
                :string str
                :index  0
                :cps    cps
                :budget 0.0
                :last-time (float-time)
                :follow (plist-get config :follow)
                :inhibit-read-only inhibit-read-only-setting
                :done-callback (plist-get config :done-callback))))
      (if typewrite-test-mode
          (typewrite--execute-job-synchronously job)
        (let ((state (typewrite--get-state)))
          (push job (typewrite-state-jobs state))
          (typewrite--ensure-timer)))
      job)))

(defun typewrite--tick ()
  "Advance all active `typewrite' jobs once.

For each job in the state's job list, compute how many characters should be
inserted based on the CPS and the elapsed time since `typewrite-job-last-time'.
Insert that many characters at the job's marker, update its index, and remove
finished or dead jobs.

If no jobs remain after this tick, cancel the timer and set it to nil."
  (let* ((now (float-time))
         (state (typewrite--get-state)))
    (setf (typewrite-state-jobs state)
          (cl-remove-if-not (lambda (job) (typewrite--process-job job now))
                            (typewrite-state-jobs state)))
    ;; if no jobs remain, stop the timer
    (when (and (null (typewrite-state-jobs state))
               (typewrite-state-timer state))
      (cancel-timer (typewrite-state-timer state))
      (setf (typewrite-state-timer state) nil))))

(defun typewrite--process-job (job now)
  "Advance JOB at time NOW.

Return JOB when it should remain active, or nil to drop it."
  (let ((buf (typewrite-job-buffer job)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((str (typewrite-job-string job))
               (len (length str)))
          (cond
           ((typewrite--job-finished-p job len)
            (typewrite--run-done-callback job)
            nil)
           (t
            (typewrite--advance-job job len now)
            (if (typewrite--job-finished-p job len)
                (progn
                  (typewrite--run-done-callback job)
                  nil)
              job))))))))

(defun typewrite--job-finished-p (job len)
  "Return non-nil when JOB has already inserted LEN characters."
  (>= (or (typewrite-job-index job) 0) len))

(defun typewrite--run-done-callback (job)
  "Invoke JOB's done callback, if any."
  (when-let* ((cb (typewrite-job-done-callback job)))
    (funcall cb job)))

(defun typewrite--advance-job (job len now)
  "Advance JOB towards LEN characters at timestamp NOW."
  (let* ((to-write (typewrite--calculate-write-budget job now)))
    (when (> to-write 0)
      (typewrite--insert-chunk job len to-write))))

(defun typewrite--calculate-write-budget (job now)
  "Update JOB's budget using NOW and return number of chars to write."
  (let* ((cps       (or (typewrite-job-cps job) 20.0))
         (last-time (or (typewrite-job-last-time job) now))
         (budget    (+ (or (typewrite-job-budget job) 0.0)
                       (* cps (- now last-time))))
         (to-write  (floor budget)))
    (setf (typewrite-job-budget job) (- budget to-write)
          (typewrite-job-last-time job) now)
    to-write))

(defun typewrite--insert-chunk (job len to-write)
  "Insert up to TO-WRITE characters for JOB into its buffer of length LEN."
  (let* ((marker (typewrite-job-marker job))
         (follow (typewrite-job-follow job))
         (str (typewrite-job-string job))
         (old-idx (or (typewrite-job-index job) 0))
         (new-idx old-idx))
    (save-excursion
      (let ((inhibit-read-only (typewrite-job-inhibit-read-only job)))
        (goto-char marker)
        (dotimes (_ to-write)
          (when (< new-idx len)
            (insert (aref str new-idx))
            (setq new-idx (1+ new-idx))))))
    (setf (typewrite-job-index job) new-idx)
    (when (and follow (> new-idx old-idx))
      (typewrite--follow-marker job marker))))

(defun typewrite--follow-marker (job marker)
  "Scroll windows showing JOB's buffer to MARKER."
  (dolist (win (get-buffer-window-list (typewrite-job-buffer job) nil t))
    (set-window-point win (marker-position marker))))

(defun typewrite-kill-jobs ()
  "Stop all active typewriter jobs and cancel the driver timer.

This clears the job list and cancels the timer if it is currently running.
Does *not* run any job `done-callback's."
  (interactive)
  (let ((state (typewrite--get-state)))
    ;; Drop all jobs on the floor.
    (setf (typewrite-state-jobs state) nil)
    ;; Cancel the timer if it's still alive.
    (when-let ((timer (typewrite-state-timer state)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (typewrite-state-timer state) nil))))

;;;;
;; Test Helpers
;;;;

(defmacro typewrite-test-with-isolated-state (&rest body)
  "Execute BODY with isolated typewriter state.

This creates a fresh state container for the duration of BODY,
preventing tests from interfering with each other or with global state.

Example:
  (typewrite-test-with-isolated-state
    (let ((typewrite-test-mode t))
      (with-temp-buffer
        (typewrite-enqueue-job \"test\" (current-buffer))
        (should (string= \"test\" (buffer-string))))))"
  (declare (indent 0))
  `(let ((typewrite--test-state (make-typewrite-state)))
     ,@body))

(defun typewrite-test-create-simple-job (text &optional cps)
  "Create a simple synchronous typewriter job for testing.

TEXT is the string to type. CPS is the characters per second (default 50).
Returns a cons of (JOB . BUFFER) where JOB is the typewrite-job and
BUFFER is the temporary buffer. The job is executed synchronously.

The caller is responsible for killing the buffer after testing.

Example:
  (let* ((result (typewrite-test-create-simple-job \"Hello, world!\"))
         (job (car result))
         (buf (cdr result)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p \"Hello\" (buffer-string))))
      (kill-buffer buf)))"
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *typewrite-test*"))
           (job (typewrite-enqueue-job text buf
                                       :cps (or cps 50)
                                       :newline-before nil)))
      (cons job buf))))

(defmacro typewrite-test-with-job (text &rest body)
  "Execute BODY with a test typewriter job for TEXT.

The job is created synchronously in a temporary buffer. Within BODY,
the variable `test-buffer' is bound to the buffer and `test-job' is
bound to the job.

Example:
  (typewrite-test-with-job \"Hello, world!\"
    (with-current-buffer test-buffer
      (should (string= \"Hello, world!\" (buffer-string)))))"
  (declare (indent 1))
  `(typewrite-test-with-isolated-state
     (let* ((result (typewrite-test-create-simple-job ,text))
            (test-job (car result))
            (test-buffer (cdr result)))
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p test-buffer)
           (kill-buffer test-buffer))))))

(provide 'typewrite)
;;; typewrite.el ends here
