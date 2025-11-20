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
;;  Description
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

(defvar typewrite--jobs nil
  "List of active typewriter jobs.")

(defvar typewrite--timer nil
  "Driver timer for active typewriter jobs.")

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
  (let ((seconds (or interval typewrite-tick-interval)))
    (setq seconds (if (and (numberp seconds) (> seconds 0))
                      seconds
                    typewrite-tick-interval))
    (unless typewrite--timer
      (setq typewrite--timer
            (run-at-time 0 seconds #'typewrite--tick)))))

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
      (push job typewrite--jobs)
      (typewrite--ensure-timer)
      job)))

(defun typewrite--tick ()
  "Advance all active `typewrite' jobs once.

For each job in `typewrite--jobs', compute how many characters should be
inserted based on the CPS and the elapsed time since `typewrite-job-last-time'.
Insert that many characters at the job's marker, update its index, and remove
finished or dead jobs.

If no jobs remain after this tick, cancel `typewrite--timer' and set it to nil."
  (let* ((now (float-time))
         (alive-jobs nil))
    (dolist (job typewrite--jobs)
      (let ((buf (typewrite-job-buffer job)))
        (if (not (buffer-live-p buf))
            ;; buffer died, drop the job silently
            nil
          (with-current-buffer buf
            (let* ((str (typewrite-job-string job))
                   (len (length str))
                   (idx (or (typewrite-job-index job) 0)))
              (if (>= idx len)
                  ;; Already finished; just fire callback once and drop
                  (when-let* ((cb (typewrite-job-done-callback job)))
                    (funcall cb job))
                (let* ((cps       (or (typewrite-job-cps job) 20.0))
                       (last-time (or (typewrite-job-last-time job) now))
                       (budget    (+ (or (typewrite-job-budget job) 0.0)
                                     (* cps (- now last-time))))
                       (to-write  (floor budget)))
                  (setf (typewrite-job-budget job) (- budget to-write)
                        (typewrite-job-last-time job) now)
                  (when (> to-write 0)
                    (let ((marker (typewrite-job-marker job))
                          (follow (typewrite-job-follow job))
                          (new-idx idx))
                      (save-excursion
                        (let ((inhibit-read-only (typewrite-job-inhibit-read-only job)))
                          (goto-char marker)
                          (dotimes (_ to-write)
                            (when (< new-idx len)
                              (insert (aref str new-idx))
                              (setq new-idx (1+ new-idx)))))
                        ;; marker auto-advances as we insert
                        )
                      (setf (typewrite-job-index job) new-idx)
                      (when (and follow (> new-idx idx))
                        (dolist (win (get-buffer-window-list buf nil t))
                          (set-window-point win (marker-position marker))))))
                  ;; Keep job alive if it still has work,
                  ;; regardless of whether we wrote this tick.
                  (when (< (typewrite-job-index job) len)
                    (push job alive-jobs)))))))))
    ;; update global job list
    (setq typewrite--jobs (nreverse alive-jobs))
    ;; if no jobs remain, stop the timer
    (when (and (null typewrite--jobs)
               typewrite--timer)
      (cancel-timer typewrite--timer)
      (setq typewrite--timer nil))))

(defun typewrite-kill-jobs ()
  "Stop all active typewriter jobs and cancel the driver timer.

This clears `typewrite--jobs' and cancels `typewrite--timer' if it is
currently running. Does *not* run any job `done-callback's."
  (interactive)
  ;; Drop all jobs on the floor.
  (setq typewrite--jobs nil)
  ;; Cancel the timer if it's still alive.
  (when (and typewrite--timer (timerp typewrite--timer))
    (cancel-timer typewrite--timer)
    (setq typewrite--timer nil)))

(provide 'typewrite)
;;; typewrite.el ends here
