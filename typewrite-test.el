;;; typewrite-test.el --- Tests for typewrite.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Niten

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the typewrite package using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'typewrite)

;;;; Basic Functionality Tests

(ert-deftest typewrite-test-simple-insertion ()
  "Test that text is inserted synchronously in test mode."
  (typewrite-test-with-job "Hello, world!"
    (with-current-buffer test-buffer
      (should (string= "Hello, world!" (buffer-string))))))

(ert-deftest typewrite-test-empty-string ()
  "Test that empty string works correctly."
  (typewrite-test-with-job ""
    (with-current-buffer test-buffer
      (should (string= "" (buffer-string))))))

(ert-deftest typewrite-test-multiline-text ()
  "Test insertion of multiline text."
  (typewrite-test-with-job "Line 1\nLine 2\nLine 3"
    (with-current-buffer test-buffer
      (should (string-match-p "Line 1" (buffer-string)))
      (should (string-match-p "Line 2" (buffer-string)))
      (should (string-match-p "Line 3" (buffer-string))))))

;;;; Job State Tests

(ert-deftest typewrite-test-job-completion ()
  "Test that jobs are marked as complete after insertion."
  (typewrite-test-with-job "Complete"
    (should (typewrite-job-p test-job))
    (should (= (typewrite-job-index test-job)
               (length (typewrite-job-string test-job))))))

(ert-deftest typewrite-test-job-buffer-reference ()
  "Test that job maintains correct buffer reference."
  (typewrite-test-with-job "Test"
    (should (eq (typewrite-job-buffer test-job) test-buffer))
    (should (buffer-live-p (typewrite-job-buffer test-job)))))

;;;; State Isolation Tests

(ert-deftest typewrite-test-state-isolation ()
  "Test that test state is properly isolated."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf1 (generate-new-buffer " *test1*"))
           (buf2 (generate-new-buffer " *test2*")))
      (unwind-protect
          (progn
            (typewrite-enqueue-job "Test1" buf1 :newline-before nil)
            (typewrite-enqueue-job "Test2" buf2 :newline-before nil)
            (with-current-buffer buf1
              (should (string= "Test1" (buffer-string))))
            (with-current-buffer buf2
              (should (string= "Test2" (buffer-string)))))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest typewrite-test-no-global-pollution ()
  "Test that tests don't pollute global state."
  (let ((initial-jobs (typewrite-state-jobs typewrite--default-state)))
    (typewrite-test-with-job "Test"
      ;; Inside test, do something
      (should (buffer-live-p test-buffer)))
    ;; After test, global state should be unchanged
    (should (equal initial-jobs
                   (typewrite-state-jobs typewrite--default-state)))))

;;;; Configuration Tests

(ert-deftest typewrite-test-custom-cps ()
  "Test that custom CPS setting is respected."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *test*"))
           (job (typewrite-enqueue-job "Test" buf
                                       :cps 100
                                       :newline-before nil)))
      (unwind-protect
          (should (= (typewrite-job-cps job) 100))
        (kill-buffer buf)))))

(ert-deftest typewrite-test-newline-before-disabled ()
  "Test that :newline-before nil works correctly."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "Existing"))
            (typewrite-enqueue-job "New" buf :newline-before nil)
            (with-current-buffer buf
              (should (string= "ExistingNew" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest typewrite-test-at-end-disabled ()
  "Test that :at-end nil respects current point."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "Start  End")
              (goto-char 7))  ; Between the two spaces
            (typewrite-enqueue-job "Middle" buf
                                  :at-end nil
                                  :newline-before nil)
            (with-current-buffer buf
              (should (string= "Start Middle End" (buffer-string)))))
        (kill-buffer buf)))))

;;;; Callback Tests

(ert-deftest typewrite-test-done-callback ()
  "Test that done-callback is called after completion."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *test*"))
           (callback-called nil)
           (callback-job nil))
      (unwind-protect
          (progn
            (typewrite-enqueue-job "Test" buf
                                  :newline-before nil
                                  :done-callback (lambda (job)
                                                  (setq callback-called t
                                                        callback-job job)))
            (should callback-called)
            (should (typewrite-job-p callback-job)))
        (kill-buffer buf)))))

;;;; Budget Calculation Tests

(ert-deftest typewrite-test-calculate-write-budget ()
  "Test character budget calculation."
  (let* ((job (make-typewrite-job
               :cps 50.0
               :last-time 100.0
               :budget 0.0))
         (now 101.0)
         (budget (typewrite--calculate-write-budget job now)))
    (should (= budget 50))
    (should (< (typewrite-job-budget job) 1.0))))

(ert-deftest typewrite-test-budget-accumulation ()
  "Test that budget accumulates correctly over time."
  (let* ((job (make-typewrite-job
               :cps 10.0
               :last-time 100.0
               :budget 5.5)))
    ;; After 0.5 seconds at 10 cps, we get 5 more chars
    (let ((budget (typewrite--calculate-write-budget job 100.5)))
      (should (= budget 10))  ; floor(5.5 + 5.0) = 10
      (should (< (typewrite-job-budget job) 1.0)))))

;;;; Special Characters Tests

(ert-deftest typewrite-test-unicode-characters ()
  "Test that Unicode characters are handled correctly."
  (typewrite-test-with-job "Hello 世界 🌍"
    (with-current-buffer test-buffer
      (should (string-match-p "世界" (buffer-string)))
      (should (string-match-p "🌍" (buffer-string))))))

(ert-deftest typewrite-test-special-characters ()
  "Test insertion of special characters."
  (typewrite-test-with-job "Tab\there\nNewline\rCarriage"
    (with-current-buffer test-buffer
      (should (string-match-p "Tab" (buffer-string)))
      (should (string-match-p "here" (buffer-string))))))

;;;; Error Handling Tests

(ert-deftest typewrite-test-invalid-buffer ()
  "Test error handling for invalid buffer."
  (typewrite-test-with-isolated-state
    (let ((typewrite-test-mode t))
      (should-error (typewrite-enqueue-job "Test" "not-a-buffer")
                    :type 'user-error))))

(ert-deftest typewrite-test-non-string-input ()
  "Test error handling for non-string input."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *test*")))
      (unwind-protect
          (should-error (typewrite-enqueue-job 123 buf)
                        :type 'user-error)
        (kill-buffer buf)))))

(ert-deftest typewrite-test-invalid-cps ()
  "Test error handling for invalid CPS value."
  (typewrite-test-with-isolated-state
    (let* ((typewrite-test-mode t)
           (buf (generate-new-buffer " *test*")))
      (unwind-protect
          (should-error (typewrite-enqueue-job "Test" buf :cps -10)
                        :type 'user-error)
        (kill-buffer buf)))))

;;;; Scheduler Tests

(ert-deftest typewrite-test-immediate-scheduler ()
  "Test that immediate scheduler executes synchronously."
  (let* ((scheduler (typewrite--create-immediate-scheduler))
         (called nil)
         (callback (lambda () (setq called t))))
    (funcall (typewrite-scheduler-start-fn scheduler) 1.0 callback)
    (should called)))

(ert-deftest typewrite-test-timer-scheduler-creation ()
  "Test that timer scheduler is created correctly."
  (let ((scheduler (typewrite--create-timer-scheduler)))
    (should (typewrite-scheduler-p scheduler))
    (should (functionp (typewrite-scheduler-start-fn scheduler)))
    (should (functionp (typewrite-scheduler-stop-fn scheduler)))))

;;;; Kill Jobs Tests

(ert-deftest typewrite-test-kill-jobs ()
  "Test that kill-jobs clears all jobs."
  (typewrite-test-with-isolated-state
    (let ((state (typewrite--get-state)))
      ;; Add some fake jobs
      (setf (typewrite-state-jobs state)
            (list (make-typewrite-job :buffer (current-buffer))))
      (should (> (length (typewrite-state-jobs state)) 0))
      ;; Kill jobs
      (typewrite-kill-jobs)
      ;; Verify cleared
      (should (null (typewrite-state-jobs state))))))

(provide 'typewrite-test)
;;; typewrite-test.el ends here
