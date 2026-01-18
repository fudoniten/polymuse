;;; polymuse-test.el --- Tests for polymuse.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Niten

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the polymuse package using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'polymuse)

;;;; Mock Backend Tests

(ert-deftest polymuse-test-create-mock-backend ()
  "Test creating a mock backend."
  (let ((backend (polymuse-create-mock-backend)))
    (should (polymuse-mock-backend-p backend))
    (should (eq (polymuse-backend-id backend) 'test-backend))
    (should (string= (polymuse-backend-model backend) "mock-model"))))

(ert-deftest polymuse-test-mock-backend-default-response ()
  "Test mock backend with default response."
  (polymuse-test-with-isolated-state
    (let* ((backend (polymuse-create-mock-backend))
           (response-received nil)
           (received-response nil))
      (polymuse-request-review backend
                               '((test . "prompt"))
                               :callback (lambda (resp info)
                                          (setq response-received t
                                                received-response resp)))
      (should response-received)
      (should (stringp received-response))
      (should (string= received-response "Mock review response for testing.")))))

(ert-deftest polymuse-test-mock-backend-custom-response ()
  "Test mock backend with custom response function."
  (polymuse-test-with-isolated-state
    (let* ((backend (polymuse-create-mock-backend
                    (lambda (req) "Custom response")))
           (received-response nil))
      (polymuse-request-review backend
                               '((test . "prompt"))
                               :callback (lambda (resp info)
                                          (setq received-response resp)))
      (should (string= received-response "Custom response")))))

;;;; Prompt Composition Tests

(ert-deftest polymuse-test-compose-prompt-from-context-basic ()
  "Test basic prompt composition from context."
  (let* ((context (polymuse-test-fixture-simple-context))
         (prompt (polymuse--compose-prompt-from-context context)))
    (should (polymuse--valid-prompt-p prompt))
    (should (assq 'review-request prompt))
    (should (assq 'environment prompt))
    (should (assq 'context prompt))
    (should (assq 'current prompt))
    (should (assq 'task prompt))))

(ert-deftest polymuse-test-compose-prompt-has-mode ()
  "Test that prompt includes major mode."
  (let* ((context (polymuse-test-fixture-simple-context))
         (prompt (polymuse--compose-prompt-from-context context))
         (env (alist-get 'environment prompt)))
    (should (assq 'major_mode env))
    (should (string= (alist-get 'major_mode env) "emacs-lisp-mode"))))

(ert-deftest polymuse-test-compose-prompt-with-tools ()
  "Test prompt composition with tools."
  (let* ((context (polymuse-test-fixture-context-with-tools))
         (prompt (polymuse--compose-prompt-from-context context))
         (req (alist-get 'review-request prompt)))
    (should (assq 'tools req))
    (let ((tools (alist-get 'tools req)))
      (should (assq 'tool-list tools)))))

(ert-deftest polymuse-test-compose-prompt-context-strings ()
  "Test that context strings are included in prompt."
  (let* ((context (polymuse-test-fixture-simple-context))
         (prompt (polymuse--compose-prompt-from-context context))
         (ctx (alist-get 'context prompt)))
    (should (string= (alist-get 'backward-context ctx)
                    "Previous code context here."))
    (should (string= (alist-get 'forward-context ctx)
                    "Following code context here."))))

(ert-deftest polymuse-test-compose-prompt-current-unit ()
  "Test that current unit is included in prompt."
  (let* ((context (polymuse-test-fixture-simple-context))
         (prompt (polymuse--compose-prompt-from-context context))
         (current (alist-get 'current prompt)))
    (should (string-match-p "example-function"
                           (alist-get 'focus-region current)))))

;;;; Context Validation Tests

(ert-deftest polymuse-test-valid-context-params ()
  "Test validation of context parameters."
  (let ((valid-context (polymuse-test-fixture-simple-context)))
    (should (polymuse--valid-context-params-p valid-context))))

(ert-deftest polymuse-test-invalid-context-params ()
  "Test detection of invalid context parameters."
  (let ((invalid-context '(:mode-prompt "test")))
    (should-not (polymuse--valid-context-params-p invalid-context))))

(ert-deftest polymuse-test-valid-prompt ()
  "Test validation of complete prompt structure."
  (let* ((context (polymuse-test-fixture-simple-context))
         (prompt (polymuse--compose-prompt-from-context context)))
    (should (polymuse--valid-prompt-p prompt))))

;;;; Region Helper Tests

(ert-deftest polymuse-test-valid-region ()
  "Test valid region detection."
  (should (polymuse--valid-region-p '(1 . 10)))
  (should (polymuse--valid-region-p '(5 . 5)))
  (should-not (polymuse--valid-region-p '(10 . 5)))
  (should-not (polymuse--valid-region-p nil))
  (should-not (polymuse--valid-region-p "not-a-region")))

(ert-deftest polymuse-test-region-empty ()
  "Test empty region detection."
  (should (polymuse--region-empty-p nil))
  (should (polymuse--region-empty-p '(5 . 5)))
  (should-not (polymuse--region-empty-p '(1 . 10))))

(ert-deftest polymuse-test-region-length ()
  "Test region length calculation."
  (should (= (polymuse--region-length '(1 . 10)) 9))
  (should (= (polymuse--region-length '(5 . 5)) 0))
  (should (= (polymuse--region-length nil) 0)))

(ert-deftest polymuse-test-region-string ()
  "Test extracting string from region."
  (with-temp-buffer
    (insert "Hello, world!")
    (should (string= (polymuse--region-string '(1 . 6)) "Hello"))
    (should (string= (polymuse--region-string '(8 . 13)) "world"))
    (should (string= (polymuse--region-string nil) ""))))

;;;; Display Logic Tests

(ert-deftest polymuse-test-format-review-for-display ()
  "Test review formatting for display."
  (let* ((response "Test review content")
         (timestamp (encode-time 0 30 14 15 1 2025))
         (formatted (polymuse--format-review-for-display response timestamp)))
    (should (stringp formatted))
    (should (string-match-p "14:30:00" formatted))
    (should (string-match-p "Test review content" formatted))
    (should (string-match-p ">>>" formatted))))

(ert-deftest polymuse-test-format-review-with-nil-timestamp ()
  "Test review formatting with nil timestamp uses current time."
  (let* ((response "Test")
         (formatted (polymuse--format-review-for-display response)))
    (should (stringp formatted))
    (should (string-match-p ">>>" formatted))))

;;;; Review State Tests

(ert-deftest polymuse-test-create-mock-review-state ()
  "Test creating a mock review state."
  (with-temp-buffer
    (let ((review (polymuse-test-create-mock-review-state)))
      (should (polymuse-review-state-p review))
      (should (string= (polymuse-review-state-id review) "test-review"))
      (should (eq (polymuse-review-state-status review) 'idle))
      (should (polymuse-mock-backend-p (polymuse-review-state-backend review)))
      (kill-buffer (polymuse-review-state-output-buffer review)))))

(ert-deftest polymuse-test-review-state-with-custom-backend ()
  "Test creating review state with custom backend."
  (with-temp-buffer
    (let* ((backend (polymuse-create-mock-backend
                    (lambda (req) "Custom")))
           (review (polymuse-test-create-mock-review-state backend)))
      (should (eq (polymuse-review-state-backend review) backend))
      (kill-buffer (polymuse-review-state-output-buffer review)))))

;;;; State Isolation Tests

(ert-deftest polymuse-test-isolated-state ()
  "Test that isolated state doesn't affect global state."
  (let ((initial-timer (polymuse-global-state-timer
                       polymuse--default-global-state)))
    (polymuse-test-with-isolated-state
      ;; Inside test, modify state
      (let ((state (polymuse--get-global-state)))
        (setf (polymuse-global-state-timer state) 'test-timer)))
    ;; After test, global state should be unchanged
    (should (eq (polymuse-global-state-timer polymuse--default-global-state)
                initial-timer))))

(ert-deftest polymuse-test-multiple-isolated-states ()
  "Test that multiple isolated tests don't interfere."
  (let ((result1 nil)
        (result2 nil))
    (polymuse-test-with-isolated-state
      (setq result1 'test1))
    (polymuse-test-with-isolated-state
      (setq result2 'test2))
    (should (eq result1 'test1))
    (should (eq result2 'test2))))

;;;; Scheduler Tests

(ert-deftest polymuse-test-immediate-scheduler ()
  "Test immediate scheduler executes synchronously."
  (let* ((scheduler (polymuse--create-immediate-scheduler))
         (called nil))
    (funcall (polymuse-scheduler-start-fn scheduler)
             1.0
             (lambda () (setq called t)))
    (should called)))

(ert-deftest polymuse-test-timer-scheduler-creation ()
  "Test creating a timer-based scheduler."
  (let ((scheduler (polymuse--create-timer-scheduler)))
    (should (polymuse-scheduler-p scheduler))
    (should (functionp (polymuse-scheduler-start-fn scheduler)))
    (should (functionp (polymuse-scheduler-stop-fn scheduler)))))

(ert-deftest polymuse-test-isolated-state-uses-immediate-scheduler ()
  "Test that isolated state uses immediate scheduler by default."
  (polymuse-test-with-isolated-state
    (let* ((state (polymuse--get-global-state))
           (scheduler (polymuse-global-state-scheduler state)))
      (should (polymuse-scheduler-p scheduler))
      ;; Test that it's immediate by checking callback is called immediately
      (let ((called nil))
        (funcall (polymuse-scheduler-start-fn scheduler)
                 1.0
                 (lambda () (setq called t)))
        (should called)))))

;;;; Mock Review Integration Tests

(ert-deftest polymuse-test-with-mock-review-macro ()
  "Test polymuse-test-with-mock-review macro."
  (polymuse-test-with-mock-review
    (should (polymuse-mock-backend-p test-backend))
    (should (polymuse-review-state-p test-review))
    (should (eq (polymuse-review-state-backend test-review) test-backend))))

(ert-deftest polymuse-test-mock-review-cleanup ()
  "Test that mock review cleans up buffers."
  (let (output-buffer)
    (polymuse-test-with-mock-review
      (setq output-buffer (polymuse-review-state-output-buffer test-review))
      (should (buffer-live-p output-buffer)))
    ;; After test, buffer should be killed
    (should-not (buffer-live-p output-buffer))))

(ert-deftest polymuse-test-mock-review-request ()
  "Test making a review request with mock backend."
  (polymuse-test-with-mock-review
    (let ((response-received nil)
          (received-response nil))
      (polymuse-request-review test-backend
                               '((test . "prompt"))
                               :callback (lambda (resp info)
                                          (setq response-received t
                                                received-response resp)))
      (should response-received)
      (should (stringp received-response)))))

;;;; Context Extraction Tests

(ert-deftest polymuse-test-region-wrapper-functions ()
  "Test region wrapper functions work correctly."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun test-func ()\n  \"Test.\"\n  (+ 1 1))\n\n")
    (insert "(defun another-func ()\n  \"Another.\"\n  (+ 2 2))")
    (goto-char 10)
    (let ((unit (polymuse--get-unit-sexp)))
      (should (consp unit))
      (should (polymuse--valid-region-p unit))
      (should (> (polymuse--region-length unit) 0)))))

(ert-deftest polymuse-test-paragraph-extraction ()
  "Test paragraph extraction in text mode."
  (with-temp-buffer
    (text-mode)
    (insert "First paragraph.\n\nSecond paragraph.\n\nThird paragraph.")
    (goto-char 20)
    (let ((unit (polymuse--get-unit-paragraph)))
      (should (consp unit))
      (should (polymuse--valid-region-p unit))
      (let ((text (polymuse--region-string unit)))
        (should (string-match-p "Second" text))))))

;;;; Fixture Tests

(ert-deftest polymuse-test-simple-context-fixture ()
  "Test simple context fixture has required fields."
  (let ((context (polymuse-test-fixture-simple-context)))
    (should (plist-get context :mode-prompt))
    (should (plist-get context :local-prompt))
    (should (plist-get context :current-unit))
    (should (eq (plist-get context :major-mode) 'emacs-lisp-mode))
    (should (polymuse--valid-context-params-p context))))

(ert-deftest polymuse-test-context-with-tools-fixture ()
  "Test context with tools fixture includes tools."
  (let ((context (polymuse-test-fixture-context-with-tools)))
    (should (plist-get context :tools-prompt))
    (should (vectorp (plist-get context :tools-prompt)))
    (should (> (length (plist-get context :tools-prompt)) 0))))

;;;; Error Handling Tests

(ert-deftest polymuse-test-valid-prompt-rejects-invalid ()
  "Test that invalid prompts are rejected."
  (should-not (polymuse--valid-prompt-p '()))
  (should-not (polymuse--valid-prompt-p '((review-request . "test"))))
  (should-not (polymuse--valid-prompt-p nil)))

(ert-deftest polymuse-test-region-validation-edge-cases ()
  "Test region validation edge cases."
  (should-not (polymuse--valid-region-p '(1 . "not-a-number")))
  (should-not (polymuse--valid-region-p '("not-a-number" . 1)))
  (should (polymuse--valid-region-p '(0 . 0)))
  (should (polymuse--valid-region-p '(1 . 1))))

;;;; Integration Tests

(ert-deftest polymuse-test-full-prompt-composition-pipeline ()
  "Test the full prompt composition pipeline."
  (polymuse-test-with-isolated-state
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun test (x)\n  \"Test function.\"\n  (+ x 1))")
      (goto-char 20)
      (let* ((context (polymuse-test-fixture-simple-context))
             (prompt (polymuse--compose-prompt-from-context context)))
        (should (polymuse--valid-prompt-p prompt))
        ;; Verify we can pass this to mock backend
        (let* ((backend (polymuse-create-mock-backend))
               (response-received nil))
          (polymuse-request-review backend
                                   prompt
                                   :callback (lambda (resp info)
                                              (setq response-received t)))
          (should response-received))))))

(ert-deftest polymuse-test-mock-backend-with-complex-request ()
  "Test mock backend with complex request structure."
  (polymuse-test-with-mock-review
    (let* ((context (polymuse-test-fixture-context-with-tools))
           (prompt (polymuse--compose-prompt-from-context context))
           (response-received nil)
           (received-response nil))
      (polymuse-request-review test-backend
                               prompt
                               :system "Test system prompt"
                               :callback (lambda (resp info)
                                          (setq response-received t
                                                received-response resp)))
      (should response-received)
      (should (stringp received-response)))))

;;;; Buffer Hash Tests

(ert-deftest polymuse-test-buffer-hash-changes ()
  "Test that buffer hash changes when content changes."
  (with-temp-buffer
    (insert "Initial content")
    (let ((hash1 (polymuse--buffer-hash)))
      (should (stringp hash1))
      (insert " more content")
      (let ((hash2 (polymuse--buffer-hash)))
        (should (stringp hash2))
        (should-not (string= hash1 hash2))))))

(ert-deftest polymuse-test-buffer-hash-consistent ()
  "Test that buffer hash is consistent for same content."
  (with-temp-buffer
    (insert "Test content")
    (let ((hash1 (polymuse--buffer-hash))
          (hash2 (polymuse--buffer-hash)))
      (should (string= hash1 hash2)))))

(provide 'polymuse-test)
;;; polymuse-test.el ends here
