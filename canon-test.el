;;; canon-test.el --- Tests for canon.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Niten

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the canon package using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'canon)
(require 'org)

;;;; Entity Insertion Tests

(ert-deftest canon-test-insert-entity-basic ()
  "Test basic entity insertion."
  (canon-test-with-temp-canon "* Characters\n\n* Locations\n\n"
    (canon-insert-entity "Characters" "alice")
    (should (canon--find-entity-heading "alice"))
    (let ((text (canon-get-entity-text "alice")))
      (should (stringp text))
      (should (string-match-p ":ID: alice" text)))))

(ert-deftest canon-test-insert-multiple-entities ()
  "Test inserting multiple entities in the same type."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "alice")
    (canon-insert-entity "Characters" "bob")
    (should (canon--find-entity-heading "alice"))
    (should (canon--find-entity-heading "bob"))))

(ert-deftest canon-test-insert-entities-different-types ()
  "Test inserting entities of different types."
  (canon-test-with-temp-canon "* Characters\n\n* Locations\n\n"
    (canon-insert-entity "Characters" "alice")
    (canon-insert-entity "Locations" "castle")
    (should (canon--find-entity-heading "alice"))
    (should (canon--find-entity-heading "castle"))))

;;;; Entity Lookup Tests

(ert-deftest canon-test-lookup-existing-entity ()
  "Test looking up an existing entity."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (progn
          (should (canon--find-entity-heading "alice"))
          (let ((text (canon-get-entity-text "alice")))
            (should (stringp text))
            (should (string-match-p "Alice is the main character" text))))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-lookup-nonexistent-entity ()
  "Test looking up a nonexistent entity returns nil."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (should-not (canon--find-entity-heading "nonexistent"))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

;;;; Entity Search Tests

(ert-deftest canon-test-search-by-property ()
  "Test searching entities by property value."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((ids (canon-get-ids-by-property "Role" "protagonist")))
          (should (member "alice" ids)))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-search-by-property-multiple-results ()
  "Test searching when multiple entities match."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "hero1")
    (canon-insert-entity "Characters" "hero2")
    (with-current-buffer canon--buffer
      (goto-char (canon--find-entity-heading "hero1"))
      (org-set-property "Type" "hero")
      (goto-char (canon--find-entity-heading "hero2"))
      (org-set-property "Type" "hero"))
    (let ((ids (canon-get-ids-by-property "Type" "hero")))
      (should (= (length ids) 2))
      (should (member "hero1" ids))
      (should (member "hero2" ids)))))

(ert-deftest canon-test-search-by-property-no-results ()
  "Test searching when no entities match."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((ids (canon-get-ids-by-property "Role" "nonexistent")))
          (should (null ids)))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-get-unique-by-property ()
  "Test getting single entity by property."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((id (canon-get-id-by-property "Role" "antagonist")))
          (should (string= id "bob")))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

;;;; Section Manipulation Tests

(ert-deftest canon-test-get-entity-section ()
  "Test getting a specific section of an entity."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "test-char")
    (let ((pos (canon--find-entity-heading "test-char")))
      (with-current-buffer canon--buffer
        (goto-char pos)
        (org-end-of-subtree t t)
        (insert "\n*** Description\nThis is a test character.\n")))
    (let ((desc (canon-get-entity-section "test-char" "Description")))
      (should (stringp desc))
      (should (string-match-p "test character" desc)))))

(ert-deftest canon-test-append-to-entity ()
  "Test appending text to an entity."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "test-char")
    (canon-append-to-entity "test-char" "Additional information.")
    (let ((text (canon-get-entity-text "test-char")))
      (should (string-match-p "Additional information" text)))))

;;;; Type Management Tests

(ert-deftest canon-test-list-types ()
  "Test listing all entity types."
  (let* ((setup (canon-test-fixture-simple-canon))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (with-current-buffer canon--buffer
          (let ((types (canon--list-types)))
            (should (member "Characters" types))
            (should (member "Locations" types))
            (should (member "Events" types))
            (should (member "Items" types))))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-ensure-type-heading-existing ()
  "Test ensuring a type heading that already exists."
  (canon-test-with-temp-canon "* Characters\n\n* Locations\n\n"
    (with-current-buffer canon--buffer
      (let ((pos (canon--ensure-type-heading "Characters")))
        (should pos)
        (should (progn
                  (goto-char pos)
                  (looking-at "\\* Characters")))))))

(ert-deftest canon-test-ensure-type-heading-new ()
  "Test creating a new type heading."
  (canon-test-with-temp-canon "* Characters\n\n"
    (with-current-buffer canon--buffer
      (let ((pos (canon--ensure-type-heading "NewType")))
        (should pos)
        (should (progn
                  (goto-char pos)
                  (looking-at "\\* NewType")))))))

;;;; Subheading Tests

(ert-deftest canon-test-get-subheading-text ()
  "Test getting text from a subheading."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "test-char")
    (let ((pos (canon--find-entity-heading "test-char")))
      (with-current-buffer canon--buffer
        (canon--append-to-subheading-text pos "Notes" "Test note content.")))
    (let ((notes (with-current-buffer canon--buffer
                   (canon--get-subheading-text
                    (canon--find-entity-heading "test-char")
                    "Notes"))))
      (should (stringp notes))
      (should (string-match-p "Test note content" notes)))))

(ert-deftest canon-test-set-subheading-text ()
  "Test setting subheading text."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "test-char")
    (let ((pos (canon--find-entity-heading "test-char")))
      (with-current-buffer canon--buffer
        (canon--set-subheading-text pos "Description" "New description.")))
    (let ((desc (with-current-buffer canon--buffer
                  (canon--get-subheading-text
                   (canon--find-entity-heading "test-char")
                   "Description"))))
      (should (string= (string-trim desc) "New description.")))))

(ert-deftest canon-test-append-to-subheading ()
  "Test appending to a subheading."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "test-char")
    (let ((pos (canon--find-entity-heading "test-char")))
      (with-current-buffer canon--buffer
        (canon--append-to-subheading-text pos "Notes" "First note.\n")
        (canon--append-to-subheading-text pos "Notes" "Second note.\n")))
    (let ((notes (with-current-buffer canon--buffer
                   (canon--get-subheading-text
                    (canon--find-entity-heading "test-char")
                    "Notes"))))
      (should (string-match-p "First note" notes))
      (should (string-match-p "Second note" notes)))))

;;;; Polymuse Tool Integration Tests

(ert-deftest canon-test-tool-lookup-entity ()
  "Test Polymuse tool for looking up entities."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((result (canon-tool-lookup-entity "alice")))
          (should (stringp result))
          (should (string-match-p "Alice is the main character" result)))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-tool-lookup-nonexistent ()
  "Test Polymuse tool for looking up nonexistent entity."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((result (canon-tool-lookup-entity "nonexistent")))
          (should (stringp result))
          (should (string-match-p "No entity found" result)))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-tool-search-by-property ()
  "Test Polymuse tool for searching by property."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((result (canon-tool-search-by-property "Role" "protagonist")))
          (should (stringp result))
          (should (string-match-p "alice" result)))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-tool-list-all-entities ()
  "Test Polymuse tool for listing all entities."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (let ((result (canon-tool-list-all-entities)))
          (should (stringp result))
          (should (string-match-p "alice" result))
          (should (string-match-p "bob" result))
          (should (string-match-p "castle" result)))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-tool-suggest-entity-update ()
  "Test Polymuse tool for suggesting entity updates."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (progn
          (let ((result (canon-tool-suggest-entity-update
                        "alice"
                        "Consider adding more backstory.")))
            (should (stringp result))
            (should (string-match-p "Suggestion added" result)))
          ;; Verify suggestion was added
          (with-current-buffer canon--buffer
            (let ((suggestions (canon--get-subheading-text
                               (canon--find-entity-heading "alice")
                               "Suggestions")))
              (should (stringp suggestions))
              (should (string-match-p "backstory" suggestions)))))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

;;;; Test Mode Tests

(ert-deftest canon-test-mode-skips-prompt ()
  "Test that test mode skips file prompts."
  (let ((canon-test-mode t))
    (with-temp-buffer
      ;; In test mode, canon-mode should not prompt
      (canon-mode 1)
      ;; Should not error even though canon-file is nil
      (should (bound-and-true-p canon-mode)))))

;;;; Org-Mode Wrapper Tests

(ert-deftest canon-test-org-find-heading ()
  "Test org-mode heading finding wrapper."
  (let* ((setup (canon-test-fixture-canon-with-entities))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (with-current-buffer canon--buffer
          (let ((pos (canon--org-find-heading
                     (lambda ()
                       (equal (org-entry-get nil "ID") "alice")))))
            (should pos)
            (goto-char pos)
            (should (looking-at "\\*\\* \\[Characters\\] alice"))))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

(ert-deftest canon-test-org-collect-headings-by-level ()
  "Test collecting headings by level."
  (let* ((setup (canon-test-fixture-simple-canon))
         (canon--buffer (car setup))
         (canon-file (cdr setup)))
    (unwind-protect
        (with-current-buffer canon--buffer
          (let ((level1-headings (canon--org-collect-headings-by-level 1)))
            (should (member "Characters" level1-headings))
            (should (member "Locations" level1-headings))))
      (kill-buffer canon--buffer)
      (delete-file canon-file))))

;;;; Edge Cases and Error Handling

(ert-deftest canon-test-empty-canon ()
  "Test operations on an empty canon."
  (canon-test-with-temp-canon ""
    (with-current-buffer canon--buffer
      (let ((types (canon--list-types)))
        (should (or (null types) (equal types '())))))))

(ert-deftest canon-test-duplicate-entity-ids ()
  "Test handling of duplicate entity IDs."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "duplicate")
    ;; Insert another with same ID in different type
    (canon-insert-entity "Characters" "duplicate")
    ;; Both should exist but finding should return the first one
    (should (canon--find-entity-heading "duplicate"))))

(ert-deftest canon-test-special-characters-in-id ()
  "Test entity IDs with special characters."
  (canon-test-with-temp-canon "* Characters\n\n"
    (canon-insert-entity "Characters" "test-char-123")
    (should (canon--find-entity-heading "test-char-123"))
    (let ((text (canon-get-entity-text "test-char-123")))
      (should (string-match-p ":ID: test-char-123" text)))))

(provide 'canon-test)
;;; canon-test.el ends here
