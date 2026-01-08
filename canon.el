;;; canon.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Niten
;;
;; Author: Niten <niten@fudo.org>
;; Maintainer: Niten <niten@fudo.org>
;; Created: November 22, 2025
;; Modified: November 22, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/niten/canon
;; Package-Requires: ((emacs "29.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defvar canon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c j") #'canon-jump-to-entity)
    (define-key map (kbd "C-c C-c i") #'canon-insert-entity)
    (define-key map (kbd "C-c C-c t") #'canon-add-type)
    map))

(define-minor-mode canon-mode
  "Mode for tracking the `canon' of a work: characters, locations, etc."
  :init-value nil
  :lighter " Canon"
  :keymap canon-mode-map
  (canon--maybe-init))

(defun canon--maybe-init ()
  "Ensure `canon-file' is set, prompting if not."
  (unless canon-file
    (setq-local canon-file
                (read-file-name "Canon file: "
                                default-directory
                                "canon.org"
                                nil nil)))
  (canon--get-buffer))

(defvar-local canon-file nil
  "Canon file for the work being written.")

(defvar-local canon--buffer nil
  "Buffer visiting the `canon-file'.")

(defun canon--get-buffer ()
  "Return the buffer visiting `canon-file', opening if needed."
  (unless canon-file
    (user-error "Can't open `canon-file', not defined"))
  (let ((canon canon-file)
        (buf (or (and (buffer-live-p canon--buffer)
                      canon--buffer)
                 (setq canon--buffer
                       (find-file-noselect canon-file)))))
    (with-current-buffer buf
      (unless canon-file (setq-local canon-file canon))
      (unless (bound-and-true-p canon-mode)
        (canon-mode 1)))
    buf))

(defun canon--find-entity-heading (id &optional type)
  "Return point at heading with :ID: property = ID.

If TYPE is non-nil, require that the heading title start with [TYPE]. ID
and TYPE should be strings."
  (catch 'found
    (org-map-entries
     (lambda ()
       (when (and (equal (org-entry-get nil "ID") id)
                  (or (not type)
                      (string-match-p
                       (format "\\[%s\\]" (regexp-quote type))
                       (nth 4 (org-heading-components)))))
         (throw 'found (point))))
     t 'file)))

(defun canon--get-subheading-text (entity-pos subheading)
  "From ENTITY-POS, return text under SUBHEADING.

SUBHEADING is the entry title, minus the leading *s."
  (save-excursion
    (goto-char entity-pos)
    (org-with-wide-buffer
     (let ((end (save-excursion (org-end-of-subtree t t))))
       (if (re-search-forward
            (concat "^\\*+ +" (regexp-quote subheading) "\\b") end t)
           (let ((start (line-end-position)))
             (forward-line 1)
             (setq start (point))
             (let ((sub-end (or (save-excursion
                                  (when (re-search-forward "^\\*+ " end t)
                                    (beginning-of-line)
                                    (point)))
                                end)))
               (buffer-substring-no-properties start sub-end)))
         nil)))))

(defun canon--set-subheading-text (entity-pos subheading new-text)
  "Replace contents of SUBHEADING below ENTITY-POS with NEW-TEXT."
  (save-excursion
    (goto-char entity-pos)
    (org-with-wide-buffer
     (let ((end (save-excursion (org-end-of-subtree t t))))
       (unless (re-search-forward
                (concat "^\\*+ +" (regexp-quote subheading) "\\b") end t)
         ;; Create the subheading at end of subtree if missing
         (goto-char end)
         (insert (format "\n*** %s\n" subheading)))
       ;; Now we're on the subheading line
       (forward-line 1)
       (let* ((start (point))
              (sub-end (or (save-excursion
                             (when (re-search-forward "^\\*+ " end t)
                               (beginning-of-line)
                               (point)))
                           end)))
         (delete-region start sub-end)
         (insert (string-trim-right new-text) "\n")))))
  (with-current-buffer (canon--get-buffer)
    (save-buffer)))

(defun canon--append-to-subheading-text (entity-pos subheading new-text)
  "Append NEW-TEXT to the contents of SUBHEADING below ENTITY-POS.

If SUBHEADING does not exist, it will be created at the end of the
entity subtree and NEW-TEXT will become its initial contents."
  (save-excursion
    (goto-char entity-pos)
    (org-with-wide-buffer
     (let ((end (save-excursion (org-end-of-subtree t t))))
       ;; Ensure the subheading exists; create if missing.
       (unless (re-search-forward
                (concat "^\\*+ +" (regexp-quote subheading) "\\b") end t)
         (goto-char end)
         (insert (format "\n*** %s\n" subheading))
         (forward-line 1))
       ;; Now we're on or just after the subheading line.
       (when (looking-at "^\\*+") ; if we’re still on the heading line
         (forward-line 1))
       (let* ((body-start (point))
              (sub-end (or (save-excursion
                             (when (re-search-forward "^\\*+ " end t)
                               (beginning-of-line)
                               (point)))
                           end)))
         ;; Jump to end of current body.
         (goto-char sub-end)
         ;; If there is existing text and we’re not at BOL, add a newline
         ;; so appended text starts on a fresh line.
         (unless (or (= body-start sub-end) (bolp))
           (insert "\n"))
         (insert (string-trim-right new-text) "\n")))))
  (with-current-buffer (canon--get-buffer)
    (save-buffer)))

(defun canon--get-section-text (entity-pos)
  "Return the full text under the entity at ENTITY-POS.

This includes everything after the heading line, up to but not
including the next heading of the same or higher level (i.e., the
end of the subtree). That means it will include any PROPERTIES
drawer, subheadings, etc."
  (save-excursion
    (goto-char entity-pos)
    (org-with-wide-buffer
     (let* ((start (progn
                     (forward-line 1) ; move past the heading line
                     (point)))
            (end   (save-excursion
                     (org-end-of-subtree t t)))) ; end of subtree
       (buffer-substring-no-properties start end)))))

(defun canon--set-section-text (entity-pos new-text)
  "Replace the full text under the entity at ENTITY-POS with NEW-TEXT.

NEW-TEXT should be the complete `org-mode' subtree body (everything
after the heading). Any existing content, including property drawers and
subheadings, will be replaced."
  (save-excursion
    (goto-char entity-pos)
    (org-with-wide-buffer
     (let* ((start (progn
                     (forward-line 1) ; past heading
                     (point)))
            (end   (save-excursion
                     (org-end-of-subtree t t))))
       (delete-region start end)
       (goto-char start)
       ;; trim trailing whitespace but ensure there's a newline at end
       (insert (string-trim-right new-text) "\n"))))
  (with-current-buffer (canon--get-buffer)
    (save-buffer)))

(defun canon--get-entity-text (id)
  "Return the full canon entry for entity ID."
  (when-let* ((pos (canon--find-entity-heading id)))
    (canon--get-section-text pos)))

(defun canon--set-entity-text (id text)
  "Set the canon entry TEXT for entity ID."
  (when-let* ((pos (canon--find-entity-heading id)))
    (canon--set-section-text pos text)))

(defun canon--get-entity-section (id section)
  "Return the canon entry for entity ID, under heading SECTION."
  (when-let* ((pos (canon--find-entity-heading id)))
    (canon--get-subheading-text pos section)))

(defun canon--goto-entity (id &optional type)
  "Move point in the canon buffer to the entity with ID.

If TYPE is non-nil, restrict the match to headings whose
title starts with [TYPE]. Return point if found, or nil."
  (let ((pos (canon--find-entity-heading id type)))
    (when pos
      (goto-char pos)
      (org-reveal)
      pos)))

(defun canon--list-types ()
  "Return a list of type names from top-level headings in the canon file.

Types are taken from level-1 Org headings. The returned list contains
the types as a list of strings."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((types '()))
     (while (re-search-forward org-heading-regexp nil t)
       (let* ((components (org-heading-components))
              (level      (nth 0 components))
              (title      (nth 4 components)))
         (when (= level 1)
           (push title types))))
     (delete-dups (nreverse types)))))

(defun canon--ensure-one-blank-line (&optional buffer)
  "Ensure that from the current point in BUFFER, there is one full blank line."
  (with-current-buffer (or buffer (current-buffer))
    (let ((end (point)))
      (skip-chars-backward " \t\n")
      (delete-region (point) end))
    (unless (bolp) (insert "\n"))
    (insert "\n")))

(defun canon--ensure-type-heading (type)
  "Ensure a top-level heading named TYPE exists in the canon file."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let* ((re (format "^\\* +%s\\s-*$" (regexp-quote type)))
          (found (when (re-search-forward re nil t)
                   (line-beginning-position))))
     (unless found
       ;; Create a new  top-level heading at end of buffer.
       (goto-char (point-max))
       (canon--ensure-one-blank-line)
       (insert (format "* %s\n" type))
       (setq found (line-beginning-position 0)))
     found)))

(defun canon--insert-entity-under-type (type id)
  "Insert an entity with ID under the TYPE section of the canon file.

TYPE is the type name as a string (eg. \"characters\" or \"locations\").
ID is the entity identifier string, written into the :ID: property.

Return point at the beginning of the new entity heading."
  (org-with-wide-buffer
   (let* ((type-pos (canon--ensure-type-heading type)))
     (goto-char type-pos)
     (org-end-of-subtree t t)
     (canon--ensure-one-blank-line)
     (let ((pos (point)))
       (insert (format "** [%s] %s\n" type id))
       (insert ":PROPERTIES:\n")
       (insert (format ":ID: %s\n" id))
       (insert ":END:\n\n")
       pos))))

(defun canon-insert-entity (type id)
  "Insert a new TYPE entity into the canon with ID."
  (interactive
   (let* ((types (with-current-buffer (canon--get-buffer) (canon--list-types)))
          (default-type (car types))
          (type (completing-read
                 (if types
                     (format "Type (default %s): " default-type)
                   "Type: ")
                 types nil nil nil nil default-type))
          (id (read-string "Entity ID: ")))
     (list (format "%s" type) id)))
  (let ((buf (canon--get-buffer)))
    (with-current-buffer buf
      (let ((pos (canon--insert-entity-under-type type id)))
        (pop-to-buffer buf)
        (goto-char pos)
        (org-reveal)
        pos))))

(defun canon-add-type (type)
  "Create a new type section named TYPE in the canon file, and jump to it.

TYPE is used as the title of a top-level org heading. If a heading with
that title already exists, jump to it."
  (interactive "sNew type name: ")
  (let ((buf (canon--get-buffer)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (goto-char (canon--ensure-type-heading type))
      (unless (bolp) (insert "\n"))
      (org-reveal))))

(defun canon-jump-to-entity (id &optional type)
  "Open `canon-file' in another window and jump to the entity with ID.

If TYPE is non-nil, restrict the match to headings whose
title starts with [TYPE]. Return point if found, or nil."
  (interactive "sEntity ID: ")
  (let ((buf (canon--get-buffer)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (unless (canon--goto-entity id type)
        (message "No entity with ID %S found in %s" id canon-file)))))

(defun canon-get-entity-text (id)
  "Given an entity ID, return the full text of that entity from the canon."
  (with-current-buffer (canon--get-buffer)
    (canon--get-entity-text id)))

(defun canon-set-entity-text (id text)
  "Replace the full text for entity ID with TEXT.

All text, including properties and subheadings, will be replaced."
  (with-current-buffer (canon--get-buffer)
    (canon--set-entity-text id text)))

(defun canon-get-entity-section (id sec)
  "Return SEC heading from within the entity ID from the canon."
  (with-current-buffer (canon--get-buffer)
    (canon--get-entity-section id sec)))

(defun canon-append-to-entity (id text &optional type)
  "Append the supplied TEXT to the entity ID in the canon.

If TYPE is non-nil, restrict the match to headings whose title starts
with [TYPE]."
  (with-current-buffer (canon--get-buffer)
    (if-let* ((pos (canon--find-entity-heading id type)))
        (progn
          (org-with-wide-buffer
           (goto-char pos)
           (org-end-of-subtree t t)
           (canon--ensure-one-blank-line)
           (insert (string-trim-right text) "\n"))
          (save-buffer))
      (user-error "No entity with ID %S%s found in %s"
                  id
                  (if type (format " of type %s" type) "")
                  canon-file))))

(defun canon-get-ids-by-property (prop value)
  "Return a list of entity IDS whose property PROP has VALUE.

PROP is a property name, like AKA.
VALUE is matched exactly against one of the multi-valued entries."
  (with-current-buffer (canon--get-buffer)
    (org-with-wide-buffer
     (let (results)
       (org-map-entries
        (lambda ()
          (let* ((id (org-entry-get nil "ID" nil))
                 (vals (org-entry-get-multivalued-property nil prop)))
            (when (and id (member value vals))
              (push id results))))
        t 'file)
       (nreverse results)))))

(defun canon-get-id-by-property (prop value)
  "Return the unique entity ID whose property PROP is VALUE.

If no entities match, return nil.
If more than one entity matches, signal an error.

PROP and VALUE are strings."
  (let ((ids (canon-get-ids-by-property prop value)))
    (pcase ids
      ('() nil)
      (`(,only) only)
      (_ (error "Multiple entities match %s=%s: %S" prop value ids)))))

(provide 'canon)
;;; canon.el ends here
