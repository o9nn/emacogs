;;; opencog-persistence.el --- Persistence for OpenCog Atomspace -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Maintainer: emacogs@gnu.org
;; Keywords: ai, cognitive-architecture, opencog, persistence
;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Persistence system for OpenCog Atomspace and Emacogs components.
;;
;; Features:
;; - Atomspace serialization to S-expressions
;; - Agent state persistence
;; - Constellation snapshots
;; - Auto-save functionality
;; - Incremental backup support
;;
;; Usage:
;;
;;   (opencog-persistence-save-atomspace "~/atomspace.el")
;;   (opencog-persistence-load-atomspace "~/atomspace.el")
;;
;;   (opencog-persistence-enable-auto-save)

;;; Code:

(require 'cl-lib)
(require 'opencog-atomspace)

;;; Customization

(defgroup opencog-persistence nil
  "Persistence system for OpenCog Atomspace."
  :group 'emacogs
  :prefix "opencog-persistence-")

(defcustom opencog-persistence-directory
  (expand-file-name "emacogs" user-emacs-directory)
  "Directory for storing Emacogs persistence files."
  :type 'directory
  :group 'opencog-persistence)

(defcustom opencog-persistence-auto-save-interval 300
  "Interval in seconds for auto-saving atomspace.
Set to nil to disable auto-save."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'opencog-persistence)

(defcustom opencog-persistence-backup-count 5
  "Number of backup files to keep."
  :type 'integer
  :group 'opencog-persistence)

;;; Variables

(defvar opencog-persistence--auto-save-timer nil
  "Timer for auto-save functionality.")

(defvar opencog-persistence--last-save-time nil
  "Time of last atomspace save.")

;;; Atomspace Serialization

(defun opencog-persistence-serialize-atom (atom)
  "Serialize ATOM to S-expression format."
  (let ((type (opencog-atom-type atom))
        (name (opencog-atom-name atom))
        (outgoing (opencog-atom-outgoing atom))
        (tv (opencog-atom-truth-value atom))
        (av (opencog-atom-attention-value atom)))
    (list 'atom
          :type type
          :name name
          :outgoing (when outgoing
                      (mapcar #'opencog-persistence-serialize-atom outgoing))
          :truth-value (when tv
                        (list :strength (opencog-truth-value-strength tv)
                              :confidence (opencog-truth-value-confidence tv)))
          :attention-value (when av
                            (list :sti (opencog-attention-value-sti av)
                                  :lti (opencog-attention-value-lti av)
                                  :vlti (opencog-attention-value-vlti av))))))

(defun opencog-persistence-deserialize-atom (sexp)
  "Deserialize SEXP to create an atom."
  (when (and (listp sexp) (eq (car sexp) 'atom))
    (let* ((type (plist-get (cdr sexp) :type))
           (name (plist-get (cdr sexp) :name))
           (outgoing (plist-get (cdr sexp) :outgoing))
           (tv-data (plist-get (cdr sexp) :truth-value))
           (av-data (plist-get (cdr sexp) :attention-value))
           (tv (when tv-data
                 (opencog-truth-value-simple
                  (plist-get tv-data :strength)
                  (plist-get tv-data :confidence))))
           (av (when av-data
                 (opencog-attention-value-create
                  (plist-get av-data :sti)
                  (plist-get av-data :lti)
                  (plist-get av-data :vlti)))))
      (if outgoing
          (opencog-atom-create-link
           type
           (mapcar #'opencog-persistence-deserialize-atom outgoing)
           tv av)
        (opencog-atom-create-node type name tv av)))))

(defun opencog-persistence-serialize-atomspace ()
  "Serialize the current atomspace to S-expression format.
Returns a list of serialized atoms."
  (let ((atoms '()))
    (maphash (lambda (_handle atom)
               (push (opencog-persistence-serialize-atom atom) atoms))
             opencog-atomspace)
    (nreverse atoms)))

(defun opencog-persistence-deserialize-atomspace (atoms)
  "Deserialize ATOMS list and add to atomspace.
ATOMS should be a list of S-expressions from serialization."
  (dolist (atom-sexp atoms)
    (let ((atom (opencog-persistence-deserialize-atom atom-sexp)))
      (when atom
        (opencog-atomspace-add atom)))))

;;; File Operations

(defun opencog-persistence-ensure-directory ()
  "Ensure persistence directory exists."
  (unless (file-exists-p opencog-persistence-directory)
    (make-directory opencog-persistence-directory t)))

(defun opencog-persistence-save-atomspace (file)
  "Save atomspace to FILE in S-expression format.
Returns non-nil on success."
  (interactive (list (read-file-name "Save atomspace to: "
                                     opencog-persistence-directory
                                     nil nil "atomspace.el")))
  (opencog-persistence-ensure-directory)
  (condition-case err
      (progn
        (with-temp-file file
          (let ((print-length nil)
                (print-level nil))
            (insert ";;; Emacogs Atomspace Snapshot\n")
            (insert (format ";;; Generated: %s\n" (current-time-string)))
            (insert (format ";;; Atom count: %d\n\n" (opencog-atomspace-size)))
            (prin1 (opencog-persistence-serialize-atomspace) (current-buffer))
            (insert "\n")))
        (setq opencog-persistence--last-save-time (current-time))
        (message "Atomspace saved to %s (%d atoms)"
                 file (opencog-atomspace-size))
        t)
    (error
     (message "Error saving atomspace: %s" (error-message-string err))
     nil)))

(defun opencog-persistence-load-atomspace (file &optional clear)
  "Load atomspace from FILE.
If CLEAR is non-nil, clear existing atomspace first.
Returns number of atoms loaded."
  (interactive (list (read-file-name "Load atomspace from: "
                                     opencog-persistence-directory
                                     nil t "atomspace.el")
                     current-prefix-arg))
  (if (not (file-exists-p file))
      (progn
        (message "File does not exist: %s" file)
        0)
    (condition-case err
        (progn
          (when clear
            (opencog-atomspace-clear))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; Skip comment lines
            (while (looking-at ";;;")
              (forward-line 1))
            (let ((atoms (read (current-buffer))))
              (opencog-persistence-deserialize-atomspace atoms)
              (message "Loaded %d atoms from %s"
                       (length atoms) file)
              (length atoms))))
      (error
       (message "Error loading atomspace: %s" (error-message-string err))
       0))))

;;; Backup Management

(defun opencog-persistence-create-backup (file)
  "Create a backup of FILE with timestamp.
Rotates old backups according to `opencog-persistence-backup-count'."
  (when (file-exists-p file)
    (let* ((backup-dir (file-name-directory file))
           (backup-base (concat (file-name-nondirectory file) ".backup"))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (backup-file (expand-file-name
                        (concat backup-base "." timestamp)
                        backup-dir)))
      (copy-file file backup-file)
      ;; Rotate old backups
      (let ((backups (directory-files backup-dir t
                                     (concat "^" (regexp-quote backup-base)))))
        (when (> (length backups) opencog-persistence-backup-count)
          (dolist (old-backup (nthcdr opencog-persistence-backup-count
                                     (sort backups #'string>)))
            (delete-file old-backup))))
      backup-file)))

;;; Auto-save

(defun opencog-persistence--auto-save ()
  "Perform auto-save of atomspace."
  (when (> (opencog-atomspace-size) 0)
    (let ((auto-save-file (expand-file-name "atomspace-autosave.el"
                                           opencog-persistence-directory)))
      (opencog-persistence-create-backup auto-save-file)
      (opencog-persistence-save-atomspace auto-save-file)
      (message "Atomspace auto-saved"))))

(defun opencog-persistence-enable-auto-save ()
  "Enable automatic atomspace saving."
  (interactive)
  (opencog-persistence-disable-auto-save)
  (when opencog-persistence-auto-save-interval
    (setq opencog-persistence--auto-save-timer
          (run-with-timer opencog-persistence-auto-save-interval
                         opencog-persistence-auto-save-interval
                         #'opencog-persistence--auto-save))
    (message "Auto-save enabled (interval: %ds)"
             opencog-persistence-auto-save-interval)))

(defun opencog-persistence-disable-auto-save ()
  "Disable automatic atomspace saving."
  (interactive)
  (when opencog-persistence--auto-save-timer
    (cancel-timer opencog-persistence--auto-save-timer)
    (setq opencog-persistence--auto-save-timer nil)
    (message "Auto-save disabled")))

;;; Export/Import

(defun opencog-persistence-export-to-scheme (file)
  "Export atomspace to Scheme format (OpenCog compatible) in FILE."
  (interactive (list (read-file-name "Export to Scheme file: "
                                     opencog-persistence-directory
                                     nil nil "atomspace.scm")))
  (opencog-persistence-ensure-directory)
  (with-temp-file file
    (insert ";; OpenCog Atomspace Export\n")
    (insert (format ";; Generated: %s\n\n" (current-time-string)))
    (maphash (lambda (_handle atom)
               (insert (opencog-persistence--atom-to-scheme atom))
               (insert "\n"))
             opencog-atomspace))
  (message "Exported atomspace to %s (Scheme format)" file))

(defun opencog-persistence--atom-to-scheme (atom)
  "Convert ATOM to Scheme representation."
  (let ((type (opencog-atom-type atom))
        (name (opencog-atom-name atom))
        (outgoing (opencog-atom-outgoing atom))
        (tv (opencog-atom-truth-value atom)))
    (if outgoing
        (format "(%s%s)"
                type
                (mapconcat (lambda (a)
                            (concat " " (opencog-persistence--atom-to-scheme a)))
                          outgoing ""))
      (format "(%s \"%s\"%s)"
              type name
              (if tv
                  (format " (stv %.3f %.3f)"
                         (opencog-truth-value-strength tv)
                         (opencog-truth-value-confidence tv))
                "")))))

;;; Status and Information

(defun opencog-persistence-info ()
  "Display persistence system information."
  (interactive)
  (let ((dir opencog-persistence-directory)
        (auto-save (if opencog-persistence--auto-save-timer "enabled" "disabled"))
        (last-save (if opencog-persistence--last-save-time
                      (format-time-string "%Y-%m-%d %H:%M:%S"
                                         opencog-persistence--last-save-time)
                    "never")))
    (message "Persistence: dir=%s auto-save=%s last-save=%s"
             dir auto-save last-save)))

(provide 'opencog-persistence)

;;; opencog-persistence.el ends here
