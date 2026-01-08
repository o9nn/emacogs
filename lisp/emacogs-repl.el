;;; emacogs-repl.el --- Interactive REPL for Emacogs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Maintainer: emacogs@gnu.org
;; Keywords: ai, cognitive-architecture, repl, interactive
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

;; Interactive REPL (Read-Eval-Print-Loop) for Emacogs.
;;
;; Features:
;; - Interactive command evaluation
;; - Command history with navigation
;; - Completion for atoms and functions
;; - Syntax highlighting
;; - Multi-line input support
;; - Query language for atomspace
;;
;; Usage:
;;
;;   M-x emacogs-repl
;;
;; REPL Commands:
;;
;;   (node ConceptNode "Cat")        ; Create a concept node
;;   (link InheritanceLink [cat animal]) ; Create a link
;;   (query InheritanceLink)         ; Query for all inheritance links
;;   (atoms)                         ; List all atoms
;;   (clear)                         ; Clear atomspace
;;   (help)                          ; Show help

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'opencog-atomspace)
(require 'opencog-tensor-logic)
(require 'agent-zero)

;;; Customization

(defgroup emacogs-repl nil
  "Interactive REPL for Emacogs."
  :group 'emacogs
  :prefix "emacogs-repl-")

(defcustom emacogs-repl-prompt "emacogs> "
  "Prompt string for Emacogs REPL."
  :type 'string
  :group 'emacogs-repl)

(defcustom emacogs-repl-history-size 1000
  "Maximum number of commands to keep in history."
  :type 'integer
  :group 'emacogs-repl)

;;; Variables

(defvar emacogs-repl-buffer-name "*Emacogs REPL*"
  "Name of the Emacogs REPL buffer.")

(defvar emacogs-repl-history nil
  "Command history for REPL.")

(defvar emacogs-repl--last-result nil
  "Last evaluation result.")

;;; REPL Commands

(defun emacogs-repl--cmd-node (type name)
  "Create and add a node of TYPE with NAME."
  (let ((node (opencog-atom-create-node type name)))
    (opencog-atomspace-add node)
    (format "Created: %s" (opencog-atom-to-string node))))

(defun emacogs-repl--cmd-link (type outgoing &optional tv)
  "Create and add a link of TYPE with OUTGOING atoms and optional TV."
  (let ((link (opencog-atom-create-link type outgoing tv)))
    (opencog-atomspace-add link)
    (format "Created: %s" (opencog-atom-to-string link))))

(defun emacogs-repl--cmd-query (pattern)
  "Query atomspace for atoms matching PATTERN."
  (let ((results (opencog-query pattern)))
    (if results
        (mapconcat #'opencog-atom-to-string results "\n")
      "No results found")))

(defun emacogs-repl--cmd-atoms ()
  "List all atoms in atomspace."
  (let ((atoms '()))
    (maphash (lambda (_handle atom)
               (push (opencog-atom-to-string atom) atoms))
             opencog-atomspace)
    (if atoms
        (mapconcat #'identity (nreverse atoms) "\n")
      "Atomspace is empty")))

(defun emacogs-repl--cmd-clear ()
  "Clear the atomspace."
  (opencog-atomspace-clear)
  "Atomspace cleared")

(defun emacogs-repl--cmd-stats ()
  "Show atomspace statistics."
  (let ((stats (opencog-atomspace-stats)))
    (format "Atoms: %d | Nodes: %d | Links: %d | Types: %d"
            (plist-get stats :total-atoms)
            (plist-get stats :node-count)
            (plist-get stats :link-count)
            (plist-get stats :type-count))))

(defun emacogs-repl--cmd-get (type name)
  "Get atom by TYPE and NAME from atomspace."
  (let ((atom (opencog-atomspace-get type name)))
    (if atom
        (opencog-atom-to-string atom)
      "Atom not found")))

(defun emacogs-repl--cmd-tv (strength confidence)
  "Create a truth value with STRENGTH and CONFIDENCE."
  (opencog-truth-value-simple strength confidence))

(defun emacogs-repl--cmd-inference (tv1 tv2 rule)
  "Apply inference RULE to TV1 and TV2."
  (let ((result (pcase rule
                  ('deduction (opencog-pln-deduction tv1 tv2))
                  ('and (opencog-pln-and tv1 tv2))
                  ('or (opencog-pln-or tv1 tv2))
                  (_ (error "Unknown rule: %s" rule)))))
    (format "(%.3f %.3f)"
            (opencog-truth-value-strength result)
            (opencog-truth-value-confidence result))))

(defun emacogs-repl--cmd-agents ()
  "List all agents."
  (let ((agents '()))
    (maphash (lambda (_id agent)
               (push (format "%s [%s] - %s"
                           (agent-zero-agent-name agent)
                           (agent-zero-agent-state agent)
                           (agent-zero-agent-type agent))
                     agents))
             agent-zero-agents)
    (if agents
        (mapconcat #'identity (nreverse agents) "\n")
      "No agents")))

(defun emacogs-repl--cmd-help ()
  "Display REPL help."
  "Emacogs REPL Commands:

Atomspace Operations:
  (node TYPE NAME)              - Create a node
  (link TYPE OUTGOING)          - Create a link
  (get TYPE NAME)               - Get atom from atomspace
  (query PATTERN)               - Query atomspace
  (atoms)                       - List all atoms
  (stats)                       - Show statistics
  (clear)                       - Clear atomspace

Truth Values & Inference:
  (tv STRENGTH CONFIDENCE)      - Create truth value
  (inference TV1 TV2 RULE)      - Apply inference rule

Agents:
  (agents)                      - List all agents

System:
  (help)                        - Show this help
  (quit)                        - Exit REPL

Examples:
  (node 'ConceptNode \"Cat\")
  (link 'InheritanceLink (list cat animal))
  (query 'InheritanceLink)
  (tv 0.9 0.8)
  (inference tv1 tv2 'deduction)")

;;; Evaluation

(defun emacogs-repl--eval (input)
  "Evaluate INPUT in REPL context."
  (condition-case err
      (let* ((form (read input))
             (result (cond
                     ;; Direct commands
                     ((and (listp form) (symbolp (car form)))
                      (pcase (car form)
                        ('node (apply #'emacogs-repl--cmd-node (cdr form)))
                        ('link (apply #'emacogs-repl--cmd-link (cdr form)))
                        ('query (apply #'emacogs-repl--cmd-query (cdr form)))
                        ('get (apply #'emacogs-repl--cmd-get (cdr form)))
                        ('atoms (emacogs-repl--cmd-atoms))
                        ('clear (emacogs-repl--cmd-clear))
                        ('stats (emacogs-repl--cmd-stats))
                        ('tv (apply #'emacogs-repl--cmd-tv (cdr form)))
                        ('inference (apply #'emacogs-repl--cmd-inference (cdr form)))
                        ('agents (emacogs-repl--cmd-agents))
                        ('help (emacogs-repl--cmd-help))
                        ('quit (kill-buffer emacogs-repl-buffer-name))
                        ;; Otherwise eval normally
                        (_ (eval form t))))
                     ;; Eval other forms
                     (t (eval form t)))))
        (setq emacogs-repl--last-result result)
        (format "%s" result))
    (error
     (format "Error: %s" (error-message-string err)))))

;;; Input Handling

(defun emacogs-repl-send-input ()
  "Send current input to REPL for evaluation."
  (interactive)
  (let ((input (buffer-substring-no-properties
                (save-excursion
                  (goto-char (point-max))
                  (comint-bol)
                  (point))
                (point-max))))
    (when (string-match-p "\\S-" input)
      (push input emacogs-repl-history)
      (when (> (length emacogs-repl-history) emacogs-repl-history-size)
        (setq emacogs-repl-history
              (cl-subseq emacogs-repl-history 0 emacogs-repl-history-size)))
      (comint-send-input)
      (let ((result (emacogs-repl--eval input)))
        (insert result "\n")
        (comint-output-filter (get-buffer-process (current-buffer))
                             (concat emacogs-repl-prompt))))))

(defun emacogs-repl-previous-input ()
  "Insert previous input from history."
  (interactive)
  (when emacogs-repl-history
    (delete-region (save-excursion (comint-bol) (point)) (point-max))
    (insert (car emacogs-repl-history))
    (setq emacogs-repl-history
          (append (cdr emacogs-repl-history)
                  (list (car emacogs-repl-history))))))

;;; Completion

(defun emacogs-repl-completion-at-point ()
  "Provide completion at point for REPL."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (emacogs-repl--completion-candidates)
            :exclusive 'no))))

(defun emacogs-repl--completion-candidates ()
  "Return completion candidates for REPL."
  (append
   ;; REPL commands
   '("node" "link" "query" "get" "atoms" "clear" "stats"
     "tv" "inference" "agents" "help" "quit")
   ;; Atom types
   (mapcar #'symbol-name opencog-atom-types)
   ;; Inference rules
   '("deduction" "inversion" "abduction" "and" "or" "not")
   ;; Variables
   (all-completions "" obarray #'boundp)))

;;; Major Mode

(defvar emacogs-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "RET") #'emacogs-repl-send-input)
    (define-key map (kbd "M-p") #'emacogs-repl-previous-input)
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keymap for Emacogs REPL mode.")

(define-derived-mode emacogs-repl-mode comint-mode "Emacogs-REPL"
  "Major mode for Emacogs REPL.

\\{emacogs-repl-mode-map}"
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote emacogs-repl-prompt)))
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-process-echoes nil)
  (add-hook 'completion-at-point-functions
            #'emacogs-repl-completion-at-point nil t))

;;; Entry Point

;;;###autoload
(defun emacogs-repl ()
  "Start Emacogs interactive REPL."
  (interactive)
  (let ((buffer (get-buffer-create emacogs-repl-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacogs-repl-mode)
        (emacogs-repl-mode)
        (make-comint-in-buffer "emacogs" buffer nil)
        (goto-char (point-max))
        (insert "Emacogs REPL - Version 1.1.0\n")
        (insert "Type (help) for commands, (quit) to exit\n\n")
        (insert emacogs-repl-prompt)))
    (pop-to-buffer buffer)))

(provide 'emacogs-repl)

;;; emacogs-repl.el ends here
