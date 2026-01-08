;;; opencog-atomspace.el --- OpenCog Atomspace for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Keywords: ai, cognitive-architecture, opencog, atomspace
;; Version: 1.0.0
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

;; OpenCog Atomspace implementation for Emacs - a hypergraph database
;; for neuro-symbolic AI and cognitive architecture.
;;
;; The Atomspace is the core knowledge representation system that stores
;; atoms (nodes and links) with associated truth values and attention values.
;;
;; Key features:
;; - Hypergraph-based knowledge representation
;; - Truth value system for uncertain reasoning
;; - Attention allocation mechanism
;; - Pattern matching and queries
;; - Integration with org-mode for knowledge capture

;;; Code:

(require 'cl-lib)
(require 'eieio)

;;; Atom Types

(defconst opencog-atom-types
  '(;; Basic node types
    Node ConceptNode PredicateNode SchemaNode
    NumberNode TypeNode VariableNode
    ;; Link types
    Link ListLink SetLink OrderedLink
    EvaluationLink InheritanceLink SimilarityLink
    ImplicationLink ExecutionLink
    ;; Logic links
    AndLink OrLink NotLink
    ;; Temporal links
    SequentialLink SimultaneousLink
    ;; Cognitive links
    AttentionLink ImportanceLink
    )
  "Standard OpenCog atom types.")

;;; Truth Values

(cl-defstruct (opencog-truth-value
               (:constructor opencog-truth-value-create)
               (:copier nil))
  "Truth value for atoms with strength and confidence."
  (strength 0.5 :type float
            :documentation "Truth strength [0.0, 1.0]")
  (confidence 0.0 :type float
              :documentation "Confidence in the truth value [0.0, 1.0]"))

(defun opencog-truth-value-simple (strength confidence)
  "Create a simple truth value with STRENGTH and CONFIDENCE."
  (opencog-truth-value-create
   :strength (float (max 0.0 (min 1.0 strength)))
   :confidence (float (max 0.0 (min 1.0 confidence)))))

;;; Attention Values

(cl-defstruct (opencog-attention-value
               (:constructor opencog-attention-value-create)
               (:copier nil))
  "Attention value for importance-based resource allocation."
  (sti 0 :type integer
       :documentation "Short-term importance")
  (lti 0 :type integer
       :documentation "Long-term importance")
  (vlti 0 :type integer
        :documentation "Very long-term importance"))

;;; Atoms

(cl-defstruct (opencog-atom
               (:constructor opencog-atom--create)
               (:copier nil))
  "Base structure for all atoms in the atomspace."
  (type 'Node :type symbol
        :documentation "Atom type")
  (name nil :type (or null string)
        :documentation "Atom name (for nodes)")
  (outgoing nil :type list
             :documentation "Outgoing set (for links)")
  (incoming nil :type list
            :documentation "Incoming set")
  (truth-value (opencog-truth-value-simple 1.0 0.0)
               :type opencog-truth-value
               :documentation "Truth value")
  (attention-value (opencog-attention-value-create)
                   :type opencog-attention-value
                   :documentation "Attention value")
  (metadata nil :type list
            :documentation "Additional metadata"))

(defun opencog-atom-create-node (type name &optional tv)
  "Create a node atom of TYPE with NAME and optional truth value TV."
  (opencog-atom--create
   :type type
   :name name
   :truth-value (or tv (opencog-truth-value-simple 1.0 0.0))))

(defun opencog-atom-create-link (type outgoing &optional tv)
  "Create a link atom of TYPE with OUTGOING atoms and optional truth value TV."
  (opencog-atom--create
   :type type
   :outgoing outgoing
   :truth-value (or tv (opencog-truth-value-simple 1.0 0.0))))

(defun opencog-atom-to-string (atom)
  "Convert ATOM to string representation."
  (if (opencog-atom-name atom)
      ;; Node
      (format "(%s \"%s\")"
              (opencog-atom-type atom)
              (opencog-atom-name atom))
    ;; Link
    (format "(%s %s)"
            (opencog-atom-type atom)
            (mapconcat #'opencog-atom-to-string
                       (opencog-atom-outgoing atom)
                       " "))))

;;; Atomspace

(defvar opencog-atomspace (make-hash-table :test 'equal)
  "The global atomspace - a hash table mapping atom keys to atoms.")

(defvar opencog-atomspace-index (make-hash-table :test 'eq)
  "Index for fast atom lookup by type.")

(defun opencog-atomspace-clear ()
  "Clear the entire atomspace."
  (clrhash opencog-atomspace)
  (clrhash opencog-atomspace-index))

(defun opencog-atom-key (atom)
  "Generate a unique key for ATOM."
  (if (opencog-atom-name atom)
      ;; Node key
      (format "%s:%s" (opencog-atom-type atom) (opencog-atom-name atom))
    ;; Link key
    (format "%s:%s"
            (opencog-atom-type atom)
            (mapconcat (lambda (a) (format "%S" (opencog-atom-key a)))
                       (opencog-atom-outgoing atom)
                       ","))))

(defun opencog-atomspace-add (atom)
  "Add ATOM to the atomspace. Return the atom or existing equivalent."
  (let ((key (opencog-atom-key atom)))
    (or (gethash key opencog-atomspace)
        (progn
          (puthash key atom opencog-atomspace)
          ;; Update type index
          (let ((type (opencog-atom-type atom)))
            (push atom (gethash type opencog-atomspace-index)))
          ;; Update incoming sets for links
          (when (opencog-atom-outgoing atom)
            (dolist (target (opencog-atom-outgoing atom))
              (push atom (opencog-atom-incoming target))))
          atom))))

(defun opencog-atomspace-get (type name)
  "Get a node atom of TYPE with NAME from atomspace."
  (gethash (format "%s:%s" type name) opencog-atomspace))

(defun opencog-atomspace-get-atoms-by-type (type)
  "Get all atoms of TYPE from atomspace."
  (gethash type opencog-atomspace-index))

(defun opencog-atomspace-get-all-atoms ()
  "Get all atoms from atomspace."
  (let (atoms)
    (maphash (lambda (_key atom) (push atom atoms)) opencog-atomspace)
    atoms))

;;; Pattern Matching

(defun opencog-pattern-match (pattern atomspace-atoms)
  "Match PATTERN against ATOMSPACE-ATOMS. Return list of matches."
  (let (matches)
    (dolist (atom atomspace-atoms)
      (when (opencog-pattern-match-atom pattern atom)
        (push atom matches)))
    (nreverse matches)))

(defun opencog-pattern-match-atom (pattern atom)
  "Check if PATTERN matches ATOM."
  (cond
   ((eq pattern '_) t) ; wildcard
   ((and (symbolp pattern)
         (string-prefix-p "?" (symbol-name pattern)))
    t) ; variable - matches anything
   ((and (symbolp pattern)
         (eq pattern (opencog-atom-type atom)))
    t) ; type match
   ((and (consp pattern)
         (eq (car pattern) (opencog-atom-type atom)))
    ;; Structure match
    (if (opencog-atom-name atom)
        (equal (cadr pattern) (opencog-atom-name atom))
      (and (= (length (cdr pattern))
              (length (opencog-atom-outgoing atom)))
           (cl-every #'opencog-pattern-match-atom
                     (cdr pattern)
                     (opencog-atom-outgoing atom)))))
   (t nil)))

;;; Query System

(defun opencog-query (pattern)
  "Execute a query with PATTERN and return matching atoms."
  (let ((all-atoms (opencog-atomspace-get-all-atoms)))
    (opencog-pattern-match pattern all-atoms)))

;;; Utilities

(defun opencog-atomspace-size ()
  "Return the number of atoms in the atomspace."
  (hash-table-count opencog-atomspace))

(defun opencog-atomspace-stats ()
  "Return statistics about the atomspace."
  (let ((type-counts (make-hash-table :test 'eq))
        (total 0))
    (maphash (lambda (_key atom)
               (cl-incf total)
               (let ((type (opencog-atom-type atom)))
                 (puthash type (1+ (gethash type type-counts 0)) type-counts)))
             opencog-atomspace)
    (list :total total
          :by-type type-counts)))

;;; Interactive Functions

(defun opencog-atomspace-display ()
  "Display the current atomspace in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*OpenCog Atomspace*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "OpenCog Atomspace\n")
      (insert "=================\n\n")
      (let ((stats (opencog-atomspace-stats)))
        (insert (format "Total atoms: %d\n\n" (plist-get stats :total)))
        (insert "Atoms by type:\n")
        (maphash (lambda (type count)
                   (insert (format "  %s: %d\n" type count)))
                 (plist-get stats :by-type)))
      (insert "\nAtoms:\n")
      (maphash (lambda (_key atom)
                 (insert (format "  %s\n" (opencog-atom-to-string atom))))
               opencog-atomspace))
    (display-buffer buf)))

(provide 'opencog-atomspace)
;;; opencog-atomspace.el ends here
