;;; opencog-org-constellations.el --- Org-mode Knowledge Constellations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Keywords: org-mode, knowledge, opencog, constellations
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (opencog-atomspace "1.0.0"))

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

;; Org-mode Knowledge Constellations for OpenCog integration.
;; Provides modular deployment of Emacs Lisp AI org-mode constellations.
;;
;; Features:
;; - Convert org-mode structures to atomspace
;; - Constellation patterns for knowledge organization
;; - Semantic linking between org nodes
;; - Knowledge extraction from org documents
;; - Org-babel integration for cognitive processing
;; - Modular constellation deployment

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'opencog-atomspace)

;;; Constellation Structures

(cl-defstruct (opencog-constellation
               (:constructor opencog-constellation-create)
               (:copier nil))
  "Knowledge constellation structure."
  (id nil :type symbol
      :documentation "Constellation identifier")
  (name "" :type string
        :documentation "Constellation name")
  (nodes nil :type list
         :documentation "Nodes in constellation")
  (links nil :type list
         :documentation "Links between nodes")
  (metadata nil :type list
            :documentation "Constellation metadata")
  (org-file nil :type (or null string)
            :documentation "Source org file"))

(defvar opencog-constellations (make-hash-table :test 'eq)
  "Registry of knowledge constellations.")

;;; Org-mode to Atomspace Conversion

(defun opencog-org-headline-to-node (headline)
  "Convert org HEADLINE to an atomspace node."
  (let* ((title (org-element-property :raw-value headline))
         (level (org-element-property :level headline))
         (tags (org-element-property :tags headline))
         (node (opencog-atom-create-node 'ConceptNode title)))
    ;; Add metadata
    (setf (opencog-atom-metadata node)
          (list :level level :tags tags :type 'org-headline))
    node))

(defun opencog-org-link-to-link (link)
  "Convert org LINK to an atomspace link."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (source-node (opencog-atom-create-node 'ConceptNode "current"))
         (target-node (opencog-atom-create-node 'ConceptNode path)))
    (opencog-atom-create-link 'EvaluationLink
                              (list source-node target-node))))

(defun opencog-org-to-atomspace (org-file)
  "Parse ORG-FILE and convert its contents to atomspace."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           (headlines (org-element-map tree 'headline #'identity))
           (links (org-element-map tree 'link #'identity))
           (nodes nil)
           (atom-links nil))
      
      ;; Convert headlines to nodes
      (dolist (headline headlines)
        (let ((node (opencog-org-headline-to-node headline)))
          (opencog-atomspace-add node)
          (push node nodes)))
      
      ;; Create hierarchical links
      (let ((prev-nodes (make-vector 10 nil)))
        (dolist (headline headlines)
          (let* ((level (org-element-property :level headline))
                 (title (org-element-property :raw-value headline))
                 (current-node (opencog-atomspace-get 'ConceptNode title))
                 (parent-node (aref prev-nodes (1- level))))
            (when (and parent-node current-node)
              (let ((link (opencog-atom-create-link
                           'InheritanceLink
                           (list current-node parent-node))))
                (opencog-atomspace-add link)
                (push link atom-links)))
            (aset prev-nodes level current-node))))
      
      (list :nodes nodes :links atom-links))))

;;; Constellation Patterns

(defun opencog-create-constellation (name org-file)
  "Create a constellation named NAME from ORG-FILE."
  (let* ((id (gensym "const-"))
         (parsed (opencog-org-to-atomspace org-file))
         (constellation (opencog-constellation-create
                         :id id
                         :name name
                         :nodes (plist-get parsed :nodes)
                         :links (plist-get parsed :links)
                         :org-file org-file)))
    (puthash id constellation opencog-constellations)
    constellation))

(defun opencog-constellation-add-node (constellation node)
  "Add NODE to CONSTELLATION."
  (push node (opencog-constellation-nodes constellation))
  (opencog-atomspace-add node))

(defun opencog-constellation-link-nodes (constellation node1 node2 link-type)
  "Link NODE1 to NODE2 in CONSTELLATION with LINK-TYPE."
  (let ((link (opencog-atom-create-link link-type (list node1 node2))))
    (push link (opencog-constellation-links constellation))
    (opencog-atomspace-add link)))

;;; Semantic Extraction

(defun opencog-org-extract-concepts (text)
  "Extract concepts from TEXT and create concept nodes."
  (let ((words (split-string text "[ \t\n.,;:!?()]+" t))
        (concepts nil))
    (dolist (word words)
      (when (> (length word) 3) ; Filter short words
        (let ((node (opencog-atom-create-node 'ConceptNode word)))
          (opencog-atomspace-add node)
          (push node concepts))))
    concepts))

(defun opencog-org-extract-relationships (text)
  "Extract relationships from TEXT using simple patterns."
  (let ((relationships nil)
        ;; Simple pattern: "X is a Y", "X has Y"
        (patterns '(("\\([A-Za-z]+\\) is a \\([A-Za-z]+\\)" . InheritanceLink)
                    ("\\([A-Za-z]+\\) has \\([A-Za-z]+\\)" . EvaluationLink))))
    (dolist (pattern-pair patterns)
      (let ((pattern (car pattern-pair))
            (link-type (cdr pattern-pair)))
        (when (string-match pattern text)
          (let* ((source (match-string 1 text))
                 (target (match-string 2 text))
                 (source-node (opencog-atom-create-node 'ConceptNode source))
                 (target-node (opencog-atom-create-node 'ConceptNode target))
                 (link (opencog-atom-create-link link-type
                                                 (list source-node target-node))))
            (opencog-atomspace-add source-node)
            (opencog-atomspace-add target-node)
            (opencog-atomspace-add link)
            (push link relationships)))))
    relationships))

;;; Org-babel Integration

(defun opencog-org-babel-atomspace ()
  "Provide atomspace context to org-babel blocks."
  (let ((stats (opencog-atomspace-stats)))
    (format "Atomspace: %d atoms" (plist-get stats :total))))

(defun opencog-org-babel-query (query-pattern)
  "Execute atomspace QUERY-PATTERN from org-babel."
  (let ((results (opencog-query query-pattern)))
    (mapcar #'opencog-atom-to-string results)))

;;; Modular Constellation Deployment

(cl-defstruct (opencog-constellation-module
               (:constructor opencog-constellation-module-create)
               (:copier nil))
  "Modular constellation deployment unit."
  (name nil :type symbol
        :documentation "Module name")
  (constellations nil :type list
                  :documentation "Constellations in module")
  (dependencies nil :type list
                :documentation "Required modules")
  (state 'undeployed :type symbol
         :documentation "Deployment state"))

(defvar opencog-constellation-modules (make-hash-table :test 'eq)
  "Registry of constellation modules.")

(defun opencog-deploy-constellation-module (name org-files)
  "Deploy constellation module NAME from ORG-FILES."
  (let ((module (opencog-constellation-module-create
                 :name name
                 :state 'undeployed))
        (constellations nil))
    (dolist (org-file org-files)
      (let ((constellation (opencog-create-constellation
                            (file-name-base org-file)
                            org-file)))
        (push constellation constellations)))
    (setf (opencog-constellation-module-constellations module) constellations)
    (setf (opencog-constellation-module-state module) 'deployed)
    (puthash name module opencog-constellation-modules)
    module))

(defun opencog-undeploy-constellation-module (name)
  "Undeploy constellation module NAME."
  (let ((module (gethash name opencog-constellation-modules)))
    (when module
      (setf (opencog-constellation-module-state module) 'undeployed)
      (remhash name opencog-constellation-modules))))

;;; Constellation Queries

(defun opencog-constellation-find-related (constellation node)
  "Find nodes related to NODE in CONSTELLATION."
  (let ((related nil))
    (dolist (link (opencog-constellation-links constellation))
      (when (member node (opencog-atom-outgoing link))
        (dolist (related-node (opencog-atom-outgoing link))
          (unless (eq related-node node)
            (push related-node related)))))
    related))

(defun opencog-constellation-find-path (constellation start-node end-node)
  "Find path from START-NODE to END-NODE in CONSTELLATION."
  (let ((visited (make-hash-table :test 'equal))
        (queue (list (list start-node)))
        (path nil))
    (while (and queue (not path))
      (let* ((current-path (pop queue))
             (current-node (car (last current-path))))
        (unless (gethash (opencog-atom-key current-node) visited)
          (puthash (opencog-atom-key current-node) t visited)
          (if (eq current-node end-node)
              (setq path current-path)
            (let ((neighbors (opencog-constellation-find-related
                              constellation current-node)))
              (dolist (neighbor neighbors)
                (push (append current-path (list neighbor)) queue)))))))
    path))

;;; Visualization

(defun opencog-constellation-to-dot (constellation)
  "Convert CONSTELLATION to Graphviz DOT format."
  (let ((dot "digraph Constellation {\n"))
    ;; Add nodes
    (dolist (node (opencog-constellation-nodes constellation))
      (setq dot (concat dot
                        (format "  \"%s\" [label=\"%s\"];\n"
                                (opencog-atom-name node)
                                (opencog-atom-name node)))))
    ;; Add links
    (dolist (link (opencog-constellation-links constellation))
      (let ((outgoing (opencog-atom-outgoing link)))
        (when (= (length outgoing) 2)
          (setq dot (concat dot
                            (format "  \"%s\" -> \"%s\" [label=\"%s\"];\n"
                                    (opencog-atom-name (car outgoing))
                                    (opencog-atom-name (cadr outgoing))
                                    (opencog-atom-type link)))))))
    (concat dot "}\n")))

;;; Interactive Functions

(defun opencog-constellation-display (constellation-id)
  "Display CONSTELLATION-ID in a buffer."
  (interactive
   (list (intern (completing-read "Constellation: "
                                   (let (names)
                                     (maphash (lambda (id _c) (push id names))
                                              opencog-constellations)
                                     names)))))
  (let ((constellation (gethash constellation-id opencog-constellations)))
    (unless constellation
      (error "Constellation not found: %s" constellation-id))
    (let ((buf (get-buffer-create
                (format "*Constellation: %s*"
                        (opencog-constellation-name constellation)))))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "Constellation: %s\n"
                        (opencog-constellation-name constellation)))
        (insert "=================================\n\n")
        (insert (format "Source: %s\n"
                        (opencog-constellation-org-file constellation)))
        (insert (format "Nodes: %d\n"
                        (length (opencog-constellation-nodes constellation))))
        (insert (format "Links: %d\n\n"
                        (length (opencog-constellation-links constellation))))
        (insert "Nodes:\n")
        (dolist (node (opencog-constellation-nodes constellation))
          (insert (format "  %s\n" (opencog-atom-to-string node))))
        (insert "\nLinks:\n")
        (dolist (link (opencog-constellation-links constellation))
          (insert (format "  %s\n" (opencog-atom-to-string link)))))
      (display-buffer buf))))

(defun opencog-constellation-list ()
  "List all constellations."
  (interactive)
  (let ((buf (get-buffer-create "*OpenCog Constellations*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "OpenCog Knowledge Constellations\n")
      (insert "==================================\n\n")
      (insert (format "Total constellations: %d\n\n"
                      (hash-table-count opencog-constellations)))
      (maphash (lambda (id constellation)
                 (insert (format "%s: %s\n"
                                 id
                                 (opencog-constellation-name constellation)))
                 (insert (format "  File: %s\n"
                                 (opencog-constellation-org-file constellation)))
                 (insert (format "  Nodes: %d, Links: %d\n\n"
                                 (length (opencog-constellation-nodes constellation))
                                 (length (opencog-constellation-links constellation)))))
               opencog-constellations))
    (display-buffer buf)))

(provide 'opencog-org-constellations)
;;; opencog-org-constellations.el ends here
