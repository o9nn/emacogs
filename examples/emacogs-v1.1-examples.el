;;; emacogs-v1.1-examples.el --- Examples for Emacogs v1.1.0 features -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;;; Commentary:

;; This file contains examples demonstrating the new features in Emacogs v1.1.0:
;; - Persistence system
;; - Learning capabilities
;; - Visualization enhancements
;; - Interactive REPL

;;; Code:

(require 'emacogs)

;;; Example 1: Persistence System

(defun emacogs-example-persistence ()
  "Demonstrate persistence system features."
  (interactive)
  (message "=== Persistence System Example ===")
  
  ;; Clear and create sample atomspace
  (opencog-atomspace-clear)
  (let ((ai (opencog-atom-create-node 'ConceptNode "AI"))
        (ml (opencog-atom-create-node 'ConceptNode "MachineLearning"))
        (dl (opencog-atom-create-node 'ConceptNode "DeepLearning")))
    (opencog-atomspace-add ai)
    (opencog-atomspace-add ml)
    (opencog-atomspace-add dl)
    (opencog-atomspace-add
     (opencog-atom-create-link 'InheritanceLink (list ml ai)
                               (opencog-truth-value-simple 0.95 0.9)))
    (opencog-atomspace-add
     (opencog-atom-create-link 'InheritanceLink (list dl ml)
                               (opencog-truth-value-simple 0.9 0.85))))
  
  (message "Created %d atoms" (opencog-atomspace-size))
  
  ;; Save atomspace
  (let ((file (expand-file-name "test-atomspace.el"
                                opencog-persistence-directory)))
    (opencog-persistence-save-atomspace file)
    (message "Saved to: %s" file)
    
    ;; Clear and reload
    (opencog-atomspace-clear)
    (message "Cleared atomspace (size: %d)" (opencog-atomspace-size))
    
    (opencog-persistence-load-atomspace file)
    (message "Reloaded atomspace (size: %d)" (opencog-atomspace-size)))
  
  ;; Show persistence info
  (opencog-persistence-info))

;;; Example 2: Learning System

(defun emacogs-example-learning ()
  "Demonstrate learning system features."
  (interactive)
  (message "=== Learning System Example ===")
  
  ;; Create knowledge graph with attention values
  (opencog-atomspace-clear)
  (let ((cat (opencog-atom-create-node 'ConceptNode "Cat"))
        (dog (opencog-atom-create-node 'ConceptNode "Dog"))
        (mammal (opencog-atom-create-node 'ConceptNode "Mammal"))
        (animal (opencog-atom-create-node 'ConceptNode "Animal")))
    
    ;; Add atoms with initial attention
    (dolist (atom (list cat dog mammal animal))
      (setf (opencog-atom-attention-value atom)
            (opencog-attention-value-create 20 5 1))
      (opencog-atomspace-add atom))
    
    ;; Create links
    (let ((link1 (opencog-atom-create-link
                  'InheritanceLink (list cat mammal)
                  (opencog-truth-value-simple 0.95 0.9)))
          (link2 (opencog-atom-create-link
                  'InheritanceLink (list dog mammal)
                  (opencog-truth-value-simple 0.95 0.9)))
          (link3 (opencog-atom-create-link
                  'InheritanceLink (list mammal animal)
                  (opencog-truth-value-simple 1.0 0.95))))
      (opencog-atomspace-add link1)
      (opencog-atomspace-add link2)
      (opencog-atomspace-add link3))
    
    ;; Boost attention on "Cat"
    (opencog-learning--increase-sti cat 50)
    (message "Increased attention on Cat")
    
    ;; Spread attention
    (opencog-learning-spread-attention-network 3)
    (message "Attention spread completed")
    
    ;; Apply Hebbian learning
    (opencog-learning-hebbian-update-all)
    (message "Hebbian learning applied")
    
    ;; Mine patterns
    (let ((patterns (opencog-learning-mine-patterns 2)))
      (message "Mined %d patterns" (length patterns)))
    
    ;; Show statistics
    (opencog-learning-stats)))

;;; Example 3: Visualization

(defun emacogs-example-visualization ()
  "Demonstrate visualization features."
  (interactive)
  (message "=== Visualization Example ===")
  
  ;; Ensure we have data
  (when (= (opencog-atomspace-size) 0)
    (emacogs-example-learning))
  
  ;; Show different visualizations
  (message "Displaying system overview...")
  (opencog-visualization-system-overview)
  
  (sit-for 2)
  
  (message "Displaying attention heat map...")
  (opencog-visualization-attention-map)
  
  (sit-for 2)
  
  (message "Displaying agent activity...")
  (opencog-visualization-agent-activity)
  
  (sit-for 2)
  
  (message "Displaying truth value distribution...")
  (opencog-visualization-truth-distribution)
  
  (message "Visualization complete - check visualization buffer"))

;;; Example 4: Interactive REPL Usage

(defun emacogs-example-repl-demo ()
  "Demonstrate REPL usage with sample commands."
  (interactive)
  (message "=== REPL Demo ===")
  (message "Starting Emacogs REPL...")
  (message "Try these commands in the REPL:")
  (message "  (node 'ConceptNode \"Cat\")")
  (message "  (node 'ConceptNode \"Animal\")")
  (message "  (atoms)")
  (message "  (stats)")
  (message "  (help)")
  (emacogs-repl))

;;; Example 5: Complete v1.1.0 Workflow

(defun emacogs-example-v11-workflow ()
  "Run a complete workflow demonstrating all v1.1.0 features."
  (interactive)
  (message "=== Emacogs v1.1.0 Complete Workflow ===")
  
  ;; 1. Initialize system
  (message "\n1. Initializing Emacogs...")
  (unless emacogs-initialized
    (emacogs-initialize))
  (unless emacogs-running
    (emacogs-start))
  
  ;; 2. Create knowledge
  (message "\n2. Creating knowledge base...")
  (opencog-atomspace-clear)
  (let ((cognitive-arch (opencog-atom-create-node 'ConceptNode "CognitiveArchitecture"))
        (opencog (opencog-atom-create-node 'ConceptNode "OpenCog"))
        (emacogs (opencog-atom-create-node 'ConceptNode "Emacogs"))
        (atomspace (opencog-atom-create-node 'ConceptNode "Atomspace")))
    
    (opencog-atomspace-add cognitive-arch)
    (opencog-atomspace-add opencog)
    (opencog-atomspace-add emacogs)
    (opencog-atomspace-add atomspace)
    
    ;; Set attention values
    (dolist (atom (list cognitive-arch opencog emacogs atomspace))
      (setf (opencog-atom-attention-value atom)
            (opencog-attention-value-create 30 10 2)))
    
    ;; Create relationships
    (opencog-atomspace-add
     (opencog-atom-create-link 'InheritanceLink (list opencog cognitive-arch)
                               (opencog-truth-value-simple 0.9 0.8)))
    (opencog-atomspace-add
     (opencog-atom-create-link 'InheritanceLink (list emacogs opencog)
                               (opencog-truth-value-simple 0.95 0.85)))
    (opencog-atomspace-add
     (opencog-atom-create-link 'InheritanceLink (list atomspace opencog)
                               (opencog-truth-value-simple 1.0 0.9))))
  
  (message "   Created %d atoms" (opencog-atomspace-size))
  
  ;; 3. Apply learning
  (message "\n3. Applying learning algorithms...")
  (opencog-learning-spread-attention-network 2)
  (opencog-learning-hebbian-update-all)
  (opencog-learning-mine-patterns)
  
  ;; 4. Visualize
  (message "\n4. Generating visualizations...")
  (opencog-visualization-system-overview)
  (sit-for 1)
  
  ;; 5. Persist
  (message "\n5. Persisting atomspace...")
  (let ((save-file (expand-file-name "workflow-atomspace.el"
                                     opencog-persistence-directory)))
    (opencog-persistence-save-atomspace save-file)
    (message "   Saved to: %s" save-file))
  
  ;; 6. Show statistics
  (message "\n6. System Statistics:")
  (message "   Atomspace: %d atoms" (opencog-atomspace-size))
  (opencog-learning-stats)
  (opencog-persistence-info)
  
  (message "\n=== Workflow Complete ===")
  (message "Check *Emacogs Visualization* buffer for visual output"))

;;; Interactive Menu

(defun emacogs-v11-examples-menu ()
  "Display menu for v1.1.0 examples."
  (interactive)
  (let ((choice (read-char-choice
                 "Emacogs v1.1.0 Examples:
1. Persistence System
2. Learning System
3. Visualization
4. Interactive REPL
5. Complete Workflow
q. Quit

Choose: " '(?1 ?2 ?3 ?4 ?5 ?q))))
    (pcase choice
      (?1 (emacogs-example-persistence))
      (?2 (emacogs-example-learning))
      (?3 (emacogs-example-visualization))
      (?4 (emacogs-example-repl-demo))
      (?5 (emacogs-example-v11-workflow))
      (?q (message "Exiting examples menu")))))

(provide 'emacogs-v1.1-examples)

;;; emacogs-v1.1-examples.el ends here
