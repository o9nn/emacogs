;;; emacogs-examples.el --- Examples for using Emacogs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;;; Commentary:

;; This file contains examples demonstrating the use of Emacogs
;; cognitive architecture system.

;;; Code:

(require 'emacogs)

;;; Example 1: Basic Atomspace Usage

(defun emacogs-example-basic-atomspace ()
  "Demonstrate basic atomspace operations."
  (interactive)
  (message "=== Basic Atomspace Example ===")
  
  ;; Clear atomspace
  (opencog-atomspace-clear)
  
  ;; Create concept nodes
  (let ((cat (opencog-atom-create-node 'ConceptNode "Cat"))
        (mammal (opencog-atom-create-node 'ConceptNode "Mammal"))
        (animal (opencog-atom-create-node 'ConceptNode "Animal")))
    
    ;; Add to atomspace
    (opencog-atomspace-add cat)
    (opencog-atomspace-add mammal)
    (opencog-atomspace-add animal)
    
    ;; Create inheritance links with truth values
    (let ((link1 (opencog-atom-create-link
                  'InheritanceLink
                  (list cat mammal)
                  (opencog-truth-value-simple 0.95 0.9)))
          (link2 (opencog-atom-create-link
                  'InheritanceLink
                  (list mammal animal)
                  (opencog-truth-value-simple 1.0 0.95))))
      (opencog-atomspace-add link1)
      (opencog-atomspace-add link2))
    
    (message "Created %d atoms" (opencog-atomspace-size)))
  
  ;; Display atomspace
  (opencog-atomspace-display))

;;; Example 2: Probabilistic Inference

(defun emacogs-example-inference ()
  "Demonstrate probabilistic inference."
  (interactive)
  (message "=== Inference Example ===")
  
  ;; Create truth values
  (let ((tv1 (opencog-truth-value-simple 0.9 0.8))
        (tv2 (opencog-truth-value-simple 0.85 0.75)))
    
    ;; Perform deduction
    (let ((result (opencog-pln-deduction tv1 tv2)))
      (message "Deduction: A->B (%.2f, %.2f) & B->C (%.2f, %.2f) => A->C (%.2f, %.2f)"
               (opencog-truth-value-strength tv1)
               (opencog-truth-value-confidence tv1)
               (opencog-truth-value-strength tv2)
               (opencog-truth-value-confidence tv2)
               (opencog-truth-value-strength result)
               (opencog-truth-value-confidence result)))
    
    ;; Perform conjunction
    (let ((result (opencog-pln-and tv1 tv2)))
      (message "AND: (%.2f, %.2f) AND (%.2f, %.2f) => (%.2f, %.2f)"
               (opencog-truth-value-strength tv1)
               (opencog-truth-value-confidence tv1)
               (opencog-truth-value-strength tv2)
               (opencog-truth-value-confidence tv2)
               (opencog-truth-value-strength result)
               (opencog-truth-value-confidence result)))))

;;; Example 3: Multi-Agent System

(defun emacogs-example-agents ()
  "Demonstrate multi-agent system."
  (interactive)
  (message "=== Multi-Agent Example ===")
  
  ;; Create reasoning agent
  (let ((reasoner (agent-zero-create
                   "Reasoner"
                   'reasoning
                   '(logic inference deduction)
                   (lambda (agent)
                     (message "[%s] Processing reasoning tasks..."
                              (agent-zero-agent-name agent))))))
    
    ;; Create learning agent
    (let ((learner (agent-zero-create
                    "Learner"
                    'learning
                    '(pattern-recognition adaptation)
                    (lambda (agent)
                      (message "[%s] Learning patterns..."
                               (agent-zero-agent-name agent))))))
      
      ;; Send message from reasoner to learner
      (agent-zero-send-message
       (agent-zero-agent-id reasoner)
       (agent-zero-agent-id learner)
       'request
       "Please analyze this pattern")
      
      ;; Create and assign task
      (let ((task (agent-zero-create-task
                   "Analyze data pattern"
                   'analysis
                   '(pattern-recognition)
                   (lambda (agent task)
                     (message "Task executed by %s"
                              (agent-zero-agent-name agent))
                     "Analysis complete"))))
        (agent-zero-assign-task task learner))))
  
  ;; Show status
  (agent-zero-status))

;;; Example 4: Distributed Processing

(defun emacogs-example-distributed ()
  "Demonstrate distributed processing with Infermacs."
  (interactive)
  (message "=== Distributed Processing Example ===")
  
  ;; Create channel
  (let ((chan (infermacs-make-channel 10)))
    (message "Created channel: %s" (infermacs-channel-id chan))
    
    ;; Send messages
    (infermacs-channel-send chan "Message 1")
    (infermacs-channel-send chan "Message 2")
    (infermacs-channel-send chan "Message 3")
    
    ;; Receive messages
    (message "Received: %s" (infermacs-channel-receive chan))
    (message "Received: %s" (infermacs-channel-receive chan))
    (message "Received: %s" (infermacs-channel-receive chan)))
  
  ;; Spawn process
  (let ((pid (infermacs-spawn (lambda ()
                                (message "Process executing...")
                                (+ 10 20 30 40 50)))))
    (message "Process %s result: %s" pid (infermacs-process-wait pid)))
  
  ;; Create cognitive node
  (let ((node (infermacs-create-node "localhost:9999" '(reasoning inference))))
    (message "Created node: %s at %s"
             (infermacs-node-id node)
             (infermacs-node-address node))
    (infermacs-node-connect (infermacs-node-id node)))
  
  (infermacs-limbo-info))

;;; Example 5: Knowledge Constellations

(defun emacogs-example-constellations ()
  "Demonstrate knowledge constellations from org files."
  (interactive)
  (message "=== Knowledge Constellations Example ===")
  
  ;; Check if example files exist
  (let ((kb-file (expand-file-name
                  "examples/knowledge/cognitive-architecture.org"
                  (file-name-directory (locate-library "emacogs")))))
    (if (file-exists-p kb-file)
        (progn
          ;; Create constellation from org file
          (let ((constellation (opencog-create-constellation
                                "CognitiveArchitecture"
                                kb-file)))
            (message "Created constellation: %s"
                     (opencog-constellation-name constellation))
            (message "Nodes: %d"
                     (length (opencog-constellation-nodes constellation)))
            (message "Links: %d"
                     (length (opencog-constellation-links constellation))))
          
          ;; List all constellations
          (opencog-constellation-list))
      (message "Example org file not found: %s" kb-file))))

;;; Example 6: Tensor Operations

(defun emacogs-example-tensors ()
  "Demonstrate tensor operations."
  (interactive)
  (message "=== Tensor Operations Example ===")
  
  ;; Create tensors
  (let ((tensor1 (opencog-tensor-make '(3 3)))
        (tensor2 (opencog-tensor-make '(3 3))))
    
    ;; Set values
    (opencog-tensor-set tensor1 1.0 0 0)
    (opencog-tensor-set tensor1 2.0 0 1)
    (opencog-tensor-set tensor1 3.0 0 2)
    
    (opencog-tensor-set tensor2 0.5 0 0)
    (opencog-tensor-set tensor2 0.5 0 1)
    (opencog-tensor-set tensor2 0.5 0 2)
    
    ;; Element-wise multiplication
    (let ((result (opencog-tensor-multiply tensor1 tensor2)))
      (message "Tensor multiplication result at (0,0): %.2f"
               (opencog-tensor-get result 0 0))
      (message "Tensor multiplication result at (0,1): %.2f"
               (opencog-tensor-get result 0 1)))))

;;; Example 7: Complete Workflow

(defun emacogs-example-complete-workflow ()
  "Demonstrate a complete Emacogs workflow."
  (interactive)
  (message "=== Complete Workflow Example ===")
  
  ;; Initialize system
  (unless emacogs-initialized
    (emacogs-initialize))
  
  ;; Start system
  (unless emacogs-running
    (emacogs-start))
  
  ;; Create knowledge
  (emacogs-example-basic-atomspace)
  (sit-for 1)
  
  ;; Perform inference
  (emacogs-example-inference)
  (sit-for 1)
  
  ;; Run agents
  (emacogs-example-agents)
  (sit-for 1)
  
  ;; Show dashboard
  (emacogs-dashboard))

;;; Interactive Menu

(defun emacogs-examples-menu ()
  "Display menu of Emacogs examples."
  (interactive)
  (let ((choice (read-char-choice
                 "Emacogs Examples:
1. Basic Atomspace
2. Probabilistic Inference
3. Multi-Agent System
4. Distributed Processing
5. Knowledge Constellations
6. Tensor Operations
7. Complete Workflow
q. Quit

Choose example (1-7, q): "
                 '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?q))))
    (pcase choice
      (?1 (emacogs-example-basic-atomspace))
      (?2 (emacogs-example-inference))
      (?3 (emacogs-example-agents))
      (?4 (emacogs-example-distributed))
      (?5 (emacogs-example-constellations))
      (?6 (emacogs-example-tensors))
      (?7 (emacogs-example-complete-workflow))
      (?q (message "Exiting examples menu")))))

(provide 'emacogs-examples)
;;; emacogs-examples.el ends here
