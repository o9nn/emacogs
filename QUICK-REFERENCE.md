# Emacogs Quick Reference Guide

## Version 1.1.0

### Quick Start

```elisp
;; Load Emacogs
(add-to-list 'load-path "/path/to/emacogs/lisp")
(require 'emacogs)

;; Initialize and start
(emacogs-initialize)
(emacogs-start)

;; View dashboard
(emacogs-dashboard)

;; Start REPL
(emacogs-repl)
```

## Core Commands

### System Control
- `M-x emacogs-initialize` - Initialize system
- `M-x emacogs-start` - Start cognitive system
- `M-x emacogs-stop` - Stop system
- `M-x emacogs-restart` - Restart system
- `M-x emacogs-dashboard` - View dashboard

### Atomspace Operations
- `M-x opencog-atomspace-display` - View all atoms
- `M-x opencog-atomspace-clear` - Clear atomspace
- Create node: `(opencog-atom-create-node 'ConceptNode "Name")`
- Create link: `(opencog-atom-create-link 'InheritanceLink (list atom1 atom2))`
- Add to atomspace: `(opencog-atomspace-add atom)`

### Persistence (v1.1.0)
- `M-x opencog-persistence-save-atomspace` - Save to file
- `M-x opencog-persistence-load-atomspace` - Load from file
- `M-x opencog-persistence-enable-auto-save` - Enable auto-save
- `M-x opencog-persistence-disable-auto-save` - Disable auto-save
- `M-x opencog-persistence-info` - Show persistence info

### Learning (v1.1.0)
- `M-x opencog-learning-enable` - Enable learning
- `M-x opencog-learning-disable` - Disable learning
- `M-x opencog-learning-spread-attention-network` - Spread attention
- `M-x opencog-learning-hebbian-update-all` - Apply Hebbian learning
- `M-x opencog-learning-mine-patterns` - Mine patterns
- `M-x opencog-learning-stats` - Show statistics

### Visualization (v1.1.0)
- `M-x opencog-visualization-system-overview` - System overview
- `M-x opencog-visualization-graph` - Graph visualization
- `M-x opencog-visualization-attention-map` - Attention heat map
- `M-x opencog-visualization-agent-activity` - Agent activity
- `M-x opencog-visualization-truth-distribution` - Truth values

### REPL (v1.1.0)
- `M-x emacogs-repl` - Start REPL
- In REPL:
  - `(node 'ConceptNode "Name")` - Create node
  - `(link 'InheritanceLink [atom1 atom2])` - Create link
  - `(query 'InheritanceLink)` - Query atoms
  - `(atoms)` - List all atoms
  - `(stats)` - Show statistics
  - `(help)` - Show help
  - `(quit)` - Exit REPL

## Agent System

### Agent Commands
- `M-x agent-zero-status` - View agent status
- `M-x agent-zero-start-orchestration` - Start agents
- `M-x agent-zero-stop-orchestration` - Stop agents

### Creating Agents
```elisp
(agent-zero-create
 "AgentName"
 'agent-type
 '(capability1 capability2)
 (lambda (agent)
   ;; Agent processing function
   ))
```

## Knowledge Constellations

### Commands
- `M-x opencog-constellation-list` - List constellations
- `M-x opencog-constellation-display` - Display constellation

### Creating from Org-mode
```elisp
(opencog-create-constellation "Name" "/path/to/file.org")
```

## Distributed System (Infermacs)

### Commands
- `M-x infermacs-limbo-info` - View system info
- `M-x infermacs-limbo-demo` - Run demo

### Channels
```elisp
;; Create channel
(setq chan (infermacs-make-channel 5))

;; Send/receive
(infermacs-channel-send chan data)
(infermacs-channel-receive chan)
```

## Truth Values

### Creating Truth Values
```elisp
;; Simple truth value (strength, confidence)
(opencog-truth-value-simple 0.9 0.8)
```

### PLN Inference
```elisp
;; Deduction: A->B, B->C => A->C
(opencog-pln-deduction tv1 tv2)

;; Conjunction: A AND B
(opencog-pln-and tv1 tv2)

;; Disjunction: A OR B
(opencog-pln-or tv1 tv2)
```

## Examples

### Run Example Menus
- `M-x emacogs-examples-menu` - v1.0.0 examples
- `M-x emacogs-v11-examples-menu` - v1.1.0 examples

### Demo
- `M-x emacogs-demo` - Run system demo

## Configuration

### Customization Groups
- `M-x customize-group RET emacogs`
- `M-x customize-group RET opencog-persistence`
- `M-x customize-group RET opencog-learning`
- `M-x customize-group RET opencog-visualization`

### Common Settings
```elisp
;; Auto-start on initialization
(setq emacogs-auto-start t)

;; Auto-save interval (seconds)
(setq opencog-persistence-auto-save-interval 300)

;; Learning update interval (seconds)
(setq opencog-learning-update-interval 60)

;; Visualization max nodes
(setq opencog-visualization-max-nodes 50)
```

## Typical Workflow

### 1. Create Knowledge
```elisp
;; Create concepts
(let ((ai (opencog-atom-create-node 'ConceptNode "AI"))
      (ml (opencog-atom-create-node 'ConceptNode "MachineLearning")))
  (opencog-atomspace-add ai)
  (opencog-atomspace-add ml)
  
  ;; Create relationship
  (opencog-atomspace-add
   (opencog-atom-create-link 'InheritanceLink (list ml ai)
                             (opencog-truth-value-simple 0.9 0.8))))
```

### 2. Apply Learning
```elisp
;; Spread attention
(opencog-learning-spread-attention-network 5)

;; Apply Hebbian learning
(opencog-learning-hebbian-update-all)

;; Mine patterns
(opencog-learning-mine-patterns)
```

### 3. Visualize
```elisp
;; View system overview
(opencog-visualization-system-overview)

;; View attention map
(opencog-visualization-attention-map)
```

### 4. Persist
```elisp
;; Save atomspace
(opencog-persistence-save-atomspace "~/my-knowledge.el")

;; Load later
(opencog-persistence-load-atomspace "~/my-knowledge.el")
```

## Troubleshooting

### System Not Starting
1. Check initialization: `M-x emacogs-initialize`
2. Check messages: `*Messages*` buffer
3. Verify load-path includes lisp directory

### Modules Not Loading
1. Check Emacs version: >= 29.1
2. Check org-mode version: >= 9.0
3. Verify file syntax: look for errors in *Messages*

### Auto-save Not Working
1. Enable explicitly: `M-x opencog-persistence-enable-auto-save`
2. Check interval setting: `opencog-persistence-auto-save-interval`
3. Check persistence directory exists

### Learning Not Active
1. Enable explicitly: `M-x opencog-learning-enable`
2. Check atomspace has atoms with attention values
3. Verify update interval: `opencog-learning-update-interval`

## Getting Help

- `M-x emacogs-tutorial` - Interactive tutorial
- `M-x emacogs-info` - System information
- `(help)` in REPL - REPL commands
- Check documentation: EMACOGS.md
- Run examples: `M-x emacogs-examples-menu`

## License

Emacogs is part of GNU Emacs.
Licensed under GNU General Public License v3.0 or later.
