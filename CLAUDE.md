# CLAUDE.md - Emacogs Development Guide

## Project Overview

**Emacogs** is a cognitive architecture system for GNU Emacs implementing OpenCog as an Emacs-based neuro-symbolic atomspace fabric. It provides multi-agent autonomous orchestration (Agent-Zero), tensor logic for probabilistic reasoning, Inferno-inspired distributed cognitive kernels (Infermacs e-limbo), and org-mode knowledge constellations.

**Current Version**: 1.2.0-dev
**License**: GNU GPL v3.0
**Emacs Requirement**: 29.1+

## Architecture Components

| Module | File | Purpose |
|--------|------|---------|
| Atomspace | `lisp/opencog-atomspace.el` | Hypergraph knowledge representation with truth/attention values |
| Tensor Logic | `lisp/opencog-tensor-logic.el` | PLN inference, fuzzy logic, forward chaining |
| Agent-Zero | `lisp/agent-zero.el` | Multi-agent orchestration, task scheduling |
| Infermacs | `lisp/infermacs-limbo.el` | CSP channels, distributed processing |
| Constellations | `lisp/opencog-org-constellations.el` | Org-mode to atomspace conversion |
| Persistence | `lisp/opencog-persistence.el` | Save/load atomspace, auto-save |
| Learning | `lisp/opencog-learning.el` | Attention spreading, Hebbian learning |
| Visualization | `lisp/opencog-visualization.el` | ASCII graphs, heat maps, dashboards |
| REPL | `lisp/emacogs-repl.el` | Interactive command interface |
| Network | `lisp/opencog-network.el` | Distributed atomspace sync (v1.2.0) |
| Main | `lisp/emacogs.el` | System integration, initialization |

## Directory Structure

```
emacogs/
├── lisp/                    # Emacs Lisp source files
│   ├── emacogs.el           # Main integration module
│   ├── emacogs-repl.el      # Interactive REPL
│   ├── opencog-*.el         # OpenCog components (atomspace, tensor-logic, etc.)
│   ├── opencog-network.el   # Network protocol (v1.2.0)
│   ├── agent-zero.el        # Agent orchestration
│   └── infermacs-limbo.el   # Distributed computing
├── examples/                # Example code and knowledge bases
│   ├── emacogs-examples.el  # v1.0.0 examples (7 demos)
│   ├── emacogs-v1.1-examples.el  # v1.1.0 examples
│   ├── knowledge/           # Org-mode knowledge bases
│   └── test-modules.sh      # Module verification script
├── test/                    # ERT test suite (v1.2.0)
│   └── lisp/
│       └── emacogs-test.el  # Comprehensive tests
├── doc/                     # Documentation
├── EMACOGS.md              # Comprehensive documentation
├── README.emacogs.md       # Quick start guide
├── QUICK-REFERENCE.md      # Command reference
├── IMPLEMENTATION-SUMMARY.md # Implementation details
├── CHANGELOG.md            # Version history
├── CLAUDE.md               # AI development guide (v1.2.0)
└── .github/agents/         # GitHub custom agents
```

## Build & Test Commands

```bash
# Verify all modules load correctly
./examples/test-modules.sh

# Load and test in Emacs
emacs -Q -L lisp -l emacogs --eval "(emacogs-initialize)" --eval "(emacogs-demo)"

# Byte-compile modules
emacs -Q -L lisp --batch -f batch-byte-compile lisp/emacogs*.el lisp/opencog*.el lisp/agent-zero.el lisp/infermacs-limbo.el

# Run ERT test suite (v1.2.0)
emacs -Q -L lisp -l ert -l test/lisp/emacogs-test.el -f ert-run-tests-batch-and-exit

# Run tests interactively
M-x ert RET "emacogs-" RET
```

## Quick Start (Elisp)

```elisp
;; Load and initialize
(add-to-list 'load-path "/path/to/emacogs/lisp")
(require 'emacogs)
(emacogs-initialize)
(emacogs-start)

;; View dashboard
M-x emacogs-dashboard

;; Run demo
M-x emacogs-demo

;; Start interactive REPL
M-x emacogs-repl
```

## Coding Conventions

### File Headers
```elisp
;;; module-name.el --- Short description  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Emacogs Team
;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: AI, cognitive, knowledge
```

### Naming Conventions
- **Module prefix**: All functions use module prefix (`opencog-`, `agent-zero-`, `infermacs-`, `emacogs-`)
- **Private functions**: Double-dash for internal use (`opencog-tensor--helper`)
- **Predicates**: End with `-p` (`opencog-atom-node-p`)
- **Constructors**: Use `-create` suffix (`agent-zero-agent-create`)

### Data Structures
- Use `cl-defstruct` with `:constructor` and `(:copier nil)`
- Include `:type` declarations and docstrings
- Store metadata as plists in `:metadata` slots

### Error Handling
- Use `error` with descriptive messages
- Validate inputs at function entry
- Return nil for graceful degradation where appropriate

### Thread Safety
Hash tables are NOT thread-safe. Use:
- Channels (`infermacs-channel`) for inter-process communication
- Timer-based orchestration for coordinated execution

## Key Interactive Commands

### Core System
- `emacogs-initialize` - Initialize system
- `emacogs-start` / `emacogs-stop` - Start/stop system
- `emacogs-dashboard` - View status dashboard
- `emacogs-demo` - Run demonstration

### Atomspace
- `opencog-atomspace-display` - View all atoms
- `opencog-atomspace-clear` - Clear atomspace

### Persistence (v1.1.0)
- `opencog-persistence-save-atomspace` - Save to file
- `opencog-persistence-load-atomspace` - Load from file
- `opencog-persistence-enable-auto-save` - Enable auto-save

### Learning (v1.1.0)
- `opencog-learning-enable` - Enable learning
- `opencog-learning-spread-attention-network` - Spread attention
- `opencog-learning-hebbian-update-all` - Apply Hebbian learning

### Visualization (v1.1.0)
- `opencog-visualization-system-overview` - System dashboard
- `opencog-visualization-graph` - Graph visualization
- `opencog-visualization-attention-map` - Attention heat map

## Development Workflow

1. **Read existing code** before making changes
2. **Maintain modularity** - each component should work independently
3. **Add interactive commands** for user-facing features
4. **Write docstrings** for all functions
5. **Update documentation** in relevant .md files
6. **Test with examples** via `emacogs-examples-menu`
7. **Verify module loading** with `test-modules.sh`

## v1.2.0 Development Progress

### Completed
- [x] Create ERT test suite (`test/lisp/emacogs-test.el`)
- [x] Atomspace synchronization protocol (`opencog-network.el`)
- [x] Node discovery and connection management
- [x] Distributed query execution
- [x] CLAUDE.md development guide

### In Progress
- [ ] Test coverage for all core functions
- [ ] CI/CD integration with GitHub Actions
- [ ] Performance benchmarking framework

### Planned (v1.2.0)
- [ ] Optimize hash table operations
- [ ] Lazy evaluation for large atomspaces
- [ ] MOSES-style evolutionary learning
- [ ] Import from external knowledge bases (JSON-LD, RDF)

### Future (v1.3.0+)
- [ ] Reinforcement learning integration
- [ ] REST API foundation
- [ ] NLP integration
- [ ] Web-based visualization

## Code Patterns

### Creating Atoms
```elisp
(let ((node (opencog-atom-create-node 'ConceptNode "Name")))
  (opencog-atomspace-add node)
  (let ((link (opencog-atom-create-link
               'InheritanceLink
               (list node other-node)
               (opencog-truth-value-simple 0.9 0.8))))
    (opencog-atomspace-add link)))
```

### Creating Agents
```elisp
(agent-zero-create
 "MyAgent"
 'worker
 '(reasoning learning)
 (lambda (agent)
   ;; Agent processing logic
   (message "Processing %s" (agent-zero-agent-name agent))))
```

### Channel Communication
```elisp
(let ((chan (infermacs-make-channel 5)))
  (infermacs-channel-send chan data)
  (infermacs-channel-receive chan))
```

## Related Files

- **GitHub Agent**: `.github/agents/emacogs.agent.md` - Expert agent configuration
- **Examples**: `examples/emacogs-examples.el` - Interactive demonstrations
- **Knowledge Bases**: `examples/knowledge/*.org` - Sample org-mode knowledge

## Statistics

- **Total modules**: 11 (10 core + 1 network)
- **Lines of code**: ~4,500+
- **Interactive commands**: 40+
- **Example functions**: 12
- **Test cases**: 25+

## Contact & Resources

- Documentation: See EMACOGS.md for comprehensive details
- Issues: Report bugs via standard GNU Emacs channels
- Tutorial: `M-x emacogs-tutorial`
