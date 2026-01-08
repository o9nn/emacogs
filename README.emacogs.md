# Emacogs Extension for GNU Emacs

This repository includes **Emacogs**, a cognitive architecture system built on GNU Emacs.

## About Emacogs

Emacogs is an implementation of OpenCog as an Emacs-based neuro-symbolic atomspace fabric for cognitive architecture with agent-zero as multi-agent autonomous orchestration workbench. It features:

- **OpenCog Atomspace**: Hypergraph-based knowledge representation
- **Tensor Logic**: Probabilistic reasoning and inference
- **Agent-Zero**: Multi-agent orchestration system
- **Infermacs e-limbo**: Distributed cognitive kernels inspired by Inferno OS
- **Org-mode Constellations**: Knowledge representation using org-mode

## Quick Start

### Loading Emacogs

```elisp
(add-to-list 'load-path "/path/to/emacogs/lisp")
(require 'emacogs)

;; Initialize and start the system
(emacogs-initialize)
(emacogs-start)

;; View the dashboard
(emacogs-dashboard)
```

### Running Examples

```elisp
;; Load examples
(add-to-list 'load-path "/path/to/emacogs/examples")
(require 'emacogs-examples)

;; Run examples menu
M-x emacogs-examples-menu

;; Or run the demo
M-x emacogs-demo
```

## Documentation

- **[EMACOGS.md](EMACOGS.md)** - Comprehensive Emacogs documentation
- **[examples/README.md](examples/README.md)** - Examples and tutorials
- **[README](README)** - Standard GNU Emacs README

## Module Files

Located in `lisp/`:

- `opencog-atomspace.el` - Core atomspace implementation
- `opencog-tensor-logic.el` - Probabilistic logic and inference
- `agent-zero.el` - Multi-agent orchestration
- `infermacs-limbo.el` - Distributed cognitive kernels
- `opencog-org-constellations.el` - Org-mode knowledge integration
- `emacogs.el` - Main integration module

## Example Knowledge Bases

Located in `examples/knowledge/`:

- `cognitive-architecture.org` - Knowledge about cognitive architectures
- `agent-systems.org` - Knowledge about agent systems
- `distributed-systems.org` - Knowledge about distributed systems

## Interactive Commands

- `M-x emacogs-initialize` - Initialize the system
- `M-x emacogs-start` - Start the cognitive architecture
- `M-x emacogs-stop` - Stop the system
- `M-x emacogs-dashboard` - View system dashboard
- `M-x emacogs-demo` - Run demonstration
- `M-x emacogs-tutorial` - View tutorial

## Requirements

- GNU Emacs 29.1 or later
- Org-mode 9.0 or later

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    EMACOGS                          │
│         Cognitive Architecture System               │
├─────────────────────────────────────────────────────┤
│  ┌────────────┐  ┌────────────┐  ┌────────────┐   │
│  │  OpenCog   │  │   Tensor   │  │ Agent-Zero │   │
│  │ Atomspace  │◄─┤   Logic    │◄─┤Orchestration│  │
│  └────────────┘  └────────────┘  └────────────┘   │
│         │                │                │         │
│  ┌──────▼────────────────▼────────────────▼──────┐ │
│  │        Infermacs e-limbo                      │ │
│  │    Distributed Cognitive Kernels              │ │
│  └───────────────────────────────────────────────┘ │
│                      │                              │
│  ┌───────────────────▼──────────────────────────┐ │
│  │   Org-mode Knowledge Constellations          │ │
│  └──────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

## License

Emacogs is part of GNU Emacs and is licensed under the GNU General Public License v3.0 or later.

## Contributing

Contributions to Emacogs are welcome! Please follow the standard GNU Emacs contribution guidelines found in the CONTRIBUTE file.

## Support

For questions and issues:
- Check the [documentation](EMACOGS.md)
- Run the tutorial: `M-x emacogs-tutorial`
- View examples: `M-x emacogs-examples-menu`

## Standard Emacs Information

For information about building, installing, and using GNU Emacs itself, see the [main README](README) file.
