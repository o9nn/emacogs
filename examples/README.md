# Emacogs Examples

This directory contains examples and sample knowledge bases for the Emacogs cognitive architecture system.

## Directory Structure

```
examples/
├── emacogs-examples.el          # Interactive examples demonstrating Emacogs features
├── test-modules.sh              # Script to verify module installation
└── knowledge/                   # Sample knowledge bases in org-mode
    ├── cognitive-architecture.org
    ├── agent-systems.org
    └── distributed-systems.org
```

## Running Examples

### From Emacs

1. Load Emacogs:
```elisp
(add-to-list 'load-path "/path/to/emacogs/lisp")
(require 'emacogs)
```

2. Load examples:
```elisp
(add-to-list 'load-path "/path/to/emacogs/examples")
(require 'emacogs-examples)
```

3. Run example menu:
```
M-x emacogs-examples-menu
```

### Individual Examples

#### Example 1: Basic Atomspace
```
M-x emacogs-example-basic-atomspace
```
Demonstrates creating nodes, links, and basic atomspace operations.

#### Example 2: Probabilistic Inference
```
M-x emacogs-example-inference
```
Demonstrates PLN inference rules with truth values.

#### Example 3: Multi-Agent System
```
M-x emacogs-example-agents
```
Demonstrates creating agents, inter-agent communication, and task assignment.

#### Example 4: Distributed Processing
```
M-x emacogs-example-distributed
```
Demonstrates Infermacs channels, processes, and distributed nodes.

#### Example 5: Knowledge Constellations
```
M-x emacogs-example-constellations
```
Demonstrates loading org-mode files as knowledge constellations.

#### Example 6: Tensor Operations
```
M-x emacogs-example-tensors
```
Demonstrates tensor creation and operations.

#### Example 7: Complete Workflow
```
M-x emacogs-example-complete-workflow
```
Runs through a complete workflow integrating all components.

## Sample Knowledge Bases

### cognitive-architecture.org

Contains hierarchical knowledge about cognitive architectures including:
- OpenCog and AtomSpace
- SOAR
- ACT-R
- Machine Learning
- Neuro-Symbolic AI

### agent-systems.org

Contains knowledge about agent systems:
- Agent types (reactive, deliberative, hybrid)
- Multi-agent systems
- Agent architectures (BDI, subsumption)
- Agent learning

### distributed-systems.org

Contains knowledge about distributed systems:
- Concurrency and parallelism
- Communication patterns
- Consistency models
- Fault tolerance
- Inferno OS concepts

## Using Knowledge Bases

Load a knowledge base into a constellation:

```elisp
(opencog-create-constellation 
  "MyKnowledge" 
  (expand-file-name "knowledge/cognitive-architecture.org" 
                    (file-name-directory (locate-library "emacogs-examples"))))
```

Query the constellation:

```elisp
(opencog-constellation-list)
(opencog-constellation-display 'const-1)
```

## Creating Your Own Knowledge Bases

1. Create an org-mode file with hierarchical structure:

```org
#+TITLE: My Knowledge Base

* Top Level Concept
** Sub Concept 1
*** Details
** Sub Concept 2
```

2. Load it as a constellation:

```elisp
(opencog-create-constellation "MyKB" "/path/to/myfile.org")
```

3. The system will:
   - Convert headlines to ConceptNodes
   - Create InheritanceLinks for hierarchy
   - Add all to the atomspace

## Testing

Verify module installation:

```bash
./test-modules.sh
```

This checks that all modules exist and have proper provide statements.

## Tips

- Use `M-x emacogs-dashboard` to see system status
- Use `M-x opencog-atomspace-display` to view the atomspace
- Use `M-x agent-zero-status` to view agent status
- Use `M-x infermacs-limbo-info` to view distributed system info

## Troubleshooting

If modules don't load:
1. Check that load-path includes the lisp directory
2. Verify dependencies are installed (Emacs 29.1+, org-mode 9.0+)
3. Check for any syntax errors in the modules

If examples don't work:
1. Initialize the system: `M-x emacogs-initialize`
2. Start the system: `M-x emacogs-start`
3. Check the *Messages* buffer for errors

## Further Reading

See the main [EMACOGS.md](../EMACOGS.md) documentation for comprehensive information about the architecture and all features.
