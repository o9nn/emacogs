#!/bin/bash
# Simple syntax check for Emacogs modules

echo "Checking Emacogs module syntax..."

cd "$(dirname "$0")/.."

# Check if files exist
modules=(
    "lisp/opencog-atomspace.el"
    "lisp/opencog-tensor-logic.el"
    "lisp/agent-zero.el"
    "lisp/infermacs-limbo.el"
    "lisp/opencog-org-constellations.el"
    "lisp/emacogs.el"
)

echo "Verifying module files exist..."
all_exist=true
for module in "${modules[@]}"; do
    if [ -f "$module" ]; then
        echo "✓ $module exists"
    else
        echo "✗ $module NOT FOUND"
        all_exist=false
    fi
done

if [ "$all_exist" = true ]; then
    echo ""
    echo "All module files are present!"
    
    # Check basic syntax (look for provide statement)
    echo ""
    echo "Checking for proper provide statements..."
    for module in "${modules[@]}"; do
        module_name=$(basename "$module" .el)
        if grep -q "(provide '$module_name)" "$module"; then
            echo "✓ $module has proper provide statement"
        else
            echo "⚠ $module might be missing provide statement"
        fi
    done
    
    # Count lines of code
    echo ""
    echo "Lines of code per module:"
    for module in "${modules[@]}"; do
        lines=$(wc -l < "$module")
        echo "  $module: $lines lines"
    done
    
    total_lines=$(cat "${modules[@]}" | wc -l)
    echo ""
    echo "Total lines of code: $total_lines"
    
    exit 0
else
    echo ""
    echo "Some module files are missing!"
    exit 1
fi
