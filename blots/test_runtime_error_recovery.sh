#!/bin/bash

echo "Testing runtime error recovery in REPL..."
echo ""
echo "This test should show:"
echo "1. Variables work before error"
echo "2. Runtime error is reported but doesn't poison the session" 
echo "3. Variables continue to work after error"
echo ""

# Test runtime error recovery
echo -e "x = 42\nprint(\"x is: {}\", x)\nundefined_function()\ny = 100\nprint(\"y is: {}\", y)\nquit" | cargo run -p blots