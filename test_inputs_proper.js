// Set up inputs like the web app would
globalThis.inputs = { base_salary: 50000, base_salary_modifier: 1.2 };

// Include the transpiled Blots code
const fs = require('fs');
const { execSync } = require('child_process');

// Generate a simple test
const blotCode = `
print("inputs: {}", inputs)
print("inputs.base_salary: {}", inputs.base_salary)
print("inputs.base_salary_modifier: {}", inputs.base_salary_modifier)
salary = inputs.base_salary * inputs.base_salary_modifier
print("salary: {}", salary)
`;

// Write test file
fs.writeFileSync('/tmp/test_inputs.blot', blotCode);

// Transpile and execute
try {
    const transpiledCode = execSync('cargo run -p blots -- /tmp/test_inputs.blot --transpile', { 
        cwd: '/Users/paul/src/blots-lang',
        encoding: 'utf8'
    });
    
    // Execute the transpiled code
    eval(transpiledCode);
} catch (error) {
    console.error('Error:', error.message);
}