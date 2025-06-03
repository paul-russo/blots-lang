// Test the full workflow like the web app would do
const { execSync } = require('child_process');

// Set up inputs like the web app
global.inputs = { base_salary: 50000, base_salary_modifier: 1.2 };

// The problematic Blots code
const blotCode = 'salary = inputs.base_salary * inputs.base_salary_modifier\nprint("salary: {}", salary)';

// Transpile it
const transpiledCode = execSync('cargo run -p blots -- /dev/stdin --transpile', { 
    cwd: '/Users/paul/src/blots-lang',
    input: blotCode,
    encoding: 'utf8'
});

console.log('=== Transpiled Code ===');
console.log(transpiledCode.split('\n').slice(-3).join('\n')); // Show just the relevant part

console.log('\n=== Execution Result ===');
// Execute the transpiled code
eval(transpiledCode);