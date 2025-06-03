import { evaluate } from './pkg/blots_wasm.js';

console.log('Testing array + number addition...');

try {
  const result = evaluate('[1,2] + 2', '{}');
  console.log('Result:', result);
  console.log('ERROR: This should have thrown an error!');
} catch (e) {
  console.log('Success: Got expected error:', e.message);
}

console.log('\nTesting valid number + number addition...');
try {
  const result = evaluate('5 + 2', '{}');
  console.log('Result:', result);
} catch (e) {
  console.log('ERROR: Valid addition failed:', e.message);
}

console.log('\nTesting valid string + string concatenation...');
try {
  const result = evaluate('"hello" + "world"', '{}');
  console.log('Result:', result);
} catch (e) {
  console.log('ERROR: Valid concatenation failed:', e.message);
}