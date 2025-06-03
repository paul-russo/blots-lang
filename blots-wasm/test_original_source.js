// Test script to verify original source preservation in function values

import { evaluate } from './pkg/blots_wasm.js';

async function test() {
    try {
        console.log('Testing original source preservation...');
        
        // Test simple function definition
        const result = evaluate('complex_add = (a, b) => [a[0] + b[0], a[1] + b[1]]', {});
        
        console.log('Result:', result);
        
        console.log('Result bindings keys:', result.bindings ? Object.keys(result.bindings) : 'none');
        
        if (result.bindings && result.bindings.complex_add) {
            const funcValue = result.bindings.complex_add;
            console.log('Function value:', JSON.stringify(funcValue, null, 2));
            
            if (funcValue.Lambda) {
                console.log('Function body:', funcValue.Lambda.body);
                console.log('Expected: [a[0] + b[0], a[1] + b[1]]');
                console.log('Success:', funcValue.Lambda.body === '[a[0] + b[0], a[1] + b[1]]');
            }
        }
        
        console.log('Test completed successfully!');
    } catch (error) {
        console.error('Test failed:', error);
    }
}

test();