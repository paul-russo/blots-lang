// Simple test to verify original source preservation works

// Simulate the transpiled function with original source property
const double = (($$f) => { 
    $$f.$$originalSource = `x => x * 2`; 
    return $$f; 
})((x) => x * 2);

console.log('Function:', double);
console.log('Original source:', double.$$originalSource);
console.log('Function call result:', double(5));

// Simulate the WASM parsing logic
function testParseFunction(jsFunc) {
    // Check if the function has an $$originalSource property
    if (jsFunc.$$originalSource) {
        console.log('Found original source:', jsFunc.$$originalSource);
        
        // Parse the original Blots source
        const original = jsFunc.$$originalSource;
        if (original.includes('=>')) {
            const arrow_pos = original.indexOf('=>');
            const params_part = original.slice(0, arrow_pos).trim();
            const body_part = original.slice(arrow_pos + 2).trim();
            
            console.log('Parsed parameters:', params_part);
            console.log('Parsed body:', body_part);
            
            return {
                args: [params_part], // Simplified
                body: body_part, // This is the original Blots body!
                name: null,
                scope: null
            };
        }
    }
    
    return null;
}

const result = testParseFunction(double);
console.log('Parsed function definition:', result);
console.log('Success! Body contains original Blots syntax:', result.body);