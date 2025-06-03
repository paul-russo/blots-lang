import { transpile_with_inline_eval } from './blots-wasm/pkg/blots_wasm.js';

// Test the inline evaluation transpilation
const code = `
a = 1
output b = 2
c = a + b
`;

try {
    console.log("Testing inline evaluation transpilation:");
    const transpiled = transpile_with_inline_eval(code);
    console.log("Transpiled code:");
    console.log(transpiled);
    
    // Look for $$results.values entries
    if (transpiled.includes("$$results.values")) {
        console.log("\n✅ Found $$results.values entries - output assignments should now be captured");
    } else {
        console.log("\n❌ No $$results.values entries found");
    }
} catch (error) {
    console.error("Error:", error);
}