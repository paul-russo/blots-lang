// Error mapping utilities for Blots runtime

const sourceMapCache = new Map();

function parseStackTrace(stack) {
    const lines = stack.split('\n');
    const frames = [];
    
    for (const line of lines) {
        // Parse stack frame - handles various formats
        const match = line.match(/at\s+(?:(.+?)\s+\()?(.+?):(\d+):(\d+)\)?/);
        if (match) {
            frames.push({
                functionName: match[1] || '<anonymous>',
                fileName: match[2],
                line: parseInt(match[3], 10),
                column: parseInt(match[4], 10)
            });
        }
    }
    
    return frames;
}

function findSourceLocation(jsLine, jsColumn, sourceMap) {
    // Find the closest mapping that matches our JS position
    let bestMatch = null;
    let minDistance = Infinity;
    
    for (const mapping of sourceMap.mappings) {
        if (mapping.generated_line === jsLine) {
            const distance = Math.abs(mapping.generated_column - jsColumn);
            if (distance < minDistance) {
                minDistance = distance;
                bestMatch = mapping;
            }
        }
    }
    
    if (!bestMatch) {
        // Try to find a mapping on a nearby line
        for (const mapping of sourceMap.mappings) {
            const lineDistance = Math.abs(mapping.generated_line - jsLine);
            if (lineDistance <= 2) {
                const distance = lineDistance * 1000 + Math.abs(mapping.generated_column - jsColumn);
                if (distance < minDistance) {
                    minDistance = distance;
                    bestMatch = mapping;
                }
            }
        }
    }
    
    return bestMatch;
}

function formatBlotsError(error, sourceMap) {
    const stack = error.stack || error.toString();
    const frames = parseStackTrace(stack);
    
    if (frames.length === 0) {
        return error.toString();
    }
    
    // Find the first frame that we can map
    let mappedFrame = null;
    let sourceLocation = null;
    
    for (const frame of frames) {
        // Skip frames from eval or internal code
        if (frame.fileName.includes('eval') || frame.fileName.includes('__')) {
            const location = findSourceLocation(frame.line, frame.column, sourceMap);
            if (location) {
                mappedFrame = frame;
                sourceLocation = location;
                break;
            }
        }
    }
    
    if (!sourceLocation) {
        return error.toString();
    }
    
    // Format the error with Blots source location
    const sourceContent = sourceMap.sources_content[0];
    const lines = sourceContent.split('\n');
    const errorLine = lines[sourceLocation.source_range.start.line - 1] || '';
    
    let result = `${error.name || 'Error'}: ${error.message || ''}`;
    result += `\n  at ${sourceMap.sources[0]}:${sourceLocation.source_range.start.line}:${sourceLocation.source_range.start.column}`;
    
    // Show the source line with a pointer
    if (errorLine) {
        result += '\n\n' + errorLine;
        result += '\n' + ' '.repeat(sourceLocation.source_range.start.column - 1) + '^';
    }
    
    return result;
}

// Wrapper function to execute transpiled code with error mapping
function executeWithSourceMap(code, sourceMap) {
    sourceMapCache.set('current', sourceMap);
    
    try {
        // Create a new function from the code and execute it
        const fn = new Function(code);
        return fn();
    } catch (error) {
        // Map the error back to Blots source
        const mappedError = formatBlotsError(error, sourceMap);
        const newError = new Error(mappedError);
        newError.originalError = error;
        newError.sourceMap = sourceMap;
        throw newError;
    } finally {
        sourceMapCache.delete('current');
    }
}

// Enhanced error handler for runtime errors
function wrapRuntimeFunction(fn, fnName) {
    return function(...args) {
        try {
            return fn.apply(this, args);
        } catch (error) {
            const sourceMap = sourceMapCache.get('current');
            if (sourceMap) {
                const mappedError = formatBlotsError(error, sourceMap);
                const newError = new Error(mappedError);
                newError.originalError = error;
                newError.sourceMap = sourceMap;
                throw newError;
            }
            throw error;
        }
    };
}

// Export functions for use in the runtime
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        executeWithSourceMap,
        wrapRuntimeFunction,
        formatBlotsError
    };
}