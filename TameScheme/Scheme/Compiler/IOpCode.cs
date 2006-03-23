// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interface for classes that can compile a particular BOp         IOpCode.cs |
// +----------------------------------------------------------------------------+
// | Copyright (c) 2005 Andrew Hunter                                           |
// |                                                                            |
// | Permission is hereby granted, free of charge, to any person obtaining a    |
// | copy of this software and associated documentation files (the "Software"), |
// | to deal in the Software without restriction, including without limitation  |
// | the rights to use, copy, modify, merge, publish, distribute, sublicense,   |
// | and/or sell copies of the Software, and to permit persons to whom the      |
// | Software is furnished to do so, subject to the following conditions:       |
// |                                                                            |
// | The above copyright notice and this permission notice shall be included in |
// | all copies or substantial portions of the Software.                        |
// |                                                                            |
// | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR |
// | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   |
// | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    |
// | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER |
// | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    |
// | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        |
// | DEALINGS IN THE SOFTWARE.                                                  |
// +----------------------------------------------------------------------------+

using System;
using System.Reflection.Emit;
using Tame.Scheme.Compiler.Analysis;
using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler
{
    /// <summary>
    /// This interface should be implemented by classes that convert scheme BExpression operations into IL operations.
    /// </summary>
    /// <remarks>
    /// These classes are usually automatically loaded from the Scheme assembly when the Compiler class is initialised,
    /// and are typically singleton objects. This may not be true for a highly customised scheme implementation, which
    /// may provide new compilers for opcodes if it wishes.
    /// </remarks>
    public interface IOpCode
    {
        /// <summary>
        /// An opportunity to 'pre-compile' this operation, adjusting the compiler state in advance. This is a chance to, for example, set up
        /// local variables before any IL is actually emitted. Normally when this is called, the compiler has set up the IL generator and the
        /// state for the context the IL will be compiled in (generally this means that it's started building the Call method of an IFunction
        /// class, but has not yet emitted any code).
        /// </summary>
        /// <param name="op">The operation that will be compiled.</param>
        /// <param name="compilerState">The compiler state for this opcode.</param>
        /// <param name="whichCompiler">The compiler that this opcode is being compiled as part of.</param>
        void PreCompileOp(Operation op, State compilerState, Compiler whichCompiler);

        /// <summary>
        /// Compiles the specified operation into a IL bytecode sequence.
        /// </summary>
        /// <param name="op">The operation to compile</param>
        /// <param name="generator">The IL generator that this operation should be compiled to</param>
        /// <param name="compilerState">The compiler state object that is storing the state of this compile run</param>
        /// <param name="whichCompiler">The compiler object that is invoking this call</param>
        void CompileOp(Operation op, ILGenerator generator, State compilerState, Compiler whichCompiler);
    }
}
