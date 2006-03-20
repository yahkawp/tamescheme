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
    /// and are typically singleton objects. This may not be true for a highly customised scheme implementation.
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
