// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Compiler for the PushRelativeValue opcode             PushRelativeValue.cs |
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
using System.Reflection;
using System.Reflection.Emit;

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler.BOp
{
    /// <summary>
    /// IL Compiler for the PushRelativeValue opcode.
    /// </summary>
    [CompilesOp(Op.PushRelativeValue)]
    public sealed class PushRelativeValue : IOpCode
    {
        #region IOpCode Members

        public void PreCompileOp(Operation op, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            Data.Environment.RelativeBinding relBinding = (Data.Environment.RelativeBinding)op.a;

            // Tell the compiler to load the top-level environment if it's used in this BExpression
            if (relBinding.ParentCount == compilerState.FrameLevel) compilerState.NeedTopLevel = true;

            // We don't currently support 'external' environments other than the top-level environment.
            if (relBinding.ParentCount > compilerState.FrameLevel) throw new InvalidOperationException("The compiler currently does not support retrieving values from environments other than the top-level one, or local environments declared directly as part of the BExpression being compiled.");
        }

        public void CompileOp(Operation op, ILGenerator il, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            Data.Environment.RelativeBinding relBinding = (Data.Environment.RelativeBinding)op.a;

            if (relBinding.ParentCount >= compilerState.FrameLevel)
            {
                // environment.topLevel.values[symbol]
                il.Emit(OpCodes.Ldloc, compilerState.TopLevelLocal);                    // object[] array of the top-level environment (set up by the Compiler object)
                il.Emit(OpCodes.Ldsfld, compilerState.Symbol(relBinding.Symbol));       // Symbol number we want (in top-level environments, corresponds to the array entry)
                il.Emit(OpCodes.Ldelem_Ref);                                            // Load the value from the environment

                // TODO: maybe throw exception if this is Undefined.Value?
            }
            else
            {
                // Get information about where this field is stored
                Analysis.SymbolUsage usage = compilerState.UsageForSymbol(new Analysis.Location(relBinding.Offset, compilerState.FrameLevel - relBinding.ParentCount));
            }
        }

        #endregion
    }
}
