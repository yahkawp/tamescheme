// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Compiler for the Push opcode                                       Push.cs |
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

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler.BOp
{
    /// <summary>
    /// IL compiler for the Push opcode.
    /// </summary>
    [CompilesOp(Op.Push)]
    public sealed class Push : IOpCode
    {
        #region IOpCode Members

        public void PreCompileOp(Tame.Scheme.Runtime.Operation op, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            // Define the static data for this operation
            if (!CanBeLiteral(op.a))
            {
                compilerState.DefineStaticData(op.a);
            }
        }

        public void CompileOp(Tame.Scheme.Runtime.Operation op, System.Reflection.Emit.ILGenerator il, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            // Load the static data field for this operation
            if (op.a is int)
            {
                il.Emit(OpCodes.Ldc_I4, (int)op.a);
                il.Emit(OpCodes.Box, typeof(int));
            }
            else if (op.a is long)
            {
                il.Emit(OpCodes.Ldc_I8, (long)op.a);
                il.Emit(OpCodes.Box, typeof(long));
            }
            else if (op.a is string)
            {
                il.Emit(OpCodes.Ldstr, (string)op.a);
                il.Emit(OpCodes.Box, typeof(string));
            }
            else if (op.a is float)
            {
                il.Emit(OpCodes.Ldc_R4, (float)op.a);
                il.Emit(OpCodes.Box, typeof(float));
            }
            else if (op.a is double)
            {
                il.Emit(OpCodes.Ldc_R8, (double)op.a);
                il.Emit(OpCodes.Box, typeof(double));
            }
            else
            {
                if (CanBeLiteral(op.a)) throw new InvalidOperationException("BUG in the opcode compiler for Push: CanBeLiteral() returned true for a value we do not have a literal form for");
                il.Emit(OpCodes.Ldsfld, compilerState.DefineStaticData(op.a));

                Type argType = op.a.GetType();
                if (argType.IsValueType)
                {
                    //il.Emit(OpCodes.Box, argType);
                }
            }
        }

        #endregion

        private bool CanBeLiteral(object o)
        {
            if (o is int || o is long || o is string || o is float || o is double) 
                return true;
            else
                return false;
        }
    }
}
