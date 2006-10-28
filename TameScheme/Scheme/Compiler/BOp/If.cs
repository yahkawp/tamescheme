// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Compiler for the If opcode                                           If.cs |
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
    [CompilesOp(Op.If)]
    public sealed class If : IOpCode
    {
        #region IOpCode Members

        public void PreCompileOp(Operation op, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
        }

        public void CompileOp(Operation op, ILGenerator il, Analysis.State compilerState, Compiler compiler)
        {
            // Get the label to branch to if the value on top of the stack isn't false
            Label labelOffset = compilerState.LabelWithOffset(il, (int)op.a);

            // Location to branch to if the test fails
            Label notFalsePop = il.DefineLabel();
            Label notFalseNoPop = labelOffset;
            Label isFalse = il.DefineLabel();

            // Check that we've got a boolean value on top of the stack
            il.Emit(OpCodes.Isinst, typeof(bool));
            il.Emit(OpCodes.Dup);

            // Branch if it's null: the result is not false (and we've left a value on the stack)
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Beq, notFalsePop);

            // Unbox, and branch to labelOffset if false (no values left on the stack)
            il.Emit(OpCodes.Unbox, typeof(bool));
            il.Emit(OpCodes.Brtrue, notFalseNoPop);
            il.Emit(OpCodes.Br, isFalse);

            // Is not false: continue with the following code
            il.MarkLabel(notFalsePop);
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Br, labelOffset);

            // Is false: continue from this point
            il.MarkLabel(isFalse);
        }

        #endregion
    }
}
