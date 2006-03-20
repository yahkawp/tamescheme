using System;
using System.Reflection.Emit;

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler.BOp
{
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
            }
            else if (op.a is long)
            {
                il.Emit(OpCodes.Ldc_I8, (long)op.a);
            }
            else if (op.a is string)
            {
                il.Emit(OpCodes.Ldstr, (string)op.a);
            }
            else if (op.a is float)
            {
                il.Emit(OpCodes.Ldc_R4, (float)op.a);
            }
            else if (op.a is double)
            {
                il.Emit(OpCodes.Ldc_R8, (double)op.a);
            }
            else
            {
                if (CanBeLiteral(op.a)) throw new InvalidOperationException("BUG in the opcode compiler for Push: CanBeLiteral() returned true for a value we do not have a literal form for");
                il.Emit(OpCodes.Ldfld, compilerState.DefineStaticData(op.a));
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
