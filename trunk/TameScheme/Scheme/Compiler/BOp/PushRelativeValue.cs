using System;
using System.Reflection.Emit;

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler.BOp
{
    [CompilesOp(Op.PushRelativeValue)]
    public sealed class PushRelativeValue : IOpCode
    {
        #region IOpCode Members

        public void PreCompileOp(Operation op, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            throw new System.Exception("The method or operation is not implemented.");
        }

        public void CompileOp(Operation op, ILGenerator generator, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            throw new System.Exception("The method or operation is not implemented.");
        }

        #endregion
    }
}
