using System;
using System.Reflection;
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
            Data.Environment.RelativeBinding relBinding = (Data.Environment.RelativeBinding)op.a;

            if (relBinding.ParentCount == compilerState.Level) compilerState.NeedTopLevel = true;
            if (relBinding.ParentCount > compilerState.Level) throw new InvalidOperationException("The compiler currently does not support retrieving values from environments other than the top-level one, or local environments declared directly as part of the BExpression being compiled.");
        }

        public void CompileOp(Operation op, ILGenerator il, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
            Data.Environment.RelativeBinding relBinding = (Data.Environment.RelativeBinding)op.a;

            if (relBinding.ParentCount >= compilerState.Level)
            {
                // Load the symbol from the top-level environment... FIXME: SLOW!

                //
                // What we probably want to do is adjust top-level environments so that they always contain an entry for every
                // symbol that's defined, instead of dynamically allocating space as symbols are used. That would mean that
                // the location of a specific symbol was always known.
                //
                FieldInfo topLevelSymbol = compilerState.Symbol(relBinding.Symbol);

                // environment.topLevel.values[symbol]
                il.Emit(OpCodes.Ldloc, compilerState.TopLevelLocal);
                il.Emit(OpCodes.Ldsfld, compilerState.Symbol(relBinding.Symbol));
                il.Emit(OpCodes.Ldelem, typeof(object));
            }
            else
            {
                // Get information about where this field is stored
                Analysis.SymbolUsage usage = compilerState.UsageForSymbol(new Analysis.Location(relBinding.Offset, compilerState.Level - relBinding.ParentCount));
            }
        }

        #endregion
    }
}
