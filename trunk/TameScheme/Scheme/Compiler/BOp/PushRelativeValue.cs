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

                // environment[symbol]
                il.Emit(OpCodes.Ldfld, topLevelSymbol);
                il.Emit(OpCodes.Ldarg_0);
                il.EmitCall(OpCodes.Call, typeof(Data.Environment).GetProperty("Item", new Type[] { typeof(Data.Symbol) }).GetGetMethod(), null);
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
