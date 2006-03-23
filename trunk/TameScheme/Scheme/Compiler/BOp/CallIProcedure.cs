using System;
using System.Reflection;
using System.Reflection.Emit;

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler.BOp
{
    /// <summary>
    /// Compiles the CallIProcedure instruction.
    /// </summary>
    /// <remarks>
    /// It may be worth noting that if the top-level environment is changed by a procedure call (say by a new symbol being defined), then
    /// strange things will happen. In particular, as the environment array is noted in a local, writes to the environment will go to an
    /// inactive location.
    /// 
    /// TODO: modify the environment so that it will only modify the object array when it is not part of an evaluating expression. (Ie,
    /// at safe points)
    /// </remarks>
    [CompilesOp(Op.CallIProcedure)]
    public class CallIProcedure : IOpCode
    {
        #region IOpCode Members

        public void PreCompileOp(Operation op, Tame.Scheme.Compiler.Analysis.State compilerState, Compiler whichCompiler)
        {
        }

        public void CompileOp(Operation op, ILGenerator il, Tame.Scheme.Compiler.Analysis.State state, Compiler compiler)
        {
            // At this point, we have an IProcedure on top of the stack, and a fixed number of arguments on the stack.
            // We need to put the arguments into an object[] array, and call the procedure.

            if (state.IProcedureLocal == null)
            {
                state.IProcedureLocal = il.DeclareLocal(typeof(Procedure.IProcedure));
            }

            if (state.ArgumentLocal == null)
            {
                state.ArgumentLocal = il.DeclareLocal(typeof(object[]));
            }

            if (state.TempLocal == null)
            {
                state.TempLocal = il.DeclareLocal(typeof(object));
            }

            // Cast the element on top of the stack to IProcedure
            il.Emit(OpCodes.Castclass, typeof(Procedure.IProcedure));

            // Store it in the local variable
            il.Emit(OpCodes.Stloc, state.IProcedureLocal);

            // Build the array
            int argCount = (int)op.a;

            il.Emit(OpCodes.Ldc_I4, argCount);
            il.Emit(OpCodes.Newarr, typeof(object));
            il.Emit(OpCodes.Stloc, state.ArgumentLocal);

            if (argCount > 0)
            {
                // Construct the argument array

                // This is less than ideal: at this point, the stack contains a list of arguments which needs to be turned into an array
                // for the operation. This means shuffling them through a temp variable at the moment.
                //
                // Ideally we want to build the array as we go: this would mean either identifying the operations that push values for
                // a future procedure call and adding them to a object array on the fly. This could be done while compiling the
                // BExpression (as a flag on the operation), or as part of a precompilation phase (complicated, really).
                //
                // Of course, a more .NET way of doing it would be to have the procedures declared with their parameters: this is still
                // not ideal as we'd have to check that we've got a suitable function at runtime, and .NET has no real way of doing
                // (lambda x x) style functions (varargs not supported in managed code). So, for simplicities sake, we'll stick with
                // this for now.

                for (int arg = 0; arg < argCount; arg++)
                {
                    il.Emit(OpCodes.Stloc, state.TempLocal);
                    il.Emit(OpCodes.Ldloc, state.ArgumentLocal);
                    il.Emit(OpCodes.Ldc_I4, arg);
                    il.Emit(OpCodes.Ldloc, state.TempLocal);
                    il.Emit(OpCodes.Stelem_Ref);
                }
            }

            // Push the environment (argument 1)
            il.Emit(OpCodes.Ldloc, state.IProcedureLocal);
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Ldloca, state.ArgumentLocal);

            // Call the procedure
            il.EmitCall(OpCodes.Call, typeof(Procedure.IProcedure).GetMethod("Call", new Type[] { typeof(Data.Environment), typeof(object[]).MakeByRefType() }), null);
        }

        #endregion
    }
}
