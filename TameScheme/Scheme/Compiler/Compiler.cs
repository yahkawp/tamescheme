// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | .NET IL compiler for TameScheme                                Compiler.cs |
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
using System.Collections;
using System.Reflection;
using System.Reflection.Emit;

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler
{
    /// <summary>
    /// The compiler class is the base for the TameScheme .NET IL compiler.
    /// </summary>
    public class Compiler
    {
        #region Constructors

        public Compiler()
        {
            // Set the module that we're building in
            module = defaultModule;

            // Set the set of instruction assemblers that we should use for this type
            assemblers = GetOpcodesForType(GetType());
        }

        static Compiler()
        {
            lock (typeof(Compiler))
            {
                // Initialise all the opcodes in this assembly
                LoadOpcodesForType(typeof(Compiler), new Assembly[] { typeof(Compiler).Assembly });

                // Construct a default assembly and module builder
                if (defaultAssembly == null)
                {
                    AppDomain theDomain = System.Threading.Thread.GetDomain();
                    AssemblyName assemblyName = new AssemblyName("CompiledScheme");
                    defaultAssembly = theDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
                    defaultModule = defaultAssembly.DefineDynamicModule("Functions");
                }
            }

        }

        #endregion

        #region The builders

        // The assembly/module that we use by default for putting code into
        private static AssemblyBuilder defaultAssembly = null;
        private static ModuleBuilder defaultModule = null;

        // The number of the function (when generating names for otherwise unnamed functions)
        private static int functionNumber = 0;

        // The module that this particular compiler is targetting
        private ModuleBuilder module;

        #endregion

        #region Compiler settings

        // Variables and functions that affect the behaviour of the code generated by this compiler

        /// <summary>
        /// Set to true to instruct the compiler to generate code that supports scheme continuations.
        /// </summary>
        private bool allowContinuations = false;

        /// <summary>
        /// Set this flag to true to instruct the compiler to generate 'continuable' code when compiling functions.
        /// </summary>
        /// <remarks>
        /// Continuations are supported in .NET by emulating the scheme stack behaviour. This is somewhat fragile (it is broken when
        /// calling a function that doesn't support the feature, for example), and it also imposes a performance penalty. 
        /// </remarks>
        public bool AllowContinuations
        {
            get
            {
                return allowContinuations;
            }
            set
            {
                allowContinuations = value;
            }
        }

        #endregion

        #region Analysis

        // Functions that help with analysing the code that we're compiling

        #endregion

        #region Emitting code

        /// <summary>
        /// Takes the given scheme and top-level environment and compiles it into an object implementing the
        /// IProcedure inteface. (This compiles a procedure with no arguments)
        /// </summary>
        /// <param name="scheme">The scheme to compile</param>
        /// <param name="topLevel">The top-level environment to compile in</param>
        /// <returns>A new IProcedure object</returns>
        public Type Compile(object scheme, Data.Environment topLevel)
        {
            // Build a BExpression from the supplied scheme
            BExpression expr = BExpression.BuildExpression(scheme, topLevel);
            return Compile(expr, null);
        }

        /// <summary>
        /// Takes a BExpression and compiles an IProcedure object.
        /// </summary>
        /// <param name="expr">The BExpression to compile</param>
        /// <param name="state">The CompileState that the BExpression was compiled under</param>
        /// <returns>A new IProcedure object that executes the given scheme.</returns>
        public Type Compile(BExpression expr, string typeName)
        {
            Analysis.State state = new Analysis.State();

            // Construct a TypeBuilder to compile the type into
            TypeBuilder builder = TypeBuilderForFunction(typeName, null);
            state.RootType = builder;

            // Compile the call itself
            CompileCall(builder, expr, state);

            // Compile anything that's needed into the root/static types
            state.FinishStaticType();

            ILGenerator initGen = state.RootType.DefineTypeInitializer().GetILGenerator(8192);
            state.BuildInitialiser(initGen);
            initGen.Emit(OpCodes.Ret);

            // Build the type
            return builder.CreateType();
        }

        /// <summary>
        /// Creates a type builder for a scheme lambda expression.
        /// </summary>
        /// <param name="typeName">The name to give this type (or null if this type should have no name)</param>
        /// <param name="parent">The parent type to nest this type in (or null to define it as part of the module this compiler is handling)</param>
        /// <returns>A new TypeBuilder designed to contain the scheme lambda expression.</returns>
        protected TypeBuilder TypeBuilderForFunction(string typeName, TypeBuilder parent)
        {
            if (typeName == null)
            {
                // Make up a new type name
                lock (typeof(Compiler))
                {
                    typeName = "TS__SchemeFunction__" + (functionNumber++).ToString();
                }
            }

            lock (module)
            {
                // Create an IProcedure TypeBuilder with this name (public if there's no parent type, private otherwise)
                TypeBuilder procedureType;

                if (parent == null)
                {
                    procedureType = module.DefineType(typeName, TypeAttributes.Class | TypeAttributes.Public | TypeAttributes.Sealed, typeof(Object));
                }
                else
                {
                    procedureType = parent.DefineNestedType(typeName, TypeAttributes.Class | TypeAttributes.NestedPrivate | TypeAttributes.Sealed);
                }

                procedureType.AddInterfaceImplementation(typeof(Procedure.IProcedure));

                return procedureType;
            }
        }

        /// <summary>
        /// Defines a new method builder that implements the IProcedure 'Call' function
        /// </summary>
        /// <param name="typeBuilder">The TypeBuilder to define this Call as a part of</param>
        /// <param name="functionName">The function name to construct</param>
        /// <returns>A new MethodBuilder object that </returns>
        protected MethodBuilder DefineCall(TypeBuilder typeBuilder, string functionName)
        {
            if (functionName == null) functionName = "Call";

            // Define the 'Call' function in the specified type builder and return a suitable method builder
            MethodBuilder call;

            call = typeBuilder.DefineMethod(functionName, MethodAttributes.Public | MethodAttributes.Virtual | MethodAttributes.Final, 
                typeof(object), new Type[] { typeof(Data.Environment), typeof(object[]).MakeByRefType() });

            if (functionName.Equals("Call")) 
            {
                typeBuilder.DefineMethodOverride(call, typeof(Procedure.IProcedure).GetMethod("Call"));
            }

            return call;
        }

        protected void CompileCall(TypeBuilder type, BExpression expr, Analysis.State state)
        {
            // Create the MethodBuilder that will define this function
            MethodBuilder call = DefineCall(type, null);

            // Tell the state we're compiling a new BExpression from scratch
            state.StartBExpression();

            try
            {
                // Create the IL code generator
                ILGenerator il = call.GetILGenerator(8192);

                // Precompile the BExpression
                for (int instruction = 0; instruction < expr.expression.Length; instruction++)
                {
                    // Compile this operation
                    PreCompileOp(expr.expression[instruction], state);
                }

                // Put the top level environment into a local variable, if it's needed (PreCompilation should have established that)
                if ( state.NeedTopLevel)
                {
                    state.TopLevelLocal = il.DeclareLocal(typeof(object[]));

                    il.Emit(OpCodes.Ldarg_1);
                    il.Emit(OpCodes.Ldfld, typeof(Data.Environment).GetField("topLevel"));
                    il.Emit(OpCodes.Ldfld, typeof(Data.Environment).GetField("values"));
                    il.Emit(OpCodes.Stloc, state.TopLevelLocal);
                }

                // Begin compiling the BExpression into IL
                for (int instruction = 0; instruction < expr.expression.Length; instruction++)
                {
                    // Mark a label here
                    state.DefineNextLabel(il);

                    // Compile this operation
                    CompileOp(expr.expression[instruction], il, state);
                }

                // Mark the final label
                state.DefineNextLabel(il);

                // Return
                il.Emit(OpCodes.Ret);
            }
            finally
            {
                // Finished with this BExpression
                state.FinishBExpression();
            }
        }

        private IList assemblers;

        protected void PreCompileOp(Operation op, Analysis.State state)
        {
            IOpCode assembler;

            if ((int)op.operation < assemblers.Count)
            {
                assembler = (IOpCode)assemblers[(int)op.operation];
            }
            else
            {
                assembler = null;
            }

            if (assembler != null)
            {
                assembler.PreCompileOp(op, state, this);
            }
            else
            {
                return; // TODO: change to the exception (this is useful for debugging, though)
                //throw new NotSupportedException("A compiler was asked to compile an operation - " + op.operation.ToString() + " - which it does not have an IL assembler for");
            }
        }


        protected void CompileOp(Operation op, ILGenerator il, Analysis.State state)
        {
            IOpCode assembler;

            if ((int)op.operation < assemblers.Count)
            {
                assembler = (IOpCode)assemblers[(int)op.operation];
            }
            else
            {
                assembler = null;
            }

            if (assembler != null)
            {
                assembler.CompileOp(op, il, state, this);
            }
            else
            {
                throw new NotSupportedException("A compiler was asked to compile an operation - " + op.operation.ToString() + " - which it does not have an IL assembler for");
            }
        }

        #endregion

        #region Operations

        private static IDictionary opcodeTables = new Hashtable();

        protected static IList GetOpcodesForType(Type whichType)
        {
            // Sanity check
            if (!typeof(Compiler).IsAssignableFrom(whichType)) throw new InvalidOperationException("Compiler.GetOpcodesForType can only be called with a type that is a subclass of Compiler");

            lock (typeof(Compiler))
            {
                while (whichType != null && whichType != typeof(object))
                {
                    // See if there's an entry for this type
                    if (opcodeTables.Contains(whichType)) return (IList)opcodeTables[whichType];

                    // Try the base type
                    whichType = whichType.BaseType;
                }

                return null;
            }
        }

        protected static void SetOpcodesForType(Type whichType, IList opcodes)
        {
            // Sanity check
            if (!typeof(Compiler).IsAssignableFrom(whichType)) throw new InvalidOperationException("Compiler.SetOpcodesForType can only be called with a type that is a subclass of Compiler");

            // Set the opcodes that this type can use
            lock (typeof(Compiler))
            {
                opcodeTables[whichType] = opcodes;
            }
        }

        protected static void LoadOpcodesForType(Type whichType, Assembly[] whichAssemblies) {
            // Sanity check
            if (!typeof(Compiler).IsAssignableFrom(whichType)) throw new InvalidOperationException("Compiler.LoadOpcodesForType can only be called with a type that is a subclass of Compiler");

            // Construct the array that will hold the results
            ArrayList result = new ArrayList(256);
            for (int x = 0; x < 256; x++) result.Add(null);

            // The opcode interface type
            Type opcodeType = typeof(IOpCode);

            // Iterate through the list of assemblies
            foreach (Assembly assembly in whichAssemblies)
            {
                // Get the types from the current assembly
                Type[] types = assembly.GetExportedTypes();

                // Go through and find the types that derive from IOpCode and have a CompilesOpCodeAttribute applied to them
                foreach (Type candidateOpcodeType in types)
                {
                    // Must be a class
                    if (!candidateOpcodeType.IsClass) continue;

                    // Must be assignable from IOpCode
                    if (opcodeType.IsAssignableFrom(candidateOpcodeType))
                    {
                        CompilesOpAttribute[] opcodeAttribute = (CompilesOpAttribute[])candidateOpcodeType.GetCustomAttributes(typeof(CompilesOpAttribute), true);
                        ConstructorInfo simpleConstructor = candidateOpcodeType.GetConstructor(new Type[0]);

                        int opcodeNumber = (int)opcodeAttribute[0].Op;

                        // Must have an attribute indicating what assigns it, a () constructor and not be already defined
                        if (opcodeAttribute.Length > 0 && simpleConstructor != null && result[(int)opcodeAttribute[0].Op] == null)
                        {
                            // Construct this object
                            result[(int)opcodeAttribute[0].Op] = simpleConstructor.Invoke(new object[0]);
                        }
                    }
                }
            }

            // Define these opcodes
            SetOpcodesForType(whichType, result);
        }

        #endregion
    }
}
