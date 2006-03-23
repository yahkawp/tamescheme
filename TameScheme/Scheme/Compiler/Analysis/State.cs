using System;
using System.Reflection;
using System.Collections;
using System.IO;
using System.Reflection.Emit;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;

namespace Tame.Scheme.Compiler.Analysis
{
    /// <summary>
    /// This class defines the current state of the IL compiler. It is used, in particular, to store information about where
    /// variables can be located.
    /// </summary>
    /// <remarks>
    /// The state is created with no environments pushed.
    /// </remarks>
    public class State
    {

        #region Variables

        /// <summary>
        /// The current 'level' of this environment
        /// </summary>
        private int level = 0;

        /// <summary>
        /// Retrieves the 'level' of this environment (how many times it has been pushed: this is the level value that should be used for
        /// locations that are considered to be in the most recently pushed environment)
        /// </summary>
        public int Level
        {
            get
            {
                return level;
            }
        }

        /// <summary>
        /// A list of the symbols that need to be initialised as a part of this class
        /// </summary>
        private ArrayList initialiseSymbols = new ArrayList();

        #endregion

        #region Pushing/popping the environment

        /// <summary>
        /// Called when a new environment is pushed onto the stack (which might happen when we move to compiling a SProcedure)
        /// </summary>
        /// <param name="invalid">
        /// Mark any symbols in the current environment as 'invalid' and unavailable in the new environment.
        /// </param>
        /// <remarks>
        /// It is possible to call LocalLocation/FieldLocation on variables that are on levels 'above' this one.
        /// If you do this, 
        /// </remarks>
        public void PushEnvironment(bool invalid)
        {
            // Increase the level
            level++;

            // Push a new environment state
            envState = new EnvironmentState(envState, invalid);
        }

        /// <summary>
        /// Called when an environment is finished with: ie, when the environment is finished with.
        /// </summary>
        public void PopEnvironment()
        {
            // Sanity check
            if (level <= 0)
            {
                throw new InvalidOperationException("The compiler tried to pop the top-level environment (which makes no sense)");
            }

            // Decrease the level
            level--;

            // Pop the environment state
            envState = envState.PreviousState;
        }

        #endregion

        #region The environment state class

        private class EnvironmentState
        {
            /// <summary>
            /// The hashtable with the current environment states
            /// </summary>
            private Hashtable variableStates = new Hashtable();

            /// <summary>
            /// The list of fields that exist in the current environment (used when working out constructors, etc)
            /// </summary>
            private ArrayList fields = new ArrayList();

            /// <summary>
            /// If this is set, any variables in the state 'below' this one are considered invalid (no long current)
            /// </summary>
            public readonly bool invalidBelow;

            /// <summary>
            /// The environment state 'below' this one
            /// </summary>
            public readonly EnvironmentState PreviousState;

            public EnvironmentState(EnvironmentState previousState, bool invalidBelow)
            {
                this.PreviousState = previousState;
                this.invalidBelow = invalidBelow;

                if (!invalidBelow && previousState != null)
                {
                    this.needTopLevel = previousState.needTopLevel;
                }
                else
                {
                    this.needTopLevel = false;
                }
            }

            /// <summary>
            /// Defines where a particular symbol is within this environment
            /// </summary>
            public void DefineSymbol(Location where, SymbolUsage usage)
            {
                variableStates[where] = usage;

                if (usage.IsField) fields.Add(usage);
            }

            /// <summary>
            /// Returns information about the usage for a given symbol
            /// </summary>
            public SymbolUsage UsageForSymbol(Location where)
            {
                EnvironmentState thisState = this;

                while (thisState != null)
                {
                    SymbolUsage usage = (SymbolUsage)thisState.variableStates[where];
                    if (usage != null) return usage;

                    if (!invalidBelow)
                        thisState = PreviousState;
                    else
                        break;
                }

                // If thisState is non-null at this point, it indicates where the states have become invalid; we should indicate this so that the compiler can put them into fields

                return null;
            }

            /// <summary>
            /// The fields declared as part of this environment state
            /// </summary>
            public ArrayList Fields
            {
                get
                {
                    return fields;
                }
            }

            /// <summary>
            /// Set to true during pre-compilation if the top-level environment is used in this function
            /// </summary>
            public bool needTopLevel;

            /// <summary>
            /// A dictionary of local variables
            /// </summary>
            public IDictionary localDictionary;
        }

        private EnvironmentState envState;

        #endregion

        #region Dealing with environment locations

        /// <summary>
        /// Marks the given environment location as being stored in a local variable.
        /// </summary>
        /// <param name="where">Specifies the location (in BExpression terms) of this variable</param>
        /// <param name="var">The local variable number that is being used to store this value</param>
        public void LocalLocation(Location where, int var)
        {
            envState.DefineSymbol(where, new SymbolUsage(where, var));
        }

        /// <summary>
        /// Marks the given location as being stored in a field.
        /// </summary>
        /// <param name="where">The environment location that's represented by this field.</param>
        /// <param name="whichField">The field that will store this location.</param>
        public void FieldLocation(Location where, FieldInfo whichField)
        {
            envState.DefineSymbol(where, new SymbolUsage(where, whichField));
        }

        public SymbolUsage UsageForSymbol(Location where)
        {
            if (envState == null) return null;

            return envState.UsageForSymbol(where);
        }

        /// <summary>
        /// Retrieve the list of variables that have been defined as fields for this state.
        /// </summary>
        public ArrayList Fields
        {
            get
            {
                return envState.Fields;
            }
        }

        /// <summary>
        /// Whether or not the current environment state has used any values from a top-level environment.
        /// </summary>
        public bool NeedTopLevel
        {
            get
            {
                if (envState == null) return true;

                return envState.needTopLevel;
            }
            set
            {
                EnvironmentState state = envState;

                while (state != null)
                {
                    state.needTopLevel = value;

                    if (state.invalidBelow) return;
                    state = state.PreviousState;
                }
            }
        }

        private enum BuiltinLocals
        {
            TopLevel, Argument, IProcedure, TempStorage
        }

        /// <summary>
        /// The LocalBuilder containing the object[] array with the top-level environment in it
        /// </summary>
        public LocalBuilder TopLevelLocal
        {
            get
            {
                return (LocalBuilder)LocalDictionary[BuiltinLocals.TopLevel];
            }
            set
            {
                LocalDictionary[BuiltinLocals.TopLevel] = value;
            }
        }

        /// <summary>
        /// The local variable that should contain the IProcedure that we're going to call (used while building the argument arrays)
        /// </summary>
        public LocalBuilder IProcedureLocal
        {
            get
            {
                return (LocalBuilder)LocalDictionary[BuiltinLocals.IProcedure];
            }
            set
            {
                LocalDictionary[BuiltinLocals.IProcedure] = value;
            }
        }

        /// <summary>
        /// The object[] array that contains the list of arguments for a array.
        /// </summary>
        public LocalBuilder ArgumentLocal
        {
            get
            {
                return (LocalBuilder)LocalDictionary[BuiltinLocals.Argument];
            }
            set
            {
                LocalDictionary[BuiltinLocals.Argument] = value;
            }
        }

        /// <summary>
        /// The object local that is used for temporary storage.
        /// </summary>
        public LocalBuilder TempLocal
        {
            get
            {
                return (LocalBuilder)LocalDictionary[BuiltinLocals.TempStorage];
            }
            set
            {
                LocalDictionary[BuiltinLocals.TempStorage] = value;
            }
        }

        IDictionary localDictionary = new Hashtable();

        /// <summary>
        /// The dictionary containing local variables
        /// </summary>
        protected IDictionary LocalDictionary
        {
            get
            {
                EnvironmentState state = envState;

                while (state != null)
                {
                    if (state.localDictionary != null) return state.localDictionary;

                    if (state.invalidBelow)
                    {
                        state.localDictionary = new Hashtable();
                        return state.localDictionary;
                    }
                    state = state.PreviousState;
                }

                return localDictionary;
            }
        }

        /// <summary>
        /// A 'named' local variable, within the current context (up to the point an environment state was created
        /// invalidating the current context). This is a more general version of the TopLevelLocal, ArgumentLocal and
        /// IProcedureLocal properties.
        /// </summary>
        /// <param name="name">The name of the local variable to look up.</param>
        /// <returns>null if the local variable is not defined, or a LocalBuilder otherwise</returns>
        public LocalBuilder NamedLocal(string name)
        {
            return (LocalBuilder)LocalDictionary[name];
        }

        /// <summary>
        /// Defines a named local variable to have a specific value.
        /// </summary>
        /// <param name="name">The name of the local variable to define</param>
        /// <param name="definition">The definition of the local variable</param>
        public void DefineLocal(string name, LocalBuilder definition)
        {
            LocalDictionary[name] = definition;
        }

        #endregion

        #region Building types

        // Certain operations (for example, Push), need to be able to store data types, or work out the 'new' value of a symbol (which is constant during any one run, but varies between times)
        // For this reason we keep a 'core' typebuilder that we can add new fields to to store this data.

        private TypeBuilder rootType;
        private IDictionary rootTypeDefinedFields = new Hashtable();

        /// <summary>
        /// The 'root' type is a type used to store the static values associated with a particular object. This includes values that need to
        /// be serialised: ie, those are put onto the stack using Push, and symbols, which are constant during execution but need to be
        /// initialised on startup.
        /// </summary>
        public TypeBuilder RootType
        {
            get
            {
                return rootType;
            }
            set
            {
                if (rootType == null)
                {
                    rootType = value;
                }
                else
                {
                    throw new InvalidOperationException("An attempt was made to set the 'root' type twice for a particular compiler state (this doesn't make a lot of sense)");
                }
            }
        }

        protected string FieldNameForSymbol(Data.ISymbolic symbol)
        {
            // Get the scheme name for this symbol
            string schemeSymbol = symbol.ToString();

            // Build the name of the symbol; limit it to valid .NET field name characters
            StringBuilder symbolName = new StringBuilder("ts__SYM_");

            // TODO: if not a LiteralSymbol or a Symbol, then we should add in the type here to prevent a possible collision

            for (int chr = 0; chr < schemeSymbol.Length; chr++)
            {
                // Get the current character
                char thisChar = schemeSymbol[chr];

                // Add it to the string builder
                if ((thisChar >= 'a' && thisChar <= 'z') || (thisChar >= 'A' && thisChar <= 'Z'))
                {
                    symbolName.Append(thisChar);
                }
                else
                {
                    symbolName.Append("_" + ((int)thisChar).ToString() + "_");
                }
            }

            return symbolName.ToString();
        }

        /// <summary>
        /// Returns a static readonly field (which is optionally private) that contains the integer value for this symbol.
        /// </summary>
        /// <remarks>You must set a root type to use this function.</remarks>
        /// <param name="symbol">The symbol that we need a field for</param>
        /// <returns>The </returns>
        public FieldInfo Symbol(Data.ISymbolic symbol)
        {
            // Get the name for this symbol
            string symbolName = FieldNameForSymbol(symbol);
            
            // Return the existing field if one has already been created.
            FieldInfo symbolField = (FieldBuilder)rootTypeDefinedFields[symbolName];
            if (symbolField != null) return symbolField;

            // Construct the field for this symbol
            FieldBuilder symbolBuilder = rootType.DefineField(symbolName, typeof(int), FieldAttributes.InitOnly | FieldAttributes.Static | FieldAttributes.FamANDAssem);

            if (symbolBuilder == null) throw new InvalidOperationException("Failed to define symbol field " + symbolName);

            // Add this to the list of symbols to initialise
            initialiseSymbols.Add(symbol);
            rootTypeDefinedFields[symbolName] = symbolBuilder;

            // Return the result
            return symbolBuilder;
        }

        /// <summary>
        /// Builds the code required to initialise the root class
        /// </summary>
        /// <param name="initCode">An IL generator for the initialiser for the root class</param>
        public void BuildInitialiser(ILGenerator initCode)
        {
            Type symbolType = typeof(Data.Symbol);
            ConstructorInfo symbolConstructor = typeof(Data.Symbol).GetConstructor(new Type[] { typeof(string) });

            // Initialise the symbol fields appropriately
            foreach (Data.Symbol symbol in initialiseSymbols)
            {
                // Get the field that will contain this symbol
                string symbolName = FieldNameForSymbol(symbol);
                FieldInfo symbolField = (FieldInfo)this.rootTypeDefinedFields[symbolName];

                // Add code to construct a suitable symbol object
                // TODO: should probably be serialising/deserialising the symbol here; will stick with the name for now.
                initCode.Emit(OpCodes.Ldstr, symbol.ToString());
                initCode.Emit(OpCodes.Newobj, symbolConstructor);
                initCode.EmitCall(OpCodes.Call, typeof(Data.Symbol).GetProperty("SymbolNumber").GetGetMethod(), null);
                initCode.Emit(OpCodes.Stsfld, symbolField);
            }
        }

        #endregion

        #region Static Data

        // Operations like 'Push' deal with static data: this information must be serialised into an object so that it can be
        // pushed onto the stack.

        private TypeBuilder staticDataBuilder = null;
        private ArrayList staticObjects = new ArrayList();
        private ArrayList staticFields = new ArrayList();
        private Hashtable serialisableTypes = new Hashtable();

        /// <summary>
        /// The static data type: this can be set exactly once, or will be built as part of the root type. This type builder is used
        /// to store any static data required as part of a compiled expression.
        /// </summary>
        public TypeBuilder StaticData
        {
            get
            {
                if (staticDataBuilder == null)
                {
                    staticDataBuilder = RootType.DefineNestedType("ts__staticData");
                }

                return staticDataBuilder;
            }

            set
            {
                if (staticDataBuilder != null) throw new InvalidOperationException("The StaticData type builder can only be set once");
                staticDataBuilder = value;
            }
        }

        /// <summary>
        /// Returns a FieldBuilder object, which will contain static data for a specific object.
        /// </summary>
        /// <param name="staticObject">The object to create</param>
        /// <returns></returns>
        public FieldBuilder DefineStaticData(object staticObject)
        {
            // If this object is already defined in a field, return the previous definition
            for (int objNum = 0; objNum < staticObjects.Count; objNum++)
            {
                if (staticObjects[objNum] == staticObject)
                {
                    return (FieldBuilder)staticFields[objNum];
                }
            }

            // This object must be serialisable
            if (!serialisableTypes.Contains(staticObject.GetType()))
            {
                SerializableAttribute[] isSerializable = (SerializableAttribute[])staticObject.GetType().GetCustomAttributes(typeof(SerializableAttribute), true);

                if (isSerializable.Length <= 0)
                {
                    throw new InvalidOperationException("The compiler was asked to compile a BExpression containing a non-serialisable static object (of type " + staticObject.GetType().ToString() + ")");
                }

                serialisableTypes[staticObject.GetType()] = true;
            }

            // Add this data to the static data array
            staticObjects.Add(staticObject);

            // Create a field to eventually store it
            FieldBuilder builder = StaticData.DefineField("ts__static_" + staticObjects.Count.ToString(), staticObject.GetType(), FieldAttributes.FamORAssem | FieldAttributes.Static|FieldAttributes.InitOnly);
            staticFields.Add(builder);

            return builder;
        }

        /// <summary>
        /// Finishes creating the static data type: defines an initialiser that fills in all the various fields.
        /// </summary>
        /// <returns></returns>
        public Type FinishStaticType()
        {
            // Use an object[] array for our serialised objects, not an ArrayList (marginally faster and more convienient)
            object[] staticObjectArray = new object[staticObjects.Count];
            staticObjects.CopyTo(staticObjectArray);

            // Serialise the static data
            BinaryFormatter formatter = new BinaryFormatter();
            MemoryStream stream = new MemoryStream();

            formatter.Serialize(stream, staticObjectArray);

            byte[] staticData = stream.ToArray();

            // Define the static data in the type
            FieldBuilder staticDataField = StaticData.DefineInitializedData("ts__static_serialised", staticData, FieldAttributes.Static | FieldAttributes.Private);
            FieldBuilder dataArrayField = StaticData.DefineField("ts__static_array", typeof(object[]), FieldAttributes.Static | FieldAttributes.Private | FieldAttributes.InitOnly);

            // Define the initialiser
            ConstructorBuilder typeInit = StaticData.DefineTypeInitializer();
            ILGenerator il = typeInit.GetILGenerator(256 + 64*staticFields.Count);

            // Assemble the initialiser

            // stream = new MemoryStream(ts__static_serialised)
            LocalBuilder memStream = il.DeclareLocal(typeof(MemoryStream));
            il.Emit(OpCodes.Ldsfld, staticDataField);
            il.Emit(OpCodes.Newobj, typeof(MemoryStream).GetConstructor(new Type[] { typeof(byte[]) }));
            il.Emit(OpCodes.Stloc, memStream);

            // formatter = new BinaryFormatter()
            LocalBuilder format = il.DeclareLocal(typeof(BinaryFormatter));
            il.Emit(OpCodes.Newobj, typeof(BinaryFormatter).GetConstructor(new Type[0]));
            il.Emit(OpCodes.Stloc, format);

            // dataArrayField = formatter.Deserialize(stream);
            il.Emit(OpCodes.Ldloc, memStream);
            il.Emit(OpCodes.Ldloc, format);
            il.EmitCall(OpCodes.Call, typeof(BinaryFormatter).GetMethod("Deserialize", new Type[] { typeof(Stream) }), null);
            il.Emit(OpCodes.Castclass, typeof(object[]));
            il.Emit(OpCodes.Stsfld, dataArrayField);

            // Define all the static fields
            for (int index = 0; index < staticFields.Count; index++)
            {
                FieldBuilder field = (FieldBuilder)staticFields[index];

                // Load the value for this field
                il.Emit(OpCodes.Ldsfld, dataArrayField);
                il.Emit(OpCodes.Ldc_I4, index);
                il.Emit(OpCodes.Ldelem, typeof(object));

                // Cast to the appropriate type
                il.Emit(OpCodes.Castclass, field.FieldType);

                // Store in the field
                il.Emit(OpCodes.Stsfld, field);
            }

            // Return
            il.Emit(OpCodes.Ret);

            // Return the result
            return StaticData.CreateType();
        }

        #endregion

        #region Labels

        // Branches in a BExpression can be by label or by location. As we can be compiling several objects simultaneously -
        // consider (lambda () (lambda () 1)) - this is also implemented as a stack.

        private class LabelState
        {
            public LabelState()
            {
                this.previous = null;
            }

            public LabelState(LabelState previous)
            {
                this.previous = previous;
            }

            private LabelState previous;
            private int currentLabelCount;
            private IList labels = new ArrayList();
            private IDictionary namedLabels = new Hashtable();

            public void DefineNextLabel(ILGenerator il)
            {
                while (currentLabelCount >= labels.Count)
                {
                    labels.Add(il.DefineLabel());
                }

                il.MarkLabel((Label)labels[currentLabelCount]);
                currentLabelCount++;
            }

            public Label LabelWithOffset(ILGenerator il, int offset)
            {
                while (currentLabelCount + offset >= labels.Count)
                {
                    labels.Add(il.DefineLabel());
                }

                return (Label)labels[currentLabelCount + offset];
            }

            public void DefineNamedLabel(ILGenerator il, string name)
            {
                Label namedLabel;

                if (namedLabels.Contains(name))
                {
                    namedLabel = (Label)namedLabels[name];
                }
                else
                {
                    namedLabel = il.DefineLabel();
                    namedLabels[name] = namedLabel;
                }

                il.MarkLabel(namedLabel);
            }

            public Label LabelWithName(ILGenerator il, string name)
            {
                Label namedLabel;

                if (namedLabels.Contains(name))
                {
                    return (Label)namedLabels[name];
                }
                else
                {
                    namedLabel = il.DefineLabel();
                    namedLabels[name] = namedLabel;

                    return namedLabel;
                }
            }

            public LabelState Previous
            {
                get { return previous; } 
            }
        }

        private LabelState labelState = null;

        public void StartBExpression()
        {
            labelState = new LabelState(labelState);
        }

        public void FinishBExpression()
        {
            labelState = labelState.Previous;
        }

        public void DefineNextLabel(ILGenerator il)
        {
            labelState.DefineNextLabel(il);
        }

        public Label LabelWithOffset(ILGenerator il, int offset)
        {
            return labelState.LabelWithOffset(il, offset);
        }

        public void DefineNamedLabel(ILGenerator il, string name)
        {
            labelState.DefineNamedLabel(il, name);
        }

        public Label LabelWithName(ILGenerator il, string name)
        {
            return labelState.LabelWithName(il, name);
        }

        #endregion
    }
}
