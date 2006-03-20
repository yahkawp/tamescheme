// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme environment class                                    Environment.cs |
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
using System.Collections.Specialized;

namespace Tame.Scheme.Data
{
	/// <summary>
	/// Representation of a scheme environment.
	/// </summary>
    /// <remarks>
    /// 'Top-Level' environments maintain entries for every symbol that is defined, and must be disposed when it is no longer required.
    /// 'Top-Level' environments can only be environments with no parent.
    /// </remarks>
	public sealed class Environment : IDisposable
	{
        private void MakeEmptyTopLevel()
        {
            lock (SymbolTable.SyncRoot)
            {
                int count = 0;
                foreach (ISymbolic sym in SymbolTable.AllSymbols)
                {
                    envTable[sym.HashValue] = count++;
                }

                values = new object[count];
                nextAvailable = count;

                for (int x = 0; x < values.Length; x++) values[x] = Data.Unspecified.Value;

                SymbolTable.NewSymbol += this.NewTopLevelSymbol;
            }
        }

        /// <summary>
        /// Constructs a new, empty, top-level environment.
        /// </summary>
		public Environment()
		{
			envTable = new HybridDictionary();

            // This is a top-level environment
            this.IsTopLevel = true;
            this.TopLevel = this;

            MakeEmptyTopLevel();
		}

        /// <summary>
        /// Constructs a new, empty local or top-level environment
        /// </summary>
        /// <param name="parent">null if this is a top-level environment, or a parent environment</param>
        public Environment(Environment parent)
		{
			envTable = new HybridDictionary();
            values = new object[0];
            nextAvailable = 0;

			this.parent = parent;

            if (parent == null)
            {
                this.IsTopLevel = true;
                this.TopLevel = this;

                MakeEmptyTopLevel();
            }
            else
            {
                this.IsTopLevel = false;
                this.TopLevel = parent.TopLevel;
            }
		}

        /// <summary>
        /// Constructs a new environment. This may create a local environment with no parent if parent is null and topLevel is false.
        /// </summary>
        /// <param name="parent">The 'parent' environment, or null if this should be a top-level environment.</param>
        /// <param name="topLevel">If false, then this call will always create a local environment and never a top-level one, even when parent is null.</param>
        public Environment(Environment parent, bool topLevel)
        {
            envTable = new HybridDictionary();
            values = new object[0];
            nextAvailable = 0;

            this.parent = parent;

            if (parent == null && topLevel)
            {
                this.IsTopLevel = true;
                this.TopLevel = this;

                MakeEmptyTopLevel();
            }
            else
            {
                this.IsTopLevel = false;
                if (parent != null)
                    this.TopLevel = parent.TopLevel;
                else
                    this.TopLevel = null;
            }
        }
        
        /// <summary>
        /// Constructs a new local environment containing the specified symbols and values.
        /// </summary>
        /// <param name="symbolsToOffsets">A dictionary mapping symbol HashValues to offsets into the values array.</param>
        /// <param name="values">The values to store in this environment.</param>
        /// <param name="parent">The parent environment. This must not be null.</param>
		public Environment(HybridDictionary symbolsToOffsets, ICollection values, Environment parent)
		{
			this.envTable = symbolsToOffsets;
			this.values = new object[values.Count];
            values.CopyTo(this.values, 0);
            this.nextAvailable = values.Count;

			this.parent = parent;
            this.IsTopLevel = false;
            this.TopLevel = parent.TopLevel;
        }

        /// <summary>
        /// Constructs a new local environment containing the specified symbols and values.
        /// </summary>
        /// <param name="symbolsToOffsets">A dictionary mapping symbol HashValues to offsets into the values array.</param>
        /// <param name="values">The values to store in this environment.</param>
        /// <param name="totalSize">The total size of the environment (must be greater than values.Count). Extra fields are initialised to null (not Unspecified!)</param>
        /// <param name="parent">The parent environment.</param>
        public Environment(HybridDictionary symbolsToOffsets, ICollection values, int totalSize, Environment parent)
        {
            this.envTable = symbolsToOffsets;
            this.values = new object[totalSize];
            values.CopyTo(this.values, 0);
            this.nextAvailable = this.values.Length;

            this.parent = parent;
            this.IsTopLevel = false;
            this.TopLevel = parent.TopLevel;
        }

        /// <summary>
        /// Constructs a local, empty environment with the symbols mapped to specific values.
        /// </summary>
        /// <param name="symbolsToOffsets">A dictionary mapping symbols to offsets.</param>
        /// <param name="initialSize">The total size of the environment.</param>
        /// <param name="parent">The parent environment.</param>
        public Environment(HybridDictionary symbolsToOffsets, int initialSize, Environment parent)
        {
            this.envTable = symbolsToOffsets;
            this.values = new object[initialSize];
            this.nextAvailable = values.Length;

            this.parent = parent;
            this.IsTopLevel = false;
            this.TopLevel = parent.TopLevel;
        }

		#region Variables

		/// <summary>
		/// The environment table. Maps symbol numbers to address locations
		/// </summary>
		/// <remarks>
		/// Normally the table is small (while evaluating a procedure, for example), but sometimes can become very large.
		/// 
		/// The table may also be non-existent to create an anonymous environment.
		/// </remarks>
		HybridDictionary envTable = null;

		/// <summary>
		/// The array of values in this environment.
		/// </summary>
        /// <remarks>
        /// Exactly ONE thread (the interpreter thread) is allowed to write object values without locking. Other threads should lock:
        /// this does not guarantee the values won't change, but does guarantee that the location array won't be reallocated.
        /// This is public, as very fast access is often required.
        /// </remarks>
        public object[] values;

        /// <summary>
        /// Size to grow the environment by (when necessary)
        /// </summary>
        const int valueGrowth = 256;

        /// <summary>
        /// The next available value slot
        /// </summary>
        int nextAvailable;

		/// <summary>
		/// The environment this one should inherit from.
		/// </summary>
		Environment parent = null;

        /// <summary>
        /// Whether or not this environment is a top-level environment
        /// </summary>
        public readonly bool IsTopLevel;

        /// <summary>
        /// The top-level environment relative to this one.
        /// </summary>
        public readonly Environment TopLevel;

		#endregion

		#region Accessing the environment

		#region Temporary bindings

		// These can be used to 'temporarily' rebind values to other values
		HybridDictionary temporaryHistory = null;

		/// <summary>
		/// 'Temporarily' binds the given symbol, remembering the old location for later
		/// </summary>
		/// <param name="symbol">The symbol to assign a temporary binding to</param>
		/// <remarks>
		/// Using 'temporary' binding allows syntax like let to re-use an existing environment. The locations created by this call are not
		/// re-used: this is so that syntax like (let ((x 1)) (lambda () x)) won't find that later things overwrite that value of x.
		/// </remarks>
		public void BindTemporary(ISymbolic symbol)
		{
            lock (this)
            {
                if (IsTopLevel) throw new InvalidOperationException("BindTemporary() is not valid for a top-level environment");

                // Construct a history object
                if (temporaryHistory == null) temporaryHistory = new HybridDictionary();

                // If there's no history for this symbol, create one
                if (!temporaryHistory.Contains(symbol.HashValue))
                {
                    temporaryHistory[symbol.HashValue] = new Stack();
                }

                // If this symbol has a binding, remember it
                if (envTable.Contains(symbol.HashValue))
                {
                    ((Stack)temporaryHistory[symbol.HashValue]).Push(envTable[symbol.HashValue]);
                }

                // Create a new location for this symbol
                if (nextAvailable >= values.Length)
                {
                    // Reallocate the values
                    object[] newValues = new object[values.Length + valueGrowth];
                    values.CopyTo(newValues, 0);
                    values = newValues;
                }

                values[nextAvailable] = Unspecified.Value;
                envTable[symbol.HashValue] = nextAvailable++;
            }
		}

		/// <summary>
		/// Unbinds a value previously bound with BindTemporary. This restores the original binding
		/// </summary>
		/// <param name="symbol">The symbol to unbind</param>
		public void UnbindTemporary(ISymbolic symbol)
		{
            lock (this)
            {
                // Sanity check
                if (temporaryHistory == null)
                    throw new InvalidOperationException("Can't unbind a temporary value if no temporary values have previously been bound");
                if (!temporaryHistory.Contains(symbol.HashValue))
                    throw new InvalidOperationException("Can't unbind a temporary value if its symbol has not previously been bound");

                Stack symbolHistory = (Stack)temporaryHistory[symbol.HashValue];
                if (symbolHistory.Count <= 0)
                {
                    if (!envTable.Contains(symbol.HashValue))
                    {
                        throw new InvalidOperationException("Can't unbind a temporary value if its symbol has not previously been bound");
                    }
                    else
                    {
                        envTable.Remove(symbol.HashValue);
                        return;
                    }
                }

                // Pop the history
                envTable[symbol.HashValue] = symbolHistory.Pop();
            }
		}

		#endregion

		/// <summary>
		/// Access the environment by symbol hash value
		/// </summary>
		public object this[object hashValue]
		{
			get
			{
                lock (this)
                {
                    if (!envTable.Contains(hashValue))
                    {
                        // Recurse if we can to the parent environment
                        if (parent != null)
                        {
                            return parent[hashValue];
                        }
                        else
                        {
                            throw new Exception.SymbolNotFound("Symbol \"" + hashValue.ToString() + "\" was not found in the environment");
                        }
                    }

                    return values[(int)envTable[hashValue]];
                }
			}
			set
			{
                lock (this)
                {
                    if (!envTable.Contains(hashValue))
                    {
                        if (IsTopLevel)
                        {
                            throw new InvalidOperationException("Unable to define a new symbol in a top level environment (only symbols defined in the SymbolTable may be accessed in a top-level environment)");
                        }

                        lock (this)
                        {
                            // Add a new value
                            if (nextAvailable >= values.Length)
                            {
                                // Allocate some more space
                                object[] newValues = new object[values.Length + valueGrowth];
                                values.CopyTo(newValues, 0);
                                values = newValues;
                            }

                            values[nextAvailable] = Unspecified.Value;
                            envTable[hashValue] = nextAvailable++;
                        }
                    }

                    values[(int)envTable[hashValue]] = value;
                }
			}
		}

		/// <summary>
		/// Retrieves the value associated with a given Symbol
		/// </summary>
		public object this[ISymbolic symbol]
		{
			get
			{
				try
				{
					return this[symbol.HashValue];
				}
				catch (Exception.SymbolNotFound)
				{
					throw new Exception.SymbolNotFound("Symbol \"" + symbol.ToString() + "\" was not found in the environment");
				}
			}
			set
			{
				this[symbol.HashValue] = value;
			}
		}

		/// <summary>
		/// Retrieves the value associated with a Symbol with the given name
		/// </summary>
		public object this[string symbolName]
		{
			get
			{
				return this[new Symbol(symbolName)];
			}
			set
			{
				this[new Symbol(symbolName)] = value;
			}
		}

		/// <summary>
		/// Undefine a symbol by its hash value
		/// </summary>
		public void Undefine(object symbolHash)
		{
			values[(int)envTable[symbolHash]] = Unspecified.Value;
		}

		/// <summary>
		/// Undefine a symbol by number
		/// </summary>
		public void Undefine(ISymbolic symbol)
		{
			Undefine(symbol.HashValue);
		}

		/// <summary>
		/// Undefine a symbol by name
		/// </summary>
		public void Undefine(string symbolName)
		{
			Undefine(SymbolTable.NumberForSymbol(symbolName));
		}

		/// <summary>
		/// The parent environment: symbols are inherited from here
		/// </summary>
		public Environment Parent
		{
			get
			{
				return parent;
			}
			set
			{
				parent = value;
			}
		}

		/// <summary>
		/// Tests if the environment contains the given symbol
		/// </summary>
		/// <returns>true if this environment or its parents contain the given symbol</returns>
		public bool Contains(ISymbolic symbol)
		{
            lock (this)
            {
                if (!envTable.Contains(symbol.HashValue))
                {
                    if (parent != null)
                        return parent.Contains(symbol);
                    else
                        return false;
                }
                else
                {
                    return true;
                }
            }
		}

		/// <summary>
		/// Removes all values from this environment
		/// </summary>
		public void Clear()
		{
            lock (this)
            {
                envTable = new HybridDictionary();
            }
		}

		#endregion

		#region Direct bindings

		/// <summary>
		/// Represents a concrete binding: the location of a symbol in an environment
		/// </summary>
		/// <remarks>
		/// These are constructed by the Environment object only
		/// </remarks>
		public sealed class Binding
		{
			internal Binding(Environment env, int offset, Data.ISymbolic symbol)
			{
				this.env = env;
				this.offset = offset;
				this.symbol = symbol;
			}

			int offset;
			Environment env;
			Data.ISymbolic symbol;

			/// <summary>
			/// Gets the environment this binding maps to
			/// </summary>
			public Environment Environment { get { return env; } }

			/// <summary>
			/// Gets/sets the value associated with this binding
			/// </summary>
			public object Value
			{
				get
				{
					return env.values[offset];
				}
				set
				{
					env.values[offset] = value;
				}
			}

			/// <summary>
			/// Sets the value associated with this binding (works around a .NET bug, I think)
			/// </summary>
			/// <param name="value"></param>
			public void SetValue(object value)
			{
				env.values[offset] = value;
			}

			/// <summary>
			/// Turns this Binding into a RelativeBinding
			/// </summary>
			/// <param name="relativeTo">The environment to create a binding relative to</param>
			/// <param name="topLevel">If non-null, the environment that's 'one level up' from the lowermost environment in env. Used when compiling expressions, as the top level and local environments are separate.</param>
			/// <returns>null if no relative binding was available, </returns>
			public RelativeBinding RelativeTo(Data.Environment relativeTo, Data.Environment topLevel)
			{
				int parentCount = 0;

				// Search for the environment that's the same as the one this binding refers to
				while (relativeTo != null && relativeTo != env)
				{
					parentCount++;
					relativeTo = relativeTo.Parent;
				}

				if (relativeTo == null)
				{
					// ... continue the search in the top level environment
					while (topLevel != null && topLevel != env)
					{
						parentCount++;
						topLevel = topLevel.Parent;
					}
				}

				if (relativeTo == null && topLevel == null) return null;

				// Return the result
				return new RelativeBinding(parentCount, offset, symbol);
			}

			/// <summary>
			/// Returns true if two bindings refer to the same object
			/// </summary>
			public override bool Equals(object obj)
			{
				if (obj is Binding)
				{
					Binding otherBinding = (Binding) obj;

					return (otherBinding.offset==offset) && (otherBinding.env==env);
				}

				return false;
			}

			public override int GetHashCode()
			{
				return offset.GetHashCode()^env.values.GetHashCode();
			}
		}

		/// <summary>
		/// RelativeBinding is a more fragile version of Binding. It defines where a symbol is bound relative to an environment, whereas Binding
		/// defines an exact location. This makes it possible to apply it to another environment with the same structure (unlike Binding, which will
		/// only ever refer to the same environment)
		/// </summary>
		public sealed class RelativeBinding
		{
			internal RelativeBinding(int parentCount, int offset, Data.ISymbolic symbol)
			{
				this.parentCount = parentCount;
				this.offset = offset;
				this.symbol = symbol;
			}

			#region Variables

			int offset;
			int parentCount;
			Data.ISymbolic symbol;

			public ISymbolic Symbol { get { return symbol; } }
            public int Offset { get { return offset; } }
            public int ParentCount { get { return parentCount; } }

			#endregion

			#region Getting the value

			public override string ToString()
			{
				return string.Format("Relative {0} ^{1} ->{2}", symbol.ToString(), parentCount, offset);
			}

			/// <summary>
			/// Retrieves the value of this binding when applied to the given environment
			/// </summary>
			/// <param name="env">The environment to find the value of this binding in</param>
			/// <returns>The value of this binding</returns>
			public object ValueInEnvironment(Environment env)
			{
				for (int x=0; x<parentCount; x++) env = env.parent;
				return env.values[offset];
			}

			/// <summary>
			/// Sets the value of this binding when applied to the given environment
			/// </summary>
			public void SetValueInEnvironment(Environment env, object value)
			{
				for (int x=0; x<parentCount; x++) env = env.parent;
				env.values[offset] = value;
			}

			#endregion

			#region Equality

			public override bool Equals(object obj)
			{
				if (obj is RelativeBinding)
				{
					RelativeBinding objBinding = (RelativeBinding)obj;

					return objBinding.offset==offset && objBinding.parentCount==parentCount;
				}

				return false;
			}

			public override int GetHashCode()
			{
				return offset.GetHashCode() ^ parentCount.GetHashCode();
			}

			#endregion
		}

		/// <summary>
		/// Retrieves the Binding for a specific symbol. This can be used for faster access.
		/// </summary>
		/// <returns>The binding representing the location where this symbol is bound to, or null if it is unbound</returns>
		public Binding BindingForSymbol(Data.ISymbolic symbol)
		{
            lock (this)
            {
                if (envTable.Contains(symbol.HashValue))
                {
                    return new Binding(this, (int)envTable[symbol.HashValue], symbol);
                }
                else
                {
                    if (parent != null)
                    {
                        return parent.BindingForSymbol(symbol);
                    }
                    else
                    {
                        return null;
                    }
                }
            }
		}

		private RelativeBinding RelativeBindingForSymbol(Data.ISymbolic symbol, int count)
		{
            lock (this)
            {
                if (envTable.Contains(symbol.HashValue))
                {
                    return new RelativeBinding(count, (int)envTable[symbol.HashValue], symbol);
                }
                else
                {
                    if (parent != null)
                    {
                        return parent.RelativeBindingForSymbol(symbol, count + 1);
                    }
                    else
                    {
                        return null;
                    }
                }
            }
		}

		/// <summary>
		/// Finds the 'relative' binding for a specific symbol.
		/// </summary>
		/// <param name="symbol">The symbol to find the relative binding for</param>
		/// <returns>The relative binding for the given symbol</returns>
		/// <remarks>
		/// 'Relative' bindings are less concrete than regular Bindings and depend on context. They can be evaluated in the context of another
		/// environment, provided that environment has the same 'structure' (symbols allocated in the same location for this environment
		/// and all its parents: you can guarantee this by defining the symbols in the same order when creating the environment(s))
		/// </remarks>
		public RelativeBinding RelativeBindingForSymbol(Data.ISymbolic symbol)
		{
			return RelativeBindingForSymbol(symbol, 0);
		}

		/// <summary>
		/// Constructs a copy of the symbol -> location dictionary used by this environment.
		/// </summary>
		/// <returns>A copy of the symbol dictionary</returns>
		/// <remarks>This can be used to quickly construct environments with a similar structure to this one</remarks>
		public HybridDictionary CopySymbols()
		{
            lock (this)
            {
                // Slow, but .NET seems to think that dictionaries aren't copyable. Sigh.
                HybridDictionary newDictionary = new HybridDictionary();

                foreach (int symbolNumber in envTable.Keys)
                {
                    newDictionary[symbolNumber] = envTable[symbolNumber];
                }

                return newDictionary;
            }
		}

		/// <summary>
		/// The 'size' of this environment (number of defined values)
		/// </summary>
		public int Size
		{
			get { return nextAvailable; }
		}

		#endregion

		public override string ToString()
		{
            lock (this)
            {
                System.Text.StringBuilder res = new System.Text.StringBuilder();

                res.Append("{\n");

                foreach (int symbolNumber in envTable.Keys)
                {
                    res.Append("  " + SymbolTable.SymbolForNumber(symbolNumber) + " => (" + envTable[symbolNumber].ToString() + ")\n");
                }

                res.Append("}");

                return res.ToString();
            }
		}

        /// <summary>
        /// Event handler called when a new symbol is defined (only if this is a top-level environment)
        /// </summary>
        private void NewTopLevelSymbol(ISymbolic newSymbol, int number)
        {
            lock (this)
            {
                if (number != nextAvailable) throw new InvalidOperationException("A top-level environment received a new symbol that does not correspond monotonically to the last defined symbol value (got " + number + " but was expecting " + nextAvailable + ")");

                // Reallocate the values if necessary
                if (nextAvailable >= values.Length)
                {
                    object[] newValues = new object[values.Length + valueGrowth];
                    values.CopyTo(newValues, 0);
                    values = newValues;
                }

                // Define the new symbol
                envTable[newSymbol.HashValue] = number;
                values[number] = Data.Unspecified.Value;

                nextAvailable++;
            }
        }

        #region IDisposable Members

        public void Dispose()
        {
            if (IsTopLevel)
            {
                SymbolTable.NewSymbol -= NewTopLevelSymbol;
            }
        }

        #endregion
    }
}
