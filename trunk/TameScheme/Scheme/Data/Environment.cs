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
	public sealed class Environment
	{
		public Environment()
		{
			envTable = new HybridDictionary();
			values = new ArrayList();
		}

		public Environment(Environment parent)
		{
			envTable = new HybridDictionary();
			values = new ArrayList();

			this.parent = parent;
		}

		public Environment(HybridDictionary symbolsToOffsets, ICollection values, Environment parent)
		{
			this.envTable = symbolsToOffsets;
			this.values = new ArrayList(values);

			this.parent = parent;
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
		ArrayList values = null;

		/// <summary>
		/// The environment this one should inherit from.
		/// </summary>
		Environment parent = null;

		#endregion

		#region Accessing the environment

		/// <summary>
		/// Access the environment by symbol number
		/// </summary>
		public object this[int symbolNumber]
		{
			get
			{
				if (!envTable.Contains(symbolNumber))
				{
					// Recurse if we can to the parent environment
					if (parent != null)
					{
						return parent[symbolNumber];
					}
					else
					{
						throw new Exception.SymbolNotFound("Symbol \"" + SymbolTable.SymbolForNumber(symbolNumber) + "\" was not found in the environment");
					}
				}

				return values[(int)envTable[symbolNumber]];
			}
			set
			{
				if (!envTable.Contains(symbolNumber)) 
				{
					envTable[symbolNumber] = values.Count;
					values.Add(Unspecified.Value);
				}

				values[(int)envTable[symbolNumber]] = value;
			}
		}

		/// <summary>
		/// Retrieves the value associated with a given Symbol
		/// </summary>
		public object this[Symbol symbol]
		{
			get
			{
				return this[symbol.SymbolNumber];
			}
			set
			{
				this[symbol.SymbolNumber] = value;
			}
		}

		/// <summary>
		/// Retrieves the value associated with a Symbol with the given name
		/// </summary>
		public object this[string symbolName]
		{
			get
			{
				return this[SymbolTable.NumberForSymbol(symbolName)];
			}
			set
			{
				this[SymbolTable.NumberForSymbol(symbolName)] = value;
			}
		}

		/// <summary>
		/// Undefine a symbol by number
		/// </summary>
		public void Undefine(int symbolNumber)
		{
			values[(int)envTable[symbolNumber]] = Unspecified.Value;
		}

		/// <summary>
		/// Undefine a symbol by number
		/// </summary>
		public void Undefine(Symbol symbol)
		{
			Undefine(symbol.SymbolNumber);
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
		public bool Contains(Symbol symbol)
		{
			if (!envTable.Contains(symbol.SymbolNumber))
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

		/// <summary>
		/// Removes all values from this environment
		/// </summary>
		public void Clear()
		{
			envTable = new HybridDictionary();
		}

		#endregion

		#region Direct bindings

		/// <summary>
		/// Represents a concrete binding: the location of a symbol in an environment
		/// </summary>
		/// <remarks>
		/// These are constructed by the Environment object only
		/// </remarks>
		public class Binding
		{
			internal Binding(Environment env, int offset, int symbolNumber)
			{
				this.env = env;
				this.offset = offset;
				this.symbolNumber = symbolNumber;
			}

			int offset;
			Environment env;
			int symbolNumber;

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
		public class RelativeBinding
		{
			internal RelativeBinding(int parentCount, int offset, int symbolNumber)
			{
				this.parentCount = parentCount;
				this.offset = offset;
				this.symbolNumber = symbolNumber;
			}

			#region Variables

			int offset;
			int parentCount;
			int symbolNumber;

			public int SymbolNumber { get { return symbolNumber; } }
			public Symbol Symbol { get { return new Symbol(symbolNumber); } }

			#endregion

			#region Getting the value

			public override string ToString()
			{
				return string.Format("Relative {0} ^{1} ->{2}", Data.SymbolTable.SymbolForNumber(symbolNumber), parentCount, offset);
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
		/// <returns>The binding representing the location where this symbol is bound to</returns>
		/// <remarks>Nonexistent symbols are bound to the top-level environment</remarks>
		public Binding BindingForSymbol(Data.Symbol symbol)
		{
			if (envTable.Contains(symbol.SymbolNumber))
			{
				return new Binding(this, (int)envTable[symbol.SymbolNumber], symbol.SymbolNumber);
			}
			else
			{
				if (parent != null)
				{
					return parent.BindingForSymbol(symbol);
				}
				else
				{
					this[symbol] = Unspecified.Value;
					return new Binding(this, (int)envTable[symbol.SymbolNumber], symbol.SymbolNumber);
				}
			}
		}

		private RelativeBinding RelativeBindingForSymbol(Data.Symbol symbol, int count)
		{
			if (envTable.Contains(symbol.SymbolNumber))
			{
				return new RelativeBinding(count, (int)envTable[symbol.SymbolNumber], symbol.SymbolNumber);
			}
			else
			{
				if (parent != null)
				{
					return parent.RelativeBindingForSymbol(symbol, count+1);
				}
				else
				{
					this[symbol] = Unspecified.Value;
					return new RelativeBinding(count, (int)envTable[symbol.SymbolNumber], symbol.SymbolNumber);
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
		public RelativeBinding RelativeBindingForSymbol(Data.Symbol symbol)
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
			// Slow, but .NET seems to think that dictionaries aren't copyable. Sigh.
			HybridDictionary newDictionary = new HybridDictionary();

			foreach (int symbolNumber in envTable.Keys)
			{
				newDictionary[symbolNumber] = envTable[symbolNumber];
			}

			return newDictionary;
		}

		/// <summary>
		/// The 'size' of this environment (number of defined values)
		/// </summary>
		public int Size
		{
			get { return values.Count; }
		}

		#endregion

		public override string ToString()
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
}
