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
		}

		public Environment(Environment parent)
		{
			this.parent = parent;
		}

		#region Variables

		/// <summary>
		/// The environment table
		/// </summary>
		/// <remarks>Normally the table is small (while evaluating a procedure, for example), but sometimes can become very large</remarks>
		HybridDictionary envTable = new HybridDictionary();

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

				return envTable[symbolNumber];
			}
			set
			{
				envTable[symbolNumber] = value;
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
				envTable[symbol.SymbolNumber] = value;
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
			envTable.Remove(symbolNumber);
		}

		/// <summary>
		/// Undefine a symbol by number
		/// </summary>
		public void Undefine(Symbol symbol)
		{
			envTable.Remove(symbol.SymbolNumber);
		}

		/// <summary>
		/// Undefine a symbol by name
		/// </summary>
		public void Undefine(string symbolName)
		{
			envTable.Remove(SymbolTable.NumberForSymbol(symbolName));
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
