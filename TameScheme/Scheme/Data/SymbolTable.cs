// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Table mapping symbol names to/from numbers                  SymbolTable.cs |
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

namespace Tame.Scheme.Data
{
	/// <summary>
	/// Static class used for converting symbols into numbers and vice-versa
	/// </summary>
	internal class SymbolTable
	{
		#region Table variables

		static Hashtable symbolTable = new Hashtable();				// Maps symbols to numbers
		static ArrayList symbols = new ArrayList();					// Maps numbers to symbols

		#endregion

		#region Dealing with symbols

		internal class NonExistentSymbolException : Exception.SchemeException
		{
			public NonExistentSymbolException(int symbolNumber) : base("Symbol number \"" + symbolNumber + "\" was not found in the list of symbols")
			{
			}
		}

		public static string SymbolForNumber(int number)
		{
			lock (symbolTable.SyncRoot)
			{
				// Throw an exception if this symbol is not in the table
				if (number >= symbols.Count) throw new NonExistentSymbolException(number);

				// Temporary symbols have negative values
				if (number < 0) return string.Format("#[temporary {0}]", -number);

				// Retrieve the name from the table
				return (string)symbols[number];
			}
		}

		public static int NumberForSymbol(string symbolName)
		{
			lock (symbolTable.SyncRoot)
			{
				// Scheme symbols are case-insensitive
				symbolName = symbolName.ToLower();

				// Return the known value if we have one
				if (symbolTable.Contains(symbolName)) return (int)symbolTable[symbolName];

				// Create a new symbol if not
				int res = symbols.Count;
				symbols.Add(symbolName);
				symbolTable[symbolName] = res;

				return res;
			}
		}

		#endregion
	}
}
