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
	public class SymbolTable
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

        /// <summary>
        /// Given a symbol number, returns the symbol name.
        /// </summary>
        /// <param name="number">The number of the symbol to get a name for.</param>
        /// <returns>The name of the symbol with the specified name.</returns>
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

        /// <summary>
        /// For a given symbol name, returns a number. For symbols that have not been previously encountered, this will assign a number
        /// dynamically.
        /// </summary>
        /// <param name="symbolName">The symbol name that we want to turn into a number.</param>
        /// <returns>A number for the given symbol name. This is both the canonical representation of this symbol, and the slot
        /// it will occupy in a top-level environment.</returns>
		public static int NumberForSymbol(string symbolName)
		{
            int res;
			lock (symbolTable.SyncRoot)
			{
				// Scheme symbols are case-insensitive
				symbolName = symbolName.ToLower();

				// Return the known value if we have one
				if (symbolTable.Contains(symbolName)) return (int)symbolTable[symbolName];

				// Create a new symbol if not
				res = symbols.Count;
				symbols.Add(symbolName);
				symbolTable[symbolName] = res;

                allSymbols = null;
            }

            OnNewSymbol(new Symbol(symbolName), res);

            return res;
        }

        /// <summary>
        /// Declares a temporary (typically 'anonymous') symbol, making it possible to use it in top-level environment as well as in local environments.
        /// </summary>
        /// <remarks>It is possible, but not sensible, to declare a named Symbol as temporary. This will result in an unused slot in the top-level environments.</remarks>
        /// <param name="tempSymbol">The temporary symbol that should be declared</param>
        /// <returns>The number assigned to this temporary symbol.</returns>
        public static int DeclareTemporarySymbol(ISymbolic tempSymbol)
        {
            int res = -1;
            lock (symbolTable.SyncRoot)
            {
                if (!symbolTable.Contains(tempSymbol.HashValue))
                {
                    // Create a new symbol if this one is not already declared
                    res = symbols.Count;

                    symbols.Add(tempSymbol);
                    symbolTable[tempSymbol] = res;

                    allSymbols = null;
                }
            }

            if (res >= 0) OnNewSymbol(tempSymbol, res);

            return res;
        }

        public static object SyncRoot { get { return symbolTable.SyncRoot; } }

        private static IList allSymbols = null;
        public static IList AllSymbols 
        { 
            get 
            {
                // Build the allSymbols list if necessary
                if (allSymbols == null)
                {
                    lock (symbolTable.SyncRoot)
                    {
                        if (allSymbols == null)
                        {
                            allSymbols = new ArrayList();

                            foreach (object sym in symbols)
                            {
                                if (sym is string)
                                {
                                    allSymbols.Add(new Symbol((string)sym));
                                }
                                else if (sym is ISymbolic)
                                {
                                    allSymbols.Add(sym);
                                }
                            }
                        }
                    }
                }

                // Return the list of all symbols being managed by this table
                return allSymbols; 
            } 
        }

        public delegate void NewSymbolEvent(ISymbolic newSymbol, int number);
        public static event NewSymbolEvent NewSymbol;

        public static void OnNewSymbol(ISymbolic newSymbol, int number)
        {
            if (NewSymbol != null) NewSymbol(newSymbol, number);
        }

		#endregion
	}
}
