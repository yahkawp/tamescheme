// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Class that renames bound variables in macros                     Binder.cs |
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

using Tame.Scheme.Data;

namespace Tame.Scheme.Syntax.Transformer
{
	// TODO: investigate binding in (let ((x 1)) (let ((x 2)) x))
	// I have a doubt as to whether or not behaviour will always be correct in instances like this

	/// <summary>
	/// The job of the binder is to take syntax transformed through a Transformation and find any elements that were inserted literally and
	/// are in a binding context and rename them. This satisfies the R5RS conditions for hygenic macros.
	/// </summary>
	/// <remarks>
	/// Syntax classes that introduce bindings can use the IBinding interface.
	/// 
	/// The Binder has the additional job of assigning temporary variable names. This means you need to persist it while completing the expansion
	/// of syntax, but you can discard it afterwards to allow temporary names to be reused.
	/// </remarks>
	public class Binder
	{
		protected Binder() { }

        private static Binder sharedBinder = new Binder();
        public static Binder SharedBinder { get { return sharedBinder; } }

		int temporarySymbol = -1;
		protected Symbol NewTemporarySymbol()
		{
            lock (this)
            {
                return new Symbol(temporarySymbol--);
            }
		}

		/// <summary>
		/// Class used to represent the current state of a binding operation
		/// </summary>
		public class BindingState
		{
			internal BindingState(Binder owner, Runtime.CompileState compileState)
			{
				this.owner = owner;
				this.compileState = compileState;
			}
			internal BindingState(BindingState previousState)
			{
				this.previousState = previousState;
				this.owner = previousState.owner;
				this.compileState = previousState.compileState;
			}

			internal Hashtable reboundSymbols = new Hashtable();					// Maps symbols to symbols
			internal BindingState previousState = null;								// The 'previous' state in the heirarchy of states
			internal Binder owner = null;											// The 'owner' (used to assign temporary variable names)
			internal Runtime.CompileState compileState = null;						// The current compilation state

			/// <summary>
			/// Requests that a symbol be bound to another within this binding context
			/// </summary>
			public void BindSymbol(object symbol, object newSymbol)
			{
				reboundSymbols[symbol] = newSymbol;
			}

			/// <summary>
			/// Requests that a symbol be bound to another outside of this binding context
			/// </summary>
			/// <remarks>
			/// This may be used for things like (define x 2) which can introduce a symbol into the 'parent' environment, rather than creating
			/// a local environment.
			/// </remarks>
			public void BindExternalSymbol(object symbol, object newSymbol)
			{
				previousState.BindSymbol(symbol, newSymbol);
			}

            /// <summary>
            /// The current CompileState for the binding this state represents.
            /// </summary>
            public Runtime.CompileState CompileState { get { return compileState; } }

			/// <summary>
			/// Retrieves the binding for a specific symbol
			/// </summary>
			public object SymbolBinding(object symbol)
			{
				if (reboundSymbols.Contains(symbol))
				{
					return reboundSymbols[symbol];
				}
				else if (previousState != null)
				{
					return previousState.SymbolBinding(symbol);
				}
				else
				{
					// Literal symbols with no match in their environment are rebound back to being normal symbols
					if (symbol is ISymbolic)
					{
						ISymbolic litSym = (ISymbolic)symbol;

						if (litSym.Location != null && !litSym.Location.Contains(litSym.Symbol)) return litSym.Symbol;
					}

					// Other literal symbols and everything else is passed through intact
					return symbol;
				}
			}

			/// <summary>
			/// Constructs a new temporary symbol
			/// </summary>
			/// <returns>A previously unused temporary symbol</returns>
			public Symbol TemporarySymbol()
			{
				return owner.NewTemporarySymbol();
			}

			/// <summary>
			/// Invokes binding on the given scheme.
			/// </summary>
			/// <param name="scheme">The scheme whose bindings need to be updated</param>
			/// <returns>The same scheme with free symbols changed to temporaries</returns>
			public object Bind(object scheme)
			{
				return owner.BindScheme(scheme, compileState, this);
			}
		}

		/// <summary>
		/// Takes a scheme expression, usually produced by a syntax Transformation, finds any expressions that bind LiteralSymbols and replaces them
		/// with temporary symbols.
		/// </summary>
		/// <param name="scheme">The scheme expression to change the bindings on</param>
		/// <param name="topLevel">The top-level environment this will be evaluated in</param>
		/// <returns>A rewritten scheme expression</returns>
		public object BindScheme(object scheme, Runtime.CompileState compileState)
		{
			return BindScheme(scheme, compileState, new BindingState(this, compileState));
		}

		private object BindScheme(object scheme, Runtime.CompileState compileState, BindingState state)
		{
			if (scheme is Pair)
			{
				// If we have a pair, there may be binding behaviour to consider: look at the first element to find out
				Pair schemePair = (Pair)scheme;
				object firstElement = schemePair.Car;
				object firstSymbolValue = null;

				if (firstElement is Symbol)
				{
					// Look up the symbol value in the top-level environment (we're only interested in syntax at this point)
					if (compileState.TopLevel.Contains((Symbol)firstElement))
						firstSymbolValue = compileState.TopLevel[(Symbol)firstElement];
				}
				else if (firstElement is ISymbolic)
				{
					// Other symbolic values: also look up in the top-level environment
                    ISymbolic iSym = (ISymbolic)firstElement;
                    Data.Environment symbolEnv = iSym.Location;
                    if (symbolEnv == null) symbolEnv = compileState.TopLevel;

					if (symbolEnv.Contains(iSym))
						firstSymbolValue = symbolEnv[iSym.Symbol];
				}

				if (firstSymbolValue != null && firstSymbolValue is SchemeSyntax)
				{
					// Get the implementation
					ISyntax syntaxImplementation = ((SchemeSyntax)firstSymbolValue).Implementation;

					// Try to match (even without binding, this should be valid scheme)
					SyntaxEnvironment matchEnvironment = null;
					int syntaxMatch = -1;

					if (syntaxImplementation is IBinding || syntaxImplementation is IQuoted)
					{
						// We only actually use the match if this is a binding or quoted syntax
						syntaxMatch = ((SchemeSyntax)firstSymbolValue).Syntax.Match(schemePair.Cdr, compileState, out matchEnvironment);

						// Do nothing if there's no match
						if (syntaxMatch < 0) return scheme;
					}

					// The first symbol represents some syntax: we may have to rename bound variables, or deal with quoted symbols
					BindingState outerState = state;								// The state in which the symbol representing the syntax is bound

					// Quote anything that needs quoting
					if (syntaxImplementation is IQuoted) 
					{
						scheme = ((IQuoted)syntaxImplementation).QuoteScheme(schemePair, matchEnvironment, state);
						schemePair = (Data.Pair) scheme;
					}

					// If this is binding syntax, handle that
					if (syntaxImplementation is IBinding)
					{
						// This syntax has its very own binding state
						state = new BindingState(state);

						// ... which it is given an opportunity to modify
						return new Pair(schemePair.Car, ((IBinding)syntaxImplementation).BindScheme(schemePair.Cdr, matchEnvironment, syntaxMatch, state));
					}
					else
					{
						// Quoting can now proceed much as for functions (except the first element is bound in the 'old' state)
						// TODO: these could be treated exactly the same, in fact
						Pair newList = null;											// First element in the new list
						Pair newListEnd = null;											// Last element in the new list

						Pair listElement = schemePair;

						while (listElement != null)
						{
							// Construct a new element with suitable binding
							Pair thisNewElement = null;

							// Append to the list (importantly note that the first element, representing the syntax, is bound to the 'outer' state)
							if (newList == null)
							{
								thisNewElement = new Pair(BindScheme(listElement.Car, compileState, outerState), null);

								newListEnd = newList = thisNewElement;
							}
							else
							{
								thisNewElement = new Pair(BindScheme(listElement.Car, compileState, state), null);

								newListEnd.Cdr = thisNewElement;
								newListEnd = thisNewElement;
							}

							// Move to the next element
							if (listElement.Cdr == null || listElement.Cdr is Pair)
							{
								listElement = (Pair)listElement.Cdr;
							}
							else
							{
								// This list is improper: bind the final element
								newListEnd.Cdr = BindScheme(listElement.Cdr, compileState, state);

								listElement = null;
							}
						}

						// The result is the list we just constructed
						return newList;
					}
				}
				else
				{
					// The first symbol does not represent syntax: we assume this will act as a function call: just bind each of the parameters to create a new list
					Pair newList = null;											// First element in the new list
					Pair newListEnd = null;											// Last element in the new list

					Pair listElement = schemePair;

					while (listElement != null)
					{
						// Construct a new element with suitable binding
						Pair thisNewElement = new Pair(BindScheme(listElement.Car, compileState, state), null);

						// Append to the list
						if (newList == null)
						{
							newListEnd = newList = thisNewElement;
						}
						else
						{
							newListEnd.Cdr = thisNewElement;
							newListEnd = thisNewElement;
						}

						// Move to the next element
						if (listElement.Cdr == null || listElement.Cdr is Pair)
						{
							listElement = (Pair)listElement.Cdr;
						}
						else
						{
							// This list is improper: bind the final element
							newListEnd.Cdr = BindScheme(listElement.Cdr, compileState, state);

							listElement = null;
						}
					}

					// The result is the list we just constructed
					return newList;
				}
			}
			else if (scheme is Symbol || scheme is ISymbolic)
			{
				return state.SymbolBinding(scheme);
			}
			else if (scheme is ICollection)
			{
				// Remove any ISymbolics from vectors
				object[] newVector = new object[((ICollection)scheme).Count];
				int offset = 0;

				foreach (object item in (ICollection)scheme)
				{
					if (item is ISymbolic)
					{
						newVector[offset++] = ((ISymbolic)item).Symbol;
					}
					else
					{
						newVector[offset++] = item;
					}
				}

				return newVector;
			}
			else
			{
				// We can pass everything else through unaltered
				return scheme;
			}
		}
	}
}
