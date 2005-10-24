// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The results of matching some syntax                   SyntaxEnvironment.cs |
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

namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// A SyntaxEnvironment contains the results of matching against a SyntaxElement object (and any 'child' objects that it may have had)
	/// </summary>
	public class SyntaxEnvironment
	{
		public SyntaxEnvironment()
		{
			ClearMatches();
		}

		#region Data

		SyntaxNode syntaxTree = null;
		SyntaxNode currentNode = null;

		#endregion

		#region Building the syntax tree

		public void ClearMatches()
		{
			syntaxTree = new SyntaxNode();
			currentNode = syntaxTree;
		}

		/// <summary>
		/// Called when a matcher matches the start of a list (or vector - these are not treated differently for the purposes of a syntax tree)
		/// </summary>
		public void StartList()
		{
			SyntaxNode listNode = new SyntaxNode();

			currentNode.AddChild(listNode);
			currentNode = listNode;
		}

		/// <summary>
		/// Called when starting to match values against a symbol (for ellipsises, the symbols can have no values)
		/// </summary>
		/// <param name="symbol">The symbol to add</param>
		public void StartSymbol(Data.Symbol symbol)
		{
			SyntaxNode symbolNode = new SyntaxNode(symbol);

			currentNode.AddChild(symbolNode);
			currentNode = symbolNode;
		}

		/// <summary>
		/// Adds a value node to the current symbol node
		/// </summary>
		/// <param name="value">The value to add</param>
		/// <remarks>You MUST only call this after a symbol has been started</remarks>
		public void AddValue(object value)
		{
			currentNode.AddChild(new SyntaxNode(value));
		}

		/// <summary>
		/// Finishes the current symbol, started with AddSymbol
		/// </summary>
		public void FinishSymbol()
		{
			// Sanity check
			if (currentNode.Parent == null) throw new NotSupportedException("SyntaxEnvironment.FinishSymbol called for the root syntax node");
			if (!currentNode.IsSymbol) throw new NotSupportedException("SyntaxEnvironment.FinishSymbol called with no matching StartSymbol");

			currentNode = currentNode.Parent;
		}

		/// <summary>
		/// Finishes the current list/vector structure
		/// </summary>
		public void FinishList()
		{
			// Sanity check
			if (currentNode.Parent == null) throw new NotSupportedException("SyntaxEnvironment.FinishList called for the root syntax node");
			if (!currentNode.IsList) throw new NotSupportedException("SyntaxEnvironment.FinishList called with no matching StartList");

			currentNode = currentNode.Parent;
		}

		#endregion

		#region Retrieving syntax

		/// <summary>
		/// Returns true if this environment contains the given symbol number
		/// </summary>
		public bool Contains(int symbolNumber)
		{
			return Contains(new Data.Symbol(symbolNumber));
		}

		/// <summary>
		/// Returns true if this environment contains the given symbol
		/// </summary>
		public bool Contains(string symbolName)
		{
			return Contains(new Data.Symbol(symbolName));
		}

		/// <summary>
		/// Returns true if this environment contains the given symbol
		/// </summary>
		public bool Contains(Data.Symbol symbol)
		{
			return syntaxTree.ChildWithSymbol(symbol) != null;
		}

		/// <summary>
		/// Gets the leftmost node in the syntax tree that matches the given symbol
		/// </summary>
		public SyntaxNode this[string symbol]
		{
			get { return this[new Data.Symbol(symbol)]; }
		}

		/// <summary>
		/// Gets the leftmost node in the syntax tree that matches the given symbol
		/// </summary>
		public SyntaxNode this[int symbolNumber]
		{
			get { return this[new Data.Symbol(symbolNumber)]; }
		}

		/// <summary>
		/// Gets the leftmost node in the syntax tree that matches the given symbol
		/// </summary>
		public SyntaxNode this[Data.Symbol symbol]
		{
			get
			{
				SyntaxNode symbolNode = syntaxTree.GrandchildWithSymbol(symbol);
				return symbolNode;
			}
		}

		public SyntaxNode SyntaxTree
		{
			get { return syntaxTree; }
		}

		#endregion

		// Original implementation: this really doesn't work for R5RS matching (well, it works for the matching, but we can't adequately retrieve things)
#if false
		#region Data

		HybridDictionary elements = new HybridDictionary();					// Maps syntax elements to object arrays
		HybridDictionary symbols = new HybridDictionary();					// Maps specifically bound symbols to object arrays
		HybridDictionary ellipsisSymbols = new HybridDictionary();			// Symbols => dictionary of EllipsisDepths => values
		HybridDictionary symbolDepth = new HybridDictionary();				// Symbols => preferred ellipsis depth (ie, in ((a ...) ...) the preferred depth is 2

		#endregion

		#region Ellipsis recursion

		/// <summary>
		/// Given syntax like ((a ...) ...) we have two ellipsises that need to be dealt with: an outer and an inner value. Deeper recursions
		/// are possible as well. We need a way to express the first a in the first list, the third a in the second list, etc. This class
		/// provides that means.
		/// </summary>
		/// <remarks>
		/// What if you have ((a ...) ...) and you ask just for the 'first a'? Scheme requires that we implicitly use the first list
		/// in this case.
		/// </remarks>
		public class EllipsisDepth
		{
			/// <summary>
			/// Constructs a new EllipsisDepth object at the top level
			/// </summary>
			public EllipsisDepth()
			{
				ellipsises = new ArrayList();
			}

			/// <summary>
			/// Constructs an EllipsisDepth object
			/// </summary>
			/// <param name="toClone"></param>
			public EllipsisDepth(EllipsisDepth toClone)
			{
				ellipsises = (ArrayList)toClone.ellipsises.Clone();
			}

			ArrayList ellipsises;							// The 'ellipsis repeat count', from outermost to innermost

			/// <summary>
			/// Indicates the starts of a (potentially) repeated block
			/// </summary>
			public void StartToRepeat()
			{
				ellipsises.Add(0);
			}

			/// <summary>
			/// Indicates we've moved to the next element in the current block
			/// </summary>
			public void NextElement()
			{
				int len = ellipsises.Count-1;
				int lastCount = (int)ellipsises[len];

				ellipsises[len] = lastCount+1;
			}

			/// <summary>
			/// Indicates that we've finished with a repeated block
			/// </summary>
			public void FinishRepeating()
			{
				ellipsises.RemoveAt(ellipsises.Count-1);
			}

			/// <summary>
			/// Retrieves the current depth of ellipsises in this object
			/// </summary>
			public int Depth
			{
				get
				{
					return ellipsises.Count;
				}
			}

			#region Hashing

			public override bool Equals(object obj)
			{
				if (!(obj is EllipsisDepth)) return false;

				EllipsisDepth toCompare = (EllipsisDepth)obj;

				// Short-circuit: return false if the depths are different
				if (toCompare.Depth != Depth) return false;

				// Compare each item in turn
				IEnumerator enum1 = ellipsises.GetEnumerator();
				IEnumerator enum2 = toCompare.ellipsises.GetEnumerator();

				while (enum1.MoveNext() && enum2.MoveNext())
				{
					if ((int)enum1.Current != (int)enum2.Current) return false;
				}

				return true;

				// Equals seems not to compare the objects that are contained by the ArrayList
				//bool res = toCompare.ellipsises.Equals(ellipsises);
				//return res;
			}

			static private int myTypeHash = typeof(EllipsisDepth).GetHashCode();
			public override int GetHashCode()
			{
				int hashCode = myTypeHash^ellipsises.Count.GetHashCode();
				int pos = 0;

				foreach (int count in ellipsises) 
				{
					hashCode ^= count.GetHashCode();
					hashCode ^= pos.GetHashCode();
					pos++;
				}

				return hashCode;

				//ArrayList HashCodes seem to be different for objects that contain the same items?
				//return ellipsises.GetHashCode()^myTypeHash;
			}

			#endregion
		}

		#endregion

		#region Consuming objects (building syntax)

		/// <summary>
		/// Stores an object as matching a given SyntaxElement
		/// </summary>
		/// <param name="element">The element that's been successfully matched against</param>
		/// <param name="matches">The value to store with it</param>
		/// <param name="ellipsisDepth">The 'depth' in any containing ellipsises</param>
		/// <remarks>Some elements are matched against multiple times (those with ellipsises): each object is stored in these cases</remarks>
		public void AddObject(SyntaxElement element, object matches, EllipsisDepth ellipsisDepth)
		{
			// Store the match for this element
			// TODO: this was based on my previous understanding of how scheme deals with ellipsises, which appears to have been mistaken. This array should go away: note that this requires a rewrite of the let syntax object.
			ArrayList elementMatches = elements.Contains(element)?(ArrayList)elements[element]:new ArrayList();
			elementMatches.Add(matches);
			elements[element] = elementMatches;

			// Bound symbols are a special case that we store additionally in the symbols array
			if (element.type == SyntaxElement.ElementType.BoundSymbol)
			{
				symbols[((Data.Symbol)element.element).SymbolNumber] = elementMatches;

				// We also store them so we can retrieve them via an ellipsis stack (iff the stack has any depth)
				if (ellipsisDepth.Depth > 0) 
				{
					HybridDictionary depthDictionary;

					if (ellipsisSymbols.Contains(((Data.Symbol)element.element).SymbolNumber))
					{
						depthDictionary = (HybridDictionary)ellipsisSymbols[((Data.Symbol)element.element).SymbolNumber];
					}
					else
					{
						depthDictionary = new HybridDictionary();
						ellipsisSymbols[((Data.Symbol)element.element).SymbolNumber] = depthDictionary;
					}

					// Store with reference to this particular ellipsis depth
					depthDictionary[new EllipsisDepth(ellipsisDepth)] = matches;
					symbolDepth[((Data.Symbol)element.element).SymbolNumber] = ellipsisDepth.Depth;
				}
			}
		}

		#endregion

		#region Retrieving syntax

		/// <summary>
		/// Retrieves the matches against a specific symbol number
		/// </summary>
		/// <remarks>Treat the returned IList as immutable (.NET does not have a standard immutable indexed list, so ILists are used here for convienience)</remarks>
		public IList this[int symbolNumber]
		{
			get
			{
				return (IList)symbols[symbolNumber];
			}
		}

		/// <summary>
		/// Retrieves the matches against a symbol with the specified name
		/// </summary>
		/// <remarks>Treat the returned IList as immutable (.NET does not have a standard immutable indexed list, so ILists are used here for convienience)</remarks>
		public IList this[string symbolName]
		{
			get
			{
				return this[Data.SymbolTable.NumberForSymbol(symbolName)];
			}
		}

		/// <summary>
		/// Retrieves the matches against a specific symbol
		/// </summary>
		/// <remarks>Treat the returned IList as immutable (.NET does not have a standard immutable indexed list, so ILists are used here for convienience)</remarks>
		public IList this[Data.Symbol symbol]
		{
			get
			{
				return this[symbol.SymbolNumber];
			}
		}

		/// <summary>
		/// Retrieves the matches against a specific syntax element
		/// </summary>
		/// <remarks>Treat the returned IList as immutable (.NET does not have a standard immutable indexed list, so ILists are used here for convienience)</remarks>
		public IList this[SyntaxElement element]
		{
			get
			{
				return (IList)elements[element];
			}
		}

		/// <summary>
		/// Retrieves the object that matched a specific symbol, repeated a specific number of times
		/// </summary>
		/// <param name="symbolNumber">The symbol number we matched against</param>
		/// <param name="depth">The (recursive) depth of the symbol when repeats are in use</param>
		/// <returns>The symbol that was matched against</returns>
		/// <exception cref="IndexOutOfRangeException">Thrown if the symbol does not exist at the specified depth</exception>
		public object GetRepeatedMatch(int symbolNumber, EllipsisDepth depth)
		{
			// Symbols exist at a single 'preferred' depth (as scheme requires that symbols can't repeat): we can only match at that depth
			EllipsisDepth realDepth = new EllipsisDepth(depth);
			MakePreferredDepth(realDepth, symbolNumber);

			// Depths of 0 means that there was no repetition
			if (realDepth.Depth == 0) return this[symbolNumber][0];

			if (!ellipsisSymbols.Contains(symbolNumber)) throw new IndexOutOfRangeException("Attempted to access a syntax symbol that was not matched against");

			// Get the dictionary containing all the matches for this symbol
			HybridDictionary symbolMatches = (HybridDictionary)ellipsisSymbols[symbolNumber];

			if (!symbolMatches.Contains(realDepth)) throw new IndexOutOfRangeException("Attempted to access a symbol that was matched against, but was not matched with the given repetition count");

			// Return the result
			return symbolMatches[realDepth];
		}

		/// <summary>
		/// Function to convert an EllipsisDepth to the one preferred for a given symbol
		/// </summary>
		/// <param name="depth">The EllipsisDepth object to convert</param>
		/// <param name="forSymbol">The symbol number to convert for</param>
		public void MakePreferredDepth(EllipsisDepth depth, int forSymbol)
		{
			int preferredDepth = 0;

			// Work out the preferred depth for this symbol
			if (symbolDepth.Contains(forSymbol))
			{
				preferredDepth = (int)symbolDepth[forSymbol];
			}

			// Remove/add layers until we reach that depth
			if (preferredDepth == depth.Depth) return;

			while (preferredDepth > depth.Depth)
			{
				// Increase the depth (will use the first symbol at that depth)
				depth.StartToRepeat();
			}

			while (preferredDepth < depth.Depth)
			{
				// Decrease the depth
				depth.FinishRepeating();
			}
		}

		/// <summary>
		/// Returns true if this environment contains the given symbol number
		/// </summary>
		public bool Contains(int symbolNumber)
		{
			return symbols.Contains(symbolNumber);
		}

		public bool Contains(string symbolName)
		{
			return Contains(Data.SymbolTable.NumberForSymbol(symbolName));
		}

		public bool Contains(Data.Symbol symbol)
		{
			return Contains(symbol.SymbolNumber);
		}

		public bool Contains(SyntaxElement element)
		{
			return elements.Contains(element);
		}

		public bool Contains(int symbolNumber, EllipsisDepth depth)
		{
			// If the depth is 0, then this symbol will only be in the symbols dictionary
			if (depth.Depth == 0) return Contains(symbolNumber);
			
			// A given symbol has a 'preferred' depth: the depth at which it appears. Repair the depth object to have that depth
			EllipsisDepth realDepth = new EllipsisDepth(depth);
			MakePreferredDepth(realDepth, symbolNumber);

			// It's possible the preferred depth is 0
			if (realDepth.Depth == 0) return Contains(symbolNumber);

			// Return false if this symbol is not in the list of ellipsisSymbols
			if (!ellipsisSymbols.Contains(symbolNumber)) return false;

			// See if there's an object for this symbol at this depth
			return ((HybridDictionary)ellipsisSymbols[symbolNumber]).Contains(realDepth);
		}

		#endregion

		public override string ToString()
		{
			string res = "";

			foreach (SyntaxElement elem in elements.Keys)
			{
				res += " (" + elem.ToString() + " => " + Runtime.Interpreter.ToString(elements[elem]) + ")";
			}

			return "{" + res + " }";
		}
#endif
	}
}
