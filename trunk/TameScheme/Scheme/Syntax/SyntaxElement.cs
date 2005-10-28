// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | A single possible line of syntax                          SyntaxElement.cs |
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
	/// A single element of scheme syntax. Can be a bound item, a literal symbol, a literal item, a list or a vector. Lists may have an ellipsis.
	/// </summary>
	public sealed class SyntaxElement
	{
		#region Constructors

		/// <summary>
		/// Constructs a syntax element that matches against a symbol
		/// </summary>
		/// <param name="theSymbol">The symbol to match against</param>
		/// <param name="isLiteral">If true, the symbol is matched literally, otherwise it indicates a bound value in the syntax environment</param>
		public SyntaxElement(Data.ISymbolic theSymbol, bool isLiteral)
		{
			element = theSymbol;
			if (isLiteral)
			{
				type = ElementType.Literal;
			}
			else
			{
				type = ElementType.BoundSymbol;
			}
		}

		/// <summary>
		/// Constructs a syntax element that matches against a list or a vector
		/// </summary>
		/// <param name="items">The items to match against (more SyntaxElements)</param>
		/// <param name="isVector">If true, this matches against a vector, otherwise a list</param>
		/// <param name="isEllipsis">If true, the last element is repeated 0 or more times</param>
		/// <exception cref="NotSupportedException">One or more of the items already has a 'parent' syntax item</exception>
		public SyntaxElement(ICollection items, bool isVector, bool isEllipsis)
		{
			element = items;

			// Set the parents of each item
			int offset = 0;
			foreach (SyntaxElement elem in items)
			{
				if (elem.parent != null && elem.parent != this) throw new NotSupportedException("Attempt to construct a list/vector SyntaxElement using SyntaxElements that are already part of another list");

				elem.parent = this;
				elem.offset = offset++;
			}

			// Set the type of this item
			if (isVector)
			{
				if (isEllipsis)
				{
					type = ElementType.EllipsisVector;
				}
				else
				{
					type = ElementType.Vector;
				}
			}
			else
			{
				if (isEllipsis)
				{
					type = ElementType.EllipsisList;
				}
				else
				{
					type = ElementType.List;
				}
			}
		}

		/// <summary>
		/// Constructs a syntax element that matches against an improper list
		/// </summary>
		/// <param name="items">The items to match against (more SyntaxElements)</param>
		/// <param name="isImproper">If true, the list of items should be treated as improper, otherwise a proper list</param>
		/// <exception cref="NotSupportedException">One or more of the items already has a 'parent' syntax item</exception>
		public SyntaxElement(ICollection items, bool isImproper)
		{
			if (isImproper)
			{
				type = ElementType.ImproperList;
				element = items;
			}
			else
			{
				type = ElementType.List;
				element = items;
			}

			// Set the parents of each item
			int offset = 0;
			foreach (SyntaxElement elem in items)
			{
				if (elem.parent != null && elem.parent != this) throw new NotSupportedException("Attempt to construct a list SyntaxElement using SyntaxElements that are already part of another list");

				elem.parent = this;
				elem.offset = offset++;
			}
		}

		/// <summary>
		/// Constructs a syntax element that matches against a literal object
		/// </summary>
		/// <param name="obj">The object to match against</param>
		public SyntaxElement(object obj)
		{
			if (obj == null)
			{
				// (Have to deal with this seperately)
				type = ElementType.EmptyList;
			}
			else
			{
				type = ElementType.Literal;
			}
			element = obj;
		}

		#endregion

		#region Data

		public enum ElementType
		{
			EmptyList, Literal, BoundSymbol, List, ImproperList, EllipsisList, Vector, EllipsisVector
		}

		private ElementType type;										// The type of this element
		private object element;											// The object that this element refers to

		private SyntaxElement parent = null;							// The 'parent' syntax element for this element
		private int offset = 0;											// The offset within the parent element

		static Data.Symbol ellipsis = new Data.Symbol("...");			// The ellipsis symbol is treated as special

		private Hashtable boundSymbols = null;							// Quick lookup table used while building syntax transformers

		#endregion

		#region Getting information about this element

		public ElementType Type
		{
			get { return type; }
		}

		/// <summary>
		/// The contents of the element, if it represents a list or vector
		/// </summary>
		public ICollection ListOrVectorContents
		{
			get { return (ICollection)element; }
		}

		/// <summary>
		/// The symbol that this element represents (literal or bound)
		/// </summary>
		public Data.ISymbolic SymbolValue
		{
			get { return (Data.ISymbolic)element; }
		}

		/// <summary>
		/// The literal object value of this symbol
		/// </summary>
		public object LiteralValue
		{
			get { return element; }
		}

		/// <summary>
		/// The 'parent' element that contains this one
		/// </summary>
		public SyntaxElement Parent
		{
			get { return parent; }
		}

		/// <summary>
		/// The 'offset' within the collection representing the parent element
		/// </summary>
		public int Offset
		{
			get { return offset; }
		}

		/// <summary>
		/// Finds the common element that is the parent to this one and the specified element (the elements should be part of the same tree)
		/// </summary>
		/// <param name="elem">The element to find the common parent with respect to this element</param>
		/// <returns>The SyntaxElement that is the common parent of this element and the specified element</returns>
		/// <exception cref="NotSupportedException">If the elements have no common parent</exception>
		/// <exception cref="NullReferenceException">If elem is null</exception>
		public SyntaxElement CommonParent(SyntaxElement elem)
		{
			if (elem == null) 
				throw new NullReferenceException("CommonParent() called with a null element");

			// Trivial case
			if (elem == this) return this;

			// Build up a list of the parents of each element
			Stack ourParents = new Stack();
			Stack theirParents = new Stack();

			// Build up the list of parents for this element
			SyntaxElement curParent = this;
			while (curParent != null)
			{
				ourParents.Push(curParent);
				curParent = curParent.parent;
			}

			// Build up the list of parents for the other element
			curParent = elem;
			while (curParent != null)
			{
				theirParents.Push(curParent);
				curParent = curParent.parent;
			}

			// Both elements must share a common ancestor, or this call is invalid
			if (ourParents.Peek() != theirParents.Peek())
				throw new NotSupportedException("CommonParent() called on two SyntaxElements that are not part of the same tree");

			// Pop elements until we find the common parent
			object commonParent = ourParents.Pop();
			theirParents.Pop();

			while (ourParents.Count > 0 && theirParents.Count > 0 && ourParents.Peek() == theirParents.Peek())
			{
				commonParent = ourParents.Pop();
				theirParents.Pop();
			}

			// Return the result
			return (SyntaxElement)commonParent;
		}

		private void FillBoundSymbols(Hashtable boundTable)
		{
			switch (type)
			{
				case ElementType.BoundSymbol:
					// This is a bound symbol: add to the hash table
					boundTable.Add(((Data.ISymbolic)element).HashValue, this);
					break;

				case ElementType.ImproperList:
				case ElementType.EllipsisList:
				case ElementType.List:
				case ElementType.EllipsisVector:
				case ElementType.Vector:
					// Fill out each of the elements that make up the list/vector
					foreach (SyntaxElement elem in (ICollection)element)
					{
						elem.FillBoundSymbols(boundTable);
					}
					break;

				default:
					// Nothing to do
					break;
			}
		}

		/// <summary>
		/// Returns true if the pattern specified by this SyntaxElement binds the given symbol
		/// </summary>
		public bool ContainsBoundSymbol(Data.ISymbolic symbol)
		{
			if (boundSymbols == null)
			{
				// Build the table containing the pattern variables bound by this expression
				boundSymbols = new Hashtable();
				FillBoundSymbols(boundSymbols);
			}

			// Check to see if the supplied symbol is a bound one
			return boundSymbols.Contains(symbol.HashValue);
		}

		/// <summary>
		/// Returns true if the pattern specified by this SyntaxElement binds the symbol with the given number
		/// </summary>
		public bool ContainsBoundSymbol(int symbolNumber)
		{
			return ContainsBoundSymbol(new Data.Symbol(symbolNumber));
		}

		/// <summary>
		/// Returns true if the pattern specified by this SyntaxElement binds the symbol with the given name
		/// </summary>
		public bool ContainsBoundSymbol(string symbolName)
		{
			return ContainsBoundSymbol(new Data.Symbol(symbolName));
		}

		/// <summary>
		/// Returns the SyntaxElement that is a child of this syntax element and which binds the given symbol (or null)
		/// </summary>
		public SyntaxElement BindingForSymbol(Data.ISymbolic symbol)
		{
			if (boundSymbols == null)
			{
				// Build the table containing the pattern variables bound by this expression
				boundSymbols = new Hashtable();
				FillBoundSymbols(boundSymbols);
			}

			// Find the element that binds this symbol in the hash table
			return (SyntaxElement)boundSymbols[symbol.HashValue];
		}

		#endregion

		#region Matching

		private bool Match(object matchAgainst, SyntaxEnvironment boundEnvironment, bool isEllipsis)
		{
			Data.Pair list;
			int itemCount;
			IEnumerator itemEnum;
			SyntaxElement ellipsisItem;
			object currentMatch = matchAgainst;

			// Begin matching
			switch (type)
			{
				case ElementType.Literal:
					// (We don't bother storing these in the syntax environment - not sure that collecting, say, a bunch of 'true' values is particularly useful)
					if (!element.Equals(matchAgainst))
					{
						// An exception: ISymbolic entries can also match an identical Symbol
						if (element is Data.ISymbolic)
						{
							return ((Data.ISymbolic)element).Symbol.Equals(matchAgainst);
						}
						else
						{
							return false;
						}
					}
					else
					{
						return true;
					}

				case ElementType.BoundSymbol:
					boundEnvironment.StartSymbol((Data.ISymbolic)element);
					boundEnvironment.AddValue(matchAgainst);
					boundEnvironment.FinishSymbol();
					return true;

				case ElementType.List:
					boundEnvironment.StartList();
					foreach (SyntaxElement item in (ICollection)element)
					{
						// Check that the next element is a Pair
						if (currentMatch == null) return false;
						if (!(currentMatch is Data.Pair)) return false;
						list = (Data.Pair)currentMatch;

						// Match this element against this item
						if (!item.Match(list.Car, boundEnvironment, false)) return false;

						// Move to the next element
						currentMatch = list.Cdr;
					}

					// Make sure there are no trailing items
					if (currentMatch != null) return false;

					// Success
					boundEnvironment.FinishList();
					return true;

				case ElementType.ImproperList:
					boundEnvironment.StartList();

					// Match as for a list against all except the last element
					itemCount = ((ICollection)element).Count;
					itemEnum = ((ICollection)element).GetEnumerator();
					for (int x=0; x<itemCount-1; x++)
					{
						itemEnum.MoveNext();
						SyntaxElement item = (SyntaxElement)itemEnum.Current;

						// Check that the next element is a Pair
						if (currentMatch == null) return false;
						if (!(currentMatch is Data.Pair)) return false;
						list = (Data.Pair)currentMatch;

						// Match this element against this item
						if (!item.Match(list.Car, boundEnvironment, false)) return false;

						// Move to the next element
						currentMatch = list.Cdr;
					}

					// Match the last item
					itemEnum.MoveNext();
					SyntaxElement improperItem = (SyntaxElement)itemEnum.Current;
					if (!improperItem.Match(currentMatch, boundEnvironment, false)) return false;

					// Success
					boundEnvironment.FinishList();
					return true;

				case ElementType.EllipsisList:
					// Match as for a list against all except the last element
					boundEnvironment.StartList();

					itemCount = ((ICollection)element).Count;
					itemEnum = ((ICollection)element).GetEnumerator();
					for (int x=0; x<itemCount-1; x++)
					{
						itemEnum.MoveNext();
						SyntaxElement item = (SyntaxElement)itemEnum.Current;

						// Check that the next element is a Pair
						if (currentMatch == null) return false;
						if (!(currentMatch is Data.Pair)) return false;
						list = (Data.Pair)currentMatch;

						// Match this element against this item
						if (!item.Match(list.Car, boundEnvironment, false)) return false;

						// Move to the next element
						currentMatch = list.Cdr;
					}

					// Match the last item, repeatedly
					itemEnum.MoveNext();
					ellipsisItem = (SyntaxElement)itemEnum.Current;

					while (currentMatch != null)
					{
						// Check that the next element is a Pair
						if (currentMatch == null) return false;
						if (!(currentMatch is Data.Pair)) return false;
						list = (Data.Pair)currentMatch;

						// Match this element against the last item
						if (!ellipsisItem.Match(list.Car, boundEnvironment, true)) return false;

						// Move to the next element
						currentMatch = list.Cdr;
					}

					// Success
					boundEnvironment.FinishList();
					return true;

				case ElementType.Vector:
				case ElementType.EllipsisVector:
					boundEnvironment.StartList();

					itemCount = ((ICollection)element).Count;
					itemEnum = ((ICollection)element).GetEnumerator();

					if (type == ElementType.EllipsisVector) itemCount--;

					// Check types
					if (!(currentMatch is ICollection)) return false;	// Not a vector
					if (!(currentMatch is Data.Pair)) return false;		// Pairs are not vectors

					// Get the items to match enumerator
					IEnumerator matchEnum = ((ICollection)currentMatch).GetEnumerator();

					for (int x=0; x<itemCount; x++)
					{
						// Get the next item
						itemEnum.MoveNext();
						SyntaxElement item = (SyntaxElement)itemEnum.Current;

						if (!matchEnum.MoveNext()) return false;							// Vector is too short

						// Try matching the item
						if (!item.Match(matchEnum.Current, boundEnvironment, false)) return false;
					}

					if (type == ElementType.EllipsisVector)
					{
						// If there's an ellipsis, repeatedly match against the last element
						itemEnum.MoveNext();
						ellipsisItem = (SyntaxElement)itemEnum.Current;

						while (matchEnum.MoveNext())
						{
							// Try matching this item
							if (!ellipsisItem.Match(matchEnum.Current, boundEnvironment, true)) return false;
						}
					}
					else
					{
						// Fail if there are more items in the vector
						if (matchEnum.MoveNext()) return false;
					}

					// Success: store in the environment
					boundEnvironment.FinishList();
					return true;

				case ElementType.EmptyList:
					return matchAgainst==null;
			}

			return false;
		}

		/// <summary>
		/// Matches this SyntaxElement against a .NET object
		/// </summary>
		/// <param name="matchAgainst">The object to match against</param>
		/// <param name="boundEnvironment">
		/// 'Bound' values are defined into this environment. 'Ellipsis' values are defined as ICollection objects, the rest are
		/// just defined as objects. This environment is cleared before matching, and may contain garbage afterwards if the match was
		/// unsuccessful.
		/// </param>
		/// <returns>true if the match is successful, false otherwise</returns>
		public bool Match(object matchAgainst, out SyntaxEnvironment boundEnvironment)
		{
			// Clear the environment
			boundEnvironment = new SyntaxEnvironment();

			return Match(matchAgainst, boundEnvironment, false);
		}

		#endregion

		#region Factory methods

		/// <summary>
		/// Builds a syntax element from a scheme string
		/// </summary>
		/// <param name="literals">The literals in the element</param>
		/// <param name="elementDefinition">The element definition</param>
		/// <returns>A syntax element</returns>
		public static SyntaxElement MakeElementFromScheme(string literals, string elementDefinition)
		{
			Data.Pair literalList = (Data.Pair)Runtime.Parse.Parser.Parse(literals);
			object definition = Runtime.Parse.Parser.Parse(elementDefinition);

			return MakeElementFromScheme(definition, literalList);
		}

		/// <summary>
		/// Builds a SyntaxElement from a scheme expression (expressed as an object)
		/// </summary>
		/// <param name="schemeObject">The object expressing the syntax</param>
		/// <param name="literals">Literals in the syntax expression (a collection of Symbols)</param>
		/// <returns>The syntax element built from the object</returns>
		public static SyntaxElement MakeElementFromScheme(object schemeObject, ICollection literals)
		{
			ListDictionary literalDictionary = new ListDictionary();

			if (literals != null)
			{
				foreach (Data.ISymbolic sym in literals)
				{
					literalDictionary[sym.HashValue] = true;
				}
			}

			return MakeElementFromScheme(schemeObject, literalDictionary);
		}

		/// <summary>
		/// Builds a SyntaxElement from a scheme expression (expressed as an object)
		/// </summary>
		/// <param name="schemeObject">The object expressing the syntax</param>
		/// <param name="literals">Literals in the syntax expression (a dictionary mapping Symbols to booleans)</param>
		/// <returns>The syntax element built from the object</returns>
		public static SyntaxElement MakeElementFromScheme(object schemeObject, IDictionary literals)
		{
			if (schemeObject == null) return new SyntaxElement(null);

			// Create an element depending on the type
			if (schemeObject is Data.ISymbolic)
			{
				// Symbols can be literal or not
				if (literals.Contains(((Data.ISymbolic)schemeObject).HashValue) && ((bool)literals[((Data.ISymbolic)schemeObject).HashValue]) == true)
				{
					return new SyntaxElement((Data.ISymbolic)schemeObject, true);
				}
				else
				{
					return new SyntaxElement((Data.ISymbolic)schemeObject, false);
				}
			}
			else if (schemeObject is Data.Pair)
			{
				// Pairs can be improper or have ellipsises as well as be just plain old lists
				bool isEllipsis = false;
				ArrayList items = new ArrayList();

				while (schemeObject != null && schemeObject is Data.Pair)
				{
					Data.Pair item = (Data.Pair)schemeObject;

					if (ellipsis.Equals(item.Car))
					{
						isEllipsis = true;
						schemeObject = item.Cdr;
						break;
					}

					items.Add(MakeElementFromScheme(item.Car, literals));

					schemeObject = item.Cdr;
				}

				if (schemeObject != null)
				{
					// There must not be anything after an ellipsis
					if (isEllipsis) throw new Exception.SyntaxError("Found items after an ellipsis while defining syntax");

					// This happens when the list is improper
					items.Add(MakeElementFromScheme(schemeObject, literals));

					return new SyntaxElement(items, true);
				}

				return new SyntaxElement(items, false, isEllipsis);
			}
			else if (schemeObject is ICollection)
			{
				// Vectors can have ellipsises as well as just be plain old vectors
				bool isEllipsis = false;
				ArrayList items = new ArrayList();

				foreach (object item in (ICollection)schemeObject)
				{
					if (isEllipsis) throw new Exception.SyntaxError("Found items after an ellipsis while defining syntax");

					if (ellipsis.Equals(item))
					{
						// Flag an ellipsis and continue (so an error gets thrown if there's stuff following it)
						isEllipsis = true;
						continue;
					}

					items.Add(MakeElementFromScheme(item, literals));
				}

				return new SyntaxElement(items, true, isEllipsis);
			}
			else
			{
				// Standard behaviour - literal matching
				return new SyntaxElement(schemeObject);
			}
		}

		#endregion

		#region Object utility functions

		public override bool Equals(object obj)
		{
			// We can only equal other SyntaxElements
			if (!(obj is SyntaxElement)) return false;

			SyntaxElement compareTo = (SyntaxElement)obj;

			// Types must be the same
			if (compareTo.type != type) return false;

			// Compare depending on the type
			switch (type)
			{
				case ElementType.BoundSymbol:
				case ElementType.Literal:
					// Symbols and literals must be exactly equivalent
					return element.Equals(compareTo.element);

				case ElementType.EllipsisList:
				case ElementType.ImproperList:
				case ElementType.List:
				case ElementType.Vector:
					// element is a collection, and everything must be equal to the values in compareTo
					if (((ICollection)element).Count != ((ICollection)compareTo.element).Count) return false;

					IEnumerator ourEnum = ((ICollection)element).GetEnumerator();
					IEnumerator theirEnum = ((ICollection)compareTo.element).GetEnumerator();

					while (ourEnum.MoveNext() && theirEnum.MoveNext())
					{
						if (!ourEnum.Current.Equals(theirEnum.Current)) return false;
					}

					return true;

				case ElementType.EmptyList:
					return true;
			}

			return false;
		}

		bool calculatedHash = false;
		int hash;
		public override int GetHashCode()
		{
			// Use the cached version of the hash if available
			if (calculatedHash) return hash;

			// Otherwise, calculate the hash
			switch (type)
			{
				case ElementType.EllipsisList:
				case ElementType.ImproperList:
				case ElementType.List:
				case ElementType.Vector:
					// element is a collection, so build the hash from the collection
					hash = type.GetHashCode();

					foreach (SyntaxElement elem in (ICollection)element)
					{
						hash ^= elem.GetHashCode();
					}
					break;

				default:
					// Default is just the XOR of the type and the element hash
					hash = type.GetHashCode()^element.GetHashCode();
					break;
			}

			calculatedHash = true;
			return hash;
		}

		public override string ToString()
		{
			string res = "";

			switch (type)
			{
				case ElementType.EllipsisList:
				case ElementType.EllipsisVector:
				case ElementType.Vector:
				case ElementType.List:
				case ElementType.ImproperList:
					// Build the list of elements
					IEnumerator elemEnum = ((ICollection)element).GetEnumerator();
					string lastElement = null;

					while (elemEnum.MoveNext())
					{
						// Add the element we got the last time through the loop
						if (lastElement != null) res += lastElement + " ";

						// Get the next element
						lastElement = ((SyntaxElement)elemEnum.Current).ToString();
					}

					// Add the final element
					if (type == ElementType.ImproperList) res += ". ";
					res += lastElement;

					// Add brackets and ellipsises as necessay
					if (type == ElementType.EllipsisList || type == ElementType.EllipsisVector)
						res += " ...";

					if (type == ElementType.List || type == ElementType.EllipsisList || type == ElementType.ImproperList)
						res = "(" + res + ")";
					else
						res = "#(" + res + ")";
					break;

				case ElementType.EmptyList:
					res = "()";
					break;

				default:
					res = Runtime.Interpreter.ToString(element);

					// TODO: distinguish literal symbols from bound symbols
					break;
			}

			return res;
		}

		#endregion
	}
}
