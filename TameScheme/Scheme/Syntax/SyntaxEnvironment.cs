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
		public void StartSymbol(Data.ISymbolic symbol)
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
		public bool Contains(Data.ISymbolic symbol)
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
		public SyntaxNode this[Data.ISymbolic symbol]
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
	}
}
