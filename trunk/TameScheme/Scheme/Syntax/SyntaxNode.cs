// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | A node in a syntax tree                                      SyntaxNode.cs |
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

using Tame.Scheme.Data;

namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// SyntaxNode represents a node in the syntax tree represented by a SyntaxEnvironment. It's designed to allow R5RS syntax patterns to
	/// 'walk' the tree. Syntax trees have a fairly fixed structure: most nodes should be lists, with the bottom two layers being symbols
	/// and values (ie, all leaves must be values, and their parents must be symbols).
	/// </summary>
	/// <remarks>A symbol node MAY have 0 children, ie be a leaf, if it was matched as part of an ellipsis expression with 0 matches. Similarly for a list node.</remarks>
	public sealed class SyntaxNode
	{
		/// <summary>
		/// Constructs a list syntax node
		/// </summary>
		public SyntaxNode()
		{
			symbols = new HybridDictionary();
			root = this;
		}

		/// <summary>
		/// Constructs a symbol syntax node (the children of this node should be zero or more values)
		/// </summary>
		/// <param name="symbol">The symbol this node should represent</param>
		public SyntaxNode(Symbol symbol)
		{
			if (symbol == null)
				throw new NullReferenceException("Symbol syntax nodes cannot be created with a null symbol");

			this.symbol = symbol;
			ourType = NodeType.Symbol;
			root = this;
		}

		/// <summary>
		/// Constructs a value syntax node (this node should be a leaf only)
		/// </summary>
		/// <param name="value">The value of this object</param>
		/// <remarks>This is probably an API bug, but if you want a value that happens to be a symbol, remember to cast to an object. Ie, if you intend a value, always use new SymbolNode((object)aValue) for clarity</remarks>
		public SyntaxNode(object value)
		{
			this.value = value;

			ourType = NodeType.Value;
			root = this;
		}

		#region Data

		// If both of these are null, this is a list node
		NodeType ourType;												// The type of this node
		Symbol symbol = null;											// Iff a symbol, the symbol we're representing
		object value = null;											// Iff a value, the value we're representing

		SyntaxNode root = null;											// The node that is the root of the tree
		SyntaxNode parent = null;										// The node that is a parent of this node
		ArrayList children = new ArrayList();							// The nodes that are children of this node
		int offset = -1;												// The offset we have within the parent node

		HybridDictionary symbols = null;								// For a list node, a hash of the symbols that are contained in this node, and where they exist (maps symbols to the indexes in the arrays)

		#endregion

		#region Adding to the tree

		/// <summary>
		/// Removes this node from the tree (makes it a parent of its own tree)
		/// </summary>
		/// <remarks>This is a costly operation sometimes: avoid doing this if at all possible.</remarsk>
		/// <exception cref="NotSupportException">The node is already the root of a tree</exception>
		public void RemoveFromTree()
		{
			// Check sanity
			if (parent == null)
				throw new System.NotSupportedException("You cannot add a sibling to the root node of a syntax tree");

			// Adjust the symbol markers
			SyntaxNode currentParent = parent;
			SyntaxNode currentChild = this;
			while (currentParent != null) 
			{
				int curOffset = currentChild.offset;

				foreach (Symbol key in currentParent.symbols.Keys)
				{
					int curVal = (int)currentParent.symbols[key];

					if (curVal == curOffset)
						parent.symbols[key] = null;						// This symbol was deleted
					else if (curVal > curOffset)
						parent.symbols[key] = curVal-1;					// This symbol will now have a different offset
				}

				// Also adjust the known offsets for each symbol
				for (int x = offset+1; x < currentParent.children.Count; x++)
				{
					SyntaxNode node = (SyntaxNode)currentParent.children[x];
					node.offset--;
				}

				currentChild = currentParent;
				currentParent = currentParent.parent;
			}

			// Remove from the parent
			parent.children.RemoveAt(offset);
			parent = null;

			// We're now a separate tree
			root = this;
			offset = -1;
		}

		/// <summary>
		/// Adds a new node as a sibling (on the left of the current node)
		/// </summary>
		/// <param name="newNode">The node that should become a sibling of this one</param>
		/// <exception cref="NotSupportedException">The node is the root of a tree, or an attempt was made to add a node type that was not valid as a child for this node (eg, a list as the child of a symbol)</exception>
		public void AddSibling(SyntaxNode newNode)
		{
			// Check sanity
			if (parent == null)
				throw new System.NotSupportedException("You cannot add a sibling to the root node of a syntax tree");

			// Remove newNode from any existing tree
			if (newNode.parent != null) newNode.RemoveFromTree();

			// Add newNode to our parent
			parent.AddChild(newNode);
		}

		/// <summary>
		/// Adds a new node as a child of this node
		/// </summary>
		/// <param name="newNode"></param>
		/// <exception cref="NotSupportedException">This node is a value node, or this node is a symbol node and the new node is not a value node</exception>
		public void AddChild(SyntaxNode newNode)
		{
			// Check sanity
			if (ourType == NodeType.Value)
				throw new System.NotSupportedException("Value nodes must always be at the root of a syntax node tree");
			if (ourType == NodeType.Symbol && newNode.ourType != NodeType.Value)
				throw new System.NotSupportedException("Only value nodes can be added as the child nodes of symbol nodes in a syntax tree");

			// Remove newNode from any existing tree
			if (newNode.parent != null) newNode.RemoveFromTree();

			// Add newNode to this node
			children.Add(newNode);
			newNode.parent = this;
			newNode.root = root;
			newNode.offset = children.Count-1;

			// If a symbol, we need to mark where this exists in each parent
			if (newNode.ourType == NodeType.Symbol)
			{
				SyntaxNode currentParent = this;
				SyntaxNode currentChild = newNode;

				while (currentParent != null)
				{
					// Finish if this parent value already knows where to find a symbol value (so the default route always goes to the left)
					if (currentParent.symbols.Contains(newNode.symbol)) break;

					int offset = currentChild.offset;

					currentParent.symbols.Add(newNode.symbol, offset);

					currentChild = currentParent;
					currentParent = currentParent.parent;
				}
			}
		}

		#endregion

		#region Getting information about this node

		public enum NodeType
		{
			List, Symbol, Value
		}

		/// <summary>
		/// Retrieves the type of this node (list, value or symbol)
		/// </summary>
		public NodeType Type 
		{
			get
			{
				if (symbol != null)
					return NodeType.Symbol;
				else if (value != null)
					return NodeType.Value;
				else
					return NodeType.List;
			}
		}

		/// <summary>
		/// True if this is a symbol node
		/// </summary>
		public bool IsSymbol
		{
			get { return ourType == NodeType.Symbol; }
		}

		/// <summary>
		/// True if this is a value node
		/// </summary>
		public bool IsValue
		{
			get { return ourType == NodeType.Value; }
		}

		/// <summary>
		/// True if this is a list node
		/// </summary>
		public bool IsList
		{
			get { return ourType == NodeType.List; }
		}

		/// <summary>
		/// Retrieves the 'value' of this node
		/// </summary>
		public object Value
		{
			get
			{
				if (ourType == NodeType.Symbol) return Child.Value;
				else if (ourType != NodeType.Value) throw new NotSupportedException("You cannot retrieve the value of a non-value syntax node");
				return value;
			}
		}

		/// <summary>
		/// Retrieves the symbol this node represents
		/// </summary>
		public Symbol Symbol
		{
			get 
			{ 
				if (ourType == NodeType.Value && parent != null) return parent.Symbol;
				else if (ourType != NodeType.Symbol) throw new NotSupportedException("You cannot retrieve the symbol of a list syntax node (or a value node with no parent)");

				return symbol;
			}
		}

		#endregion

		#region Navigating the tree

		/// <summary>
		/// Gets the 'first child' of this node, or null if there are no children
		/// </summary>
		public SyntaxNode Child
		{
			get
			{
				if (children.Count <= 0) return null;

				return (SyntaxNode)children[0];
			}
		}

		/// <summary>
		/// The leftmost sibling of this node, or null if this is the last sibling
		/// </summary>
		public SyntaxNode Sibling
		{
			get
			{
				if (parent == null)
					throw new NotSupportedException("It is not possible to get the sibling of a root syntax node");

				if (offset+1 >= parent.children.Count)
					return null;

				return (SyntaxNode)parent.children[offset+1];
			}
		}

		/// <summary>
		/// The 'nth' sibling of this node
		/// </summary>
		/// <param name="count">The number of nodes to skip</param>
		/// <returns>The 'nth' sibling of this node, or null if no such sibling exists. An exception will be thrown if you try to go more than one item from any 'edge' (ie a null node exists at the left and right extremes, but exceptions are thrown if you try to skip that)</returns>
		/// <exception cref="NotSupportedException">Thrown if this is a root node (ie, has no siblings)</exception>
		/// <exception cref="IndexOutOfRangeException">
		/// Thrown if we try to go more than 1 node from the left or right edge of the sibling list.
		/// </exception>
		public SyntaxNode NthSibling(int count)
		{
			if (parent == null)
				throw new NotSupportedException("It is not possible to get the sibling of a root syntax node");

			int newOffset = offset + count;
			int parentCount = parent.children.Count;

			if (newOffset > parentCount || newOffset < -1)
				throw new IndexOutOfRangeException("NthSibling() was called with an index that falls out of bounds for this syntax node");

			if (newOffset >= parentCount || newOffset < 0) return null;

			return (SyntaxNode)parent.children[newOffset];
		}

		/// <summary>
		/// The node that is the parent of this node, or null if this is the root node
		/// </summary>
		public SyntaxNode Parent
		{
			get
			{
				return parent;
			}
		}

		/// <summary>
		/// The node that is at the root of the tree that this syntax node is a member of
		/// </summary>
		public SyntaxNode Root
		{
			get
			{
				return root;
			}
		}

		/// <summary>
		/// Retrieves the child syntax node containing the given symbol (ie, moves one level closer to the given symbol)
		/// </summary>
		/// <param name="symbol">The symbol we're looking for</param>
		/// <returns>null if this symbol is not present, or the child of this node that is 'nearer' to that symbol</returns>
		/// <remarks>Can only be called on list nodes</remarks>
		/// <exception cref="NotSupportedException">If called on a node that is not a list node</exception>
		public SyntaxNode ChildWithSymbol(Symbol symbol)
		{
			if (ourType != NodeType.List) throw new NotSupportedException("ChildWithSymbol can only be called on list syntax nodes");

			if (symbols.Contains(symbol) && symbols[symbol] != null)
				return (SyntaxNode)children[(int)symbols[symbol]];
			else
				return null;
		}

		/// <summary>
		/// Iterates down the tree until a node representing the given symbol is found. Always goes to the left if a symbol appears multiple times.
		/// </summary>
		/// <param name="symbol">The symbol we're looking for</param>
		/// <returns>A symbol syntax node</returns>
		/// <exception cref="InvalidOperationException">If called on a symbol node that does not match the specified symbol</exception>
		/// <exception cref="NotSupportedException">If called on a node that is not a symbol or a list node</exception>
		public SyntaxNode GrandchildWithSymbol(Symbol searchSymbol)
		{
			// If this is a symbol, and
			if (ourType == NodeType.Symbol && symbol.Equals(searchSymbol)) return this;
			if (ourType == NodeType.Symbol) throw new InvalidOperationException("GrandchildWithSymbol called on a symbol node that does not match the specified symbol");

			// Move towards the symbol, if we can
			SyntaxNode symbolChild = ChildWithSymbol(searchSymbol);
			if (symbolChild == null) return null;

			// Recurse down the tree
			return symbolChild.GrandchildWithSymbol(searchSymbol);
		}

		#endregion

		#region Changing to strings

		private string IndentString(string input, bool isLast)
		{
			// Helps format things so that when we call ToString we get something formatted like this:
			//		Foo
			//		+- Bar
			//		+- Baz
			//		|  +- Quux
			//		+- Goofle

			// The first line of input is indented with '+- '
			string res = "+- " + input;

			// Each subsequent line should be prefixed with '   ' or '|  ' depending on the value of isLast
			for (int x=0; x<res.Length; x++) 
			{
				if (res[x] == '\n')
				{
					res = res.Substring(0, x+1) + (isLast?"   ":"|  ") + res.Substring(x+1);
				}
			}

			return res;
		}

		public override string ToString()
		{
			// String for ourselves
			string res = "Unknown";

			if (ourType == NodeType.List)
			{
				res = "List";
			}
			else if (ourType == NodeType.Symbol)
			{
				res = "Symbol " + symbol.ToString();
			}
			else if (ourType == NodeType.Value)
			{
				if (value == null)
					res = "Value (null)";
				else
					res = "Value " + value.ToString();
			}

			// Make strings for each of the children
			if (children != null && children.Count > 0)
			{
				for (int x=0; x<children.Count; x++)
				{
					res += "\n" + IndentString(children[x].ToString(), x+1 == children.Count);
				}
			}

			return res;
		}

		#endregion
	}
}
