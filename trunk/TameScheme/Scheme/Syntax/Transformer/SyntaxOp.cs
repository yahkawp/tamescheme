// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | A single operation performed while transforming syntax         SyntaxOp.cs |
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

namespace Tame.Scheme.Syntax.Transformer
{
	/// <summary>
	/// SyntaxOp represents a single operation performed while transforming a matched pattern (in the form of a SyntaxNode) to another.
	/// </summary>
	public struct SyntaxOp
	{
		/// <summary>
		/// Types of operations that can be performed while evaluating syntax
		/// </summary>
		public enum Op
		{
			// General
			/// <summary>
			/// Branches by a given offset
			/// </summary>
			Branch,

			// Moving

			/// <summary>
			/// Moves to the parent element of the current syntax node. It is a failure to try to move above the root node.
			/// </summary>
			MoveUp,

			/// <summary>
			/// Moves to the leftmost child node of the current syntax node. It is a failure to try to move below the lowest leaf node.
			/// </summary>
			MoveDown,

			/// <summary>
			/// Branch operation. Move down if we can (as for MoveDown) or branch if we cannot
			/// </summary>
			MoveDownOrBranch,

			/// <summary>
			/// Moves a specified number of nodes, or branches if we can't.
			/// </summary>
			MoveNumberRight,

			/// <summary>
			/// Moves a specified number of nodes, or branches if we can't.
			/// </summary>
			MoveNumberRightOrBranch,

			// Writing

			/// <summary>
			/// Writes a Literal value
			/// </summary>
			WriteLiteral,

			/// <summary>
			/// Writes the value of the current node
			/// </summary>
			WriteValue,

			/// <summary>
			/// Begins writing a vector of values
			/// </summary>
			BeginVector,

			/// <summary>
			/// Begins writing a list of values
			/// </summary>
			BeginList,

			/// <summary>
			/// Finishes writing a vector of values
			/// </summary>
			FinishVector,

			/// <summary>
			/// Finishes writing a list of values
			/// </summary>
			FinishList,

			/// <summary>
			/// Finishes writing an improper list of values
			/// </summary>
			FinishListImproper,
		}

		/// <summary>
		/// The operation that we want to perform
		/// </summary>
		public Op op;

		/// <summary>
		/// The parameter for this operation (if available)
		/// </summary>
		public object param;

		/// <summary>
		/// The amount to branch (if a branch operation)
		/// </summary>
		public int branch;
	}
}
