using System;
using System.Collections;

using Tame.Scheme.Data;

namespace Tame.Scheme.Syntax.Transformer
{
	// TODO: presently, matching () against () causes an exception

	/// <summary>
	/// Represents a syntax transformation.
	/// </summary>
	public class Transformation : ICollection
	{
		public Transformation()
		{
		}

		public Transformation(ICollection ops)
		{
			this.ops = new ArrayList(ops);
		}

		#region Data

		ArrayList ops = new ArrayList();						// The operations that make up this transformation

		#endregion

		#region Modifying

		public void Add(SyntaxOp op)
		{
			ops.Add(op);
		}

		public void Add(ICollection newOps)
		{
			this.ops.AddRange(newOps);
		}

		public void Add(SyntaxOp.Op op)
		{
			SyntaxOp finalOp;

			finalOp.op = op;
			finalOp.branch = 0;
			finalOp.param = null;

			Add(finalOp);
		}

		public void Add(SyntaxOp.Op op, object param)
		{
			SyntaxOp finalOp;

			finalOp.op = op;
			finalOp.param = param;
			finalOp.branch = 0;

			Add(finalOp);
		}

		public void Add(SyntaxOp.Op op, object param, int branch)
		{
			SyntaxOp finalOp;

			finalOp.op = op;
			finalOp.param = param;
			finalOp.branch = branch;

			Add(finalOp);
		}

		#endregion

		#region ICollection Members

		public bool IsSynchronized
		{
			get
			{
				return false;
			}
		}

		public int Count
		{
			get
			{
				return ops.Count;
			}
		}

		public void CopyTo(Array array, int index)
		{
			ops.CopyTo(array, index);
		}

		public object SyncRoot
		{
			get
			{
				return ops.SyncRoot;
			}
		}

		#endregion

		#region IEnumerable Members

		public IEnumerator GetEnumerator()
		{
			return ops.GetEnumerator();
		}

		#endregion

		#region Converting to strings

		public override string ToString()
		{
			string res = "Syntax> ";

			foreach (SyntaxOp op in ops)
			{
				res += "(";
				res += op.op.ToString();
				if (op.param != null) res += " " + op.param.ToString();
				if (op.branch != 0) res += " " + op.branch.ToString();
				res += ") ";
			}

			return res.Substring(0, res.Length-1);
		}

		#endregion

		#region Running the transformation

		/// <summary>
		/// Runs the code associated with this transformation, changing the given syntaxTree into an S-Expression
		/// </summary>
		/// <param name="syntaxTree">The syntax tree that matched the pattern used to build this Transformation</param>
		/// <returns>A transformed SExpression</returns>
		/// <remarks>
		/// Literals symbols are often added as LiteralSymbol objects instead of Symbol: this makes it possible to easily identify symbols that
		/// came from the template rather than from the pattern that was matched. However, note that this function does not deal with bindings:
		/// the result does not (yet) conform to the R5RS definition of hygiene. Finding literal symbols in binding contexts and substituting
		/// temporary symbols is required for this.
		/// </remarks>
		public object Transform(SyntaxNode syntaxTree)
		{
			SyntaxNode currentNode = syntaxTree.Child;					// The node we're currently pointing at (we start at the root)
			int pc = 0;													// The program counter

			object result = null;										// The result
			ArrayList currentList = null;								// The current list/vector we're building
			Stack previousLists = new Stack();							// Previous lists/vectors

			SyntaxNode newNode = null;

			while (pc < ops.Count)
			{
				// Get the next operation
				SyntaxOp thisOp = (SyntaxOp)ops[pc];

				// Evaluate it
				switch (thisOp.op)
				{
						// = Moving =

					case SyntaxOp.Op.Branch:
						pc += thisOp.branch;
						break;

					case SyntaxOp.Op.MoveDown:
						currentNode = currentNode.Child;
						break;

					case SyntaxOp.Op.MoveDownOrBranch:
						if (currentNode.Child == null)
							pc += thisOp.branch;
						else
							currentNode = currentNode.Child;
						break;

					case SyntaxOp.Op.MoveUp:
						currentNode = currentNode.Parent;
						break;

					case SyntaxOp.Op.MoveNumberRight:
						currentNode = currentNode.NthSibling((int)thisOp.param);
						break;

					case SyntaxOp.Op.MoveNumberRightOrBranch:
						newNode = currentNode.NthSibling((int)thisOp.param);

						if (newNode == null)
							pc += thisOp.branch;
						else
							currentNode = newNode;
						break;

						// = Writing things =

					case SyntaxOp.Op.WriteValue:
					case SyntaxOp.Op.WriteLiteral:
						// Get the thing we're writing
						object valueToWrite;

						if (thisOp.op == SyntaxOp.Op.WriteValue)
							valueToWrite = currentNode.Value;
						else
							valueToWrite = thisOp.param;

						// Set the result
						result = valueToWrite;

						// If we're building a list/vector, add the value there
						if (currentList != null) currentList.Add(valueToWrite);
						break;

						// = Constructing lists and vectors =

					case SyntaxOp.Op.BeginList:
					case SyntaxOp.Op.BeginVector:
						// These both work the same way (but might be distinguishable later, I suppose)
						if (currentList != null) previousLists.Push(currentList);

						currentList = new ArrayList();
						break;

					case SyntaxOp.Op.FinishVector:
						// Build the vector object
						object[] vector = new object[currentList.Count];
						currentList.CopyTo(vector, 0);

						// Set the result
						result = vector;

						// Pop the current list, and add this as an item if we're building another list somewhere
						currentList = null;
						if (previousLists.Count > 0) currentList = (ArrayList)previousLists.Pop();

						if (currentList != null) currentList.Add(result);
						break;

					case SyntaxOp.Op.FinishList:
					case SyntaxOp.Op.FinishListImproper:
						Pair list = null;

						if (currentList.Count == 0)
						{
							// Special case: the empty list
							result = null;
						}
						else if (thisOp.op == SyntaxOp.Op.FinishListImproper && currentList.Count == 1)
						{
							// Special case: single-element improper list is just that element
							result = currentList[0];
						}
						else
						{
							// Build the list
							list = new Pair(currentList[0], null);
							Pair thisElement = list;
							Pair lastElement = null;

							for (int listItem=1; listItem<currentList.Count; listItem++)
							{
								thisElement.Cdr = new Pair(currentList[listItem], null);

								lastElement = thisElement;
								thisElement = (Pair)thisElement.Cdr;
							}

							// Make improper if necessary
							if (thisOp.op == SyntaxOp.Op.FinishListImproper) lastElement.Cdr = ((Pair)lastElement.Cdr).Car;

							// Result is the list
							result = list;
						}

						// Pop the next list, if it's available
						currentList = null;
						if (previousLists.Count > 0) currentList = (ArrayList)previousLists.Pop();

						if (currentList != null) currentList.Add(result);
						break;
				}

				// Sanity check
				if (currentNode == null)
				{
					throw new InvalidOperationException("While transforming syntax: moved to a null node (this is a bug: either the syntax transformation code is bad, or was performed on a SyntaxNode generated from an incorrect pattern)");
				}

				// Next operation
				pc++;
			}

			// Return the result
			return result;
		}

		#endregion
	}
}
