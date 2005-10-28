// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | A compiler for transforming patterns to templates        SyntaxCompiler.cs |
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
	/// <summary>
	/// SyntaxCompiler is a class designed to turn scheme syntax definitions (literals, patterns & templates) into a translation object
	/// that can take a match against a given pattern in the form of SyntaxEnvironment and produce code.
	/// </summary>
	public class SyntaxCompiler
	{
		// Nng, coming from programming scheme to actually implementing it is a bit of a wrench here.
		//
		// The problem, of course is ellipsises. R5RS is really unclear how these are to processed in the final pattern.
		//		'b ...' -> 'b ...'					is easy: descend the tree to 'b', then subsequently get siblings
		//		'(b ...) ...' -> '(b ...) ...'		is harder: we have to ascend an extra level to do the second ellipsis
		//		'(b) ...' -> '(b ...)'				is really annoying. The output structure is different from the input structure
		//
		// Note that we need to take account of empty lists: ie '(b ...) ...' -> '(b ...) ...' matched against '(1 2) () (3 4)' for example

		/// <summary>
		/// Generates a compiler that will produce scheme against the given pattern
		/// </summary>
		/// <param name="pattern">The SyntaxElement we're matching against</param>
		public SyntaxCompiler(SyntaxElement pattern)
		{
			this.pattern = pattern;
		}

		#region Data

		SyntaxElement pattern = null;							// The pattern we're matching against

		#endregion

		#region Compiling a pattern to a template

		static private Symbol ellipsis = new Symbol("...");

		/// <summary>
		/// Represents the state of the transformation produced by this compiler so far
		/// </summary>
		protected sealed class CompileState
		{
			public CompileState() { }

			/// <summary>
			/// The 'current' element that the node pointer will be at when the compiled code is executed
			/// </summary>
			public SyntaxElement currentElement = null;

			/// <summary>
			/// The top-level environment that we are compiling in
			/// </summary>
			public Data.Environment topLevelEnvironment = null;
		}

		/// <summary>
		/// Produces a sequence of commands that move the 'current' node from its present location (as given by the state) to a new location
		/// </summary>
		/// <param name="state">State at the start of the move (updated to contain the final state)</param>
		/// <param name="dest">The syntax element whose initial node we should be moving to</param>
		/// <returns>A transformation that will move the node pointer to the specified element</returns>
		/// <remarks>
		/// Only literal and list/vector syntax elements get entries in the tree. 
		/// 
		/// Moving between sibling elements after dealing with an ellipsis may not have the intended consequences.
		/// </remarks>
		protected Transformation CompileMove(CompileState state, SyntaxElement dest)
		{
			// Start building the result
			Transformation result = new Transformation();

			// There's nothing to do if we're already at the given element
			if (dest == state.currentElement) return result;

			// It is an error to move to a syntax element that is not a list or a bound symbol
			if (dest.Type == SyntaxElement.ElementType.Literal)
				throw new NotSupportedException("The syntax compiler cannot move to a literal element, as these are not represented in the output syntax tree");

			// Find the common parent for the current element and the destination element
			SyntaxElement commonParent = state.currentElement.CommonParent(dest);

			// (Note: CommonParent never returns null, and because we've already done the identical case above, it will always return an ancestor element)

			if (commonParent == dest)
			{
				// We can just move up from the current position to get to the destination
				while (state.currentElement != dest)
				{
					result.Add(SyntaxOp.Op.MoveUp);
					state.currentElement = state.currentElement.Parent;
				}

				return result;
			}

			// Move to an element below the common parent
			if (state.currentElement == commonParent)
			{
				result.Add(SyntaxOp.Op.MoveDown);

				// Sigh. Braniancs at Microsoft are not very good at collection classes
				IEnumerator weOnlyWantTheFirstElement;								// ... butUnfortunatelyMicrosoftAreStupid
				weOnlyWantTheFirstElement = state.currentElement.ListOrVectorContents.GetEnumerator();
				weOnlyWantTheFirstElement.MoveNext();
				state.currentElement = (SyntaxElement)weOnlyWantTheFirstElement.Current;
			}

			while (state.currentElement.Parent != commonParent)
			{
				result.Add(SyntaxOp.Op.MoveUp);

				state.currentElement = state.currentElement.Parent;
			}

			// Trace the path to the destination element
			Stack path = new Stack();
			SyntaxElement destAncestor = dest;
			while (destAncestor != commonParent)
			{
				path.Push(destAncestor);
				destAncestor = destAncestor.Parent;
			}

			// Follow the path to the destination element
			while (path.Count > 0)
			{
				// This element should be a sibling of the current element
				SyntaxElement thisStage = (SyntaxElement)path.Pop();

				// Work out how far we have to move
				int startOffset = state.currentElement.Offset;
				int endOffset = thisStage.Offset;

				int movementOffset = 0;
				int increment = startOffset>endOffset?-1:1;

				// .NET collection classes are lame: we have to convert the collection of elements to an ArrayList to proceed
				ArrayList elements = new ArrayList(state.currentElement.Parent.ListOrVectorContents);

				// We can't just take the difference between offsets, as literal elements are not represented in the tree
				for (int pos=startOffset; pos != endOffset; pos += increment)
				{
					if (((SyntaxElement)elements[pos]).Type != SyntaxElement.ElementType.Literal) movementOffset += increment;
				}

				if (movementOffset != 0)
				{
					// Move to the specified element
					result.Add(SyntaxOp.Op.MoveNumberRight, movementOffset);
				}
				state.currentElement = (SyntaxElement)elements[endOffset];

				// Sanity check
				if (state.currentElement != thisStage)
					throw new NotSupportedException("(Bug?) destination we moved to while compiling syntax was not where we were expecting to end up");

				if (path.Count > 0)
				{
					// Move downwards
					result.Add(SyntaxOp.Op.MoveDown);

					// GROAN, I HATE .NET COLLECTION CLASSES
					IEnumerator weOnlyWantTheFirstElement = state.currentElement.ListOrVectorContents.GetEnumerator();

					if (!weOnlyWantTheFirstElement.MoveNext())
						throw new NotSupportedException("(Bug?) destination we moved to while compiling syntax does not appear to have any children");
					state.currentElement = (SyntaxElement)weOnlyWantTheFirstElement.Current;
				}
			}

			// Double check the state
			if (state.currentElement != dest)
				throw new InvalidOperationException("Oops, SyntaxCompiler.CompileMove failed to move to the right place (this is a bug)");

			return result;
		}

		/// <summary>
		/// Given a template object, finds the very last bound symbol mentioned. This can then be used to find out the corresponding
		/// ellipsis in the pattern.
		/// </summary>
		/// <param name="template">The template to search</param>
		/// <returns>The last bound symbol in the template, or null if there was none</returns>
		protected Data.ISymbolic LastBoundSymbolForTemplate(object template)
		{
			if (template is ISymbolic)
			{
				// If this is a symbol, then return it
				if (pattern.ContainsBoundSymbol((ISymbolic)template))
					return (ISymbolic)template;
				else
					return null;
			}
			else if (template is Pair || template is ICollection)
			{
				// Make a stack of the objects to check
				Stack toCheck = new Stack();

				if (template is Pair)
				{
					// Add each element in the list to the stack of things to check
					Pair current = (Pair)template;

					while (current != null)
					{
						toCheck.Push(current.Car);

						if (current.Cdr == null || current.Cdr is Pair)
						{
							current = (Pair)current.Cdr;
						}
						else
						{
							toCheck.Push(current.Cdr);
							current = null;
						}
					}
				}
				else
				{
					// Add each element in the template to the stack of things to check
					foreach (object item in (ICollection)template) toCheck.Push(item);
				}

				// The stack toCheck now contains a list of the items we need to check against, in reverse, so we check each one until we find a bound symbol
				object checkObject;

				while ((checkObject=toCheck.Pop())!=null)
				{
					ISymbolic res = LastBoundSymbolForTemplate(checkObject);
					if (res != null) return res;
				}
			}

			return null;
		}

		/// <summary>
		/// Returns the ancestor of elem is an ellipsis (or null)
		/// </summary>
		protected SyntaxElement EllipsisAncestor(SyntaxElement elem)
		{
			if (elem == null) return null;

			SyntaxElement res = elem.Parent;

			while (res != null && (res.Type != SyntaxElement.ElementType.EllipsisList && res.Type != SyntaxElement.ElementType.EllipsisVector))
			{
				res = res.Parent;
			}

			return res;
		}

		/// <summary>
		/// Finds the 'umbrella' element for a subtemplate, that should be followed by an ellipsis
		/// </summary>
		/// <param name="template">The template to find the umbrella for</param>
		/// <returns>null if no umbrella element exists, or the umbrella item.</returns>
		/// <remarks>
		/// The 'umbrella' element is the element that contains a repetition introduced by an ellipsis, and which contains all the nodes that
		/// will be visited by the template.
		/// 
		/// Note that literals have no umbrella item (as they do not appear in the output tree)
		/// </remarks>
		protected SyntaxElement Umbrella(object template)
		{
			// The current umbrella item
			SyntaxElement umbrella = null;

			// The 'new' umbrella item depends on the value of template
			if (template is ISymbolic)
			{
				// The Symbol must exist as a bound variable - the umbrella item is at the point where the symbol occurs
				return pattern.BindingForSymbol((ISymbolic)template);
			}
			else if (template is Pair || template is ICollection)
			{
				// Flatten into an arraylist
				ArrayList items;
				
				if (template is Pair)
				{
					// List; handle the improper case
					items = new ArrayList();

					Pair currentItem = (Pair)template;
					while (currentItem != null)
					{
						items.Add(currentItem.Car);

						if (currentItem.Cdr == null || currentItem.Cdr is Pair)
						{
							currentItem = (Pair)currentItem.Cdr;
						} 
						else
						{
							items.Add(currentItem.Cdr);
							currentItem = null;
						}
					}
				}
				else
				{
					// Vector
					items = new ArrayList((ICollection)template);
				}

				// Process the items in this list/vector to find the umbrella item
				for (int itemNumber=0; itemNumber<items.Count; itemNumber++)
				{
					SyntaxElement thisUmbrella = null;
					bool isEllipsis = false;
					object item = items[itemNumber];

					// See if this element is followed by an ellipsis
					if (itemNumber+1<items.Count) isEllipsis = ellipsis.Equals(items[itemNumber+1]);

					// If it is, then 'thisUmbrella' is equal to the umbrella for this item
					if (isEllipsis)
					{
						thisUmbrella = EllipsisAncestor(Umbrella(item));

						// Need to skip the ellipsis itself
						itemNumber++;
					}
					else if (item is Pair || item is ICollection)
					{
						// No ellipsis, but a list
						thisUmbrella = Umbrella(item);
					}
					else if (item is ISymbolic && pattern.ContainsBoundSymbol((ISymbolic)item))
					{
						// Is a bound symbol
						thisUmbrella = Umbrella(item);
					}
					else
					{
						// Is a literal: skip this element
						continue;
					}

					// If there's no umbrella item for this item (which is an ellipsis item, a list or a bound symbol), return null
					if (thisUmbrella == null) return null;

					// Combine to find the current umbrella value
					if (umbrella == null)
					{
						umbrella = thisUmbrella;
					}
					else
					{
						umbrella = umbrella.CommonParent(thisUmbrella);
					}

					// There should be an item at this point
					if (umbrella == null) return null;
				}
			}
			else
			{
				// Literals have no umbrella item
				umbrella = null;
			}

			return umbrella;
		}

		/// <summary>
		/// Uses the pattern associated with this compiler to produce a Transformation that will output the appropriate transformed scheme.
		/// </summary>
		/// <param name="template">The scheme representing the template to output against</param>
		/// <param name="state">Used to store the representation of the state (altered to represent the state after the transformation on return)</param>
		/// <returns>A Transformation object that will translate a SyntaxEnvironment formed by a successful match against the pattern</returns>
		protected Transformation Compile(object template, CompileState state)
		{
			Transformation result = new Transformation();

			if (state.currentElement == null) state.currentElement = pattern;

			// What we compile to depends on what the template is
			if (template is Data.ISymbolic)
			{
				// Find the SyntaxElement that bound this symbol
				SyntaxElement binder = pattern.BindingForSymbol((ISymbolic)template);

				if (binder == null)
				{
					// This was a literal symbol, so it should be inserted as a symbol bound to a specific environment
					result.Add(SyntaxOp.Op.WriteLiteral, new LiteralSymbol((ISymbolic)template, state.topLevelEnvironment));
				}
				else
				{
					// This is a bound symbol: insert its value in literal context
					result.Add(CompileMove(state, binder));
					result.Add(SyntaxOp.Op.WriteValue);
				}
			}
			else if (template is Data.Pair || template is ICollection)
			{
				// Pairs build up lists, ICollections build up vectors
				// Ellipsises need special treatment
				// Improper lists are a bit of a pain, too

				ArrayList contents;								// The contents of the list/vector
				bool isImproper = false;						// true iff the template is an improper list
				bool isVector = false;							// true iff the template is building a vector

				if (template is Pair)
				{
					// Put the contents of the list this represents into contents
					contents = new ArrayList();

					Pair currentItem = (Pair)template;
					while (currentItem != null)
					{
						contents.Add(currentItem.Car);

						if (currentItem.Cdr == null || currentItem.Cdr is Pair)
						{
							currentItem = (Pair)currentItem.Cdr;
						}
						else
						{
							isImproper = true;
							contents.Add(currentItem.Cdr);
							currentItem = null;
						}
					}
				}
				else
				{
					// A bit easier: just initialise with the contents (we're making a vector)
					contents = new ArrayList((ICollection)template);

					isVector = true;
				}

				// Begin the list/vector
				if (isVector)
					result.Add(SyntaxOp.Op.BeginVector);
				else
					result.Add(SyntaxOp.Op.BeginList);

				// For each item in the list ...
				int item;

				for (item = 0; item<contents.Count; item++)
				{
					bool isEllipsis = false;								// True if this item is followed by an ellipsis

					if (item+1<contents.Count && contents[item+1] is ISymbolic && ellipsis.Equals(contents[item+1]))
					{
						// This item is followed by an ellipsis: use ellipsis handling behaviour
						isEllipsis = true;
					}

					if (!isEllipsis)
					{
						// If there isn't an ellipsis, then we can just process this symbol as normal
						Transformation symbolTransform = Compile(contents[item], state);

						result.Add(symbolTransform);
					}
					else
					{
						// Handle ellipsises

						// R5RS is pretty specific about how these are handled in the pattern, but vague about how they are handled in
						// the template.
						//
						// The key is matching a template ellipsis to one in the pattern. This gives us a 'root' node, under which the
						// ellipsis data occurs. This is not simple. Consider '(b) ...' <-> '(b ...)' (consider both as patterns and templates)
						//
						// The 'root' node of an ellipsis is an EllipsisList or an EllipsisVector, and its values are the rightmost entries.
						// This node forms an 'umbrella' of all the nodes visited by the template to the left of the ellipsis. IE, we find
						// the common parent of all the 'visited' nodes of the template, then move upwards until we find an ellipsis element:
						// this forms the root of this ellipsis element. If no such element exists, then the ellipsis has no match.
						//
						// TODO: how to catch the case '(b) ...' -> 'b'? This is a scheme error, but at present will be handled implicitly.

						// Find the 'umbrella' item: the SyntaxElement that contains the ellipsis that we're handling here
						SyntaxElement ellipsisUmbrella = EllipsisAncestor(Umbrella(contents[item]));

						if (ellipsisUmbrella == null)
						{
							throw new Exception.SyntaxError("Could not match an ellipsis in a syntax template to the pattern");
						}

						// Find the element that's being repeated (last element under ellipsisUmbrella)
						// TODO: it's increasingly looking like we should use the IList interface and not the ICollection interface for ListOrVectorContents
						SyntaxElement ellipsisElement = null;
						IEnumerator ellipEnum = ellipsisUmbrella.ListOrVectorContents.GetEnumerator();
						while (ellipEnum.MoveNext())
							ellipsisElement = (SyntaxElement)ellipEnum.Current;

						// Move to the umbrella item
						result.Add(CompileMove(state, ellipsisUmbrella));

						// Create the transformer for each element of the ellipsis (we add this in later: we need to know the size for now to calculate branches)
						// NOTE: UNTIL WE ACTUALLY ADD THIS IN, THE STATE IS INCORRECT FOR THE VALUE IN RESULT
						state.currentElement = ellipsisElement;													// We move to this element later on...

						// Transformation that writes out the contents of the repeated values
						Transformation ellipsisTransform = Compile(contents[item], state);						// We perform this transformation later

						// Transformation that returns to the top of the tree (the item underneath the umbrella)
						Transformation ellipsisFlyback = CompileMove(state, ellipsisElement);					// ... and also this move

						// Work out the number of items we have to move across to get to the first ellipsis item (remember literal elements do not appear in the output tree)
						int countToItem = 0;
						foreach (SyntaxElement element in ellipsisUmbrella.ListOrVectorContents)
						{
							if (element.Type != SyntaxElement.ElementType.Literal) countToItem++;
						}

						// (Before the element) Try to move to just before the ellipsis item (STATE INCORRECT FOR RESULT)

						bool lowCount = countToItem <= 1;						// True if the very first item beneath the umbrella is also the first repeated item (needs to be handled differently)

						if (lowCount)
						{
							// Move to the first ellipsis item
							result.Add(SyntaxOp.Op.MoveDownOrBranch, null, ellipsisTransform.Count + ellipsisFlyback.Count + 3);
						}
						else
						{
							// Move to the first ellipsis item
							// TODO: fix that oops

							result.Add(SyntaxOp.Op.MoveDown);
							result.Add(SyntaxOp.Op.MoveNumberRightOrBranch, countToItem-1, ellipsisTransform.Count + ellipsisFlyback.Count + 2);
						}

						// (Actual element) Write the element itself (STATE INCORRECT FOR RESULT)
						result.Add(ellipsisTransform);

						// (Element loop) move to the next element, if it exists (STATE INCORRECT FOR RESULT)
						result.Add(ellipsisFlyback);											// Move back to top of tree (STATE NOW CORRECT)
						result.Add(SyntaxOp.Op.MoveNumberRightOrBranch, 1, 1);					// Branching by 1 skips out of the end (we're still on ellipsisElement, so all is well)

						// Return to the element loop
						result.Add(SyntaxOp.Op.Branch, null, -1 - 1 - ellipsisFlyback.Count - ellipsisTransform.Count);

						// Move back up to the umbrella item (so that we're in the right place in the tree even if no elements were found)
						result.Add(SyntaxOp.Op.MoveUp);
						state.currentElement = state.currentElement.Parent;

						if (state.currentElement != ellipsisUmbrella)
							throw new InvalidOperationException("Oops, while processing an ellipsis, we failed to return to the correct element (THIS IS A BUG)");

						// Skip the ellipsis itself (ie, don't write it as a literal)
						item++;
					}
				}

				// Finish the list/vector
				//
				// TODO: A curiousity: this makes (a b . ...) work as a template: do we want to catch this case and report an error, or keep it as a feature?
				if (isVector)
				{
					result.Add(SyntaxOp.Op.FinishVector);
				}
				else
				{
					if (isImproper)
						result.Add(SyntaxOp.Op.FinishListImproper);
					else
						result.Add(SyntaxOp.Op.FinishList);
				}
			}
			else
			{
				// Everything else is inserted literally
				result.Add(SyntaxOp.Op.WriteLiteral, template);
			}

			return result;
		}

		public Transformation Compile(object template, Data.Environment topLevel)
		{
			CompileState state = new CompileState();

			state.currentElement = pattern;
			state.topLevelEnvironment = topLevel;

			return Compile(template, state);
		}

		#endregion
	}
}
