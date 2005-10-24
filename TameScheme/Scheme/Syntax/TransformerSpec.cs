// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Definition for a transformer spec object                TransformerSpec.cs |
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


#if false
namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// Scheme syntax is defined by transformer specs: these translate syntax to SExpressions. This is essentially the implementation of the
	/// syntax-rules syntax.
	/// 
	/// The first item in the syntax specs is ignored: in syntax-rules, this is always the name of the syntax being defined and is redundant.
	/// 
	/// Syntax spec should be a well-formed list, containing pairs of the form (&lt;pattern&gt; &lt;template&gt;)
	/// </summary>
	public sealed class TransformerSpec
	{
		#region Construction
		/// <summary>
		/// Constructs the transformer spec with the given list of 
		/// </summary>
		/// <param name="literals">A list of symbols (a Pair, usually): symbols treated as literals in the syntaxSpec</param>
		/// <param name="syntaxSpec">The specification to use (a pair, a list containing items of the form (&lt;pattern&gt; &lt;template&gt;))</param>
		public TransformerSpec(ICollection literals, object syntaxSpec)
		{
			SetSpec(literals, syntaxSpec);
		}

		void SetSpec(ICollection literals, object syntaxSpec)
		{
			// The syntax spec must be a list of the form ((pattern template) ...)
			if (!(syntaxSpec is Data.Pair)) 
			{
				throw new Exception.SyntaxError("Syntax specifications must be a list of the form ((pattern template) ...)");
			}

			// Turn the literals into a dictionary (speeds things up a bit, especially later on)
			ListDictionary literalDictionary = new ListDictionary();

			if (literals != null)
			{
				foreach (Data.Symbol sym in literals)
				{
					literalDictionary[sym] = true;
				}
			}

			// Parse the list of patterns
			ArrayList patterns = new ArrayList();
			ArrayList templates = new ArrayList();

			foreach (object element in ((Data.Pair)syntaxSpec)) 
			{
				// Must be of the form (pattern template)
				if (!(element is Data.Pair) || ((Data.Pair)element).Cdr == null || !(((Data.Pair)element).Cdr is Data.Pair))
				{
					throw new Exception.SyntaxError("Syntax specifications must be a list of the form ((pattern template) ...)");
				}

				// Get the pattern and the template
				object pattern = ((Data.Pair)element).Car;
				object template = ((Data.Pair)element)[1];

				// Parse the pattern
				SyntaxElement patternSyntax = SyntaxElement.MakeElementFromScheme(pattern, literalDictionary);

				// Add to the lists of patterns and templates
				patterns.Add(patternSyntax);
				patterns.Add(template);
			}

			// Work out how the syntax is transformed by the patterns
			
		}
		#endregion

		#region Transforming actions

		// Basic actions are:
		//   Write literal symbol (optionally renaming if in a binding context, or if it would be bound to a different environment)
		//   Write bound symbol
		//   Start new list
		//   Finish list
		//   Finish list and make improper
		//   Finish vector (ie, turn the list we were building into a vector)

		/// <summary>
		/// Interface implemented by transformer classes
		/// </summary>
		private interface ITransformer
		{
			/// <summary>
			/// Performs this transformation
			/// </summary>
			/// <param name="inputEnvironment">The syntax environment the transformation is taking place within</param>
			/// <param name="topLevel">The toplevel environment this syntax is being transformed within</param>
			/// <param name="locals">The stub of the local environment this syntax will execute in</param>
			/// <param name="ellipsisDepth">The ellipsis repeat count</param>
			/// <returns>An object representing the result of this transformation</returns>
			object Transform(SyntaxEnvironment inputEnvironment, Data.Environment topLevel, Data.Environment locals, SyntaxEnvironment.EllipsisDepth ellipsisDepth);

			/// <summary>
			/// Works out if this transform should be repeated as part of an ellipsis pattern.
			/// </summary>
			/// <param name="inputEnvironment">The syntax environment that this syntax is being transform within</param>
			/// <param name="depth">The ellipsis repeat count that we'll want symbols for</param>
			/// <returns>True if Transform will succeed, False if it will fail</returns>
			bool HasDataForEllipsisDepth(SyntaxEnvironment inputEnvironment, SyntaxEnvironment.EllipsisDepth depth);
		}

		private class ListTransformer : ITransformer
		{
			public ListTransformer(ICollection transformers)
			{
				this.transformers = transformers;
			}

			protected ICollection transformers;

			/// <summary>
			/// Produces an arraylist containing what should be the eventual contents of the list
			/// </summary>
			/// <param name="inputEnvironment"></param>
			/// <param name="topLevel"></param>
			/// <param name="locals"></param>
			/// <param name="ellipsisDepth"></param>
			/// <returns></returns>
			protected virtual ArrayList MakeFinalObjects(SyntaxEnvironment inputEnvironment, Tame.Scheme.Data.Environment topLevel, Tame.Scheme.Data.Environment locals, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth ellipsisDepth)
			{
				ArrayList res = new ArrayList();

				// Create the result by running all of our transformers in turn
				foreach (ITransformer itemTransformer in transformers)
				{
					if (!(itemTransformer is EllipsisTransformer)) 
					{
						// Standard behaviour
						object itemValue = itemTransformer.Transform(inputEnvironment, topLevel, locals, ellipsisDepth);

						res.Add(itemValue);
					}
					else
					{
						// Ellipsis behaviour for this item (add it potentially multiple times)
						ellipsisDepth.StartToRepeat();

						ellipsisDepth.FinishRepeating();
					}
				}

				return res;
			}

			#region ITransformer Members

			public virtual object Transform(SyntaxEnvironment inputEnvironment, Tame.Scheme.Data.Environment topLevel, Tame.Scheme.Data.Environment locals, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth ellipsisDepth)
			{
				// Use the transformers to build a list, which we return
				ArrayList objects = MakeFinalObjects(inputEnvironment, topLevel, locals, ellipsisDepth);

				// Special case: no objects produces the empty list
				if (objects.Count == 0) return null;

				// Turn the objects into a scheme list
				return new Data.Pair(objects);
			}

			public virtual bool HasDataForEllipsisDepth(SyntaxEnvironment inputEnvironment, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth depth)
			{
				// We have data iff all of our 'child' transformers also have data
				foreach (ITransformer itemTransformer in transformers)
				{
					if (!itemTransformer.HasDataForEllipsisDepth(inputEnvironment, depth)) return false;
				}

				return true;
			}

			#endregion
		}

		private sealed class ImproperListTransformer : ListTransformer
		{
			public ImproperListTransformer(ICollection transformers) : base(transformers)
			{
			}

			public override object Transform(SyntaxEnvironment inputEnvironment, Tame.Scheme.Data.Environment topLevel, Tame.Scheme.Data.Environment locals, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth ellipsisDepth)
			{
				// Get the final objects
				ArrayList finalObjects = MakeFinalObjects(inputEnvironment, topLevel, locals, ellipsisDepth);

				// Produce an improper list from the result
				if (finalObjects.Count == 0) return null;								// TODO: this is technically an error
				if (finalObjects.Count == 1) return finalObjects[0];					// Degenerate case (possible with creative use of ellipsises)

				// The last object we added to the list
				object lastObject = finalObjects[finalObjects.Count - 1];				// Initially this is the 'final' object in the improper list
				
				// Iterate backwards over the list of values
				for (int objNumber = finalObjects.Count-2; objNumber >= 0; objNumber--)
				{
					// The next (techincally the previous) object in the list
					Data.Pair nextObject = new Data.Pair(finalObjects[objNumber], lastObject);

					// Move back along the list
					lastObject = nextObject;
				}

				// lastObject now contains the final object in the list
				return lastObject;
			}

		}

		private sealed class VectorTransformer : ListTransformer
		{
			public VectorTransformer(ICollection transformers) : base(transformers)
			{ }

			public override object Transform(SyntaxEnvironment inputEnvironment, Tame.Scheme.Data.Environment topLevel, Tame.Scheme.Data.Environment locals, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth ellipsisDepth)
			{
				// Convert to a .NET array instead of a scheme list
				ArrayList vectorContents = MakeFinalObjects(inputEnvironment, topLevel, locals, ellipsisDepth);

				object[] res = new object[vectorContents.Count];
				vectorContents.CopyTo(res);

				return res;
			}

		}

		/// <summary>
		/// The EllipsisTransformer is kind of a special case. The Transform() method should never be called directly: instead it's used to
		/// signify that its contents should be repeatedly placed into the output (until HasDataForEllipsisDepth() returns false).
		/// This works with the ListTransformer to implement ellipsises (ellipsises can only occur in lists or vectors)
		/// </summary>
		private sealed class EllipsisTransformer : ITransformer
		{
			public EllipsisTransformer(ITransformer innerTransformer)
			{
				this.innerTransformer = innerTransformer;
			}

			private ITransformer innerTransformer;

			/// <summary>
			/// Accessor for the inner transformer
			/// </summary>
			public ITransformer InnerTransformer
			{
				get { return innerTransformer; }
			}

			#region ITransformer Members

			public object Transform(SyntaxEnvironment inputEnvironment, Tame.Scheme.Data.Environment topLevel, Tame.Scheme.Data.Environment locals, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth ellipsisDepth)
			{
				// Because this class is a special case, it is an error to ever call this (there is no sensible result for this method)
				throw new InvalidOperationException("An attempt was made to use the EllipsisTransformer as a 'standard' transformer (it is a special case)");
			}

			public bool HasDataForEllipsisDepth(SyntaxEnvironment inputEnvironment, Tame.Scheme.Syntax.SyntaxEnvironment.EllipsisDepth depth)
			{
				// We have data if the inner transformer also has data
				return innerTransformer.HasDataForEllipsisDepth(depth);
			}

			#endregion
		}
		#endregion
	}
}
#endif
