// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | User-defined syntax                                          UserSyntax.cs |
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

using Tame.Scheme.Runtime;
using Tame.Scheme.Syntax.Transformer;

namespace Tame.Scheme.Syntax.Library
{
	/// <summary>
	/// UserSyntax represents syntax defined by the define-syntax operator.
	/// </summary>
    [SchemeGroup(SchemeGroup.UserExtension), SchemeUsage(SchemeUsage.Normal)]
	public class UserSyntax : SchemeSyntax, ISyntax, ITransformingSyntax
	{
		public UserSyntax(Syntax syntaxMatcher, ArrayList transformers) : base(syntaxMatcher, null)
		{
			this.transformers = transformers;
		}

		ArrayList transformers;

		#region ISyntax Members

		public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			// Get the transformation to use
			Transformation matchingTransformer = (Transformation)transformers[syntaxMatch];

			// Perform the transformation
			object translatedScheme = matchingTransformer.Transform(env.SyntaxTree);

			// Rename any temporary variables
			translatedScheme = state.TemporaryBinder.BindScheme(translatedScheme, state);

			// Compile the result
			return BExpression.BuildExpression(translatedScheme, state);
		}

		#endregion

        #region ITransformingSyntax Members

        public object TransformScheme(object scheme, CompileState state)
        {
            // Match the scheme against our syntax
            // TODO: do we need to skip the first symbol?
            SyntaxEnvironment binding;
            int match = Syntax.Match(scheme, state, out binding);

            // Get the transformation to use
            // TODO: throw a nice exception if we failed to get a match
			Transformation matchingTransformer = (Transformation)transformers[match];

            // Perform the transformation
            object translatedScheme = matchingTransformer.Transform(binding.SyntaxTree);

            // Rename any temporary variables
            translatedScheme = state.TemporaryBinder.BindScheme(translatedScheme, state);

            // Return the result
            return translatedScheme;
        }

        #endregion
    }
}
