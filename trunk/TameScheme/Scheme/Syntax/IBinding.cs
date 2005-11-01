// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interface for syntax that introduces bindings                  IBinding.cs |
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

namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// Classes implementing IBinding provide syntax that introduces binding.
	/// </summary>
	/// <remarks>
	/// Information about binding is required to perform renaming while processing R5RS syntax. Symbols introduced as free variables by the
	/// syntax are stored as LiteralSymbols, while those passed in from the pattern remain as Symbols.
	/// </remarks>
	public interface IBinding : ISyntax
	{
		/// <summary>
		/// Given some scheme and a BindingState for this particular binding environment, performs hygenic binding (converts any variable usage
		/// to temporary variables as appropriate).
		/// </summary>
		/// <param name="scheme">The scheme representing the arguments for this syntax</param>
		/// <param name="syntaxEnv">The results of matching against the patterns provided for this syntax</param>
		/// <param name="syntaxMatch">The number of the line of syntax that was matched</param>
		/// <param name="state">The binding state for this section of scheme</param>
		/// <remarks>
		/// The BindingState can be amended as required for performing suitable binding: it can also invoke the normal Binder behaviour
		/// (which is what you normally want to do).
		/// </remarks>
		object BindScheme(object scheme, SyntaxEnvironment syntaxEnv, int syntaxMatch, Transformer.Binder.BindingState state);
	}
}
