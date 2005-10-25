// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interface for syntax that does not use bindings                 IQuoted.cs |
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
	/// IQuoted syntax objects are syntax objects that perform some kind of quoting on their parameters, which would make symbol renaming an
	/// inappropriate operation.
	/// </summary>
	/// <remarks>
	/// Typically this would be used for operations such as (quote x) where it would be improper to produce the list (#[LiteralSymbol x]) or
	/// (##TEMP_001). People interested in modifying the scheme language will probably note that this can also be used to perform more exotic
	/// operations, though because this is only used while expanding syntax caution must be exercised.
	/// 
	/// If an object implements both IQuoted and IBinding, quoting is performed first (before the new binding environment is created). 
	/// This is useful for things like (let ((x y) (y x)) where the first 'y' is different from the second.
	/// </remarks>
	public interface IQuoted : ISyntax
	{
		/// <summary>
		/// Transforms scheme about to be rebound for reasons of hygiene into its 'quoted' form.
		/// </summary>
		/// <param name="scheme">The scheme that this syntax will be matched against</param>
		/// <param name="bindState">State object indicating how variables are currently bound</param>
		/// <returns>Some scheme adjusted for the quoting semantics of this syntax</returns>
		/// <remarks>
		/// LiteralSymbols are symbols used by the syntax system to indicate free variables introduced while expanding syntax. While these are
		/// handy for dealing with operations such as let, where they need to be renamed to be temporary, this would be a problem for operators
		/// like quote, as you would wind up with things like (define-syntax oops (syntax-rules () ((oops) '(quoted))) yielding 
		/// (#[LiteralSymbol quoted]) instead of (quoted). This function provides an early opportunity for these types of syntax to
		/// change these values back into 'real' symbols.
		/// 
		/// If any modification is performed, objects MUST be copied, not altered.
		/// 
		/// The binding state passed is primarily intended for reference. It IS permissible to modify it, but not recommended. It represents
		/// the 'outer' binding state. In particular, rebinding the symbol that represents this syntax (which may not be the symbol you think
		/// it is) may cause strange things to happen.
		/// </remarks>
		object QuoteScheme(object scheme, SyntaxEnvironment matchEnvironment, Transformer.Binder.BindingState bindState);
	}
}
