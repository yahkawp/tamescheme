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
		/// Given scheme supposed to represent this syntax (no pattern matching will have been performed), update state to reflect the
		/// values that have been bound.
		/// </summary>
		/// <param name="syntaxEnv">The results of matching against the patterns provided for this syntax</param>
		/// <param name="state">The binding state for this section of scheme</param>
		void UpdateBindingForScheme(SyntaxEnvironment syntaxEnv, Transformer.Binder.BindingState state);
	}
}
