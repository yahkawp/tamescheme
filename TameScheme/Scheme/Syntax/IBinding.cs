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
		void UpdateBindingForScheme(object scheme, Transformer.Binder.BindingState state);
	}
}
