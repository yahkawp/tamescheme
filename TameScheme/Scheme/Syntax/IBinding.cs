using System;

namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// Classes implementing IBinding provide syntax that introduces binding.
	/// </summary>
	/// <remarks>
	/// Information about binding is required to perform renaming while 
	/// </remarks>
	public interface IBinding : ISyntax
	{
	}
}
