using System;

using Tame.Scheme.Data;

namespace Tame.Scheme.Syntax.Transformer
{
	/// <summary>
	/// The job of the binder is to take syntax transformed through a Transformation and find any elements that were inserted literally and
	/// are in a binding context and rename them. This satisfies the R5RS conditions for hygenic macros.
	/// </summary>
	/// <remarks>
	/// Classes that introduce binding can use the IBinding interface.
	/// </remarks>
	public class Binder
	{
		public static object BindScheme(object scheme, Data.Environment topLevel)
		{
			return null;
		}
	}
}
