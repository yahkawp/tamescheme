using System;

namespace Tame.Scheme.Data
{
	/// <summary>
	/// Interface implemented by objects with 'symbol-like' properties: ie, objects that can be associated with a value in an environment.
	/// </summary>
	public interface ISymbolic
	{
		/// <summary>
		/// Gets the 'ordinary' symbol that this ISymbolic object represents.
		/// </summary>
		Symbol Symbol { get; }

		/// <summary>
		/// The value by which this symbol is hashed (how it is stored in the environment)
		/// </summary>
		/// <remarks>
		/// 'Real' symbols use their symbol number here. Symbols that have different meanings (eg, temporary symbols) should use a different hash value.
		/// 
		/// ISymbolic objects should also respond to the usual GetHashValue(), Equals() methods. Note that two symbols that are equal according
		/// to their HashValues may be different otherwise. (This means, for example, a LiteralSymbol != a Symbol, but they are stored in the
		/// same location in the environment)
		/// </remarks>
		object HashValue { get; }

		/// <summary>
		/// The environment in which this symbol should be bound, or null to specify the 'nearest' environment
		/// </summary>
		Environment Location { get; }
	}
}
