using System;

namespace Tame.Scheme.Data
{
	/// <summary>
	/// Interface implemented by classes that represent a symbol somehow (but are not 'actually' a symbol)
	/// 
	/// This is used so that 'special' classes like LiteralSymbol can be changed back into real symbols when they are quoted.
	/// </summary>
	public interface ISymbolic
	{
		Symbol Symbol { get; }
	}
}
