using System;

namespace Tame.Scheme.Data
{
	/// <summary>
	/// A LiteralSymbol works as for a symbol, but rather than performing a lookup in the current environment, its value is retrieved from
	/// a specific environment. This is mainly used for ensuring that the hygenic syntax rules are obeyed.
	/// </summary>
	/// <remarks>
	/// After syntax evaluation, no LiteralSymbols should be passed further along. This is important for operators like quote, which should
	/// transform them back to plain Symbols.
	/// </remarks>
	public sealed class LiteralSymbol : ISymbolic
	{
		/// <summary>
		/// Constructs a new literal symbol
		/// </summary>
		/// <param name="symbol">The symbol being represented</param>
		/// <param name="env">The environment it should be looked up in</param>
		public LiteralSymbol(ISymbolic symbol, Environment env)
		{
			this.symbol = symbol;
			this.environment = env;
		}

		#region Data

		ISymbolic symbol;
		Environment environment;

		#endregion

		#region ISymbolic Members

		public Symbol Symbol
		{
			get
			{
				return symbol.Symbol;
			}
		}

		public object HashValue
		{
			get
			{
				return symbol.HashValue;
			}
		}

		public Data.Environment Location
		{
			get
			{
				return environment;
			}
		}

		#endregion
		
		public Environment Environment
		{
			get { return environment; }
		}

		public override bool Equals(object obj)
		{
			if (obj is LiteralSymbol) return symbol.Equals(((LiteralSymbol)obj).Symbol);

			return false;
		}

		public override int GetHashCode()
		{
			return symbol.GetHashCode() ^ typeof(LiteralSymbol).GetHashCode();
		}

		public override string ToString()
		{
			return symbol.ToString();
		}
	}
}
