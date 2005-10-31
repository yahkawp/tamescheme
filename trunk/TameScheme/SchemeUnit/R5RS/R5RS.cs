using System;
using Tame.Scheme.Runtime;

namespace SchemeUnit.R5RS
{
	/// <summary>
	/// Global environment used for the R5RS tests
	/// </summary>
	public class R5RS
	{
		public static Interpreter interpreter = new Interpreter();

		protected object Evaluate(string scheme) { return interpreter.Evaluate(scheme); }
		protected object Parse(string scheme) { return interpreter.ParseScheme(scheme); }
	}
}
