// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme interpreter class                                    Interpreter.cs |
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
using System.IO;
using System.Text;
using System.Collections;

using Tame.Scheme.Data;
using Tame.Scheme.Runtime;
using Tame.Scheme.Procedure;
using Tame.Scheme.Syntax;

namespace Tame.Scheme.Runtime
{
	/// <summary>
	/// An implementation of a scheme interpreter. The interpreter is really just a convienience class: you can perform these functions
	/// just as well by manually creating continuations, etc. However, things like building the top-level environment are liable to be
	/// much more time consuming this way. The interpreter provides the basic building blocks of the read-eval-print loop.
	/// </summary>
	public class Interpreter
	{
		/// <summary>
		/// Constructs an interpreter and the default top-level environment
		/// </summary>
		public Interpreter()
		{
			DefineStandardProcedures(5, true);
			DefineStandardSyntax(5, true);
		}

		/// <summary>
		/// Constructs an interpreter using a specific top-level environment
		/// </summary>
		/// <param name="env">The top-level environment to use. The default procedures, syntax, etc is not defined</param>
		public Interpreter(Data.Environment env)
		{
			topLevel = env;
		}

		#region The standard environment

		#region Static procedure definitions

		// Arithmetic procedures
		static IProcedure add = new Procedure.Arithmetic.Add();
		static IProcedure subtract = new Procedure.Arithmetic.Subtract();
		static IProcedure multiply = new Procedure.Arithmetic.Multiply();
		static IProcedure divide = new Procedure.Arithmetic.Divide();

		// Comparison procedures
		static IProcedure numEquals = new Procedure.Comparison.NumericEquals();
		static IProcedure greaterThan = new Procedure.Comparison.GreaterThan();
		static IProcedure greaterThanOrEqualTo = new Procedure.Comparison.GreaterThanOrEqualTo();
		static IProcedure lessThan = new Procedure.Comparison.LessThan();
		static IProcedure lessThanOrEqualTo = new Procedure.Comparison.LessThanOrEqualTo();

		#endregion

		#region Static syntax defintions

		// Quoting
		static ISyntax schemeQuote = new Syntax.Primitives.Quote();

		// Conditionals
		static ISyntax schemeIf = new Syntax.Primitives.If();

		// Flow control
		static ISyntax schemeBegin = new Syntax.Library.Begin();

		// Definitions
		static ISyntax schemeLambda = new Syntax.Primitives.Lambda();
		static ISyntax schemeDefine = new Syntax.Primitives.Define();
		static ISyntax schemeDefineSyntax = new Syntax.Primitives.DefineSyntax();
		static ISyntax schemeLet = new Syntax.Primitives.Let(Syntax.Primitives.Let.Type.Let);
		static ISyntax schemeLetStar = new Syntax.Primitives.Let(Syntax.Primitives.Let.Type.LetStar);
		static ISyntax schemeLetrec = new Syntax.Primitives.Let(Syntax.Primitives.Let.Type.Letrec);

		#endregion

		/// <summary>
		/// Defines the standard procedures in the top-level environment.
		/// </summary>
		/// <param name="revision">The revision of the Revised Report on the Scheme language to implement. Currently only version 5 is supported.</param>
		/// <param name="extensions">Whether or not any extensions to the spec should be defined</param>
		/// <remarks>
		/// Subclasses that want a non-standard environment can override this class to declare their own set of procedures (or call this class then undefine/add procedures as required)
		/// </remarks>
		public virtual void DefineStandardProcedures(int revision, bool extensions)
		{
			if (revision != 5) throw new System.Exception("Unsupported revision of the scheme language requested");

			// Arithmetic procedures
			DefineProcedure(add);
			DefineProcedure(subtract);
			DefineProcedure(multiply);
			DefineProcedure(divide);

			// Comparison procedures
			DefineProcedure(numEquals);
			DefineProcedure(lessThan);
			DefineProcedure(lessThanOrEqualTo);
			DefineProcedure(greaterThanOrEqualTo);
			DefineProcedure(greaterThan);
		}

		/// <summary>
		/// Defines the standard syntax in the top-level environment.
		/// </summary>
		/// <param name="revision">The revision of the Revised Report on the Scheme language to implement. Currently only version 5 is supported.</param>
		/// <param name="extensions">Whether or not any extensions to the spec should be defined</param>
		/// <remarks>
		/// Subclasses that want a non-standard environment can override this class to declare their own set of procedures (or call this class then undefine/add procedures as required)
		/// </remarks>
		public virtual void DefineStandardSyntax(int revision, bool extensions)
		{
			// Quoting
			DefineSyntax(schemeQuote);

			// Conditions
			DefineSyntax(schemeIf);

			// Flow control
			DefineSyntax(schemeBegin);

			// Definitions
			DefineSyntax(schemeLambda);
			DefineSyntax(schemeDefine);
			DefineSyntax(schemeDefineSyntax);

			DefineSyntax("let", schemeLet);
			DefineSyntax("let*", schemeLetStar);
			DefineSyntax("letrec", schemeLetrec);
		}

		#endregion

		#region The top-level environment

		protected Data.Environment topLevel = new Data.Environment();

		/// <summary>
		/// The top-level environment for this interpreter
		/// </summary>
		public Data.Environment TopLevelEnvironment
		{
			get { return topLevel; }
		}

		/// <summary>
		/// Defines a procedure in the top-level environment
		/// </summary>
		/// <param name="procedure">The procedure to define (it should have a PreferredName attribute)</param>
		public void DefineProcedure(IProcedure procedure)
		{
			Procedure.PreferredNameAttribute[] prefNames = (Procedure.PreferredNameAttribute[])procedure.GetType().GetCustomAttributes(typeof(Procedure.PreferredNameAttribute), true);

			if (prefNames.Length <= 0) throw new System.Exception("Can't call DefineProcedure on an anonymous procedure");

			topLevel[prefNames[0].PreferredName] = procedure;
		}

		public void DefineSyntax(ISyntax syntax)
		{
			Procedure.PreferredNameAttribute[] prefNames = (Procedure.PreferredNameAttribute[])syntax.GetType().GetCustomAttributes(typeof(Procedure.PreferredNameAttribute), true);
			if (prefNames.Length <= 0) throw new System.Exception("Can't call DefineSyntax on an anonymous syntax object");

			DefineSyntax(prefNames[0].PreferredName, syntax);
		}

		public void DefineSyntax(string symbolName, ISyntax syntax)
		{
			Syntax.SchemeSyntaxAttribute[] primitiveSyntax = (Syntax.SchemeSyntaxAttribute[])syntax.GetType().GetCustomAttributes(typeof(Syntax.SchemeSyntaxAttribute), true);
			if (primitiveSyntax.Length <= 0) throw new System.Exception("Can't call DefineSyntax on a syntax object that doesn't define any syntax");

			topLevel[symbolName] = new SchemeSyntax(primitiveSyntax[0].Syntax, syntax);
		}

		#endregion

		#region Reading

		/// <summary>
		/// The parser object that's used to turn scheme into objects. Subclasses might change this value to use a custom parser.
		/// </summary>
		protected Parse.Parser parser = new Parse.Parser();

		/// <summary>
		/// Creates a token reader from a TextReader object. Subclasses can override this to use custom tokens.
		/// </summary>
		/// <param name="reader">The text reader to read tokens from</param>
		/// <returns>A TokenReader</returns>
		protected virtual Parse.TokenReader MakeTokenReader(TextReader reader)
		{
			return new Parse.TokenReader(reader);
		}

		/// <summary>
		/// Creates a token reader from a string. Calls MakeTokenReader(TextReader reader).
		/// </summary>
		/// <param name="someScheme">The scheme string to create a reader from</param>
		/// <returns>A token reader</returns>
		protected virtual Parse.TokenReader MakeTokenReader(string someScheme)
		{
			return new Parse.TokenReader(new StringReader(someScheme));
		}

		/// <summary>
		/// Parses the scheme expression given by the string scheme
		/// </summary>
		/// <param name="scheme">The scheme expression to parse</param>
		/// <returns>A .NET object representing the parsed scheme</returns>
		public object ParseScheme(string scheme)
		{
			Parse.TokenReader reader = MakeTokenReader(scheme);

			return parser.Parse(reader);
		}

		#endregion

		#region Evaluation

		/// <summary>
		/// Treats the given string as a scheme expression, and compiles and evaluates it
		/// </summary>
		/// <param name="schemeExpression">The expression to evaluate</param>
		/// <returns>The result of parsing, compiling and evaluating the given expression</returns>
		/// <remarks>Note that Evaluate((object)"string") has a noticably different effect</remarks>
		public object Evaluate(string schemeExpression)
		{
			return Evaluate(ParseScheme(schemeExpression));
		}

		/// <summary>
		/// Interprets an BExpression, evaluating it in the top-level environment
		/// </summary>
		/// <param name="expression">The BExpression to evaluate</param>
		/// <returns>The result of evaluating the expression</returns>
		public object Evaluate(BExpression expression)
		{
			lock (this)
			{
				// Prepare the expression for evaluation
				expression.RemoveLabels();								// Strip out any labels that the expression might have
				expression = expression.RemoveNops();					// Remove any No-Ops that the expression might have

				Console.WriteLine(expression.ToString());

				BContinuation continuation = new BContinuation(expression, topLevel);

				return continuation.Continue();
			}
		}

		/// <summary>
		/// Evaluates a scheme expression in the top-level environment
		/// </summary>
		/// <param name="expression">An object containing the expression to evaluate</param>
		/// <returns>The result of the evaluation</returns>
		public object Evaluate(object expression)
		{
			lock (this)
			{
				BExpression sexpr = BExpression.BuildExpression(expression, topLevel);

				return Evaluate(sexpr);
			}
		}

		#endregion

		#region Printing

		/// <summary>
		/// Converts a .NET object to a scheme expression. (Usually reversible with ParseScheme, but not always)
		/// </summary>
		/// <param name="obj">The object to convert</param>
		/// <returns>The scheme expression.</returns>
		public static string ToString(object obj)
		{
			if (obj == null) 
			{
				// Special case: the empty list
				return "()";
			}

			if (obj is bool)
			{
				// Boolean: #t or #f
				switch ((bool)obj)
				{
					case true:
						return "#t";
					case false:
						return "#f";
					default:
						return "#this never happens but the C# compiler thinks it can";
				}
			}
			else if (obj is Data.Pair)
			{
				// Pair: (whatever ...)
				StringBuilder res = new StringBuilder("(");

				object thisItem = obj;
				while (thisItem != null)
				{
					if (thisItem is Data.Pair)
					{
						// Part of the list
						Data.Pair thisPair = (Data.Pair)thisItem;

						res.Append(ToString(thisPair.Car));
						thisItem = thisPair.Cdr;

						if (thisItem != null) res.Append(" ");
					}
					else
					{
						// Improper end to the list
						res.Append(". ");
						res.Append(ToString(thisItem));

						thisItem = null;
					}
				}

				res.Append(")");

				return res.ToString();
			}
			else if (obj is Data.Symbol)
			{
				// Just convert symbols to strings (other ISymbolic objects are treated differently)
				return obj.ToString();
			}
			else if (obj is Data.Unspecified)
			{
				// The unspecified object
				return "#unspecified";
			}
			else if (obj is string || obj is StringBuilder)
			{
				// Strings are pretty much passed as-is, except for " -> \" and \ -> \\
				string stringIn = obj.ToString();
				StringBuilder res = new StringBuilder("\"");

				foreach (char c in stringIn)
				{
					if (c == '"' || c == '\\')
					{
						res.Append("\\");
					}

					res.Append(c);
				}

				res.Append("\"");
				return res.ToString();
			}
			else if (obj is int || obj is float || obj is double || obj is long || obj is INumber)
			{
				// Convert these numbers to a string
				return obj.ToString();
			}
			else if (obj is decimal)
			{
				// Decimals are exact, so make that clear
				return "#e" + obj.ToString();
			}
			else if (obj is IList)
			{
				// ILists are vectors
				IList vector = (IList)obj;

				StringBuilder res = new StringBuilder("#(");

				// Convert each item to a string
				foreach (object o in vector)
				{
					res.Append(ToString(o));
					res.Append(" ");
				}

				// Remove the last space, and add a closing bracket
				res.Remove(res.Length-1, 1);
				res.Append(")");

				return res.ToString();
			}
			else
			{
				// General .NET object
				return "#[" + obj.GetType().FullName + " " + obj.ToString() + "]";
			}
		}

		#endregion
	}
}
