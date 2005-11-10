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
using System.Collections.Specialized;
using System.Reflection;

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
            topLevel = MakeStandardEnvironment();
		}

		/// <summary>
		/// Constructs an interpreter using a specific top-level environment
		/// </summary>
		/// <param name="env">The top-level environment to use. The default procedures, syntax, etc is not defined</param>
		public Interpreter(Data.Environment env)
		{
			topLevel = env;
        }

        #region Specifying the language

        // These functions allow us to specify what the 'baseline' scheme implementation we want is. 
        // For instance, we can import all the syntax and procedures installed in a particular assembly or specify that we want no
        // procedures that can open files.
        //
        // Default is just the procedures and syntax defined by the TameScheme module

        private class AssemblyItem
        {
            public AssemblyItem(string name, object definition)
            {
                this.Name = name;
                this.Definition = definition;
            }

            public string Name;
            public object Definition;
        }

        static Hashtable assembliesToItems = null;                             // Maps assemblies to the items they define
        static Hashtable activeAssemblies = null;                              // Contains the 'active' assemblies as keys

        static Hashtable environmentTemplate = null;                           // The template for the environment hashtable
        static ArrayList environmentValuesTemplate = null;                     // The template for the values the environment should contain

        static Data.Environment MakeStandardEnvironment()
        {
            if (assembliesToItems == null) MakeStandardDefinitions();

            lock (typeof(Interpreter))
            {
                if (environmentTemplate == null)
                {
                    // Rebuild the environment template
                    environmentTemplate = new Hashtable();
                    environmentValuesTemplate = new ArrayList();

                    foreach (Assembly asm in activeAssemblies.Keys)
                    {
                        foreach (AssemblyItem item in (ArrayList)assembliesToItems[asm])
                        {
                            // Add this item to the template
                            object hash = new Symbol(item.Name).HashValue;

                            environmentTemplate.Add(hash, environmentValuesTemplate.Count);
                            environmentValuesTemplate.Add(item.Definition);
                        }
                    }
                }

                // Create a new environment from the template
                HybridDictionary symbolsToOffsets = new HybridDictionary();

                foreach (object hash in environmentTemplate.Keys)
                {
                    symbolsToOffsets[hash] = environmentTemplate[hash];
                }

                return new Data.Environment(symbolsToOffsets, environmentValuesTemplate, null);
            }
        }

        static void MakeStandardDefinitions()
        {
            if (assembliesToItems == null)
            {
                assembliesToItems = new Hashtable();
                activeAssemblies = new Hashtable();

                // Use the definitions in the entry assembly and this assembly by default
                UseDefinitionsInAssembly(Assembly.GetEntryAssembly());
                UseMyDefinitions();
            }
        }

        /// <summary>
        /// Looks for procedure and syntax definitions in the given assembly and adds them to the 'standard' set of definitions for new intepreters
        /// </summary>
        /// <param name="assembly">The assembly to take definitions from</param>
        /// <remarks>
        /// By default, the definitions in the entry assembly and this one are used. This can also be used to re-enable assemblies disabled by
        /// DisableAssembly()
        /// </remarks>
        public static void UseDefinitionsInAssembly(Assembly assembly)
        {
            // TODO: exceptions for IProcedureGroup/ISyntaxGroup classes that don't implement Definition
            lock (typeof(Interpreter))
            {
                environmentTemplate = null;

                if (assembliesToItems == null) MakeStandardDefinitions();

                if (!assembliesToItems.Contains(assembly))
                {
                    // Get the types from this assembly
                    Type[] assemblyTypes = assembly.GetExportedTypes();

                    // Build a list of the items this assembly defines
                    ArrayList itemList = new ArrayList();

                    foreach (Type t in assemblyTypes)
                    {
                        if (!t.IsClass) continue;

                        PreferredNameAttribute prefName = (PreferredNameAttribute)Attribute.GetCustomAttribute(t, typeof(PreferredNameAttribute));
                        ConstructorInfo simpleConstructor = t.GetConstructor(new Type[0]);

                        if (typeof(Procedure.IProcedure).IsAssignableFrom(t))
                        {
                            // This is a procedure
                            if (prefName != null && simpleConstructor != null)
                            {
                                // This has a singleton implementation style, which we shall now instantiate
                                IProcedure definition = (IProcedure)simpleConstructor.Invoke(new object[0]);

                                // Create the item for this procedure definition
                                AssemblyItem newItem = new AssemblyItem(prefName.PreferredName, definition);
                                itemList.Add(newItem);
                            }
                        }

                        if (typeof(Procedure.IProcedureGroup).IsAssignableFrom(t))
                        {
                            // This is a class that provides a group of definitions
                            try
                            {
                                ProcedureDefinition[] procs = (ProcedureDefinition[])t.GetProperty("Definitions").GetGetMethod().Invoke(null, null);

                                // Create an item for each one
                                foreach (ProcedureDefinition proc in procs)
                                {
                                    itemList.Add(new AssemblyItem(proc.PreferredName, proc.Procedure));
                                }
                            }
                            catch (TargetInvocationException e)
                            {
                                throw e.GetBaseException();
                            }
                        }

                        if (typeof(Syntax.ISyntax).IsAssignableFrom(t))
                        {
                            // This implements some syntax
                            Syntax.SchemeSyntaxAttribute primitiveSyntax = (SchemeSyntaxAttribute)Attribute.GetCustomAttribute(t, typeof(SchemeSyntaxAttribute));

                            if (prefName != null && primitiveSyntax != null && simpleConstructor != null)
                            {
                                // This is a singleton syntax object, we can instantiate it
                                ISyntax syntax = (ISyntax)simpleConstructor.Invoke(new object[0]);
                                SchemeSyntax definition = new SchemeSyntax(primitiveSyntax.Syntax, syntax);

                                // Create the item for this procedure definition
                                AssemblyItem newItem = new AssemblyItem(prefName.PreferredName, definition);
                                itemList.Add(newItem);
                            }
                        }

                        if (typeof(Syntax.SchemeSyntax).IsAssignableFrom(t))
                        {
                            // This is another way to implement syntax, specifying both the syntax to match and the code generator
                            if (prefName != null && simpleConstructor != null)
                            {
                                // Instantiate this class
                                SchemeSyntax definition = (SchemeSyntax)simpleConstructor.Invoke(new object[0]);

                                // Create the item for this procedure definition
                                AssemblyItem newItem = new AssemblyItem(prefName.PreferredName, definition);
                                itemList.Add(newItem);
                            }
                        }

                        if (typeof(Syntax.ISyntaxGroup).IsAssignableFrom(t))
                        {
                            try
                            {
                                // This is a class that provides a group of definitions
                                SyntaxDefinition[] syntaxes = (SyntaxDefinition[])t.GetProperty("Definitions").GetGetMethod().Invoke(null, null);
                                Syntax.SchemeSyntaxAttribute primitiveSyntax = (SchemeSyntaxAttribute)Attribute.GetCustomAttribute(t, typeof(SchemeSyntaxAttribute));

                                // Create an item for each one
                                foreach (SyntaxDefinition syntax in syntaxes)
                                {
                                    itemList.Add(new AssemblyItem(syntax.PreferredName, new SchemeSyntax(primitiveSyntax.Syntax, syntax.Syntax)));
                                }
                            }
                            catch (TargetInvocationException e)
                            {
                                throw e.GetBaseException();
                            }
                        }
                    }

                    // Store the results
                    assembliesToItems[assembly] = itemList;
                }

                // Activate this assembly
                activeAssemblies[assembly] = true;
            }
        }

        /// <summary>
        /// Adds the definitions from the calling assembly
        /// </summary>
        public static void UseMyDefinitions()
        {
            UseDefinitionsInAssembly(Assembly.GetCallingAssembly());
        }

        /// <summary>
        /// Switches off a given assembly, so no definitions will be used from it in the standard environment for new interpreters
        /// </summary>
        /// <param name="assembly"></param>
        public static void DisableAssembly(Assembly assembly)
        {
            lock (typeof(Interpreter))
            {
                environmentTemplate = null;

                if (activeAssemblies != null && activeAssemblies.Contains(assembly))
                    activeAssemblies.Remove(assembly);
            }
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
            PreferredNameAttribute prefName = (PreferredNameAttribute)Attribute.GetCustomAttribute(procedure.GetType(), typeof(PreferredNameAttribute));

			if (prefName == null) throw new System.Exception("Can't call DefineProcedure on an anonymous procedure");

			topLevel[prefName.PreferredName] = procedure;
		}

		public void DefineSyntax(ISyntax syntax)
		{
            PreferredNameAttribute prefName = (PreferredNameAttribute)Attribute.GetCustomAttribute(syntax.GetType(), typeof(PreferredNameAttribute));
            if (prefName == null) throw new System.Exception("Can't call DefineSyntax on an anonymous syntax object");

			DefineSyntax(prefName.PreferredName, syntax);
		}

		public void DefineSyntax(string symbolName, ISyntax syntax)
		{
            Syntax.SchemeSyntaxAttribute primitiveSyntax = (SchemeSyntaxAttribute)Attribute.GetCustomAttribute(syntax.GetType(), typeof(SchemeSyntaxAttribute));
			if (primitiveSyntax == null) throw new System.Exception("Can't call DefineSyntax on a syntax object that doesn't define any syntax");

			topLevel[symbolName] = new SchemeSyntax(primitiveSyntax.Syntax, syntax);
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

				// Find the point at which this list becomes circular (if it becomes circular)
				Data.Pair loopHead = ((Data.Pair)obj).LoopHead();
				bool visitedLoopHead = false;

				object thisItem = obj;
				while (thisItem != null)
				{
					if (thisItem == loopHead)
					{
						// Circular lists are written (x y z #[...]). If only a portion of a list is circular, they are written (x . (y z #[...]))
						if (visitedLoopHead)
						{
							res.Append("#[...]");
							if (obj != loopHead) res.Append(")");
							break;
						}
						else
						{
							visitedLoopHead = true;
							if (obj != loopHead) res.Append(". (");
						}
					}

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
			else if (obj is int || obj is long || obj is INumber)
			{
				// Convert these numbers to a string
				return obj.ToString();
			}
            else if (obj is float || obj is double)
            {
                // Convert these floating point numbers to a string
                double val = (double)obj;
                double abs = Math.Abs(val);

                if (abs == 0 || (abs >= 0.00001 && abs < 100000000.0))
                {
                    return val.ToString("0.0##########");
                }
                else
                {
                    return val.ToString("0.0##########e+0");
                }
            }
            else if (obj is decimal)
            {
                // Decimals are exact, so make that clear
                decimal val = (decimal)obj;
                decimal abs = val;
                if (abs < 0) abs = -abs;

                if (abs == 0 || (abs >= (decimal)0.00001 && abs < (decimal)100000000.0))
                {
                    return "#e" + val.ToString("0.0##########");
                }
                else
                {
                    return "#e" + val.ToString("0.0##########e+0");
                }
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
                res.Remove(res.Length - 1, 1);
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

		#region Scheme behaviour

		/// <summary>
		/// Evaluates if two objects are eqv?
		/// </summary>
		/// <returns>True if the two objects are eqv?</returns>
		public static bool Eqv(object a, object b)
		{
			// Trivial case (also covers pairs, strings, vectors)
			if (a == b) return true;

			Type typeA = a.GetType();
			Type typeB = b.GetType();

            if (a is INumber) a = ((INumber)a).Simplify();
            if (b is INumber) b = ((INumber)b).Simplify();

			// Numeric types (exact)
			if (a is int || a is decimal || a is long)
			{
				if (b is int || b is decimal || b is long)
				{
					return a.Equals(b);
				}
                else if (b is Data.Number.Rational && a is decimal)
                {
                    return new Data.Number.Rational((decimal)a).IsEqualTo((Data.Number.Rational)b);
                }
				return false;
			}
			else if (a is Data.Number.Rational)
			{
				if (b is Data.Number.Rational) 
					return a.Equals(b);
				else if (b is int)
					return a.Equals(new Data.Number.Rational((int)b, 1));
				else if (b is long)
					return a.Equals(new Data.Number.Rational((long)b, 1));
                else if (b is decimal)
                    return a.Equals(new Data.Number.Rational((decimal)b));

				return false;
			}
            else if (a is Data.Number.RationalComplex)
            {
                if (b is Data.Number.RationalComplex) return a.Equals(b);

                return false;
            }

			// Numeric types (inexact)
			else if (a is float || a is double)
			{
				if (b is float || b is double)
				{
					return a.Equals(b);
				}
				return false;
			}
			else if (a is Data.Number.Complex)
			{
                if (b is Data.Number.Complex) return a.Equals(b);

                return false;
			}

			// Objects must be the same type if they're not numbers
			else if (!typeA.Equals(typeB)) return false;

			else if (a is Data.ISymbolic) return ((Data.ISymbolic)a).HashValue.Equals(((Data.ISymbolic)b).HashValue);
			else if (a is bool) return ((bool)a)==((bool)b);
			else if (a is char) return ((char)a)==((char)b);

			// Everything else isn't eqv?
			return false;
		}

		/// <summary>
		/// Evaluates if two objects are eq?
		/// </summary>
		/// <returns>True if the objects are eq?</returns>
		public static bool Eq(object a, object b)
		{
			// Trivial case (also covers pairs, strings, vectors)
			if (a == b) return true;

			Type typeA = a.GetType();
			Type typeB = b.GetType();

			if (!typeA.Equals(typeB)) return false;

			else if (a is Data.ISymbolic) return ((Data.ISymbolic)a).HashValue.Equals(((Data.ISymbolic)b).HashValue);
			else if (a is bool) return ((bool)a)==((bool)b);
			else if (a is char) return ((char)a)==((char)b);

			// Everything else isn't eq?
			return false;
		}

		/// <summary>
		/// Evaluates if two objects are equal?
		/// </summary>
		/// <returns>True if the values are equal?</returns>
		/// <remarks>As an extention to R5RS, handles circular lists</remarks>
		public static bool Equal(object a, object b)
		{
			if ((a is Pair) && (b is Pair))
			{
				// Pair enumerators take care of circular lists
				IEnumerator aEnumerator = ((Pair)a).GetEnumerator();
				IEnumerator bEnumerator = ((Pair)b).GetEnumerator();

				while (aEnumerator.MoveNext())
				{
					if (!bEnumerator.MoveNext()) return false;
					if (!Equal(aEnumerator.Current, bEnumerator.Current)) return false;
				}

				if (bEnumerator.MoveNext()) return false;

				return true;
			}
			else if ((a is ICollection) && (b is ICollection))
			{
				IEnumerator aEnumerator = ((ICollection)a).GetEnumerator();
				IEnumerator bEnumerator = ((ICollection)b).GetEnumerator();

				while (aEnumerator.MoveNext())
				{
					if (!bEnumerator.MoveNext()) return false;
					if (!Equal(aEnumerator.Current, bEnumerator.Current)) return false;
				}

				if (bEnumerator.MoveNext()) return false;

				return true;
			}
			else if ((a is string) && (b is string))
			{
				return a.Equals(b);
			}
			else
			{
				return Eqv(a, b);
			}
		}

		#endregion
	}
}
