using System;

using Tame.Scheme.Data;
using Tame.Scheme.Runtime;
using Tame.Scheme.Syntax;
using Tame.Scheme.Syntax.Transformer;
using Tame.Scheme;

namespace Tame.SchemeTest
{
	/// <summary>
	/// Aid memoir: shows the syntax tree generated for particular syntax pattern and input value
	/// </summary>
	[PreferredName("show-syntax"), SchemeSyntax("()", "(pattern matchAgainst)", "(pattern matchAgainst template)")]
	public class ShowSyntax : ISyntax
	{
		public ShowSyntax()
		{
		}

		#region ISyntax Members

		public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			BExpression res;

			object pattern = env["pattern"].Value;
			object matchAgainst = env["matchAgainst"].Value;
			object template = null;

			if (env["template"] != null) template = env["template"].Value;

			SyntaxElement matcher = SyntaxElement.MakeElementFromScheme(pattern, new System.Collections.Hashtable());
			SyntaxEnvironment newEnv = new SyntaxEnvironment();

			if (template == null)
			{
				if (matcher.Match(matchAgainst, state, out newEnv))
				{
					res = new BExpression(new Operation(Op.Push, newEnv.SyntaxTree));
				}
				else
				{
					res = new BExpression(new Operation(Op.Push, new Symbol("no-match")));
				}
			}
			else
			{
				SyntaxCompiler compiler = new SyntaxCompiler(matcher);

				if (matcher.Match(matchAgainst, state, out newEnv))
				{
					Transformation syntaxTransformer = compiler.Compile(template, state.TopLevel);

					Console.Out.WriteLine(newEnv.SyntaxTree.ToString());
					Console.Out.WriteLine(syntaxTransformer.ToString());

					object syntaxResult = syntaxTransformer.Transform(newEnv.SyntaxTree);

					Console.Out.WriteLine(syntaxResult.ToString());

					Binder testBinder = new Binder();

					syntaxResult = testBinder.BindScheme(syntaxResult, state);

					res = new BExpression(new Operation(Op.Push, syntaxResult));
				}
				else
				{
					res = new BExpression(new Operation(Op.Push, new Symbol("no-match")));
				}
			}

			return res;
		}

		#endregion
	}
}
