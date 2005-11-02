using System;

using Tame.Scheme.Syntax;
using Tame.Scheme.Runtime;
using Tame.Scheme;

namespace Tame.SchemeTest
{
    [PreferredName("show-expression"),
     SchemeSyntax("()", "(expression)")]
    public class ShowExpression : ISyntax
    {
        public ShowExpression()
        {
        }

        #region ISyntax Members

        public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, Tame.Scheme.Runtime.CompileState state, int syntaxMatch)
        {
            BExpression expr = BExpression.BuildExpression(env["expression"].Value, state);
            Console.Out.WriteLine(expr);

            return new BExpression(new Operation(Op.Push, expr));
        }

        #endregion
    }
}
