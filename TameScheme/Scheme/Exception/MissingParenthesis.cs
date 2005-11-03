using System;
using System.Text;

using Tame.Scheme.Runtime.Parse;

namespace Tame.Scheme.Exception
{
    public class MissingParenthesis : SyntaxError
    {
        public MissingParenthesis()
            : base("Missing parenthesis")
        {
        }

        public MissingParenthesis(string reason)
            : base(reason)
        {
        }

        public MissingParenthesis(string reason, TokenReader context)
            : base(reason, context)
        {
        }
    }
}
