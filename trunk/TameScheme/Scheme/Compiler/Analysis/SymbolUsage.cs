using System;
using System.Text;

namespace Tame.Scheme.Compiler.Analysis
{
    /// <summary>
    /// This class is stored in the local environments of the expression that we're compiling. It's used when
    /// looking at a BExpression when looking at the scope which a specific environment location is used.
    /// </summary>
    public class SymbolUsage
    {
        #region Initialisation
        #endregion

        #region Symbol information

        /// <summary>
        /// Set to true if this is a top level symbol
        /// </summary>
        private bool isTopLevel;

        /// <summary>
        /// Set to true if this symbol is assigned to a local variable, instead of being part of the environment
        /// passed to the function. (Symbols declared in environment functions have this property)
        /// </summary>
        private bool isLocal;

        /// <summary>
        /// Set to true if this symbol is a field in the containing class (used for lambda expressions: ie
        /// SProcedures.
        /// </summary>
        private bool isField;

        /// <summary>
        /// The first instruction in the BExpression that this symbol is used.
        /// </summary>
        private int firstUsage;

        /// <summary>
        /// The last time in the BExpression that this symbol is used.
        /// </summary>
        private int lastUsage;

        #endregion
    }
}
