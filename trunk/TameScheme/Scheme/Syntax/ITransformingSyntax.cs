using System;
using Tame.Scheme.Runtime;

namespace Tame.Scheme.Syntax
{
    /// <summary>
    /// This interface can be implemented by syntax that works by transforming some scheme into some other scheme.
    /// </summary>
    /// <remarks>
    /// The bytecode compiler does not use this, but the IL compiler does as it needs to perform binding in a
    /// separate stage to compilation.
    /// </remarks>
    public interface ITransformingSyntax
    {
        /// <summary>
        /// Given the specified scheme, transforms it into a simpler form. This does not have to be a final form: further transformations
        /// may be required. The returned scheme should (as far as is possible) not produce loops that will eventually result in this call
        /// being made again with the same argument.
        /// </summary>
        /// <param name="scheme">
        /// The scheme that should be transformed by this syntax (no matching has been done other than the syntax symbol itself)
        /// </param>
        /// <param name="state">The CompileState to perform the translation under. Typically this can just be used to indicate the top-level environment.</param>
        /// <returns>The result of transforming this scheme through this syntax</returns>
        object TransformScheme(object scheme, CompileState state);
    }
}
