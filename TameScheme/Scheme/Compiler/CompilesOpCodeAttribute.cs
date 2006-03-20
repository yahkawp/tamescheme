using System;

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler
{
    /// <summary>
    /// Attribute that can be applied to IOpCode subclasses to specify which opcode they compile. Applying this attribute allows the
    /// compiler to load opcode compilers from an assembly without needing to declare them all manually.
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    public class CompilesOpAttribute : Attribute
    {
        public CompilesOpAttribute(Op op)
        {
            this.op = op;
        }

        public Op Op { get { return op; } }
        Op op;
    }
}
