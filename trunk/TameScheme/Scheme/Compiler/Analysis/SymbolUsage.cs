using System;
using System.Reflection;

namespace Tame.Scheme.Compiler.Analysis
{
    /// <summary>
    /// This class is stored in the local environments of the expression that we're compiling. It's used when
    /// looking at a BExpression when looking at the scope which a specific environment location is used.
    /// </summary>
    public class SymbolUsage
    {
        #region Initialisation

        public SymbolUsage(Location where, FieldInfo field)
        {
            this.field = field;
            this.localVariable = -1;
            this.where = where;
        }

        public SymbolUsage(Location where, int localVariable)
        {
            this.localVariable = localVariable;
            this.field = null;
            this.where = where;
        }

        public SymbolUsage(Location where)
        {
            this.localVariable = -1;
            this.field = null;
            this.where = where;
        }

        #endregion

        #region Symbol information

        private readonly FieldInfo field;
        private readonly int localVariable;

        public readonly Location where;

        public FieldInfo Field
        {
            get
            {
                if (field == null) throw new InvalidOperationException("Tried to retrieve a FieldInfo structure for a variable that is stored in a local location (or that is invalid)");
                return field;
            }
        }

        public int LocalVariable
        {
            get
            {
                if (localVariable < 0) throw new InvalidOperationException("Tried to retrieve a local variable location for a variable that is stored in a class field (or that is invalid)");
                return localVariable;
            }
        }

        public bool IsField
        {
            get { return field != null; }
        }

        public bool IsLocalVariable
        {
            get { return localVariable >= 0; }
        }

        public bool IsInvalid
        {
            get { return field == null && localVariable < 0; }
        }

        #endregion
    }
}
