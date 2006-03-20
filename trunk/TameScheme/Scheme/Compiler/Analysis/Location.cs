using System;

namespace Tame.Scheme.Compiler.Analysis
{
    /// <summary>
    /// Represents the location of an environment value.
    /// </summary>
    public sealed class Location
    {
        #region Construction

        public Location(int offset, int level)
        {
            this.Offset = offset;
            this.Level = level;

            this.TopLevel = false;
            this.Symbol = null;
        }

        public Location(Data.Symbol symbol)
        {
            this.Offset = this.Level = -1;

            this.Symbol = symbol;
            this.TopLevel = true;
        }

        #endregion

        #region Variables

        /// <summary>
        /// True if this is a top-level environment.
        /// </summary>
        public readonly bool TopLevel;

        /// <summary>
        /// The offset into the environment of this value, or -1 if this is a top-level symbol.
        /// </summary>
        public readonly int Offset;

        /// <summary>
        /// The 'level' of the environment that stores this value, or -1 if this is a top-level symbol.
        /// </summary>
        public readonly int Level;

        /// <summary>
        /// The Data.Symbol used for this symbol if it's in a top-level environment.
        /// </summary>
        /// <remarks>
        /// Top-level symbols are unlike local symbols in that they can move around depending on where they are being used, so they
        /// are accessed by symbol number and not directly by location. This means that they are relatively slow to access compared
        /// to more local values.
        /// </remarks>
        public readonly Data.Symbol Symbol;

        #endregion

        #region Hashing, equality

        public override bool Equals(object obj)
        {
            if (obj == null || !(obj is Location)) return false;

            Location loc = (Location)obj;

            if (loc.TopLevel != TopLevel) return false;
            if (!TopLevel && loc.Level == Level && loc.Offset == Offset) return true;
            if (TopLevel && loc.Symbol.Equals(Symbol)) return true;

            return false;
        }

        public override int GetHashCode()
        {
            int hash = TopLevel ? 1 : 0;

            if (TopLevel)
            {
                hash ^= Level << 1;
                hash ^= Offset << 8;
            }
            else
            {
                hash ^= Symbol.GetHashCode() << 1;
            }

            return hash;
        }

        #endregion
    }
}
