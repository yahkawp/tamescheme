using System;
using System.Reflection;

namespace Tame.Scheme.Compiler.Analysis
{
    /// <summary>
    /// This class defines the current state of the IL compiler. It is used, in particular, to store information about where
    /// variables can be located.
    /// </summary>
    /// <remarks>
    /// The state is created with no environments pushed.
    /// </remarks>
    public class State
    {

        #region Variables

        /// <summary>
        /// The current 'level' of this environment
        /// </summary>
        private int level = 0;

        /// <summary>
        /// Retrieves the 'level' of this environment (how many times it has been pushed: this is the level value that should be used for
        /// locations that are considered to be in the most recently pushed environment)
        /// </summary>
        public int Level
        {
            get
            {
                return level;
            }
        }

        #endregion

        #region Pushing/popping the environment

        /// <summary>
        /// Called when a new environment is pushed onto the stack (which might happen when we move to compiling a SProcedure)
        /// </summary>
        /// <param name="invalid">
        /// Mark any symbols in the current environment as 'invalid' and unavailable in the new environment.
        /// </param>
        /// <remarks>
        /// It is possible to call LocalLocation/FieldLocation on variables that are on levels 'above' this one.
        /// If you do this, 
        /// </remarks>
        public void PushEnvironment(bool invalid)
        {
            // Increase the level
            level++;
        }

        /// <summary>
        /// Called when an environment is finished with: ie, when the environment is finished with.
        /// </summary>
        public void PopEnvironment()
        {
            // Sanity check
            if (level <= 0)
            {
                throw new InvalidOperationException("The compiler tried to pop the top-level environment (which makes no sense)");
            }

            // Decrease the level
            level--;
        }

        #endregion

        #region Dealing with environment locations

        /// <summary>
        /// Marks the given environment location as being stored in a local variable.
        /// </summary>
        /// <param name="where">Specifies the location (in BExpression terms) of this variable</param>
        /// <param name="var">The local variable number that is being used to store this value</param>
        public void LocalLocation(Location where, int var)
        {
        }

        /// <summary>
        /// Marks the given location as being stored in a field.
        /// </summary>
        /// <param name="where">The environment location that's represented by this field.</param>
        /// <param name="whichField">The field that will store this location.</param>
        public void FieldLocation(Location where, FieldInfo whichField)
        {
        }

        #endregion
    }
}
