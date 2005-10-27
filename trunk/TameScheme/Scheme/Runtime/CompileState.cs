using System;

using Tame.Scheme.Data;
using Tame.Scheme.Syntax;
using Tame.Scheme.Syntax.Transformer;

namespace Tame.Scheme.Runtime
{
	/// <summary>
	/// Represents the state of the compilation of some scheme into a BExpression at a given moment in time
	/// </summary>
	/// <remarks>
	/// This class contains state information passed through to ISyntax objects to enable them to access things like the top-level and
	/// local environments.
	/// </remarks>
	public sealed class CompileState
	{
		/// <summary>
		/// Constructs a default compile state
		/// </summary>
		public CompileState()
		{
			tempBinder = new Binder();
		}

		/// <summary>
		/// Constructs a new compile state using a previous state as a template
		/// </summary>
		/// <param name="oldState">The state to copy from</param>
		public CompileState(CompileState oldState)
		{
			this.tempBinder = oldState.tempBinder;
			this.topLevel = oldState.topLevel;
			this.local = oldState.local;
		}

		#region Data

		Binder tempBinder = null;								// The temporary value binder in use
		Data.Environment topLevel = null;						// The top-level environment (the environment in which the compile started)
		Data.Environment local = null;							// The local environment (a 'stub' environment, used to establish which variables are defined and when)

		#endregion

		#region Accessors

		/// <summary>
		/// Returns the temporary variable binder in use for this compilation pass.
		/// </summary>
		/// <remarks>
		/// The binder assigns temporary symbol names, and is primarily used when expanding macros.
		/// </remarks>
		public Binder TemporaryBinder
		{
			get
			{
				return tempBinder;
			}
		}

		/// <summary>
		/// Retrieves the top-level environment
		/// </summary>
		/// <remarks>
		/// This is the environment that the current compilation was initiated in.
		/// </remarks>
		public Data.Environment TopLevel
		{
			get { return topLevel; }
			set { topLevel = value; }
		}

		/// <summary>
		/// Retrieves the 'local' environment
		/// </summary>
		/// <remarks>
		/// This environment contains symbols bound to local values: for example those symbols that are bound
		/// while in a let expression. Variables in the local environment should be bound to Unspecified values:
		/// the behaviour for anything else is undefined.
		/// </remarks>
		public Data.Environment Local
		{
			get { return local; }
			set { local = value; }
		}

		#endregion
	}
}
