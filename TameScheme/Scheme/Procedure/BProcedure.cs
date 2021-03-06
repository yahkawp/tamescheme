// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interpreted scheme procedure                                 BProcedure.cs |
// +----------------------------------------------------------------------------+
// | Copyright (c) 2005 Andrew Hunter                                           |
// |                                                                            |
// | Permission is hereby granted, free of charge, to any person obtaining a    |
// | copy of this software and associated documentation files (the "Software"), |
// | to deal in the Software without restriction, including without limitation  |
// | the rights to use, copy, modify, merge, publish, distribute, sublicense,   |
// | and/or sell copies of the Software, and to permit persons to whom the      |
// | Software is furnished to do so, subject to the following conditions:       |
// |                                                                            |
// | The above copyright notice and this permission notice shall be included in |
// | all copies or substantial portions of the Software.                        |
// |                                                                            |
// | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR |
// | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   |
// | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    |
// | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER |
// | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    |
// | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        |
// | DEALINGS IN THE SOFTWARE.                                                  |
// +----------------------------------------------------------------------------+

using System;

namespace Tame.Scheme.Procedure
{
	/// <summary>
	/// BProcedure is a procedure implemented by a (interpreted) S-Expression
	/// </summary>
	/// <remarks>
	/// BProcedures are special-cased by the interpreter, but implement the IProcedure interface as well
	/// </remarks>
	public sealed class BProcedure : IProcedure, Runtime.IContextual
	{
		public BProcedure(Runtime.BExpression procedureDefinition)
		{
			this.procedureDefinition = procedureDefinition;
		}

		internal Runtime.BExpression procedureDefinition;

		public Runtime.BExpression ProcedureDefinition
		{
			get
			{
				return procedureDefinition;
			}
		}

		#region IProcedure Members

		public object Call(Data.Environment env, ref object[] args)
		{
			// Create the environment for this procedure
			Data.Environment procedureEnv = new Data.Environment(env);

			// Build a continuation
			Runtime.BContinuation cont = new Runtime.BContinuation(procedureDefinition, procedureEnv, args);

			// Run it, and return the result
			return cont.Continue();
		}

		#endregion

		public override string ToString()
		{
			return procedureDefinition.ToString();
		}

		#region IContextual Members

		Data.Environment env = null;

		public Runtime.IContextual PlaceInContext(Data.Environment context)
		{
			// Construct a new procedure in the specified context (which has the same behaviour as this one)
			BProcedure newProcedure = new BProcedure(procedureDefinition);
			newProcedure.env = context;

			// Return the result
			return newProcedure;
		}

		public Data.Environment Environment { get { return env; } }

		#endregion
	}
}
