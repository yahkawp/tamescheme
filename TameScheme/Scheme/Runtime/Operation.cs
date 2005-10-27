// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme operation class                                        Operation.cs |
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
using System.Collections;
using System.Collections.Specialized;

// TODO: tail calls are completely broken. My idea was a bad one :-(

namespace Tame.Scheme.Runtime
{
	/// <summary>
	/// The Op enum represents the available operations
	/// </summary>
	public enum Op
	{
		Nop,							// nop - do nothing

		// Basic operations
		Pop,							// pop - pops an object from the stack and discards it
		Push,							// push a - pushes object a onto the stack
		PushContext,					// push-context a - pushes object a (which must implement IContextual) to the stack
		PushBindingValue,				// push-binding-value a - pushes the value of a literal binding onto the stack
		PushRelativeValue,				// push-relative-value a - pushes the value of a relative binding onto the stack
		PushFrameItem,					// push-frame-item a - pushes frame item a (an int) onto the stack
		PushFrameList,					// push-frame-list a - pushes frame list a onto the stack (ie everything after a in the frame as a list)
		DefineBinding,					// define-binding a - defines a (an Environment.Binding) to the value of the top object on the stack
		DefineRelative,					// define-relative a - defines a (an Environment.RelativeBinding, relative to the current uppermost environment) to the value of the top object on the stack
		CallIProcedure,					// call-iprocedure a - calls the scheme procedure on the top of the stack using a values from the stack as arguments. Pushes a new frame and environment. tail-call-iprocedure is the tail equivalent
		PopEnvironment,					// pop-environment - pops the last environment from the stack
		UseEnvironment,					// use-environment a - a (an environment) is 'used' in addition to the current environment (ie, treated as an additional parent environment)
		PopFrame,						// pop-frame - removes the currently topmost frame from the frame stack. Tail equivalent is a nop.
		CreateEnvironment,				// create-environment a - a (an Operation.NewEnvironment) is used as a template to create a new environment
		CreateAndLoadEnvironment,		// create-and-load-environment a - a (an Operation.NewEnvironment) is used as a template to create a new environment and load the initial values from the frame
		CreateAndLoadEnvironmentList,	// create-and-load-environment-list a - as for create-and-load-environment, except any values on the end of the frame are stored as a list
		CreateAndLoadEnvironmentStack,	// create-and-load-environment a - as for create-and-load-environment, except values are taken from the stack
		Stop,							// stop - stop executing this S-Expression

		// Flow control operations
		If,								// if a - if the value on top of the stack is not the value 'false', then advance the program counter a positions
		IfLabel,						// if-label a - as for if, but branches to the label named 'a' in the current S-Expression
		Branch,							// branch a - branches by 'a' bytes
		BranchLabel,					// branch-label a - branches to label 'a'
		Label,							// label a - declare label 'a' (otherwise a no-op)

		// Tail operations
		TailCallIProcedure,				// tail-call-iprocedure a - as for call-iprocedure, except no new environment is pushed, and the current frame is popped before pushing a new one.
	}

	/// <summary>
	/// Representation of an individual bytecode operation
	/// </summary>
	public struct Operation
	{
		public Operation(Op operation)
		{
			this.operation = operation;
			this.a = null;
		}

		public Operation(Op operation, object a)
		{
			this.operation = operation;
			this.a = a;
		}

		#region Operation data structures

		/// <summary>
		/// Argument to the create-*-environment operations
		/// </summary>
		public sealed class NewEnvironment
		{
			/// <summary>
			/// Constructs a NewEnvironment class.
			/// </summary>
			/// <param name="symbols">A dictionary mapping symbol numbers to offsets in the value table</param>
			/// <param name="numberOfValues">The initial length of the value table</param>
			/// <param name="numberToLoad">The number of values to load from the frame/stack</param>
			public NewEnvironment(HybridDictionary symbols, int numberOfValues, int numberToLoad)
			{
				this.symbols = symbols;
				this.numberOfValues = numberOfValues;
				this.numberToLoad = numberToLoad;
			}

			internal HybridDictionary symbols;
			internal int numberOfValues;
			internal int numberToLoad;
		}

		#endregion

		#region Factory methods

		/// <summary>
		/// Given a compilation state and a symbol, produces a push-binding-value or push-relative-value operation as appropriate
		/// </summary>
		public static Operation PushSymbol(Data.Symbol symbol, CompileState state)
		{
			if (state.Local != null && state.Local.Contains(symbol))
			{
				return new Operation(Op.PushRelativeValue, state.Local.RelativeBindingForSymbol(symbol));
			}
			else
			{
				return new Operation(Op.PushBindingValue, state.TopLevel.BindingForSymbol(symbol));
			}
		}

		/// <summary>
		/// Produces a create-environment operation suitable for constructing an environment like the template (only the uppermost environment is considered)
		/// </summary>
		public static Operation CreateEnvironment(Data.Environment template)
		{
			HybridDictionary symbols = template.CopySymbols();
			
			// Ideally, we want symbols to be read-only, but .NET is uncertain on the concept of immutabilty

			return new Operation(Op.CreateEnvironment, new NewEnvironment(symbols, template.Size, 0));
		}

		/// <summary>
		/// Produces a create-load-environment operation suitable for constructing an environment like the template (only the uppermost environment is considered)
		/// </summary>
		/// <param name="template">The environment to emulate when creating the new environment</param>
		/// <param name="variableSymbols">List of Symbols making up the environment</param>
		/// <param name="isList">If true, stack must be false. Last variable is loaded as a list of remaining values from the frame.</param>
		/// <param name="stack">If true, isList must be false. Values are loaded from the stack</param>
		/// <returns>A create-load-environment operation</returns>
		/// <remarks>
		/// variableSymbols is primarily used for sanity checking. If in the template you created 'a' then 'b', you must load them in the
		/// same order.
		/// </remarks>
		public static Operation CreateLoadEnvironment(Data.Environment template, IList variableSymbols, bool isList, bool stack)
		{
			HybridDictionary symbols = template.CopySymbols();

			// Sanity check
			for (int desiredOffset=0; desiredOffset<variableSymbols.Count; desiredOffset++)
			{
				int symbolNumber = ((Data.Symbol)variableSymbols[desiredOffset]).SymbolNumber;
				int realOffset = (int)symbols[symbolNumber];

				if (realOffset != desiredOffset)
					throw new InvalidOperationException("Variables must be loaded into an environment in the order in which they were created");
			}

			// Create the operation
			Op op = Op.CreateAndLoadEnvironment;
			if (isList)
				op = Op.CreateAndLoadEnvironmentList;
			else if (stack)
				op = Op.CreateAndLoadEnvironmentStack;

			return new Operation(op, new NewEnvironment(symbols, template.Size, variableSymbols.Count));
		}

		#endregion

		/// <summary>
		/// The actual operation to perform
		/// </summary>
		public Op operation;

		/// <summary>
		/// The arguments to this operation
		/// </summary>
		public object a;

		/// <summary>
		/// Creates a string version of this operation
		/// </summary>
		public override string ToString()
		{
			string opName;

			switch (operation)
			{
				case Op.CallIProcedure: opName = "call-iprocedure"; break;
				case Op.DefineBinding: opName = "define-binding"; break;
				case Op.DefineRelative: opName = "define-relative"; break;
				case Op.If: opName = "if"; break;
				case Op.IfLabel: opName= "if-label"; break;
				case Op.Branch: opName= "branch"; break;
				case Op.BranchLabel: opName = "branch-label"; break;
				case Op.Label: opName = "label"; break;
				case Op.Nop: opName = "nop"; break;
				case Op.Pop: opName = "pop"; break;
				case Op.PopFrame: opName = "pop-frame"; break;
				case Op.PopEnvironment: opName = "pop-environment"; break;
				case Op.Push: opName = "push"; break;
				case Op.PushContext: opName = "push-context"; break;
				case Op.CreateEnvironment: opName = "create-environment"; break;
				case Op.CreateAndLoadEnvironment: opName = "create-load-environment"; break;
				case Op.CreateAndLoadEnvironmentList: opName = "create-load-environment-list"; break;
				case Op.CreateAndLoadEnvironmentStack: opName = "create-load-environment-stack"; break;
				case Op.PushFrameItem: opName = "push-frame-item"; break;
				case Op.PushFrameList: opName = "push-frame-list"; break;
				case Op.PushBindingValue: opName = "push-binding-value"; break;
				case Op.PushRelativeValue: opName = "push-relative-value"; break;
				case Op.Stop: opName = "stop"; break;
				case Op.TailCallIProcedure: opName = "tail-call-iprocedure"; break;
				case Op.UseEnvironment: opName = "use-environment"; break;

				default: opName = "unknown-operation"; break;
			}

			string res;

			res = "(" + opName;
			if (a != null) res += " " + Interpreter.ToString(a);

			return res + ")";
		}
	}
}
