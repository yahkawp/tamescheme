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
		PushSymbol,						// push-symbol a - pushes the value of symbol number a onto the stack
		PushLiteralSymbol,				// push-literal-symbol a - pushes the value of a from a specific environment onto the stack
		PushFrameItem,					// push-frame-item a - pushes frame item a (an int) onto the stack
		PushFrameList,					// push-frame-list a - pushes frame list a onto the stack (ie everything after a in the frame as a list)
		Define,							// define a - defines a (a symbol) to the value of the top object on the stack
		CallIProcedure,					// call-iprocedure a - calls the scheme procedure on the top of the stack using a values from the stack as arguments. Pushes a new frame and environment. tail-call-iprocedure is the tail equivalent
		PushEnvironment,				// push-environment - pushes a new, empty environment on to the stack
		PopEnvironment,					// pop-environment - pops the last environment from the stack
		UseEnvironment,					// use-environment a - a (an environment) is 'used' in addition to the current environment (ie, treated as an additional parent environment)
		PopFrame,						// pop-frame - removes the currently topmost frame from the frame stack. Tail equivalent is a nop.
		LoadEnvironment,				// load-environment a - a (an array of integers - int[]) is a list of symbol numbers. These are loaded from the frame into the current environment
		LoadStackEnvironment,			// load-stack-environment a - a (an array of integers - int[]) is a list of symbol numbers. These are loaded from the stack into the current environment
		LoadEnvironmentList,			// load-environment-list a - as for load-environment, but the last value
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
			this.canBeTail = false;
		}

		public Operation(Op operation, object a)
		{
			this.operation = operation;
			this.a = a;
			this.canBeTail = false;
		}

		public Operation(Op operation, object a, bool canBeTail)
		{
			this.operation = operation;
			this.a = a;
			this.canBeTail = canBeTail;
		}

		/// <summary>
		/// The actual operation to perform
		/// </summary>
		public Op operation;

		/// <summary>
		/// The arguments to this operation
		/// </summary>
		public object a;

		/// <summary>
		/// True if this operation can be a tail operation
		/// </summary>
		public bool canBeTail;

		/// <summary>
		/// Converts this operation to a tail-context version
		/// </summary>
		public Operation MakeTail()
		{
			// Normally, this is only called on operations with canBeTail set, but we allow it for any operation

			// These are the operations that transform
			switch (operation)
			{
				case Op.CallIProcedure: return new Operation(Op.TailCallIProcedure, a);			// Tail call procedures (replace instead of push frames)
				case Op.PushEnvironment: return new Operation(Op.Nop);							// In tail contexts, we don't push new environments (ie, let in a tail context)
				case Op.PopEnvironment: return new Operation(Op.Nop);							// ... neither do we pop old ones (ie, let in a tail context)
				case Op.PopFrame: return new Operation(Op.Nop);
			}

			// Default is no change
			return this;
		}

		/// <summary>
		/// Creates a string version of this operation
		/// </summary>
		public override string ToString()
		{
			string opName;

			switch (operation)
			{
				case Op.CallIProcedure: opName = "call-iprocedure"; break;
				case Op.Define: opName = "define"; break;
				case Op.If: opName = "if"; break;
				case Op.IfLabel: opName= "if-label"; break;
				case Op.Branch: opName= "branch"; break;
				case Op.BranchLabel: opName = "branch-label"; break;
				case Op.Label: opName = "label"; break;
				case Op.LoadEnvironment: opName = "load-environment"; break;
				case Op.LoadStackEnvironment: opName = "load-stack-environment"; break;
				case Op.LoadEnvironmentList: opName = "load-environment-list"; break;
				case Op.Nop: opName = "nop"; break;
				case Op.Pop: opName = "pop"; break;
				case Op.PopFrame: opName = "pop-frame"; break;
				case Op.PopEnvironment: opName = "pop-environment"; break;
				case Op.Push: opName = "push"; break;
				case Op.PushEnvironment: opName = "push-environment"; break;
				case Op.PushFrameItem: opName = "push-frame-item"; break;
				case Op.PushFrameList: opName = "push-frame-list"; break;
				case Op.PushSymbol: opName = "push-symbol"; break;
				case Op.PushLiteralSymbol: opName = "push-literal-symbol"; break;
				case Op.Stop: opName = "stop"; break;
				case Op.TailCallIProcedure: opName = "tail-call-iprocedure"; break;
				case Op.UseEnvironment: opName = "use-environment"; break;

				default: opName = "unknown-operation"; break;
			}

			string res;

			res = "(" + opName;
			if (canBeTail) res += "*";
			if (a != null) res += " " + Interpreter.ToString(a);

			return res + ")";
		}
	}
}
