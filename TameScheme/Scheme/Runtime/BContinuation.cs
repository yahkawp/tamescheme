// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Continuation that runs Operations                         BContinuation.cs |
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

namespace Tame.Scheme.Runtime
{
	/// <summary>
	/// BContinuation is a continuation for interpreted Scheme.
	/// </summary>
	/// <remarks>Presently, there's no compiled scheme, but expect a CContinuation to go with it when it arrives.</remarks>
	public sealed class BContinuation : IContinuation
	{
		public BContinuation(BExpression initialExpression, Data.Environment initialEnvironment)
		{
			currentFrame = new Frame(initialEnvironment);
			currentExpression = new Expression(initialExpression);
		}

		public BContinuation(BExpression initialExpression, Data.Environment initialEnvironment, object[] initialArgs)
		{
			currentFrame = new Frame(initialEnvironment, initialArgs);
			currentExpression = new Expression(initialExpression);
		}

		#region Continuation and interpreter classes

		/// <summary>
		/// Class representing a procedure call frame
		/// </summary>
		private sealed class Frame
		{
			public Frame() { }
			public Frame(Data.Environment env) { environment = env; }
			public Frame(Data.Environment env, object[] args) 
			{ 
				this.environment = env;
				this.args = args;
			}

			/// <summary>
			/// The arguments for this frame
			/// </summary>
			public object[] args = new object[0];

			/// <summary>
			/// The stack for this frame
			/// </summary>
			// public Stack stack = new Stack();

			/// <summary>
			/// The environment for this frame
			/// </summary>
			public Data.Environment environment = null;
		}

		/// <summary>
		/// Class representing an executing expression
		/// </summary>
		private sealed class Expression
		{
			public Expression(BExpression expr)
			{
				this.expr = expr;
				this.exprLength = expr.expression.Length;
				this.pc = 0;
			}

			/// <summary>
			/// The expression being executed
			/// </summary>
			public BExpression expr;

			/// <summary>
			/// The length of the expression being executed (cached for speed)
			/// </summary>
			public int exprLength;

			/// <summary>
			/// The position that we're in in the expression being executed
			/// </summary>
			public int pc;
		}

		#endregion

		#region Execution environment

		Stack frameStack = new Stack();								// The previous frames in the stack
		Frame currentFrame = new Frame();							// The active frame

		Stack expressionStack = new Stack();						// The stack of executing expressions
		Expression currentExpression = null;						// The active expression

		Stack evaluationStack = new Stack();						// The evaluation stack for this continuation

		#endregion

		#region IContinuation Members

		public object Continue()
		{
			lock (this)
			{
				object[] newValues = null;

				// Run this continuation
				for (;currentExpression!=null; currentExpression.pc++)
				{
					// Pop the expression if we're at the end
					if (currentExpression.pc >= currentExpression.exprLength)
					{
						if (expressionStack.Count <= 0) break;
						currentExpression = (Expression)expressionStack.Pop();
						continue;
					}

					// Fetch + execute the next instruction
					Operation op = currentExpression.expr.expression[currentExpression.pc];
					object opArg = op.a;
					switch (op.operation)
					{
							// Generally pushing things around
						case Op.Push:
							// Push opArg onto the stack
							evaluationStack.Push(opArg);
							break;

						case Op.PushContext:
							evaluationStack.Push(((IContextual)opArg).PlaceInContext(currentFrame.environment));
							break;

						case Op.PushFrameItem:
							// Push a frame item onto the stack
							evaluationStack.Push(currentFrame.args[(int)opArg]);
							break;

						case Op.PushBindingValue:
							evaluationStack.Push(((Data.Environment.Binding)opArg).Value);
							break;

						case Op.PushRelativeValue:
							evaluationStack.Push(((Data.Environment.RelativeBinding)opArg).ValueInEnvironment(currentFrame.environment));
							break;

                        case Op.AddList:
                            evaluationStack.Push(new Data.Pair(evaluationStack.Pop(), evaluationStack.Pop()));
                            break;

                        case Op.SpliceList:
                            {
                                Data.Pair toSplice = (Data.Pair)evaluationStack.Pop();

                                if (toSplice != null)
                                {
                                    Data.Pair splicePos = toSplice;

                                    // Iterate to the end of the list
                                    while (splicePos.Cdr != null)
                                    {
                                        splicePos = (Data.Pair)splicePos.Cdr;
                                    }

                                    // Perform the splicing
                                    splicePos.Cdr = evaluationStack.Pop();

                                    // Push the result
                                    evaluationStack.Push(toSplice);
                                }
                                else
                                {
                                    // Result is just the value we would have spliced in (ie, do nothing)
                                }
                                break;
                            }

						case Op.TailCallIProcedure:
						case Op.CallIProcedure:
						{
							// Fetch the procedure object from the top of the stack
							Procedure.IProcedure proc;
							object procObj = evaluationStack.Pop();
						
							try
							{ 
								proc = (Procedure.IProcedure)procObj;

								if (proc == null) 
									throw new Exception.NotAProcedureException(procObj);
							}
							catch (System.InvalidCastException ex)
							{
								throw new Exception.NotAProcedureException(procObj, ex);
							}

							// Fetch the new arguments from the stack
							int argCount = (int)opArg;
							object[] args = new object[argCount];

							for (int x=argCount-1; x>=0; x--)
							{
								args[x] = evaluationStack.Pop();
							}

							// Evaluate the procedure
							if (proc is Procedure.BProcedure)
							{
								// Special case: create a new frame for this procedure
								bool tail = op.operation==Op.TailCallIProcedure;

								// Get the procedure to execute
								Procedure.BProcedure sproc = (Procedure.BProcedure)proc;
								BExpression procExpr = sproc.procedureDefinition;

								// The new environment is the contextual environment of this function (first thing it will do is create its own environment)
								Data.Environment newEnv = sproc.Environment;

								// Create the new frame
								Frame newFrame = new Frame(newEnv, args);

								// Create the new expression
								Expression newExpr = new Expression(procExpr);
								newExpr.pc = -1;

								if (tail)
								{
									// Replace the expression and frame
									currentFrame = newFrame;
									currentExpression = newExpr;
								}
								else
								{
									// Push the expression and frame
									frameStack.Push(currentFrame); currentFrame = newFrame;
									expressionStack.Push(currentExpression); currentExpression = newExpr;
								}
							}
							else
							{
								// Call the procedure, and push the result
								evaluationStack.Push(proc.Call(currentFrame.environment, ref args));
							}
						}
							break;

						case Op.Branch:
							// Move on
							currentExpression.pc += (int)opArg;
							break;

						case Op.BranchLabel:
						{
							// This should never really be executed, as it's very slow. In case it is, we replace it with a Branch for next time
							Console.WriteLine("WARNING: BranchLabel executed");
							int newPc;

							for (newPc=0; newPc<currentExpression.exprLength; newPc++)
							{
								if (currentExpression.expr.expression[newPc].operation == Op.Label &&
									currentExpression.expr.expression[newPc].a.Equals(opArg))
								{
									break;
								}
							}

							currentExpression.expr.expression[currentExpression.pc] = new Operation(Op.Branch, newPc-currentExpression.pc);

							currentExpression.pc = newPc;
						}
							break;

						case Op.If:
						{
							// Move on if the value on top of the stack is not false
							object compareObj = evaluationStack.Pop();

							if (!(compareObj is bool) || (bool)compareObj != false)
							{
								currentExpression.pc += (int)opArg;
							}
						}
							break;

						case Op.IfLabel:
						{
							// Move on if the value on top of the stack is not false
							// This should never really be executed, as it's very slow. In case it is, we replace it with an If for next time
							int newPc;
							object compareObj = evaluationStack.Pop();

							if (!(compareObj is bool) || (bool)compareObj != false)
							{
								Console.WriteLine("WARNING: IfLabel executed");

								for (newPc=0; newPc<currentExpression.exprLength; newPc++)
								{
									if (currentExpression.expr.expression[newPc].operation == Op.Label &&
										currentExpression.expr.expression[newPc].a.Equals(opArg))
									{
										break;
									}
								}

								currentExpression.expr.expression[currentExpression.pc] = new Operation(Op.If, newPc-currentExpression.pc);

								currentExpression.pc = newPc;
							}
						}
							break;

						case Op.Label:
						case Op.Nop:
							// Do nothing
							break;

						case Op.CreateEnvironment:
						{
							// Push a new environment, with the specified contents
							Operation.NewEnvironment envDetails;
							
							envDetails = (Operation.NewEnvironment)opArg;

							// Construct the values that go in the new environment
							newValues = new object[envDetails.numberOfValues];
							// for (int val=0; val<envDetails.numberOfValues; val++) newValues[val] = Data.Unspecified.Value;

							// Create the environment
							currentFrame.environment = new Data.Environment(envDetails.symbols, newValues, currentFrame.environment);
						}
							break;

						case Op.CreateAndLoadEnvironment:
						{
							// Push a new environment, with the specified contents
							Operation.NewEnvironment envDetails = (Operation.NewEnvironment)opArg;

							// Construct the values that go in the new environment
							newValues = new object[envDetails.numberOfValues];
							for (int val=0; val<envDetails.numberToLoad; val++) newValues[val] = currentFrame.args[val];
							// for (int val=envDetails.numberToLoad; val<envDetails.numberOfValues; val++) newValues[val] = Data.Unspecified.Value;

							// Create the environment
							currentFrame.environment = new Data.Environment(envDetails.symbols, newValues, currentFrame.environment);
						}
							break;

						case Op.CreateAndLoadEnvironmentList:
						{
							// Push a new environment, with the specified contents
							Operation.NewEnvironment envDetails = (Operation.NewEnvironment)opArg;

							// Construct the values that go in the new environment
							newValues = new object[envDetails.numberOfValues];
							for (int val=0; val<envDetails.numberToLoad-1; val++) newValues[val] = currentFrame.args[val];

							// The rest of the values in the frame go in a list: build this list
							Data.Pair lastList = null;
							int argCount = currentFrame.args.Length;

							if (argCount >= envDetails.numberToLoad)
							{
								lastList = new Data.Pair(currentFrame.args, envDetails.numberToLoad-1);
							}

							// ... finally, assign the list to the final variable
							newValues[envDetails.numberToLoad-1] = lastList;

							// Any other values in the environment should be unspecified
							// for (int val=envDetails.numberToLoad; val<envDetails.numberOfValues; val++) newValues[val] = Data.Unspecified.Value;

							// Create the environment
							currentFrame.environment = new Data.Environment(envDetails.symbols, newValues, currentFrame.environment);
						}
							break;


						case Op.CreateAndLoadEnvironmentStack:
						{
							// Push a new environment, with the specified contents
							Operation.NewEnvironment envDetails = (Operation.NewEnvironment)opArg;

							// Construct the values that go in the new environment
							newValues = new object[envDetails.numberOfValues];
							for (int val=0; val<envDetails.numberToLoad; val++) newValues[val] = evaluationStack.Pop();
							// for (int val=envDetails.numberToLoad; val<envDetails.numberOfValues; val++) newValues[val] = Data.Unspecified.Value;

							// Create the environment
							currentFrame.environment = new Data.Environment(envDetails.symbols, newValues, currentFrame.environment);
						}
							break;

						case Op.PopEnvironment:
							// Pop the current environment
							currentFrame.environment = currentFrame.environment.Parent;
							break;

						case Op.Pop:
							// Pop the last value from the evaluation stack
							evaluationStack.Pop();
							break;

						case Op.PopFrame:
							// Pop the frame
							currentFrame = (Frame)frameStack.Pop();
							break;

						case Op.DefineBinding:
							((Data.Environment.Binding)opArg).SetValue(evaluationStack.Pop());
							break;

						case Op.DefineRelative:
							((Data.Environment.RelativeBinding)opArg).SetValueInEnvironment(currentFrame.environment, evaluationStack.Pop());
							break;

						default: throw new Exception.UnknownOperationException("Unknown operation: " +currentExpression.expr.expression[currentExpression.pc].ToString());
					}
				}

				// Sanity check
				if (frameStack.Count > 1) throw new System.Exception("After a continuation completed, there were still frames on the stack");

				// Return the result
				return evaluationStack.Pop();
			}
		}

		#endregion
	}
}
