// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Very basic test of the scheme assembly                       SchemeTest.cs |
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

using Tame.Scheme.Runtime;
using Tame.Scheme.Data;
using Tame.Scheme.Exception;

using Tame.Scheme.UI.Interpreter;

namespace Tame.SchemeTest
{
	class SchemeTest
	{
        static void Output(object sender, SchemeStream.WriteEventArgs args)
        {
            Console.Out.Write(args.Message);
            Console.Out.Flush();
        }

        delegate long TestDelegate(long x, long y);

        static long TestAdd(long x, long y)
        {
            return x + y;
        }

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{
            // Create the interpreter
            SchemeInterpreter terp = new SchemeInterpreter();

            // Register a test delegate function
            terp["long+"] = new TestDelegate(TestAdd);

            // Register for the output event
            terp.InterpreterOutput += Output;

            // Start the interpreter
            terp.Go();

            // Read input forever (the interpreter thread will produce output)
            for (; ; )
            {
                string nextLine = Console.ReadLine();

                terp.InterpreterInput(nextLine);
                terp.InterpreterInput("\n");
            }
		}
	}
}
