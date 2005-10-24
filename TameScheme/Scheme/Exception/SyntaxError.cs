// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Exception thrown when the parser fails                      SyntaxError.cs |
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

namespace Tame.Scheme.Exception
{
	/// <summary>
	/// Exception thrown by the parser when a syntax error is found.
	/// </summary>
	public class SyntaxError : SchemeException
	{
		public SyntaxError(string message) : base("Syntax error: " + message)
		{
		}

		public SyntaxError(string message, Runtime.Parse.TokenReader context) : base("Syntax error: " + message + " (" + context.Context + ")")
		{
			this.context = context.Context;
			this.lineCount = context.LineCount;
			this.lineCharacter = context.LineCharacter;
			this.characterCount = context.CharacterCount;
		}

		string context = null;
		int lineCount = -1;
		int characterCount = -1;
		int lineCharacter = -1;

		public string Context { get { return context; } }
		public int LineCount { get { return lineCount; } }
		public int CharacterCount { get { return characterCount; } }
		public int LineCharacter { get { return lineCharacter; } }
	}
}
