// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Representation of a scheme token                                  Token.cs |
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

namespace Tame.Scheme.Runtime.Parse
{

	public enum TokenType
	{
		// Parentheses
		OpenBracket,					// (
		CloseBracket,					// )
		OpenVector,						// #(

		// Standard scheme types
		Symbol,							// foo
		Integer,						// 12
		Decimal,						// 12.34
		Floating,						// 12.3623... (we use decimals for preference at the moment)
		INumber,						// 3/4, 1+4i
		Boolean,						// #t, #f
		String,							// "Whatever"

		// Scheme syntax elements
		Quote,							// '
		QuasiQuote,						// `
		Unquote,						// ,
        UnquoteSplicing,                // ,@

		// Lexical errors
		BadHash,						// # followed by an unrecognised sequence (ie, #X or something)
		BadNumber,						// #b222 or similar - a badly formatted number

		// Extended scheme types
		Object,							// #[System.IO.Stream(45)]

		// 'Extra' types available for use by subclasses of TokenStream
		UserType,
		UserType1,
		UserType2,
		UserType3,
		UserType4,
		UserType5,
		UserType6,
		UserType7
	}

	/// <summary>
	/// A lexical token from a scheme stream
	/// </summary>
	public class Token
	{
		public Token(TokenType type, string tokenString, object value)
		{
			this.Type = type;
			this.TokenString = tokenString;
			this.Value = value;
		}

		public string TokenString;
		public TokenType Type;
		public object Value;
	}
}
