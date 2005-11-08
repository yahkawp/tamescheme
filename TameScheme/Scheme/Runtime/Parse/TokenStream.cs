// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Stream -> tokens class                                      TokenStream.cs |
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
using System.Text;
using System.IO;

namespace Tame.Scheme.Runtime.Parse
{
	/// <summary>
	/// Class that represents a stream of scheme tokens
	/// </summary>
	public class TokenReader
	{
		/// <summary>
		/// Constructor that can be used by subclasses that don't want to provide tokens via a TextReader
		/// </summary>
		protected TokenReader() { }

		/// <summary>
		/// Constructs a token stream that reads token from the given TextReader
		/// </summary>
		/// <param name="textStream">The stream to read tokens from</param>
		public TokenReader(TextReader textStream)
		{
			this.stream = textStream;
		}

		TextReader stream;

		#region Tracking where we are in the stream

		const int contextLen = 8;

		int characterCount = 0;
		int lineCount = 1;
		int lineCharacter = 0;

		char[] context = new char[contextLen];
		int contextPos = 0;

		/// <summary>
		/// Peeks at the next character from the stream
		/// </summary>
		protected int PeekChar()
		{
			return stream.Peek();
		}

		/// <summary>
		/// Reads the next character from the stream
		/// </summary>
		protected int NextChar()
		{
			int chr = stream.Read();

			if (chr != -1)
			{
                characterCount++;
                lineCharacter++;

                context[contextPos++] = (char)chr;
				if (contextPos >= contextLen) contextPos = 0;
			}

			if (chr == '\n') 
			{
				lineCount++;
				lineCharacter = 0;
			}

			return chr;
		}

		public virtual int CharacterCount { get { return characterCount; } }
		public virtual int LineCount { get { return lineCount; } }
		public virtual int LineCharacter { get { return lineCharacter; } }
		public virtual string Context 
		{
			get 
			{
				string res = "";
				int readPos = contextPos;

				for (int x=0; x<contextLen && x<characterCount; x++)
				{
					readPos--; if (readPos < 0) readPos = contextLen-1;

					res = context[readPos] + res;
				}

				if (contextLen < characterCount) res = "..." + res;

				return res;
			}
		}

		#endregion

		#region Numbers

		private int DigitValue(char digit, int radix)
		{
			int res = -1;

			if (digit >= '0' && digit <= '9') res = digit - '0';
			else if (digit >= 'A' && digit <= 'Z') res = digit - 'A' + 10;
			else if (digit >= 'a' && digit <= 'z') res = digit - 'a' + 10;

			if (res >= radix) res = -1;

			return res;
		}

		/// <summary>
		/// Reads a simple number as token (ie, partially parses a numeric value)
		/// </summary>
		/// <param name="complexNumber">A possibly complex number (eg 1/3 or 1.2+3i or similar)</param>
		/// <param name="offset">The offset to start reading from</param>
		/// <param name="radix">The radix to parse the number in</param>
		/// <returns>A token for a simple (long/float) number, or null if this is not a number</returns>
		protected Token ReadSimpleNumber(string complexNumber, int offset, int radix)
		{
			int numberOffset = offset;				// Position where the number starts
			int currentOffset = offset;				// Current position we've parsed to
			int floatOffset = -1;					// Offset of the digits after the '.'
			int exponentOffset = -1;				// Offset of the digits after the 'e'
			int length = complexNumber.Length;		// Total length of the number
			bool isFloat = false;					// Set to true if this is a floating point number
			bool isNegative = false;				// Set to true if this is a negative number
			int totalDigits = 0;					// Total numeric digits parsed

			// TODO: the phrase: 2_ and similar expressions cause an exception here (we seem to be reading a blank symbol?!)

			if (complexNumber[currentOffset] == '-' || complexNumber[currentOffset] == '+')
			{
				// Negative/Positive number
				if (complexNumber[currentOffset] == '-') isNegative = true;
				currentOffset++;
			}

			numberOffset = currentOffset;

			// Find the last digit from the current offset
			while (currentOffset < length && DigitValue(complexNumber[currentOffset],radix)>=0) { currentOffset++; totalDigits++; }

			// See if there's any more number
			if (currentOffset < length && complexNumber[currentOffset] == '.')
			{
				// Floating point number
				currentOffset++;

				floatOffset = currentOffset;
				isFloat = true;

				while (currentOffset < length && DigitValue(complexNumber[currentOffset],radix)>=0) { currentOffset++; totalDigits++; }
			}

			if (totalDigits <= 0)
			{
				// No numeric digits parsed (might happen if this number is just '.')
				return null;
			}

			if (currentOffset < length && (complexNumber[currentOffset] == 'e' || complexNumber[currentOffset] == 'E'))
			{
				// Floating point number w/exponent
				currentOffset++;

				exponentOffset = currentOffset;
				isFloat = true;

				while (currentOffset < length && DigitValue(complexNumber[currentOffset],radix)>=0) currentOffset++;

				// Check for an invalid exponent
				if (exponentOffset == currentOffset) return null;
			}

			if (numberOffset == currentOffset) return null;			// No digits

			// Parse the number
            try
            {
                if (isFloat)
                {
                    double res = 0;
                    int digit;

                    // Produce the integer part of the number
                    double radixD = (double)radix;
                    int lastInteger = floatOffset - 1;
                    if (lastInteger < 0) lastInteger = exponentOffset - 1;
                    if (lastInteger < 0) lastInteger = currentOffset;
                    for (digit = numberOffset; digit < lastInteger; digit++)
                    {
                        int digitValue = DigitValue(complexNumber[digit], radix);
                        if (digitValue < 0) return null;
                        // .NET bug?
                        // res *= radixD
                        // res += (double)digitValue
                        // ^^ Optimiser bug? Doing this results in every digit being processed, but res misses out the last calculation: ie, if you enter '123.3', you get '12.3' back
                        res = checked((res * radixD) + ((double)digitValue));
                    }

                    // Produce the fractional part of the number
                    if (floatOffset > -1)
                    {
                        double fractional = 0;
                        double pos = 1 / radixD;

                        int lastFloat = exponentOffset - 1;
                        if (lastFloat < 0) lastFloat = currentOffset;

                        for (digit = floatOffset; digit < lastFloat; digit++)
                        {
                            int digitValue = DigitValue(complexNumber[digit], radix);
                            if (digitValue < 0) return null;
                            fractional += checked(pos * (double)digitValue);
                            pos /= radixD;
                        }

                        res += fractional;
                    }

                    // Add the exponent part of the number
                    if (exponentOffset > -1)
                    {
                        int exponent = 0;
                        for (digit = exponentOffset; digit < currentOffset; digit++)
                        {
                            int digitValue = DigitValue(complexNumber[digit], radix);
                            if (digitValue < 0) return null;
                            exponent = checked(exponent * radix + digitValue);
                        }

                        res *= checked(Math.Pow(radix, exponent));
                    }

                    if (isNegative) res = -res;

                    // Return the result
                    return new Token(TokenType.Floating, complexNumber.Substring(offset, currentOffset - offset), res);
                }
                else
                {
                    long res = 0;
                    int digit;

                    // Parse as a simple long value
                    for (digit = numberOffset; digit < currentOffset; digit++)
                    {
                        int digitValue = DigitValue(complexNumber[digit], radix);
                        if (digitValue < 0) return null;
                        res = checked(res * radix + digitValue);
                    }

                    if (isNegative) res = -res;

                    // Return the result
                    return new Token(TokenType.Integer, complexNumber.Substring(offset, currentOffset - offset), res);
                }
            }
            catch (ArithmeticException)
            {
                // Number too big/small
                throw;
            }
            catch (System.Exception)
            {
                // Failed to parse the number for some more weird reason

                // TODO: classify these reasons better
                return null;
            }
		}

		/// <summary>
		/// Reads a number from the given string with the given radix. The entire string should form a valid number for this to return a non-null value.
		/// </summary>
		/// <param name="complexNumber">The number to parse</param>
		/// <param name="radix">The radix to use</param>
		/// <returns>A parsed number</returns>
		protected Token ReadNumber(string complexNumber, int radix)
		{
			// Number might be a simple number, 1/2, 1+2i or 1/2+3/4i at this point
			int offset = 0;
			Token firstNumber = ReadSimpleNumber(complexNumber, offset, radix);

			if (firstNumber == null) return null;					// Not a number

			offset += firstNumber.TokenString.Length;
			if (offset >= complexNumber.Length) return firstNumber;	// Is a simple number

			// Number might be 1/2, 1+2i or 1/2+3/4i
			if (complexNumber[offset] == '/' && firstNumber.Type == TokenType.Integer)
			{
				// Might be 1/2 or 1/2+3/4i
				offset++;

				Token denominator = ReadSimpleNumber(complexNumber, offset, radix);

				if (denominator == null) return null;				// Not a number

				offset += denominator.TokenString.Length;
				if (offset >= complexNumber.Length)
				{
					if (denominator.Type == TokenType.Integer)
						return new Token(TokenType.INumber, complexNumber, new Data.Number.Rational((long)firstNumber.Value, (long)denominator.Value));
					else
						return null;								// Both parts of a rational must be integer
				}

				// Might be 1/2+3/4i
				if (complexNumber[offset] != '-' && complexNumber[offset] != '+' && complexNumber[offset] != 'i')
					return null;									// Not a complex rational

				Token imaginaryNumerator, imaginaryDenominator;
				if (complexNumber[offset] == 'i') 
				{
					// 1/2i (change to 0/1+1/2i)
					imaginaryNumerator = firstNumber;
					imaginaryDenominator = denominator;

					firstNumber = new Token(TokenType.Integer, "0", (long)0);
					denominator = new Token(TokenType.Integer, "1", (long)1);
				}
				else
				{
					// 1/2+3/4i
					imaginaryNumerator = ReadSimpleNumber(complexNumber, offset, radix);

					if (imaginaryNumerator == null) return null;		// Not a number
					if (imaginaryNumerator.Type != TokenType.Integer) 
						return null;									// Must be an integer

					offset += imaginaryNumerator.TokenString.Length;
					if (offset >= complexNumber.Length) return null;	// 'i' is missing

					if (complexNumber[offset] == '/')
					{
						// Has a denominator
						offset++;
						imaginaryDenominator = ReadSimpleNumber(complexNumber, offset, radix);

						if (imaginaryDenominator == null) return null;	// Not a number
						if (imaginaryDenominator.Type != TokenType.Integer) return null;	// Must be an integer denominator

						offset += imaginaryDenominator.TokenString.Length;
						if (offset >= complexNumber.Length) return null;	// 'i' is missing
					}
					else
					{
						imaginaryDenominator = new Token(TokenType.Integer, "1", (long)1);
					}
				}

				if (complexNumber[offset] != 'i') return null;		// 'i' is missing
				offset ++;
				if (offset < complexNumber.Length) return null;		// Garbage after the 'i'

				// We now have enough to make a new RationalComplex
				return new Token(TokenType.INumber, complexNumber,
					new Data.Number.RationalComplex(new Data.Number.Rational((long)firstNumber.Value, (long)denominator.Value),
					new Data.Number.Rational((long)imaginaryNumerator.Value, (long)imaginaryDenominator.Value)));
			}
			else if (complexNumber[offset] == '+' || complexNumber[offset] == '-')
			{
				// Might be 1+2i
				Token imaginary = ReadSimpleNumber(complexNumber, offset, radix);

				if (imaginary == null) return null;					// Not a number

				offset += imaginary.TokenString.Length;
				if (offset >= complexNumber.Length) return null;	// 'i' is missing

				if (firstNumber.Type == TokenType.Integer && imaginary.Type == TokenType.Integer && complexNumber[offset] == '/')
				{
					// 1+2/3i
					offset++;
					Token imaginaryDenom = ReadSimpleNumber(complexNumber, offset, radix);

					if (imaginaryDenom == null) return null;			// Not a number
					if (imaginaryDenom.Type != TokenType.Integer)
						return null;									// Not a valid rational number

					offset += imaginaryDenom.TokenString.Length;

					if (complexNumber[offset++] != 'i') return null;	// 'i' is missing
					if (offset < complexNumber.Length) return null;		// Garbage after the 'i'

					return new Token(TokenType.INumber, complexNumber,
						new Data.Number.RationalComplex(new Data.Number.Rational((long)firstNumber.Value, 1), new Data.Number.Rational((long)imaginary.Value, (long)imaginaryDenom.Value)));
				}
				else
				{
					// 1.2+3.4i
					if (complexNumber[offset++] != 'i') return null;	// 'i' is missing
					if (offset < complexNumber.Length) return null;		// Garbage after the 'i'

					double realPart, imaginaryPart;

					if (firstNumber.Type == TokenType.Integer && imaginary.Type == TokenType.Integer)
					{
						// Can be represented with a rational
						return new Token(TokenType.INumber, complexNumber,
							new Data.Number.RationalComplex(new Data.Number.Rational((long)firstNumber.Value, 1), new Data.Number.Rational((long)imaginary.Value, 1)));
					}

					// Must be an inexact complex number
					if (firstNumber.Type == TokenType.Integer)
						realPart = (double)(long)firstNumber.Value;
					else
						realPart = (double)firstNumber.Value;
					if (imaginary.Type == TokenType.Integer)
						imaginaryPart = (double)(long)imaginary.Value;
					else
						imaginaryPart = (double)imaginary.Value;
					return new Token(TokenType.INumber, complexNumber, new Data.Number.Complex(realPart, imaginaryPart));
				}
			}
			else if (complexNumber[offset] == 'i')
			{
				// 3i/3.4i?
				offset++;
				if (offset < complexNumber.Length) return null;		// Not a number
				
				if (firstNumber.Type == TokenType.Integer)
					return new Token(TokenType.INumber, complexNumber, 
						new Data.Number.RationalComplex(new Data.Number.Rational(0, 1), new Data.Number.Rational((long)firstNumber.Value, 1)));
				else
					return new Token(TokenType.INumber, complexNumber, new Data.Number.Complex(0, (double)firstNumber.Value));
			}
			else
			{
				// Not a number
				return null;
			}
		}

		#endregion

		/// <summary>
		/// Returns true if c is a character that can be part of a symbol
		/// </summary>
		/// <param name="c">The character to test</param>
		/// <returns>true if the character can be part of a symbol</returns>
		protected bool IsSymbol(char c)
		{
			if (char.IsLetterOrDigit(c)) return true;
			if (c == '.' || c == '+' || c == '-'  || c == '/' || c == '@' || c == '!' || c == '?' || c == '$' || c == '%' || c == '&' || c == '*' || c == ':' || c == '<' || c == '>' || c == '=') return true;

			return false;
		}

		/// <summary>
		/// Reads the next token from this stream
		/// </summary>
		public virtual Token ReadToken()
		{
			// Skip any whitespace or comments (ie, read up to and including the first symbol of this token)
			char lastCharacter = '\0';
			bool comment, whitespace;

			whitespace = false;
			comment = false;

			while (true) 
			{
				int chr = NextChar();
				if (chr == -1) return null;				// No further symbols available

				lastCharacter = (char)chr;

				if (comment && (lastCharacter == '\n' || lastCharacter == '\r'))
				{
					// End of comment
					comment = false;
					continue;
				}
				else if (comment) continue;

				whitespace = false;
				if (char.IsWhiteSpace(lastCharacter))
				{
					// This character is whitespace
					whitespace = true;
				}
				else if (lastCharacter == ';')
				{
					// This character marks the start of a comment
					comment = true;
				}

				// Continue if we've got whitespace, stop otherwise
				if (whitespace || comment) continue;
				break;
			}

			// At this point, lastCharacter is the first character in this token
			if (lastCharacter == '(')
			{
				// Open bracket
				return new Token(TokenType.OpenBracket, "(", null);
			}
			else if (lastCharacter == ')')
			{
				// Close bracket
				return new Token(TokenType.CloseBracket, ")", null);
			}
			else if (lastCharacter == '#')
			{
				// # - marks the start of some special syntax

				// Read the next character
				int chr = NextChar();

				if (chr == -1)
				{
					// Just a '#' with nothing else
					return new Token(TokenType.BadHash, "#", null);
				}

				lastCharacter = (char)chr;

				if (lastCharacter == '(')
				{
					// '#(' - the start of a vector
					return new Token(TokenType.OpenVector, "#(", null);
				}
				else if (lastCharacter == 'f' || lastCharacter == 'F')
				{
					// #f (boolean)
					return new Token(TokenType.Boolean, "#" + lastCharacter, false);
				}
				else if (lastCharacter == 't' || lastCharacter == 'T')
				{
					// #t (boolean)
					return new Token(TokenType.Boolean, "#" + lastCharacter, true);
				}
				else if (lastCharacter == 'b' || lastCharacter == 'B' || lastCharacter == 'o' || lastCharacter == 'O' || lastCharacter == 'd' || lastCharacter == 'D' || lastCharacter == 'x' || lastCharacter == 'X')
				{
					// #xnnnn - a number with a specific radix
					int radix = 10;

					switch (lastCharacter)
					{
						case 'b':
						case 'B':
							radix = 2; break;

						case 'o':
						case 'O':
							radix = 8; break;

						case 'd':
						case 'D':
							radix = 10; break;

						case 'x':
						case 'X':
							radix = 16; break;
					}

					// Get the following number
					StringBuilder theNumber = new StringBuilder();

					int numChr = PeekChar();
					while (numChr >= 0 && IsSymbol((char)numChr))
					{
						NextChar();
						theNumber.Append((char)numChr);
						numChr = PeekChar();
					}

					// Try to parse it
					Token numberToken = ReadNumber(theNumber.ToString(), radix);

					// Check for a bad number
					if (numberToken == null) return new Token(TokenType.BadNumber, "#" + lastCharacter + theNumber, null);

					// Return the result
					return new Token(numberToken.Type, "#" + lastCharacter + theNumber, numberToken.Value);
				}
				else if (lastCharacter == 'i' || lastCharacter == 'I')
				{
					// #innnn - an inexact number

					// Parse the number following the '#i'
					Token exactNumber = ReadToken();
					if (exactNumber == null) return new Token(TokenType.BadNumber, "#" + lastCharacter, null);

					// Convert depending on the type
					switch (exactNumber.Type)
					{
						case TokenType.Integer:
							// Convert long -> double
							return new Token(TokenType.Floating, "#" + lastCharacter + exactNumber.TokenString, (double)(long)exactNumber.Value);

						case TokenType.Decimal:
							// Convert decimal -> double
							return new Token(TokenType.Floating, "#" + lastCharacter + exactNumber.TokenString, (double)(decimal)exactNumber.Value);

						case TokenType.Floating:
							// No conversion necessary
							return new Token(TokenType.Floating, "#" + lastCharacter + exactNumber.TokenString, exactNumber.Value);

						case TokenType.INumber:
							// Convert Rational or RationalComplex to a double/Complex number
							if (exactNumber.Value is Data.Number.Rational)
							{
								return new Token(TokenType.Floating, "#" + lastCharacter + exactNumber.TokenString, ((Data.Number.Rational)exactNumber.Value).ToDouble());
							}
							else if (exactNumber.Value is Data.Number.RationalComplex)
							{
								Data.Number.RationalComplex rationalComplex = (Data.Number.RationalComplex)exactNumber.Value;
								return new Token(TokenType.INumber, "#" + lastCharacter + exactNumber.TokenString,
									new Data.Number.Complex(rationalComplex.real.ToDouble(), rationalComplex.imaginary.ToDouble()));
							}
							else
							{
								// Already inexact: no conversion necessary
								return new Token(TokenType.INumber, "#" + lastCharacter + exactNumber.TokenString, exactNumber.Value);
							}

						default:
							// Not a number
							return new Token(TokenType.BadNumber, "#" + lastCharacter + exactNumber.TokenString, null);
					}
				}
				else if (lastCharacter == 'e' || lastCharacter == 'E')
				{
					// #ennnn - an exact number

					// It's not valid to use #e with a Complex number: there is no exact equivalent

					// #e3.4 creates a .NET decimal value rather than a rational as in many types of scheme.
					// TODO: #e3.4 is parsed as a double then turned into a decimal. This should probably be fixed.
					
					// Parse the number following the '#e'
					Token inexactNumber = ReadToken();
					if (inexactNumber == null) return new Token(TokenType.BadNumber, "#" + lastCharacter, null);

					// Transform it as appropriate
					switch (inexactNumber.Type)
					{
						case TokenType.Integer:
						case TokenType.Decimal:
							// Already exact
							return new Token(inexactNumber.Type, "#" + lastCharacter + inexactNumber.TokenString, inexactNumber.Value);

						case TokenType.Floating:
							// Convert floating -> decimal (MAYBE: re-parse the floating point number as a decimal)
							return new Token(TokenType.Decimal, "#" + lastCharacter + inexactNumber.TokenString, (decimal)(double)inexactNumber.Value);

						case TokenType.INumber:
							// Can't convert Complex
							if (inexactNumber.Value is Data.Number.Complex)
								return new Token(TokenType.BadNumber, "#" + lastCharacter + inexactNumber.TokenString, null);

							// All other types are already exact
							return new Token(TokenType.INumber, "#" + lastCharacter + inexactNumber.TokenString, inexactNumber.Value);

						default:
							// Not a number
							return new Token(TokenType.BadNumber, "#" + lastCharacter + inexactNumber.TokenString, null);
					}
				}
				else if (lastCharacter == '[')
				{
					// #[ - a misc object
					// TODO: implement me
					return new Token(TokenType.BadHash, "#[", null);
				}
				else
				{
					// Unknown sequence
					return new Token(TokenType.BadHash, "#" + lastCharacter, null);
				}
			}
			else if (lastCharacter == '\'')
			{
				// Quote character
				return new Token(TokenType.Quote, "'", null);
			}
			else if (lastCharacter == '`')
			{
				// Quasiquote character
				return new Token(TokenType.QuasiQuote, "`", null);
			}
			else if (lastCharacter == ',')
			{
				// Unquote character
                if (PeekChar() == (int)'@')
                {
                    // Unquote-splicing
                    NextChar();
                    return new Token(TokenType.UnquoteSplicing, ",@", null);
                }
                else
                {
                    return new Token(TokenType.Unquote, ",", null);
                }
			}
			else if (lastCharacter == '"')
			{
				// String
				StringBuilder newString = new StringBuilder();
				StringBuilder tokenString = new StringBuilder();
				int chr = NextChar();

				tokenString.Append(lastCharacter);

				for (;;)
				{
					// Throw an exception if there is a missing "
					if (chr == -1) throw new Exception.SyntaxError("Unmatched '\"'");

					// This is a valid character
					char stringChr = (char)chr;
					tokenString.Append(stringChr);

					// See if we've reached the end of the string
					if (stringChr == '"') break;

					// Or if we have to quote the following character
					if (stringChr == '\\')
					{
						chr = NextChar();
						if (chr == -1) throw new Exception.SyntaxError("Unmatched '\"'");

						stringChr = (char)chr;
						tokenString.Append(stringChr);
					}

					// Build the string
					newString.Append(stringChr);

					// Continue
					chr = NextChar();
				}

				return new Token(TokenType.String, tokenString.ToString(), newString.ToString());
			}
			else
			{
				// Either a symbol or a number, depending on what we read - read up to the next whitespace/comment character
				// Numbers have the form nnnn (int), nnn.nnn (double), nnn/nnn (Rational), nn.nn+nn.nni (Complex) or nn/nn+nn/nni (ComplexRational)

				// Flag to indicate whether or not this symbol might in fact be a number (is made up of only number symbols)
				bool mightBeNumber = true;

				// Read the string for this symbol
				StringBuilder symbolString = new StringBuilder();

				while (IsSymbol(lastCharacter))
				{
					// Append the last character
					symbolString.Append(lastCharacter);

					if (mightBeNumber)
					{
						if (!char.IsDigit(lastCharacter) && lastCharacter != '-' && lastCharacter != '.' && lastCharacter != '+' && lastCharacter != 'e' && lastCharacter != 'i' && lastCharacter != '/') mightBeNumber = false;
					}

					// Read the next character
					int chr = PeekChar();
					if (chr == -1) chr = 32;
					lastCharacter = (char)chr;

					// Only actually 'eat' the next character if it's actually a symbol character
					if (IsSymbol(lastCharacter)) NextChar();
				}

				// Get the symbol string for this token
				string symbol = symbolString.ToString();

				// Try to build a number from this if we can
				if (mightBeNumber)
				{
					Token numberToken = ReadNumber(symbol, 10);

					if (numberToken != null) return numberToken;
				}

				// Can't build a number: must be a symbol token
				return new Token(TokenType.Symbol, symbol, new Data.Symbol(symbol));
			}
		}
	}
}
