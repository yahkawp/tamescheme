// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme parser                                                    Parser.cs |
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

namespace Tame.Scheme.Runtime.Parse
{
	/// <summary>
	/// Class that reads symbols from a TokenReader and turns them into an object
	/// </summary>
	public class Parser
	{
		public Parser()
		{
		}

		static Data.Symbol quasiquoteSymbol = new Data.Symbol("quasiquote");
		static Data.Symbol quoteSymbol = new Data.Symbol("quote");
		static Data.Symbol unquoteSymbol = new Data.Symbol("unquote");
        static Data.Symbol unquoteSpliceSymbol = new Data.Symbol("unquote-splicing");
        static Data.Symbol dotSymbol = new Data.Symbol(".");

		/// <summary>
		/// Parses a scheme expression in the default manner
		/// </summary>
		/// <returns>A scheme object</returns>
        /// <remarks>It is an error to pass scheme to this method with 'extraneous' tokens, such as trailing closing brackets</remarks>
		public static object Parse(string scheme)
		{
			TokenReader reader = new TokenReader(new System.IO.StringReader(scheme));
			Parser parse = new Parser();

			object res = parse.Parse(reader);
			if (reader.ReadToken() != null) throw new Exception.SyntaxError("Found extra tokens after the end of a scheme expression");

			return res;
		}

		/// <summary>
		/// Turns thisToken into an object, using moreTokens to get further tokens if required
		/// </summary>
		/// <returns></returns>
		protected virtual object ParseToken(Token thisToken, TokenReader moreTokens)
		{
			if (thisToken == null) throw new Exception.SyntaxError("Unexpectedly reached the end of the input", moreTokens);
			Token nextToken;

			switch (thisToken.Type)
			{
				case TokenType.Floating:
				case TokenType.Decimal:
				case TokenType.Integer:
				case TokenType.INumber:
				case TokenType.Object:
				case TokenType.Boolean:
				case TokenType.Symbol:
				case TokenType.String:
					// Just return the number
					return thisToken.Value;

				case TokenType.OpenBracket:
				case TokenType.OpenVector:
					// Is a list/vector
					ArrayList listContents = new ArrayList();
					bool improper = false;

					nextToken = moreTokens.ReadToken();
					while (nextToken != null && nextToken.Type != TokenType.CloseBracket)
					{
						// Parse this token
						listContents.Add(ParseToken(nextToken, moreTokens));

						// Fetch the next token
						nextToken = moreTokens.ReadToken();
                        if (nextToken == null) break;

						if (!improper && nextToken.Type == TokenType.Symbol && dotSymbol.Equals(nextToken.Value) && thisToken.Type == TokenType.OpenBracket)
						{
							improper = true;
							nextToken = moreTokens.ReadToken();
                            if (nextToken == null) break;

							if (nextToken.Type == TokenType.CloseBracket) throw new Exception.SyntaxError("Improperly formed dotted list", moreTokens);
						}
					}
					
					if (nextToken == null)
					{
						// Missing ')'
						throw new Exception.MissingParenthesis("Missing close parenthesis", moreTokens);
					}

					// Create a list or vector as appropriate
					if (thisToken.Type == TokenType.OpenBracket)
					{
						Data.Pair res;
						
						if (listContents.Count == 0) res = null; else res = new Data.Pair(listContents);

						if (improper && listContents.Count < 2)
						{
							throw new Exception.SyntaxError("Improperly formed dotted list", moreTokens);
						}

						if (improper)
						{
							Data.Pair lastItem = res.PairAtIndex(listContents.Count-2);
							lastItem.Cdr = ((Data.Pair)lastItem.Cdr).Car;
						}

						return res;
					}
					else
					{
						object[] res = new object[listContents.Count];

						listContents.CopyTo(res, 0);

						return res;
					}

				case TokenType.Quote:
				case TokenType.Unquote:
                case TokenType.UnquoteSplicing:
				case TokenType.QuasiQuote:
					nextToken = moreTokens.ReadToken();
					object quoted = ParseToken(nextToken, moreTokens);

					object[] quote = new object[2];

					// First symbol is quote, unquote, quasiquote depending on what the token was
					switch (thisToken.Type)
					{
						case TokenType.Quote: quote[0] = quoteSymbol; break;
						case TokenType.Unquote: quote[0] = unquoteSymbol; break;
						case TokenType.QuasiQuote: quote[0] = quasiquoteSymbol; break;
                        case TokenType.UnquoteSplicing: quote[0] = unquoteSpliceSymbol; break;
					}

					// Second symbol is the following symbol
					quote[1] = quoted;

					return new Data.Pair(quote);

				case TokenType.BadNumber:
					throw new Exception.SyntaxError("\"" + thisToken.TokenString + "\" looks like it should be a number, but it contains a syntax error", moreTokens);

				default:
					// Unknown token type
					throw new Exception.SyntaxError("The element \"" + thisToken.TokenString + "\" is being used in a context where it is not understood", moreTokens);
			}
		}

		/// <summary>
		/// Turns the contents of a TokenReader into an object
		/// </summary>
		/// <remarks>
		/// Recursive-descent via ParseToken. There may be tokens left in the reader.
		/// </remarks>
		public virtual object Parse(TokenReader reader)
		{
			Token firstToken = reader.ReadToken();

			return ParseToken(firstToken, reader);
		}

        /// <summary>
        /// Works out how many brackets are missing for the expression given by the TokenReader
        /// </summary>
        /// <param name="reader">The reader to read expressions from</param>
        /// <returns>The number of closing parenthesises that are required to complete the expression (-1 if there are too many)</returns>
        public virtual int RemainingBrackets(TokenReader reader)
        {
            int bracketCount = 0;
            Token thisToken;

            try
            {
                thisToken = reader.ReadToken();
            }
            catch (Exception.SchemeException)
            {
                thisToken = new Token(TokenType.BadSyntax, "", null);
            }
            catch (ArithmeticException)
            {
                thisToken = new Token(TokenType.BadNumber, "", null);
            }

            while (thisToken != null)
            {
                switch (thisToken.Type)
                {
                    case TokenType.OpenBracket:
                    case TokenType.OpenVector:
                        // If this begins a list or a vector, increase the bracket count
                        bracketCount++;
                        break;

                    case TokenType.CloseBracket:
                        // Close brackets indicate the end of a list or vector
                        bracketCount--;
                        break;
                }

                // Get the next token
                thisToken = reader.ReadToken();
            }

            // Set the count to -1 if there were too many brackets
            if (bracketCount < 0) bracketCount = -1;

            return bracketCount;
        }
	}
}
