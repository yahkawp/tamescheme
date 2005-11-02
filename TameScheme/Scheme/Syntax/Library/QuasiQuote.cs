// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'quasiquote' syntax							              QuasiQuote.cs |
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

using Tame.Scheme.Runtime;
using Tame.Scheme.Data;

namespace Tame.Scheme.Syntax.Library
{
    [PreferredName("quasiquote"),
     SchemeSyntax("()", "(template)"),
     SchemeGroup(SchemeGroup.Library),
     SchemeUsage(SchemeUsage.Quoting)]
    public sealed class QuasiQuote : ISyntax, IQuoted
    {
        public QuasiQuote()
        {
        }

        static Data.Symbol templateSymbol = new Data.Symbol("template");
        static Data.Symbol unquoteSymbol = new Data.Symbol("unquote");
        static Data.Symbol unquoteSpliceSymbol = new Data.Symbol("unquote-splicing");

        public BExpression BuildQuote(object quasiObject, CompileState state)
        {
            // No quasiquote block is evaluated in tail context
            if (state.TailContext) state = new CompileState(state, false);

            // Build the expression for this block (or part thereof)
            if (quasiObject is Pair)
            {
                Pair quasiPair = (Pair)quasiObject;

                if (quasiPair.Car is ISymbolic)
                {
                    // Check that the binding level is correct
                    ISymbolic iSym = (ISymbolic)quasiPair.Car;

                    if (iSym.Location == null || iSym.Location == state.TopLevel)
                    {
                        // Symbol is bound to the top level

                        if (unquoteSymbol.HashValue.Equals(iSym.HashValue))
                        {
                            // This is an unquote block
                            if (!(quasiPair.Cdr is Pair))
                                throw new Exception.SyntaxError("unquote blocks must contain a valid expression");

                            return BExpression.BuildExpression(((Pair)quasiPair.Cdr).Car, state);
                        }
                        else if (unquoteSpliceSymbol.HashValue.Equals(iSym.HashValue))
                        {
                            // This is an unquote-splicing block
                            if (!(quasiPair.Cdr is Pair))
                                throw new Exception.SyntaxError("unquote-splicing blocks must contain a valid expression");

                            return BExpression.BuildExpression(((Pair)quasiPair.Cdr).Car, state);
                        }
                    }
                }

                // This is a standard pair (build the list in reverse order)
                BExpression result = null;

                object thisElement = quasiPair;

                while (thisElement != null)
                {
                    if (thisElement is Pair)
                    {
                        Pair thisPair = (Pair)thisElement;

                        // Build the expression that creates the element for this part of the expression
                        BExpression thisExpression = BuildQuote(thisPair.Car, state);

                        // Add it to the list we're building on the stack
                        Pair innerPair = thisPair.Car as Pair;
                        if (innerPair != null && unquoteSpliceSymbol.Equals(innerPair.Car))
                        {
                            // Add this by splicing
                            thisExpression = thisExpression.Add(new BExpression(new Operation(Op.SpliceList)));
                        }
                        else
                        {
                            // Add normally
                            thisExpression = thisExpression.Add(new BExpression(new Operation(Op.AddList)));
                        }

                        // Add to the result
                        if (result == null)
                        {
                            result = thisExpression;
                        }
                        else
                        {
                            result = thisExpression.Add(result);
                        }

                        // If the next element is null, then add the empty list to start our quasiquoted list
                        if (thisPair.Cdr == null)
                        {
                            result = new BExpression(new Operation(Op.Push, null)).Add(result);
                        }

                        // Move on to the next element
                        thisElement = thisPair.Cdr;
                    }
                    else
                    {
                        // This is the last element (an improper element)
                        BExpression thisExpression = BuildQuote(thisElement, state);

                        // Add to the result
                        if (result == null)
                        {
                            result = thisExpression;
                        }
                        else
                        {
                            result = thisExpression.Add(result);
                        }

                        // We're done
                        thisElement = null;
                    }
                }

                return result;
            }
            else
            {
                // Default behaviour is just to push the specified object
                return new BExpression(new Operation(Op.Push, quasiObject));
            }
        }

        #region ISyntax Members

        public BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
        {
            // Get the template
            object template = env[templateSymbol].Value;

            return BuildQuote(template, new CompileState(state, false));
        }

        #endregion

        #region IQuoted Members

        private object Quote(object scheme, Transformer.Binder.BindingState state)
        {
            if (scheme is Pair)
            {
                Pair schemePair = (Pair)scheme;

                if (schemePair.Car is ISymbolic)
                {
                    // Check that this symbol is an unquote/unquote-splicing symbol with a top-level binding
                    ISymbolic iSym = (ISymbolic)schemePair.Car;

                    if ((iSym.Location == null || iSym.Location == state.CompileState.TopLevel))
                    {
                        if (unquoteSymbol.HashValue.Equals(iSym.HashValue) || unquoteSpliceSymbol.HashValue.Equals(iSym.HashValue))
                        {
                            // This scheme is unquoted
                            return scheme;
                        }
                    }
                }

                // This scheme list is quoted: build a list of the result (we can't recurse: the unquote symbols are only dealt with if they're at the start of a list, not somewhere in the middle)
                object res = null;
                Pair resPos = null;

                object thisObj = scheme;

                while (thisObj != null)
                {
                    object thisElement = null;                      // The quoted scheme

                    if (thisObj is Pair)
                    {
                        // Quote the Car of this element
                        thisElement = new Pair(Quote(((Pair)thisObj).Car, state), null);

                        // Move on
                        thisObj = ((Pair)thisObj).Cdr;
                    }
                    else
                    {
                        // Just quote this element
                        thisElement = Quote(thisObj, state);

                        // Finished
                        thisObj = null;
                    }

                    if (res == null)
                    {
                        res = thisElement;
                    }
                    else
                    {
                        resPos.Cdr = thisElement;
                    }

                    resPos = thisElement as Pair;
                }

                return res;
            }
            else if (scheme is ICollection)
            {
                // Transform each element in the collection to create a new vector
                ICollection col = (ICollection)scheme;
                object[] res = new object[col.Count];
                IEnumerator colEnum = col.GetEnumerator();

                for (int item = 0; item < col.Count; item++)
                {
                    colEnum.MoveNext();
                    res[item] = colEnum.Current;
                }

                return res;
            }
            else if (scheme is Data.ISymbolic)
            {
                // Unquote a symbol
                return ((Data.ISymbolic)scheme).Symbol;
            }
            else
            {
                // Just pass the scheme through
                return scheme;
            }
        }

        public object QuoteScheme(Data.Pair scheme, SyntaxEnvironment matchEnvironment, Tame.Scheme.Syntax.Transformer.Binder.BindingState bindState)
        {
            return new Pair(scheme.Car, Quote(scheme.Cdr, bindState));
        }

        #endregion
    }
}
