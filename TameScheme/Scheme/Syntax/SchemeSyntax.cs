// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Class bonding syntax implementation to definition          SchemeSyntax.cs |
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

namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// Class that links Syntax and ISyntax objects together
	/// </summary>
	public class SchemeSyntax
	{
		public SchemeSyntax(Syntax itemSyntax, ISyntax itemImplementation)
		{
			this.itemSyntax = itemSyntax;
			this.itemImplementation = itemImplementation;

			// Special case: you can subclass this and use the ISyntax interface to create a unified object
			if (itemImplementation == null && this is ISyntax) this.itemImplementation = (ISyntax)this;
		}

		Syntax itemSyntax;
		ISyntax itemImplementation;

		public Syntax Syntax { get { return itemSyntax; } }
		public ISyntax Implementation { get { return itemImplementation; } }
	}
}
