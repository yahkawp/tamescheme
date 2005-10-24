// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Class representing a scheme symbol                               Symbol.cs |
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
using System.Runtime.Serialization;

namespace Tame.Scheme.Data
{
	/// <summary>
	/// Class representing a scheme symbol
	/// </summary>
	public sealed class Symbol : ISerializable
	{
		public Symbol(int symbolNumber)
		{
			this.symbolNumber = symbolNumber;
		}

		public Symbol(string symbolName)
		{
			this.symbolNumber = SymbolTable.NumberForSymbol(symbolName);
		}

		private Symbol(SerializationInfo info, StreamingContext context)
		{
			this.symbolNumber = SymbolTable.NumberForSymbol((string)info.GetValue("symbolName", typeof(string)));
		}

		int symbolNumber;								// The number of this symbol in the symbol table

		public int SymbolNumber
		{
			get { return symbolNumber; }
		}

		#region ISerializable Members

		public void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			info.AddValue("symbolName", SymbolTable.SymbolForNumber(symbolNumber));
		}

		#endregion

		#region Hashing, etc

		public override bool Equals(object obj)
		{
			if (obj is Symbol)
			{
				if (((Symbol)obj).symbolNumber == symbolNumber)
					return true;
				else
					return false;
			}

			return false;
		}

		public override int GetHashCode()
		{
			return symbolNumber.GetHashCode();
		}

		public override string ToString()
		{
			return SymbolTable.SymbolForNumber(symbolNumber);
		}


		#endregion
	}
}
