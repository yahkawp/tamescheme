// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The caar, cadr, etc set of procedures                             CaNdr.cs |
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
using Tame.Scheme.Data;

namespace Tame.Scheme.Procedure.Lists
{
	/// <summary>
	/// The caar, caadr, etc set of procedures
	/// </summary>
    [SchemeGroup(SchemeGroup.Library), SchemeUsage(SchemeUsage.Normal)]
	public sealed class CaNdr : IProcedure, IProcedureGroup
	{
		/// <summary>
		/// Constructs a CaNdr procedure
		/// </summary>
		/// <param name="mask">The mask to use (a bit that is 1 indicates take a Cdr at this position, 0 a Car)</param>
		/// <param name="count">The number of car/cdrs to take</param>
		public CaNdr(int mask, int count)
		{
			carOrCdr = new bool[count];

			for (int bit=0; bit<count; bit++)
			{
				if ((mask&1) != 0)
				{
					carOrCdr[bit] = true;
				}
				else
				{
					carOrCdr[bit] = false;
				}

				mask >>= 1;
			}
		}

		bool[] carOrCdr;
		string name = null;

		public string Name
		{
			get
			{
				if (name != null) return name;

				name = "";
				
				foreach (bool isCdr in carOrCdr)
				{
					if (isCdr)
						name = "d" + name;
					else
						name = "a" + name;
				}

				name = "c" + name + "r";

				return name;
			}
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length != 1) throw new Exception.RuntimeException(Name + " takes one argument");
			if (!(args[0] is Pair)) throw new Exception.RuntimeException("The argument to " + Name + " must be a pair");

			object res = args[0];

			foreach (bool isCdr in carOrCdr)
			{
				if (res == null || !(res is Pair))
					throw new Exception.RuntimeException("While evaluating " + Name + ": found an element that is not a pair");

				if (isCdr)
					res = ((Pair)res).Cdr;
				else
					res = ((Pair)res).Car;
			}

			return res;
		}

		#endregion

        #region IProcedureGroup Members

        public static ProcedureDefinition[] Definitions
        {
            get 
            {
                ProcedureDefinition[] caNdr = new ProcedureDefinition[28];

                // Create caar, cadr ... cddddr functions
                int count, mask;
                int pos = 0;

                caNdr = new ProcedureDefinition[28];

                for (count = 2; count <= 4; count++)
                {
                    for (mask = 0; mask < (1 << count); mask++)
                    {
                        CaNdr newProc = new CaNdr(mask, count);

                        caNdr[pos].PreferredName = newProc.Name;
                        caNdr[pos].Procedure = newProc;

                        pos++;
                    }
                }

                return caNdr;
            }
        }

        #endregion
    }
}
