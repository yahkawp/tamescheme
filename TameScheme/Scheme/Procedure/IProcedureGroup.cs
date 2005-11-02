// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interface for IProcedure classes that can create        IProcedureGroup.cs |
// | several different scheme procedures                                        |
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

namespace Tame.Scheme.Procedure
{
    public struct ProcedureDefinition
    {
        /// <summary>
        /// The preferred name of this concrete definition
        /// </summary>
        public string PreferredName;

        /// <summary>
        /// A concrete implementation of this procedure
        /// </summary>
        public IProcedure Procedure;
    }

    /// <summary>
    /// Classes that define a group of procedures can implement this interface to help the interpreter automatically define them.
    /// </summary>
    /// <remarks>
    /// Sometimes a single IProcedure class will be able to implement its procedure in a number of different styles. These won't be
    /// picked up when the Interpreter instantiates procedures (as they have no PreferredName, and possibly a constructor which
    /// takes arguments). They can inherit from this interface instead.
    /// 
    /// Classes implementing this interface should provide a static Definitions function, returning a ProcedureDefinition[] object
    /// </remarks>
    interface IProcedureGroup : IProcedure
    {
        // <summary>
        // The suggested concrete definitions for this procedure
        // </summary>
        // static ProcedureDefinition[] Definitions { get; }
    }
}
