// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Attribute that indicates which opcode an IOpCode    CompilesOpAttribute.cs |
// | class compiles                                                             |
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

using Tame.Scheme.Runtime;

namespace Tame.Scheme.Compiler
{
    /// <summary>
    /// Attribute that can be applied to IOpCode subclasses to specify which opcode they compile. Applying this attribute allows the
    /// compiler to load opcode compilers from an assembly without needing to declare them all manually.
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    public class CompilesOpAttribute : Attribute
    {
        public CompilesOpAttribute(Op op)
        {
            this.op = op;
        }

        public Op Op { get { return op; } }
        Op op;
    }
}
