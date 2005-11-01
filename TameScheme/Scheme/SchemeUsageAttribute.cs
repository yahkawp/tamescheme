// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Attribute for specifying the intended use          SchemeUsageAttribute.cs |
// | of an object                                                               |
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

namespace Tame.Scheme
{
    [Flags]
    enum SchemeUsage
    {
        /// <summary>
        /// No special meaning is attached to the usage of this function/syntax
        /// </summary>
        Normal = 0,

        // Standard scheme behaviour

        /// <summary>
        /// This syntax or function can alter the top-level environment.
        /// </summary>
        DefineTopLevel = 1,

        /// <summary>
        /// This syntax or function can create a new environment.
        /// </summary>
        /// <remarks>
        /// For example, the scheme-report-environment function will construct a standard R5RS environment
        /// </remarks>
        CreateEnvironment = 2,

        /// <summary>
        /// This function can cause IO to occur on a port.
        /// </summary>
        InputOutput = 4,

        /// <summary>
        /// This function can create a port object.
        /// </summary>
        CreatePort = 8,

        /// <summary>
        /// This is syntax that performs quoting of some kind
        /// </summary>
        Quoting = 16,

        // Extended behaviour

        /// <summary>
        /// This function can create a .NET object
        /// </summary>
        /// <remarks>
        /// Obviously, most functions can create a .NET object. This usage type indicates that this function can create one that is outside
        /// of the normal scheme types.
        /// </remarks>
        CreateNetObject = 256,
    }

    /// <summary>
    /// This attribute can be used to specify what a function is used for. (This can be later used to filter out unwanted functionality from
    /// the scheme interpreter)
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    class SchemeUsageAttribute : Attribute
    {
        public SchemeUsageAttribute(SchemeUsage usageTypes)
        {
            this.usageTypes = usageTypes;
        }

        SchemeUsage usageTypes;
        public SchemeUsage UsageTypes { get { return usageTypes; } }
    }
}
