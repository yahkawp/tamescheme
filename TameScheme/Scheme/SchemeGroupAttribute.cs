// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Attribute for specifying the 'group' of an object  SchemeGroupAttribute.cs |
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
    public enum SchemeGroup
    {
        /// <summary>
        /// Specifies that this is a primitive procedure or syntax (part of R5RS)
        /// </summary>
        Primitive,

        /// <summary>
        /// Specifies that this is a library procedure or syntax (and part of R5RS)
        /// </summary>
        Library,

        /// <summary>
        /// Specifies something that R5RS specifies as optional.
        /// </summary>
        Optional,

        /// <summary>
        /// Specifies this is an extension to the R5RS specification following a documented standard
        /// </summary>
        StandardExtension,

        /// <summary>
        /// Specifies an extension introduced by TameScheme
        /// </summary>
        TameExtension,

        /// <summary>
        /// (The default group) Specifies an extension introduced by a user module
        /// </summary>
        UserExtension
    }

    /// <summary>
    /// This attribute specifies the 'group' a function belongs in: whether it is part of the R5RS specification as a primitive or a library
    /// function (or syntax), or as an extension to the R5RS specification.
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    class SchemeGroupAttribute : Attribute
    {
        public SchemeGroupAttribute(SchemeGroup group)
        {
            this.group = group;
        }

        SchemeGroup group;
        public SchemeGroup Group { get { return group; } }
    }
}
