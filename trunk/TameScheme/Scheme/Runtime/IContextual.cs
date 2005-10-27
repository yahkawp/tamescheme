// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'Contextual' objects                                        IContextual.cs |
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

namespace Tame.Scheme.Runtime
{
	/// <summary>
	/// Contextual objects are scheme objects that need to know the environment they were in when they were defined.
	/// </summary>
	/// <remarks>
	/// Functions are the most classic example of a contextual object. These types of scheme object may be informed of
	/// the current environment when they are pushed to the stack for the first time. You will usually need to define
	/// an ISyntax object for creating them that uses the push-context operation at the appropriate moment.
	/// </remarks>
	public interface IContextual
	{
		/// <summary>
		/// Returns a copy of this object with the given context
		/// </summary>
		/// <param name="context">The environment representing the new context</param>
		/// <returns>A new object with the specified context</returns>
		IContextual PlaceInContext(Data.Environment context);

		
		/// <summary>
		/// The environment that represents the context of this object
		/// </summary>
		Data.Environment Environment { get; }
	}
}
