// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Stream object to enable communications with the            SchemeStream.cs |
// | interpreter thread                                                         |
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
using System.IO;
using System.Text;
using System.Threading;
using System.Collections;

namespace Tame.Scheme.UI.Interpreter
{
    /// <summary>
    /// Stream class that can be used to communicate between the interpreter thread and the 
    /// </summary>
    /// <remarks>This stream is assumed to be encoded in Unicode format</remarks>
    public class SchemeStream : Stream
    {
        #region Methods called from the interpreter thread

        #region Stream type

        public override bool CanRead
        {
            get { return true; }
        }

        public override bool CanWrite
        {
            get { return true; }
        }

        public override bool CanSeek
        {
            get { return false; }
        }

        #endregion

        #region Reading

        ArrayList inputBuffer = new ArrayList();
        Mutex inputMutex = new Mutex();                                 // Mutex guarding the inputBuffer
        AutoResetEvent incomingInput = new AutoResetEvent(false);       // Event signalled when 

        public override int Read(byte[] buffer, int offset, int count)
        {
            int amountToCopy = 0;
            int amountRemaining = count;
            int bufPos = 0;

            // Claim the input mutex
            inputMutex.WaitOne();

            while (inputBuffer.Count == 0)
            {
                // Wait for input to start arriving...
                Mutex.SignalAndWait(inputMutex, incomingInput);

                // Wait for input to finish arriving
                inputMutex.WaitOne();
            }

            // If there isn't enough data in the buffer, only copy what there is
            if (amountRemaining > inputBuffer.Count)
            {
                amountToCopy = inputBuffer.Count;
            }
            else
            {
                amountToCopy = amountRemaining;
            }

            // Copy the data that we've got
            inputBuffer.CopyTo(0, buffer, bufPos, amountToCopy);

            inputBuffer.RemoveRange(0, amountToCopy);
            amountRemaining -= amountToCopy;
            bufPos += amountToCopy;

            // Release the input mutex
            inputMutex.ReleaseMutex();

            // Return the result
            return bufPos;
        }

        #endregion

        #region Writing

        Decoder unicodeDecoder = Encoding.Unicode.GetDecoder();

        public override void Flush()
        {
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            lock (this)
            {
                // Allocate space for the result
                int charCount = unicodeDecoder.GetCharCount(buffer, 0, buffer.Length, true);

                char[] outChars = new char[charCount];
                int bytesUsed, charsUsed;
                bool completed;

                unicodeDecoder.Convert(buffer, offset, count, outChars, 0, charCount, true, out bytesUsed, out charsUsed, out completed);

                // Notify our delegates of the result
                if (outChars[0] == 0xfeff)
                {
                    // Skip the first character, which is just an endian indicator
                    OnWrite(new string(outChars, 1, charsUsed - 1));
                }
                else
                {
                    // Write the whole string
                    OnWrite(new string(outChars, 0, charsUsed));
                }
            }
        }

        #endregion

        #region Seeking

        public override long Length
        {
            get { throw new System.NotSupportedException("The method or operation is not implemented."); }
        }

        public override long Position
        {
            get
            {
                throw new System.NotSupportedException("The method or operation is not implemented.");
            }
            set
            {
                throw new System.NotSupportedException("The method or operation is not implemented.");
            }
        }

        public override void SetLength(long value)
        {
            throw new System.NotSupportedException("The method or operation is not implemented.");
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            throw new System.NotSupportedException("The method or operation is not implemented.");
        }

        #endregion

        #endregion

        #region Methods called from the UI thread

        public SchemeStream()
        {
        }

        ~SchemeStream()
        {
        }

        public class WriteEventArgs : EventArgs
        {
            public WriteEventArgs(string message)
            {
                this.message = message;
            }

            string message;
            public string Message { get { return message; } }
        }

        /// <summary>
        /// Method called when the stream is written to
        /// </summary>
        /// <param name="message">The message that was written</param>
        /// <remarks>Called from the scheme thread; it's the receivers responsibility to call the UI thread</remarks>
        public virtual void OnWrite(string message)
        {
            if (TextWritten != null)
            {
                TextWritten(this, new WriteEventArgs(message));
            }
        }

        public delegate void WriteEventHandler(object sender, WriteEventArgs e);

        /// <summary>
        /// This event is raised when text is written to the stream (in the scheme interpreter thread: the receiver must call the UI thread if necessary)
        /// </summary>
        public event WriteEventHandler TextWritten;

        Encoder encoder = Encoding.Unicode.GetEncoder();

        /// <summary>
        /// Send some input to the stream
        /// </summary>
        /// <param name="message">The message to send</param>
        public void Input(string message)
        {
            lock (this)
            {
                inputMutex.WaitOne();

                // Signal that there's incoming input
                incomingInput.Set();                

                // Convert the message to a buffer
                char[] messageChars = message.ToCharArray();
                int byteCount = encoder.GetByteCount(messageChars, 0, messageChars.Length, true);

                byte[] messageBytes = new byte[byteCount];
                encoder.GetBytes(messageChars, 0, messageChars.Length, messageBytes, 0, true);

                // Store the in the standard buffer
                inputBuffer.AddRange(messageBytes);

                // Finish with the input mutex
                inputMutex.ReleaseMutex();
            }
        }

        #endregion
    }
}
