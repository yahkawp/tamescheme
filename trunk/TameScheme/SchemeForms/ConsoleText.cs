// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Control designed to deal with scheme console IO             ConsoleText.cs |
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Tame.Scheme.Runtime.Parse;
using Tame.Scheme.UI.Interpreter;

namespace Tame.Scheme.Forms
{
    internal class ConsoleText : RichTextBox
    {
        public ConsoleText()
        {
            InitializeComponent();

            // Set the input position
            inputPos = this.Text.Length;

            // Deal with console events
            consoleStream.TextWritten += new SchemeStream.WriteEventHandler(consoleStream_TextWritten);
        }

        SchemeStream consoleStream = new SchemeStream();                        // The IO stream used for this console

        /// <summary>
        /// The stream that this console receives its display from and sends its output to
        /// </summary>
        public SchemeStream ConsoleStream
        {
            get { return consoleStream; }
        }

        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            components = new System.ComponentModel.Container();
        }

        #endregion

        #region Dealing with input

        int inputPos = 0;                                   // Where text input is allowed to start

        protected override void OnSelectionChanged(EventArgs e)
        {
            if (SelectionStart < inputPos)
            {
                // It's not possible to edit a control before the input position
                ReadOnly = true;

                // It's also not possible to select across the input position
                if (SelectionStart + SelectionLength > inputPos)
                {
                    SelectionLength = inputPos - SelectionStart;
                }
            }
            else
            {
                // Any editing is allowed after the input position
                ReadOnly = false;
            }

            base.OnSelectionChanged(e);
        }

        protected override void OnTextChanged(EventArgs e)
        {
            // Give everyone else the opportunity to mess with the changed text
            base.OnTextChanged(e);

            // TODO: find a better way of stopping people from deleting the character before the input position
            while (base.Text.Length < inputPos)
            {
                AppendText(" ");
            }
        }

        protected override void OnKeyPress(KeyPressEventArgs e)
        {
            base.OnKeyPress(e);

            // If the key pressed was return and the input position is at the end of the text, then see if we can execute the current scheme
            if ((e.KeyChar == '\n' || e.KeyChar == '\r') && SelectionStart == base.Text.Length)
            {
                // Get the input text
                string inputText = InputText;

                // See if it represents some scheme we can run in the interpreter
                Parser testParser = new Parser();
                int bracketCount;
                try
                {
                    bracketCount = testParser.RemainingBrackets(new TokenReader(new StringReader(inputText)));
                }
                catch (Exception.SyntaxError)
                {
                    bracketCount = 0;
                }

                if (bracketCount <= 0)
                {
                    // Send to the interpreter
                    inputPos += inputText.Length;
                    consoleStream.Input(inputText);
                }
                else
                {
                    // Continue editing (insert tabs)
                    string tabs = "  ";
                    for (int x = 0; x < bracketCount; x++)
                    {
                        tabs += "  ";
                    }

                    base.Text += tabs;
                    base.SelectionStart = base.Text.Length;
                }
            }
        }

        #endregion

        #region Dealing with output

        /// <summary>
        /// Omission from the .NET interface: method to insert text at a given location
        /// </summary>
        /// <param name="text"></param>
        /// <param name="location"></param>
        public virtual void InsertText(string text, int location)
        {
            //
            // You know, there are some design desicions that flabbergast you in their sheer idiocy. The ICollection/IList interfaces is one
            // of them (immutability apparently not being something that should be checked at compile time. Genius, that). Here's another.
            //
            // Someone decided that 'all strings are immutable'. I'm beginning to see a blind spot here. I'm not a big fan of Strostrup, but
            // I'm pretty sure someone at MS must have read the C++ standard and perhaps glanced on the chapters that discuss mutability and
            // how it works there. It's overcomplicated, but a damn sight better than what we have to do here.
            //
            // This edits the string through a side-channel. Because while the rich text box presents an immutable string because that's all
            // .NET has, it's got a secret mutable string store that it uses itself and selfishly refuses to let anyone access to.
            // Generality of design suggests two things that aren't done here: if one class needs a mutable string, then possibly others do too,
            // and this class should present it's mutable store as an interface so that it can be changed (which you might want to do if you
            // were, oh I don't know, TRYING TO DO EXACTLY WHAT THIS CONSOLETEXT CLASS DOES. ARRRGH. There are more things that can be done
            // with text than just displaying and editing it, guys)
            //
            // This causes selection events that go nowhere to be raised as an unavoidable side-effect. But this also happens if you set fonts,
            // because Microsoft have failed to seperate the notion of view and model for the rich text box class.
            //

            // Rememeber the old selection position
            int oldSelectionPos = SelectionStart;
            int oldSelectionLength = SelectionLength;

            // Move to the place where the text will be inserted
            // Note side effects (selection changed events will be generated, selection might flicker)
            SelectionStart = location;
            SelectionLength = 0;

            // Change the selection text (wow, this is so much easier than [[text textStorage] insertString: @"Foo" atPosition: pos]. Things have really come on since NeXT designed that in 1989)
            SelectedText = text;

            // Selection is displaced by the inserted text
            if (oldSelectionPos >= location) oldSelectionPos += SelectedText.Length;

            // Move the position back
            SelectionStart = oldSelectionPos;
            SelectionLength = oldSelectionLength;
        }

        public override string Text
        {
            get
            {
                return base.Text;
            }
            set
            {
                // Preserve any user input that has been entered
                string oldText = base.Text;

                base.Text = value + oldText.Substring(inputPos);

                inputPos = value.Length;
            }
        }

        /// <summary>
        /// Gets/sets the text entered by the user (may cause input to be sent to the interpreter)
        /// </summary>
        public virtual string InputText
        {
            get
            {
                return base.Text.Substring(inputPos);
            }
            set
            {
                base.Text = base.Text.Substring(0, inputPos) + value;
            }
        }

        delegate void WriteDelegate(string text);

        public void WriteText(string text)
        {
            // Get the current selection
            int selStart = SelectionStart;
            int textLen = base.Text.Length;

            // Insert the specified text
            // Use our work-around for .NETs design failure
            InsertText(text, inputPos);

            // Move the selection position if it's after the input position
            if (selStart >= inputPos)
            {
                selStart += text.Length;
                SelectionStart = selStart;
            }
            
            // Move the input position along
            inputPos += base.Text.Length - textLen;
            if (inputPos > base.Text.Length) inputPos = base.Text.Length;
        }

        void consoleStream_TextWritten(object sender, SchemeStream.WriteEventArgs e)
        {
            // This occurs on a seperate thead: inform the main thread
            this.BeginInvoke(new WriteDelegate(WriteText), e.Message);
        }

        #endregion
    }
}
