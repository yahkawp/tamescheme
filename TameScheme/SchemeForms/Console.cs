// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme console control                                          Console.cs |
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Tame.Scheme.Forms
{
    /// <summary>
    /// Control that provides a scheme console
    /// </summary>
    public class Console : UserControl
    {
        public Console()
        {
            InitializeComponent();

            // Set the interpreter stream
            schemeInterpreter.InterpreterStream = consoleOutput.ConsoleStream;

            // Set the console properties
            this.Font = new Font("Lucida Console", 11);

            // Start the interpreter running
            schemeInterpreter.Go();
        }

        private ConsoleText consoleOutput;
        private Tame.Scheme.UI.Interpreter.SchemeInterpreter schemeInterpreter;

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
            this.components = new System.ComponentModel.Container();
            this.consoleOutput = new Tame.Scheme.Forms.ConsoleText();
            this.schemeInterpreter = new Tame.Scheme.UI.Interpreter.SchemeInterpreter(this.components);
            this.SuspendLayout();
            // 
            // consoleOutput
            // 
            this.consoleOutput.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.consoleOutput.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.consoleOutput.Location = new System.Drawing.Point(0, 0);
            this.consoleOutput.Name = "consoleOutput";
            this.consoleOutput.Size = new System.Drawing.Size(150, 150);
            this.consoleOutput.TabIndex = 0;
            this.consoleOutput.Text = "";
            // 
            // schemeInterpreter
            // 
            this.schemeInterpreter.IndentBrackets = false;
            this.schemeInterpreter.ShowBracketPrompt = false;
            // 
            // Console
            // 
            this.Controls.Add(this.consoleOutput);
            this.Name = "Console";
            this.ResumeLayout(false);

        }

        #endregion
    }
}
