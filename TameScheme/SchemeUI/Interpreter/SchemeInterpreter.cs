// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme interpreter component                          SchemeInterpreter.cs |
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
using System.Text;
using System.Collections;
using System.ComponentModel;
using System.IO;
using System.Threading;

using Tame.Scheme.Runtime;
using Tame.Scheme.Runtime.Parse;

namespace Tame.Scheme.UI.Interpreter
{
    /// <summary>
    /// This component represents an interactive scheme interpreter.
    /// </summary>
    public class SchemeInterpreter : Component
    {
        public SchemeInterpreter()
        {
            InitializeComponent();
            InitializeInterpreter();
        }

        public SchemeInterpreter(IContainer container)
        {
            container.Add(this);

            InitializeComponent();
            InitializeInterpreter();
        }

        private void InitializeInterpreter()
        {
            interpreterStream.TextWritten += Interpreter_Written;
        }

        #region Component methods

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
                if (interpreterThread != null)
                {
                    // Inform the thread we're shutting down
                    shuttingDown = true;
                    interpreterThread.Interrupt();

                    // Wait for the thread to stop (nicely)
                    if (!interpreterThread.Join(200))
                    {
                        // Stake thread through heart + bury at crossroads if it looks even the slightest bit undead
                        interpreterThread.Abort();
                    }
                }

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

        #endregion

        #region Communications with the interpreter

        bool evaluating = false;

        /// <summary>
        /// Returns true if the interpreter is currently evaluating an expression
        /// </summary>
        public bool EvaluatingExpression
        {
            get
            {
                lock (this)
                    return evaluating;
            }
        }

        /// <summary>
        /// Starts the interpreter running in interactive mode
        /// </summary>
        public void Go()
        {
            lock (this)
            {
                if (interpreterThread != null)
                {
                    // Only one thread is permitted
                    throw new NotSupportedException("Only one interpreter thread can be executing at a time");
                }

                // Set the interpreter running
                interpreterThread = new Thread(RunInterpreter);
                interpreterThread.IsBackground = true;
                interpreterThread.Start();
            }
        }

        /// <summary>
        /// Sends an interruption to the interpreter
        /// </summary>
        public void Interrupt()
        {
            lock (this)
            {
                if (interpreterThread != null)
                {
                    interpreterThread.Interrupt();
                }
            }
        }

        #endregion

        #region Events

        void Interpreter_Written(object sender, SchemeStream.WriteEventArgs args)
        {
            if (InterpreterOutput != null)
            {
                InterpreterOutput(this, args);
            }
        }

        public delegate void InterpreterOutputHandler(object sender, SchemeStream.WriteEventArgs args);

        /// <summary>
        /// Event raised when the interpreter outputs some text
        /// </summary>
        /// <remarks>
        /// This event will be raised in the interpreter thread, not the main thread.
        /// </remarks>
        public event InterpreterOutputHandler InterpreterOutput;

        /// <summary>
        /// Passes some input that the UI has accepted on to the interpreter
        /// </summary>
        /// <param name="input">The input that was received</param>
        public void InterpreterInput(string input)
        {
            interpreterStream.Input(input);
        }

        /// <summary>
        /// Event sent when the interpreter is beginning to run some code (the cached versions of the environment will be out of date until it finishes, for example)
        /// </summary>
        /// <remarks>
        /// This event is raised in the interpreter thread
        /// </remarks>
        public event EventHandler BeginningToExecute;

        /// <summary>
        /// Event sent when the interpreter has finished executing some code
        /// </summary>
        public event EventHandler FinishedExecuting;

        public virtual void OnBeginningToExecute()
        {
            if (BeginningToExecute != null)
            {
                BeginningToExecute(this, new EventArgs());
            }
        }

        public virtual void OnFinishedExecuting()
        {
            if (FinishedExecuting != null)
            {
                FinishedExecuting(this, new EventArgs());
            }
        }

        #endregion

        #region Settings

        SchemeStream interpreterStream = new SchemeStream();            // The stream where interpreter output should go/come from
        Encoding encoding = Encoding.Unicode;                           // The encoding to use for commuicating with the stream
        bool bracketPrompt = true;                                      // Whether or not to show the '4]' prompt while inputting scheme over several lines.
        bool indent = true;                                             // Whether or not to print automatic indenting when more brackets are required

        Thread interpreterThread = null;                                // The thread the interpreter is running on
        bool shuttingDown = false;                                      // True if the interpreter is shutting down

        /// <summary>
        /// The SchemeStream used for interpreter IO
        /// </summary>
        [Bindable(false)]
        public SchemeStream InterpreterStream
        {
            get
            {
                return interpreterStream;
            }
            set
            {
                if (interpreterThread != null) throw new NotSupportedException("Interpreter settings may not be changed while the intepreter is running");
                interpreterStream = value;
            }
        }

        /// <summary>
        /// Whether or not prompts for input continuation are sent to the output stream
        /// </summary>
        [Bindable(true)]
        public bool ShowBracketPrompt
        {
            get { return bracketPrompt; }
            set
            {
                if (interpreterThread != null) throw new NotSupportedException("Interpreter settings may not be changed while the intepreter is running");
                bracketPrompt = value;
            }
        }

        /// <summary>
        /// Whether or not to automatically add indentation for lines beginning with brackets
        /// </summary>
        [Bindable(true)]
        public bool IndentBrackets
        {
            get { return indent; }
            set
            {
                if (interpreterThread != null) throw new NotSupportedException("Interpreter settings may not be changed while the intepreter is running");
                indent = value;
            }
        }

        #endregion

        #region The interpreter thread

        // Variables used by the interpreter thread
        Runtime.Interpreter interpreter;
        Parser parser;

        StreamWriter output;
        StreamReader input;

        [PreferredName("show-license")]
        class ShowLicense : Procedure.IProcedure
        {
            public ShowLicense(StreamWriter output) 
            {
                this.output = output;
            }

            #region IProcedure Members

            StreamWriter output;

            public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
            {
                output.WriteLine("");
                output.WriteLine("TameScheme Copyright (c) 2005 Andrew Hunter");
                output.WriteLine("");
                output.WriteLine("Permission is hereby granted, free of charge, to any person obtaining a ");
                output.WriteLine("copy of this software and associated documentation files (the \"Software\"),");
                output.WriteLine("to deal in the Software without restriction, including without limitation ");
                output.WriteLine("the rights to use, copy, modify, merge, publish, distribute, sublicense, ");
                output.WriteLine("and/or sell copies of the Software, and to permit persons to whom the ");
                output.WriteLine("Software is furnished to do so, subject to the following conditions:");
                output.WriteLine("");
                output.WriteLine("The above copyright notice and this permission notice shall be included in");
                output.WriteLine("all copies or substantial portions of the Software.");
                output.WriteLine("");
                output.WriteLine("THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR");
                output.WriteLine("IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, ");
                output.WriteLine("FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL ");
                output.WriteLine("THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER");
                output.WriteLine("LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING ");
                output.WriteLine("FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER ");
                output.WriteLine("DEALINGS IN THE SOFTWARE.");
                output.WriteLine("");

                return null;
            }

            #endregion
        }

        /// <summary>
        /// The interpreter thread
        /// </summary>
        void RunInterpreter()
        {
            try
            {
                lock (this)
                {
                    // Create the interpreter
                    interpreter = new Runtime.Interpreter();
                    parser = new Runtime.Parse.Parser();

                    // Create the IO readers/writers
                    output = new StreamWriter(interpreterStream, encoding);
                    input = new StreamReader(interpreterStream, encoding);

                    // Define the show-license command (overriding any user command with the same name)
                    interpreter.DefineProcedure(new ShowLicense(output));
                }

                // Display our standard interactivity startup messages
                output.WriteLine(" == Welcome to TameScheme ==");
                output.WriteLine(System.Reflection.Assembly.GetAssembly(typeof(Runtime.Interpreter)).FullName);
                output.WriteLine("\nTameScheme is distributed under the MIT license and comes with no");
                output.WriteLine("warranty. Full details can be displayed with the (show-license) command.\n\n");

                while (true)
                {
                    try
                    {
                        // Begin by sleeping: this is to indicate that this thread can accept interruptions
                        Thread.Sleep(0);

                        // Write a prompt
                        output.Write("> ");
                        output.Flush();

                        // Read some scheme
                        string scheme = null;

                        scheme = input.ReadLine();

                        // Check to see if we've reached the end of the input
                        if (scheme == null) return;

                        bool moreScheme;
                        object schemeExpression = null;

                        // While there is more scheme to fetch, try to fetch it
                        do
                        {
                            // Count the brackets
                            TokenReader bracketReader = new TokenReader(new StringReader(scheme));
                            int bracketCount = parser.RemainingBrackets(bracketReader);

                            // There's more scheme to come if there are
                            if (bracketCount > 0)
                                moreScheme = true;
                            else
                                moreScheme = false;

                            if (moreScheme)
                            {
                                // Show a prompt indicating the number of brackets that need to be closed if necessary (and also maybe some indentation)
                                lock (this)
                                {
                                    if (bracketPrompt)
                                    {
                                        string prompt = string.Format("{0}) ", bracketCount);

                                        // Display some indentation as well, if requested
                                        if (indent)
                                        {
                                            int indentation = 2 + bracketCount * 2;

                                            if (indentation > 30) indentation = 30;
                                            indentation -= prompt.Length;

                                            for (int x = 0; x < indentation; x++)
                                            {
                                                prompt += " ";
                                            }
                                        }

                                        // TODO: deadlock danger here?
                                        output.Write(prompt);
                                        output.Flush();
                                    }
                                }

                                // There should be more scheme on the input to finish this off
                                Thread.Sleep(0);
                                string nextLine = input.ReadLine();

                                // Give up on EOF
                                if (nextLine == null) return;

                                // Lengthen the scheme expression
                                scheme += "\n" + nextLine;
                            }
                        }
                        while (moreScheme);

                        // scheme now contains a scheme expression (possibly more than one)
                        TokenReader schemeReader = new TokenReader(new StringReader(scheme));
                        schemeExpression = parser.Parse(schemeReader);

                        // Run the expression through the interpreter
                        lock (this) evaluating = true;
                        object result = null;

                        try
                        {
                            OnBeginningToExecute();

                            lock (interpreter)
                            {
                                result = interpreter.Evaluate(schemeExpression);
                            }
                        }
                        finally
                        {
                            lock (this) evaluating = false;

                            OnFinishedExecuting();
                        }

                        // Display the result
                        output.WriteLine("\n" + Runtime.Interpreter.ToString(result) + "\n");
                    }
                    catch (Exception.SchemeException ex)
                    {
                        // Display the exception
                        output.WriteLine("\n; " + ex.Message + "\n");
                    }
                    catch (ThreadInterruptedException)
                    {
                        lock (this)
                        {
                            // Give up if we're shutting down
                            if (shuttingDown) return;
                        }

                        // Notify of the interruption
                        output.WriteLine("\n; <Interrupt>\n");
                    }
                    catch (ThreadAbortException)
                    {
                        return;
                    }
                    catch (AppDomainUnloadedException)
                    {
                        return;
                    }
                    catch (System.Exception e)
                    {
                        // Display a general exception
                        output.WriteLine("\n; .NET exception: " + e.Message + "\n");
                        output.WriteLine(e.ToString());
                    }
                }
            }
            finally
            {
                lock (this)
                {
                    output.Close(); output = null;
                    input.Close(); input = null;

                    interpreter = null;
                    parser = null;

                    interpreterThread = null;
                }
            }
        }

        #endregion
    }
}
