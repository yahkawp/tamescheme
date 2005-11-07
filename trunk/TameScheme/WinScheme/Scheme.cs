using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Tame.Scheme.Forms;

namespace WinScheme
{
    public partial class Scheme : Form
    {
        Tame.Scheme.Forms.Console schemeConsole;                    // The scheme console control

        public Scheme()
        {
            InitializeComponent();

            // Unfortunately, VS 2005 has a bug that crashes it if we put in the console user control in the designed, so we do this manually instead
            schemeConsole = new Tame.Scheme.Forms.Console();

            schemeConsole.AutoSize = true;
            schemeConsole.Anchor = AnchorStyles.Bottom | AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
            schemeConsole.Size = toolStrip.ContentPanel.Size;
            toolStrip.ContentPanel.Controls.Add(schemeConsole);

            toolStrip.ActiveControl = schemeConsole;

            schemeConsole.SchemeInterpreter.BeginningToExecute += new EventHandler(SchemeInterpreter_BeginningToExecute);
            schemeConsole.SchemeInterpreter.FinishedExecuting += new EventHandler(SchemeInterpreter_FinishedExecuting);
        }

        #region Interpreter events

        delegate void ProgressDelegate();

        void SchemeInterpreter_FinishedExecuting(object sender, EventArgs e)
        {
            this.Invoke(new ProgressDelegate(EndProgressBar));
        }

        void SchemeInterpreter_BeginningToExecute(object sender, EventArgs e)
        {
            this.Invoke(new ProgressDelegate(StartProgressBar));
        }

        void StartProgressBar()
        {
            progressBar.Style = ProgressBarStyle.Marquee;
        }

        void EndProgressBar()
        {
            progressBar.Style = ProgressBarStyle.Blocks;
        }

        #endregion
    }
}