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

        DateTime lastTimeStarted;

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

            status.Text = "Running...";
            lastTimeStarted = DateTime.Now;
        }

        void EndProgressBar()
        {
            DateTime finished = DateTime.Now;

            progressBar.Style = ProgressBarStyle.Blocks;

            TimeSpan timeRunning = finished.Subtract(lastTimeStarted);

            if (timeRunning.Hours >= 1)
            {
                status.Text = string.Format("Finished (total run time {0}:{1})", timeRunning.Hours, timeRunning.Minutes);
            }
            else if (timeRunning.Minutes >= 1)
            {
                status.Text = string.Format("Finished (total run time {0}:{1}m)", timeRunning.Minutes, timeRunning.Seconds);
            }
            else if (timeRunning.Seconds >= 1)
            {
                status.Text = string.Format("Finished (total run time {0}.{1}s)", timeRunning.Seconds, (timeRunning.Milliseconds / 10) % 100);
            }
            else
            {
                status.Text = string.Format("Finished (total run time {0}ms)", timeRunning.TotalMilliseconds);
            }
        }

        #endregion

        private void interruptToolStripMenuItem_Click(object sender, EventArgs e)
        {
            schemeConsole.SchemeInterpreter.Interrupt();
        }
    }
}