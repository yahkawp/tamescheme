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
        }

        Tame.Scheme.Forms.Console schemeConsole;
    }
}