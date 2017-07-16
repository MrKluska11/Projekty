using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        bool czyBylaModyfikacja = false;
        bool czyPlikOtwarty = false;

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            toolStripStatusLabel1.Text = DateTime.Now.ToString() + ":" + DateTime.Now.Minute.ToString() + ":" + DateTime.Now.Second.ToString();
        }   

        private void zakończToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void otwórzToolStripMenuItem_Click(object sender, EventArgs e)
        {
            try
            {
                //openFileDialog1.Filter = "pliki tekstowe|*.txt";
                openFileDialog1.Filter = "pliki tekstowe|*.txt|pliki logów|*.ini;*.log|wszystkie pliki|*.*";
                openFileDialog1.ShowDialog();
                System.IO.StreamReader plik = new System.IO.StreamReader(openFileDialog1.FileName);
                textBox1.Text = plik.ReadToEnd();
                plik.Close();
                czyPlikOtwarty = true;
            }
            catch
            {
                MessageBox.Show("Błąd otwacia pliku!");
                return;
            }

        }

        private void zapiszToolStripMenuItem_Click(object sender, EventArgs e)
        {
            zapisz();
        }

        private void zapisz()
        {
            try
            {
                System.IO.StreamWriter plik = new System.IO.StreamWriter(openFileDialog1.FileName);
                plik.Write(textBox1.Text);
                plik.Close();
            }
            catch
            {
                MessageBox.Show("Błąd zapisywania pliku!");
                return;
            }
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            

            toolStripStatusLabel2.Text = DateTime.Now.Hour.ToString() + ":" + DateTime.Now.Minute.ToString() + ":" + DateTime.Now.Second.ToString();

            int wiersz;
            int kolumna;
            int pozycjaKursora;

            pozycjaKursora = textBox1.SelectionStart;
            wiersz = textBox1.GetLineFromCharIndex(pozycjaKursora) + 1;
            kolumna = textBox1.GetFirstCharIndexFromLine(wiersz - 1);
            kolumna = pozycjaKursora - kolumna + 1;

            toolStripStatusLabel1.Text = "W: " + wiersz.ToString() + "K:" + kolumna.ToString();
        }

        private void położenieKursoraToolStripMenuItem_Click(object sender, EventArgs e)
        {
            int wiersz;
            int kolumna;
            int pozycjaKursora;

            pozycjaKursora = textBox1.SelectionStart;
            wiersz = textBox1.GetLineFromCharIndex(pozycjaKursora) + 1;
            kolumna = textBox1.GetFirstCharIndexFromLine(wiersz - 1);
            kolumna = pozycjaKursora - kolumna + 1;

            MessageBox.Show(pozycjaKursora.ToString(), "Pozycja kursora");
            MessageBox.Show("W: " + wiersz.ToString() + "K: " + kolumna.ToString());

        }


        private void textBox1_TextChanged(object sender, EventArgs e)
        {
            czyBylaModyfikacja = true;

            if (textBox1.Text.Length > 0)
            {
                cofnijToolStripMenuItem.Enabled = true;
                kopiujToolStripMenuItem.Enabled = true;
                wytnijToolStripMenuItem.Enabled = true;
                usuńToolStripMenuItem.Enabled = true;
            }
            else
            {
                cofnijToolStripMenuItem.Enabled = false;
                kopiujToolStripMenuItem.Enabled = false;
                wytnijToolStripMenuItem.Enabled = false;
                usuńToolStripMenuItem.Enabled = false;
            }
        }

        private void zapiszJakoToolStripMenuItem_Click(object sender, EventArgs e)
        {
            saveFileDialog1.Filter = "pliki textowe|*.txt|pliki logów|*.log; *.ini| wszystkie pliki| *.*";
            saveFileDialog1.ShowDialog();
            openFileDialog1.FileName = saveFileDialog1.FileName;
            zapisz();

        }


        private void nowyToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if(czyBylaModyfikacja == true)
            {
                if(MessageBox.Show("Czy zapisać zmiany ?", "Komunikat", MessageBoxButtons.YesNo) == DialogResult.Yes)
                {
                    if(czyPlikOtwarty == true)
                    {
                        zapiszToolStripMenuItem.PerformClick();
                    }
                    else
                    {
                        zapiszJakoToolStripMenuItem.PerformClick();
                    }

                }
            }
            textBox1.Text = "";
            czyPlikOtwarty = false;
            czyBylaModyfikacja = false;
        }

        private void zmianaKoloruTłaToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (colorDialog1.ShowDialog() == DialogResult.Cancel)
            {
                return;
            }

            textBox1.BackColor = colorDialog1.Color;
        }

        private void zmianaCzcionkiToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if(fontDialog1.ShowDialog() == DialogResult.Cancel)
            {
                return;
            }

                this.Font = fontDialog1.Font;
        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            if(fontDialog1.ShowDialog() == DialogResult.Cancel)
            {
                return;
            }

            menuStrip1.Font = fontDialog1.Font;
        }

        private void toolStripButton2_Click(object sender, EventArgs e)
        {
            if(colorDialog1.ShowDialog() == DialogResult.Cancel)
            {
                return;
            }

            menuStrip1.BackColor = colorDialog1.Color;
        }

        private void cofnijToolStripMenuItem_Click(object sender, EventArgs e)
        {
            textBox1.Undo();
            cofnijToolStripMenuItem.Enabled = false;
            
        }

        private void toolStripMenuItem3_Click(object sender, EventArgs e)
        {
            textBox1.Cut();
        }

        private void kopiujToolStripMenuItem_Click(object sender, EventArgs e)
        {
            textBox1.Copy();
        }

        private void wklejToolStripMenuItem_Click(object sender, EventArgs e)
        {
            textBox1.Paste();
        }

        private void usuńToolStripMenuItem_Click(object sender, EventArgs e)
        {
            textBox1.SelectedText = "";
        }

        private void wyświetlPomocToolStripMenuItem_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Aby skorzystać z pomocy odwiedź stronę: https://support.office.com/pl-pl/article/Narz%C4%99dzie-Notatnik-c136c884-871b-4481-8ace-7c206271d50a", "Pomoc");
        }




       


    }
}
