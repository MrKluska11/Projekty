using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace sklepInternetowy.Models
{
    class PozycjaZamowienia
    {
        public int PozycjaZamowieniaId { get; set; }  //klucz główny
        public int ZamowienieId { get; set; }  //klucz obcy
        public int KursId { get; set; }  //klucz obcy
        public int Ilosc { get; set; }
        public decimal CenaZakupu { get; set; }

        public virtual Kurs kurs { get; set; }
        public virtual Zamowienie zamowienie { get; set; }
    }
}
