using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace sklepInternetowy.Models
{
    public class Zamowienie
    {
        public int ZamowienieId { get; set; }  //klucz główny
        public string Imie { get; set; }
        public string Nazwisko { get; set; }
        public string Ulica { get; set; }
        public string Miasto { get; set; }
        public string KodPocztowy { get; set; }
        public string Telefon { get; set; }
        public string Email { get; set; }
        public string Komentarz { get; set; }
        public DateTime DataZamowienia { get; set; }
        public StanZamowienia StanZamowienia { get; set; }
        public decimal WartoscZamowienia { get; set; }

        List<PozycjaZamowienia> PozycjeZamowienia { get; set; }
    }

    public enum StanZamowienia
    {
        Nowe,
        Zrealizowane
    }
}