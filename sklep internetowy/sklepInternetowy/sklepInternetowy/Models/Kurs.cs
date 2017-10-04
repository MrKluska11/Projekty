using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Web;

namespace sklepInternetowy.Models
{
    public class Kurs
    {
        public int KursId { get; set; }  //klucz główny
        public int KategoriaId { get; set; }  //klucz obcy
        [Required(ErrorMessage = "Wprowadź nazwę kursu!"]
        [StringLength(50)]
        public string TytulKursu { get; set; }
        [Required(ErrorMessage = "Wprowadź nazwę autora!")]
        [StringLength(50)]
        public string AutorKursu { get; set; }
        public DateTime DataDodania { get; set; }
        [StringLength(50)]
        public string NazwaPlikuObrazka { get; set; }
        public string OpisKursu { get; set; }
        public decimal CenaKursu { get; set; }
        public bool Bestseller { get; set; }
        public bool Ukryty { get; set; }

        public virtual Kategoria kategoria { get; set; }
    }
}