import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.TimeUnit

class Usuario(val Cedula: Int, val Nombre: String, val Apellido: String,
              val Direccion: String, val Celular: Int, var Vetado: Int, var Tipo: String, var Prestado: Int){
}

class Profesor(Cedula: Int, Nombre: String, Apellido: String, Direccion: String, Celular: Int, Vetado: Int = 0, Tipo: String= "Profesor", Prestado: Int = 0)
                extends Usuario(Cedula, Nombre, Apellido, Direccion, Celular, Vetado, Tipo, Prestado){
}

class Estudiante(Cedula: Int, Nombre: String, Apellido: String, Direccion: String, Celular: Int, Vetado: Int = 0, Tipo: String = "Estudiante", Prestado: Int = 0)
                extends Usuario(Cedula, Nombre, Apellido, Direccion, Celular, Vetado, Tipo, Prestado){
}

class Libro(val Titulo: String, val Codigo: Int, val NUmPaginas: Int, val Editorial: String,
            val Formato: String, val Categoria: String, val Autor: String)

class Prestamo(val Cedula: Int, val Nombre: String, val Apellido: String, val Codigo: Int, val Titulo: String, var FechaPrestamos: String, var FechaEntrega: String, var Estado: Int)

// ---------------------------------------------------------------------------------------

def Prestar(listaP: List[Prestamo], listaU: List[Usuario], listaL: List[Libro],  ced: Int, cod: Int, Fecha: String): List[Prestamo] = {

  def fechaEntrega: String = {
    val sdf2 = new SimpleDateFormat("dd-MM-yyyy")
    val c = Calendar.getInstance

    c.setTime(sdf2.parse(Fecha))
    c.add(Calendar.DATE, 8)

    return sdf2.format(c.getTime)
  }

  var contador:  Int = 0

  listaP.foreach(x => {
    if(x.Cedula == ced){
      contador += 1
    }
  })

  listaL.foreach(w => {
    if(w.Codigo == cod){
      listaU.foreach(y => {
        if(y.Cedula == ced){
          if(y.Vetado == 0) {
            if(y.Tipo == "Estudiante"){
              if(contador < 5){
                val Prueba:List[Prestamo]= new Prestamo(ced, y.Nombre, y.Apellido, w.Codigo, w.Titulo, Fecha, fechaEntrega, 5) :: Nil
                return Prueba
              }
              else{
                println("El Estudiante supero el limite de prestamo")
              }
            }
            else if(y.Tipo == "Profesor"){
              val Prueba:List[Prestamo]= new Prestamo(ced, y.Nombre, y.Apellido, w.Codigo, w.Titulo, Fecha, fechaEntrega, 5) :: Nil
              return Prueba
            }
          } else {
            println("Usuario Vetado por: " + y.Vetado)
          }
        }
      })
    }
  })
  return null
}

class Vencido(listaU: List[Usuario], listaP: List[Prestamo]){

  def Diferecia(FI: String, FF: String): Int = {
    val sdf = new SimpleDateFormat("dd-MM-yyyy")
    val fechaEntrega = sdf.parse(FI)
    val fechaRetorno = sdf.parse(FF)

    val diff = fechaRetorno.getTime - fechaEntrega.getTime

    val time = TimeUnit.DAYS
    val diffrence = time.convert(diff, TimeUnit.MILLISECONDS)
    return diffrence.toInt
  }

  var contador: Int = 0
  var ced: Int = 0

  listaP.foreach(x =>{

    if(Diferecia(x.FechaPrestamos, x.FechaEntrega) < 0){
      contador += 1
      ced = x.Cedula
    }
  })

  if(contador != 0){
    listaU.foreach(y =>{
      if (y.Cedula == ced){
        println("XD: " + y.Cedula + y.Vetado)
        y.Vetado += contador * 8
      }
    })
  }else{
    println("Ningun Usuario Vencido")
  }
}

class BuscarPrestado(val listaP: List[Prestamo], val cedula: Int){
  listaP.foreach(z => {
    if(z.Cedula == cedula){
      println("El Libro: " + z.Titulo)
    }
  })
}

class BuscarLibroCategoria(val lista: List[Libro], val Buscar: String){
  lista.foreach(x => {
    if(x.Categoria == Buscar){
      println("El Libro: " + x.Titulo + " y el Codigo es: " + x.Codigo + " Categoria: " + x.Categoria)
    }
  })
}

class BuscarLibroFormato(val lista: List[Libro], val Buscar: String){
  lista.foreach(x => {
    if(x.Formato == Buscar){
      println("El Libro: " + x.Titulo + " y el Codigo es: " + x.Codigo + " Formato: " + x.Formato)
    }
  })
}

class BuscarLibroAutor(val lista: List[Libro], val Buscar: String){
  lista.foreach(x => {
    if(x.Autor == Buscar){
      println("El Libro: " + x.Titulo + " y el Codigo es: " + x.Codigo + " Autor: " + x.Autor)
    }
  })
}

class BuscarLibroEditorial(val lista: List[Libro], val Buscar: String){
  lista.foreach(x => {
    if(x.Editorial == Buscar){
      println("El Libro: " + x.Titulo + " y el Codigo es: " + x.Codigo + " Editorial: " + x.Editorial)
    }
  })
}

object ScalaApp extends App {
  val user:List[Usuario]= new Estudiante(1010109960, "Juan", "Rogriguez","Cll 58 # 37-58",32115697) ::
                          new Estudiante(1000100844, "Pablo", "Espinosa","Cra 15 # 4-62",36894456) ::
                            new Profesor(1001749519, "Saul", "Garcia","Cll 32 # 9-23", 35430468) ::
                            new Profesor(1000203786, "Laura", "Sanchez","Cra 65 # 96-22", 53244666) ::Nil

  val book:List[Libro] = new Libro("Harry Potter", 231, 321, "Norma", "Impreso", "Ingenieria", "Sara") ::
                      new Libro("El Código da Vinci", 455, 456, "Nórdica", "Impreso", "Matematicas", "Camila") ::
                      new Libro("El principito,", 365, 216, "Acantilado", "CD", "Arte", "Anabelle") ::
                      new Libro("Don Quijote de la Mancha", 954, 954, "Norma", "CD", "Matematicas", "Chuky") ::
                      new Libro("Lo que el viento se llevó", 132, 564, "Nórdica", "Impreso", "Sociales", "Anastacia") ::
                      new Libro("Crepúsculo", 765, 356, "Norma", "CD", "Economia", "Chuky") :: Nil

  var lend = List[Prestamo]()
  val A: List[Prestamo] = Prestar(lend, user, book,1010109960, 231, "15-02-2021")
  val B: List[Prestamo] = Prestar(lend, user, book,1000100844, 455, "20-01-2021")
  val C: List[Prestamo] = Prestar(lend, user, book,1001749519, 365, "30-10-2021")
  val D: List[Prestamo] = Prestar(lend, user, book,1001749519, 954, "26-03-2021")
  val E: List[Prestamo] = Prestar(lend, user, book,1000203786, 132, "10-09-2021")
  val F: List[Prestamo] = Prestar(lend, user, book,1001749519, 356, "25-07-2021")

  if( A != null){lend = lend ::: A}
  if( B != null){lend = lend ::: B}
  if( C != null){lend = lend ::: C}
  if( D != null){lend = lend ::: D}
  if( E != null){lend = lend ::: E}
  if( F != null){lend = lend ::: F}


  println(Vencido(user, lend))

  println("Buscar Libros Prestados de Persona")
  println(BuscarPrestado(lend, 1001749519))

  println("Buscar por Categoria")
  println(BuscarLibroCategoria(book, "Matematicas"))

  println("Buscar por Formato")
  println(BuscarLibroFormato(book, "CD"))

  println("Buscar por Autor")
  println(BuscarLibroAutor(book, "Chuky"))

  println("Buscar por Editorial")
  println(BuscarLibroEditorial(book, "Nórdica"))




}
