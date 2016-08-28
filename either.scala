
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }


  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(x => b.map(y => f(x, y)))
}


case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty) Left("mean of empty list!")
  else Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x / y)
  catch { case e: Exception => Left(e) }

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 0.0

def parseInsuranceRateQuote( age: String, numberOfSpeedingTickets: String): Either[Exception,Double] =
  for {
    a <- Try { age.toInt }
    tickets <- Try { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(a, tickets)



def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  as.foldRight(Right(Nil:List[B]):Either[E, List[B]])(
    (x, y) => (f(x), y) match {
    case (Left(x), _) => Left(x)
    case (_, Left(x)) => Left(x)
    case (Right(a), Right(al)) => Right(a::al)
  })

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
   traverse(es)(i => i)



case class Person(name: Name, age: Age)
sealed class Name(val value: String) 
sealed class Age(val value: Int)


def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

