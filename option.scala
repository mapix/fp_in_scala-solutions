
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: =>B): B = this match {
    case None => default
    case Some(a) => a.asInstanceOf[B]
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a.asInstanceOf[B])
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = 
    this.map(f).getOrElse(None)

  def orElse2[B >: A](ob: => Option[B]): Option[B] = 
    this.flatMap2(a => ob)

  def filter2(f: A => Boolean): Option[A] =
    this.flatMap2(a => if(f(a)) Some(a) else None)
    
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def mean(xs: Seq[Double]): Option[Double] = 
  if (xs.isEmpty) None
  else Some(xs.sum/xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
  a.flatMap(x => b.flatMap(y => Some(f(x, y))))

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 0.0

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
}

def Try[A](a: => A): Option[A] = 
  try Some(a)
  catch {case e:Exception => None}

def sequence[A](a: List[Option[A]]): Option[List[A]] = 
  a.foldRight(Some(Nil:List[A]):Option[List[A]])(
    (x, y) => (x, y) match {
    case (None, _) => None 
    case (_, None) => None 
    case (Some(a), Some(al)) => Some(a::al)
  })

def parseInts(a: List[String]): Option[List[Int]] = 
  sequence(a.map(i => Try(i.toInt)))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
  a.foldRight(Some(Nil:List[B]):Option[List[B]])(
    (x, y) => (f(x), y) match {
    case (None, _) => None 
    case (_, None) => None 
    case (Some(a), Some(al)) => Some(a::al)
  })

def sequence2[A](a: List[Option[A]]): Option[List[A]] = 
  traverse(a)(i => i)


def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)


//val absO: Option[Double] => Option[Double] = lift(math.abs)
//case class Employee(name: String, department: String)
//def lookupByName(name: String): Option[Employee] = None
//val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
//val dept: String = lookupByName("Joe").map(_.dept).filter(_ != "Accounting").getOrElse("Default Dept")
