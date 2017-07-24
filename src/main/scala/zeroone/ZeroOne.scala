package zeroone

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions
import cats.Monoid
import cats.kernel.Eq
import org.scalacheck.{Arbitrary, Gen}

sealed trait ZeroOne {
  override def toString = this match {
    case ZeroOne.Program(elems) => elems.mkString("'"," ","'")
    case ZeroOne.Quoted(ZeroOne.Program(elems)) => elems.mkString("["," ","]")
    case effect: ZeroOne.StackEffect => if(effect.name.nonEmpty) effect.name else effect.action.toString
  }
}
object ZeroOne {
  final case class Program(elems: List[ZeroOne]) extends ZeroOne
  object Program {
    def empty: Program = Program(List.empty)
    def apply(elems: ZeroOne*): Program = elems.fold(Monoid[ZeroOne].empty)(_ + _) match {
      case p: Program => p
      case q: Quoted => Program(q)
      case e: StackEffect => Program(e)
    }
  }

  final case class Quoted(program: Program) extends ZeroOne
  object Quoted {
    def empty: Quoted = Quoted(Program.empty)
    def apply(elems: ZeroOne*): Quoted = elems.fold(Monoid[ZeroOne].empty)(_ + _) match {
      case p: Program => Quoted(p)
      case q: Quoted => Quoted(Program(q :: Nil))
      case e: StackEffect => Quoted(Program(e :: Nil))
    }
  }

  final case class StackEffect(action: ZeroOne => ZeroOne, name: String) extends ZeroOne
  object StackEffect {
    def empty: StackEffect = StackEffect((input) => input, "Noop")
    def apply(name: String)(action: ZeroOne => ZeroOne):StackEffect = StackEffect(action,name)


    val noop1: StackEffect = StackEffect("noop1"){
      input => input
        /*case q: Quoted => Program(q)
        case e: StackEffect => Program(e)
        case p: Program => p*/
    }
    def noop(num: Int): StackEffect = StackEffect(s"noop$num")(noop1.action)

    val nil: StackEffect = StackEffect("nil") {
      // nil == []
      input => input + Quoted.empty //in all cases just push an empty quotation
      /*case q: Quoted => q + Quoted.empty
      case e: StackEffect => Program(e,) Quoted.empty)
      case p: Program => Program(p.elems :+ Quoted.empty)*/
    }
    val zap: StackEffect = StackEffect("zap") {
      // [A] zap == ''
      case q: Quoted => Program.empty
      case e: StackEffect => Program(e,zap)
      case p: Program => p.elems.lastOption.map(_.isInstanceOf[Quoted]) match {
        case Some(true) => Program(p.elems.dropRight(1))
        case _ => Program(p.elems :+ zap)
      }
    }
    val i: StackEffect = StackEffect("i") {
      // [A] i == A
      case q: Quoted => q.program
      case e: StackEffect => Program(e, i)
      case p: Program => p.elems.lastOption match {
        case Some(Quoted(a)) => Program(p.elems.dropRight(1)) + a
        case _ => Program(p.elems :+ i)
      }
    }

    val k: StackEffect = StackEffect("k") {
      // [B] [A] k == A
      case q: Quoted => Program(q,k)
      case e: StackEffect => Program(e,k)
      case p: Program => (p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(b)), Some(Quoted(a))) => Program(p.elems.dropRight(2)) + a
        case _ => Program(p.elems :+ k)
      }
    }
    val z: StackEffect = StackEffect("k") {
      // [B] [A] z == B
      case q: Quoted => Program(q,z)
      case e: StackEffect => Program(e, z)
      case p: Program => (p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(b)), Some(Quoted(a))) => Program(p.elems.dropRight(2)) + b
        case _ => Program(p.elems :+ z)
      }
    }
    val cake: StackEffect = StackEffect("cake") {
      // [B] [A] cake == [[B] A] [A [B]]
      case q: Quoted => Program(q) + Program(cake)
      case e: StackEffect => Program(e) + Program(cake)
      case p: Program => (p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(b)), Some(Quoted(a))) => Program(p.elems.dropRight(2)) + Quoted( Quoted(b) + a ) + Quoted( a + Quoted(b) )
        case _ => Program(p.elems :+ cake)
      }
    }
    val sap: StackEffect = StackEffect("sap") {
      // [B] [A] sap == A B
      case q: Quoted => Program(q) + Program(sap)
      case e: StackEffect => Program(e) + Program(sap)
      case p: Program => (p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(b)), Some(Quoted(a))) => Program(p.elems.dropRight(2)) + a + b
        case _ => Program(p.elems :+ sap)
      }
    }
    lazy val dip: ZeroOne = cake + k
    lazy val cons: ZeroOne = cake + nil + k
    lazy val dup: ZeroOne = nil + cake + dip + dip
    lazy val unit: ZeroOne = nil + cons

    def z(num: Int): StackEffect = StackEffect(s"z$num") {
      // [B] [A] z0 == B
      // [B] [A] z1 == [B] A
      // [B] [A] z2 == [[B] A] A
      // [B] [A] z3 == [[[B] A] A] A
      case q: Quoted => Program(q,z(num))
      case e: StackEffect => Program(e, z(num))
      case p: Program => (p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(b)), Some(Quoted(a))) if num == 0 => Program(p.elems.dropRight(2)) + b
        case (Some(Quoted(b)), Some(Quoted(a))) if num > 0 => (1 until num).foldLeft(Quoted(b) + a) {
          case (prev, index) => Quoted(prev) + a
        }
        case _ => Program(p.elems :+ z(num))
      }
    }

    val b: StackEffect = StackEffect("b") {
      // [C] [B] [A] b    == [[C] B] A
      case q: Quoted => Program(q, b)
      case e: StackEffect => Program(e, b)
      case p: Program => (p.elems.dropRight(2).lastOption, p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(c)), Some(Quoted(b)), Some(Quoted(a))) => Quoted(Quoted(c)+b) + a
        case _ => Program(p.elems :+ b)
      }
    }

    val s: StackEffect = StackEffect("s") {
      // [C] [B] [A] s == [[C] B] [C] A
      case q: Quoted => Program(q,s)
      case e: StackEffect => Program(e,s)
      case p: Program => (p.elems.dropRight(2).lastOption, p.elems.dropRight(1).lastOption, p.elems.lastOption) match {
        case (Some(Quoted(c)), Some(Quoted(b)), Some(Quoted(a))) => Quoted(Quoted(c)+b) + Quoted(c) + a
        case _ => Program(p.elems :+ s)
      }
    }

    val mult: ZeroOne = b
    val plus: ZeroOne = Quoted(cons + b) + cons + s
    val pow: ZeroOne = i


  }
  implicit val zeroOneMonoid: Monoid[ZeroOne] = new Monoid[ZeroOne] {
    def empty: ZeroOne = Program.empty
    def combine(b: ZeroOne, a:ZeroOne): ZeroOne = {
        val res = (b, a) match {
          case (b: Quoted, a: Quoted) => Program(b :: a :: Nil)
          case (b: Quoted, Program(elems)) => combine(Program(b :: Nil), Program(elems)) //Program(b :: elems)
          case (b: Quoted, StackEffect(action, name)) =>  action(b)

          case (b: StackEffect, a: StackEffect) => Program.empty + Program(b :: a :: Nil)
          case (b: StackEffect, a: Program) => combine(Program(b :: Nil), a) //Program(b :: a.elems)
          case (b: StackEffect, a: Quoted) =>  Program(b :: a :: Nil)

          case (b: Program, a: Quoted) => if(b == Program.empty) Program(a :: Nil) else Program(b.elems :+ a)
          case (b: Program, a: StackEffect) => a.action(b)
          case (b: Program, a: Program) => (b.elems ++ a.elems).foldLeft(empty) {
            case (stack, elem) =>
              if (elem == Program.empty)
                stack
              else
                combine(stack, elem)
          }
        }
      //println(s"combined ($b, $a) ---->  $res")
      res
    }
  }

  implicit val eqZeroOne: Eq[ZeroOne] = new Eq[ZeroOne] {
    def eqv(x: ZeroOne, y: ZeroOne): Boolean = (x, y) match {
      case (p: Program, q: Program) => (Program.empty + p) == (Program.empty + q)
      case (p: Program, Quoted(a)) => eqv(p,Program(Quoted(a)))
      case (p: Program, e: StackEffect) => eqv(p,Program(e))
      case (q: Quoted, p: Program) => eqv(p,q)
      case (Quoted(a), Quoted(b)) => eqv(a,b)
      case (q: Quoted, p: StackEffect) => false
      case (e: StackEffect, e2: StackEffect) => e.action == e2.action && e.name == e2.name
      case (e: StackEffect, q: Quoted) => false
      case (e: StackEffect, p: Program) => eqv(p,e)
    }
  }

  implicit class concatZeroOneImpl(b: ZeroOne) {
    def +(a: ZeroOne): ZeroOne = Monoid.combine(b, a)
    def apply(a: ZeroOne): ZeroOne = Monoid.combine(b,a)
  }

  implicit def arbZeroOne: Arbitrary[ZeroOne] = {
    import StackEffect._
    val effects = Gen.oneOf(
      Gen.const(nil),
      Gen.const(noop1),
      Gen.const(zap),
      Gen.const(i),
      Gen.const(k),
      Gen.const(Program.empty),
      Gen.const(cake),
      Gen.const(cons),
      //Gen.const(dup), //dup seems to slow down testing way too much!
      Gen.const(unit),
      Gen.const(dip)
    )

    val programs = Gen.listOfN(3,effects).map{l =>
      Program(l.foldLeft(List.empty[ZeroOne]){
        case (accum, elem: Program) => accum ++ elem.elems
        case (accum, elem: ZeroOne) => accum :+ elem
      })
    }

    val quotes = programs.map(Quoted(_))

    val stack = Gen.listOfN(2,quotes).map(Program(_))

    val combined = stack.flatMap(s => programs.map(p => Program(s.elems ++ p.elems)))
    Arbitrary(
      //programs
      //combined
      Gen.oneOf(effects, combined)
    )
  }
}
