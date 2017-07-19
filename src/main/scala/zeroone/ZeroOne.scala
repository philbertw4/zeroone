package zeroone

import scala.util.{Try,Success,Failure}
import scala.language.implicitConversions

sealed trait ZeroOne {
  override def toString = this match {
    case ZeroOne.Program(elems) => elems.mkString(" ")
    case ZeroOne.Quoted(ZeroOne.Program(elems)) => elems.mkString("["," ","]")
    case effect: ZeroOne.StackEffect => if(effect.name.nonEmpty) effect.name else effect.action.toString
  }
}
object ZeroOne {
  final case class Program(elems: List[ZeroOne]) extends ZeroOne
  object Program{
    def empty: Program = Program(List.empty)
    def apply(elems: ZeroOne*): Program = {
      //Program(elems.toList)
      val list = elems.map {
        case Program(es) => es
        case Quoted(p) => List(Quoted(p))
        case StackEffect(action,name) => List(StackEffect(action,name))
      }.toList.flatten
      Program(list)
    }
  }

  final case class Quoted(program: Program) extends ZeroOne
  object Quoted {
    def apply(elems: ZeroOne*): Quoted = Quoted(Program(elems.toList))
  }

  final case class StackEffect(action: Program => Program, name: String) extends ZeroOne
  object StackEffect{
    def apply(name: String)(action: Program => Program):StackEffect = new StackEffect(action,name)
  }

  import eval._

  val noop1: StackEffect = StackEffect("noop1") {input => input}
  val noop2: StackEffect = StackEffect("noop2") {input => input}
  val noop3: StackEffect = StackEffect("noop3") {input => input}
  val noop4: StackEffect = StackEffect("noop4") {input => input}
  val noop5: StackEffect = StackEffect("noop5") {input => input}

  val zap: StackEffect = StackEffect("zap") {input =>
    input.elems.reverse.headOption match {
      case Some(Quoted(a)) => Program(input.elems.reverse.tail.reverse)
      case _ => Program(input.elems :+ zap)
    }
  }
  val nil: StackEffect = StackEffect("nil") { input =>
    Program(input.elems :+ Quoted(Program.empty))
  }
  val i: StackEffect = StackEffect("i") { input =>
    input.elems.reverse.headOption match {
      case Some(Quoted(a)) => a.eval(Program(input.elems.reverse.tail.reverse))
      case _ => Program(input.elems :+ i)
    }
  }
  val k: StackEffect = StackEffect("k"){ input =>
    val stack = input.elems.reverse
    (stack.headOption, stack.drop(1).headOption) match {
      case (Some(Quoted(a)),Some(Quoted(b))) => a.eval(Program(stack.drop(2).reverse))
      case _ => Program(stack :+ k)
    }
  }

  val z: StackEffect = StackEffect("z"){ input =>
    val stack = input.elems.reverse
    (stack.headOption, stack.drop(1).headOption) match {
      case (Some(Quoted(a)),Some(Quoted(b))) => b.eval(Program(stack.drop(2).reverse))
      case _ => Program(stack :+ z)
    }
  }

  val R: StackEffect = StackEffect("R"){ input =>
    //R is a synonym for k
    val stack = input.elems.reverse
    (stack.headOption, stack.drop(1).headOption) match {
      case (Some(Quoted(a)),Some(Quoted(b))) => a.eval(Program(stack.drop(2).reverse))
      case _ => Program(stack :+ R)
    }
  }
  val L: StackEffect = StackEffect("L"){ input =>
    //L is a synonym for z
    val stack = input.elems.reverse
    (stack.headOption, stack.drop(1).headOption) match {
      case (Some(Quoted(a)),Some(Quoted(b))) => b.eval(Program(stack.drop(2).reverse))
      case _ => Program(stack :+ L)
    }
  }
  val cons: StackEffect = StackEffect("cons") { input =>
    //[B] [A] cons == [[B] A]
    val stack = input.elems.reverse
    (stack.headOption, stack.drop(1).headOption) match {
      case (Some(Quoted(a)),Some(Quoted(b))) => Quoted(Program(Quoted(b) :: a.elems)).eval(Program(stack.drop(2).reverse))
      case _ => Program(stack :+ cons)
    }
  }
  val sap: StackEffect = StackEffect("sap") { input =>
    //[B] [A] sap == A B
    val stack = input.elems.reverse
    (stack.drop(1).headOption, stack.headOption) match {
      case (Some(Quoted(b)), Some(Quoted(a))) => b.eval(a.eval(Program(stack.drop(2).reverse)))
      case _ => Program(stack :+ sap)
    }
  }
  val unit: StackEffect = StackEffect("unit") { input =>
    val stack = input.elems.reverse
    stack.headOption match {
      case Some(Quoted(a)) => Quoted(Quoted(a)).eval(Program(stack.tail.reverse))
      case _ => Program(stack :+ unit)
    }
  }
  val run: StackEffect = StackEffect("run") { input =>
    val stack = input.elems.reverse
    stack.headOption match {
      case Some(Quoted(a)) => Program(a.eval(Program(stack.tail.reverse)).elems :+ Quoted(a))
      case _ => Program(stack :+ run)
    }
  }
  def rep(num: Int): StackEffect = StackEffect(s"rep$num") { input =>
    // rep0 == zap, rep1 == i, rep2 = run i, rep3 = run run i
    num match {
      case 0 => zap.eval(Program(input.elems.reverse))
      case 1 => i.eval(Program(input.elems.reverse))
      case n: Int if n > 1 => Program((1 until n).map(_ => run).toList :+ i).eval(Program(input.elems.reverse))
    }
  }
  val b: StackEffect = StackEffect("b") { input =>
    // [C] [B] [A] b == [[C] B] A
    val stack = input.elems.reverse
    (stack.drop(2).headOption, stack.drop(1).headOption, stack.headOption) match {
      case (Some(Quoted(pC)),Some(Quoted(pB)),Some(Quoted(pA))) => Program(Quoted(Program(Quoted(pC)::pB.elems))::pA.elems).eval(Program(stack.drop(3).reverse))
      case _ => Program(stack :+ b)
    }
  }
  val s: StackEffect = StackEffect("s") { input =>
    // [C] [B] [A] s == [[C] B] [C] A
    val stack = input.elems.reverse
    (stack.drop(2).headOption, stack.drop(1).headOption, stack.headOption) match {
      case (Some(Quoted(pC)),Some(Quoted(pB)),Some(Quoted(pA))) =>
        Program(Quoted(Program(Quoted(pC)::pB.elems))::Quoted(pC)::pA.elems).eval(Program(stack.drop(3).reverse))
      case _ => Program(stack :+ s)
    }
  }
}

object eval {
  import ZeroOne._
  implicit class ImplInterpreter(elem: ZeroOne) {
    def eval(input: Program = Program.empty): Program = elem match {
      case q: Quoted =>
        //println(s"quote $input | $q")
        Program(input.elems :+ q)
      case e: StackEffect =>
        //println(s" effect $input | $e")
        e.action(input)
      case p: Program => p.elems.foldLeft(input) {
        case(inputProg, element) =>
          println(s"$inputProg | $element")
          element.eval(inputProg)
      }
    }
  }

}

object parse {
  import ZeroOne._
  def apply(raw: String)(implicit symbolMapping: Map[Char,Program]): Program = raw.map(c=>symbolMapping.get(c)).foldLeft(Program.empty){
    case (Program(elems), Some(p)) => Program(elems ++ p.elems)
    case (Program(elems), _) => Program(elems) //unknown character so ignore and move on
  }
}

object Implicits {
  implicit class StrAsProgram(s: String) {
    def parse(implicit symbolMapping: Map[Char,ZeroOne.Program]): ZeroOne.Program = zeroone.parse(s) //let's allow any string to be parsed as a program
  }
}
