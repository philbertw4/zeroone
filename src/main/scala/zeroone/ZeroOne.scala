package zeroone

import scala.util.Try
import scala.language.implicitConversions

sealed trait ZeroOne {
  override def toString = this match {
    case ZeroOne.Program(elems) => elems.mkString(" ")
    case ZeroOne.Quoted(prog) => "[" + prog + "]"
    case ZeroOne.StackEffect(action, name) => if(name.nonEmpty) name else action.toString
    case ZeroOne.Data.Input(elems) => elems.mkString(" ")
    case ZeroOne.Data.Output(elems) => elems.reverse.mkString(" ") //makes for easier reading of program output
  }
}
object ZeroOne {

  final case class Program(elems: List[ZeroOne]) extends ZeroOne
  object Program {
    def empty = Program(List.empty[ZeroOne])
    def apply(elems: ZeroOne*): Program = Program(elems.toList)
  }

  final case class Quoted(program: Program) extends ZeroOne
  object Quoted {
    def empty = Quoted(Program.empty)
    def apply(elems: ZeroOne*): Quoted = Quoted(Program(elems.toList))
  }
  sealed trait Data extends ZeroOne {
    val elems: List[Quoted]
  }
  object Data {
    final case class Input(elems: List[Quoted]) extends Data
              object Input {
                implicit def output2input(output: Output): Input = Input(output.elems)
                def empty: Input = Input(List.empty[Quoted])
                def apply(elems: Quoted*): Input = Input(elems.toList)
              }
    final case class Output(elems: List[Quoted]) extends Data
              object Output {
                implicit def input2output(input: Input): Output = Output(input.elems)
                def empty: Output = Output(List.empty[Quoted])
                def apply(elems: Quoted*): Output = Output(elems.toList)
              }
    def empty: Input = Input(List.empty[Quoted])
    def apply(elems: Quoted*): Input = Input(elems.toList)
  }

  final case class StackEffect(action: Data.Input => Data.Output, name: String = "") extends ZeroOne

  // these combinators nothing "no operation" -- helpful for testing though
  val NOOP1: StackEffect = StackEffect((input) => input, "NOOP1")
  val NOOP2: StackEffect = StackEffect((input) => input, "NOOP2")
  val NOOP3: StackEffect = StackEffect((input) => input, "NOOP3")
  val NOOP4: StackEffect = StackEffect((input) => input, "NOOP4")
  val NOOP5: StackEffect = StackEffect((input) => input, "NOOP5")
  val NOOP6: StackEffect = StackEffect((input) => input, "NOOP6")

  // the empty quotation, as a combinator
  val nil: StackEffect = StackEffect((input) => Data.Output(Quoted.empty :: input.elems), "OP_NIL")

  val zap: StackEffect = StackEffect((input) => Data.Output(input.elems.tail), "OP_ZAP")
  val swap: StackEffect = {
    val action: Data.Input => Data.Output = input => {
      val a = input.elems.head
      val b = input.elems.drop(1).head
      Data.Output(b :: a :: input.elems.drop(1).tail)
    }
    StackEffect(action, "OP_SWAP")
  }

  val cons: StackEffect = {
    val action: Data.Input => Data.Output = input => {
      val a = input.elems.head.program
      val b = input.elems.drop(1).head.program
      Data.Output(Quoted(Program(Quoted(b) :: a.elems)) :: input.elems.drop(1).tail)
    }
    StackEffect(action, "OP_CONS")
  }
  import eval._
  val i: StackEffect = StackEffect((input) => input.elems.head.program.eval(Data.Input(input.elems.tail)), "OP_I")

  val sap: StackEffect = {
    val action: Data.Input => Data.Output = input => {
      val a = input.elems.head.program
      val b = input.elems.drop(1).head.program
      b.eval(a.eval(Data.Input(input.elems.drop(2))))
    }
    StackEffect(action, "OP_SAP")
  }

  val k: StackEffect = StackEffect((input) => input.elems.head.program.eval(Data.Input(input.elems.drop(2))), "OP_K")
  val R: StackEffect = StackEffect(k.action, "OP_R") //R is just a synonym for k
  val z: StackEffect = StackEffect((input) => input.elems.drop(1).head.program.eval(Data.Input(input.elems.drop(2))), "OP_Z")
  val L: StackEffect = StackEffect(z.action, "OP_L") //L is just a synonym for z

  //some sample symbol mappings
  object SymbolMaps {
    val emptyMapping: Map[Char,Program] = Map.empty
    val defaultMapping: Map[Char,Program] = Map(
      '1' -> Program(R),
      '0' -> Program(Quoted(
                        Quoted(
                          Quoted(cons),
                          Quoted(sap)
                        ),
                        Quoted(
                          Quoted(nil),
                          Quoted(swap)
                        )), Quoted(L))
    )
  }
}

object parse {
  import ZeroOne._
  def apply(raw: String)(implicit symbolMapping: Map[Char,Program]): Program = raw.map(c=>symbolMapping.get(c)).foldLeft(Program.empty){
    case (Program(elems), Some(p)) => Program(elems ++ p.elems)
    case (Program(elems), _) => Program(elems) //unknown character so ignore and move on
  }
}

object eval {
  import ZeroOne._

  implicit class ImplEvals(p: ZeroOne) {
    def eval(inputStack: Data.Input = Data.empty): Data.Output = p match {
      case Program(elems) => elems.foldLeft(inputStack) {case (stack, elem) => elem.eval(stack)} //eval child elements
      case quotedProg: Quoted => Data.Output(quotedProg :: inputStack.elems) //just push quote onto the stack
      case StackEffect(action,_) => action(inputStack) //pass the stack to the action thereby evaluating it
      case Data.Input(elems) => elems.foldLeft(inputStack) {case (stack, elem) => elem.eval(stack)}
      case Data.Output(elems) => elems.foldLeft(inputStack) {case (stack, elem) => elem.eval(stack)}
    }
  }
}

object Implicits {
  implicit class StrAsProgram(s: String) {
    def parse(implicit symbolMapping: Map[Char,ZeroOne.Program]): ZeroOne.Program = zeroone.parse(s) //let's allow any string to be parsed as a program
  }
}
