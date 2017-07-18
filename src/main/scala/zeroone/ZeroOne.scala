package zeroone

import scala.util.{Try,Success,Failure}
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
    override def equals(o: Any) = o match {case d: Data => d.elems == elems; case _ => false}
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

  final case class StackEffect(action: Data => Either[String,Data], name: String = "") extends ZeroOne
            object StackEffect {
              def apply(name: String)(action: Data => Either[String,Data]):StackEffect = new StackEffect(action,name)
            }

  // some implicit conversions to make our lives easier
  implicit def tryToEitherStringT[T](t: Try[T]): Either[String,T] = t match {case Success(r) => Right(r); case Failure(e) => Left(e.getMessage)}
  implicit def eitherStringTtoTry[T](either: Either[String,T]): Try[T] = either match { case Right(v) => Success(v); case Left(s) => Failure(new RuntimeException(s))}


  // these combinators nothing "no operation" -- helpful for testing though
  val NOOP1: StackEffect = StackEffect((input) => Right(input), "NOOP1")
  val NOOP2: StackEffect = StackEffect((input) => Right(input), "NOOP2")
  val NOOP3: StackEffect = StackEffect((input) => Right(input), "NOOP3")
  val NOOP4: StackEffect = StackEffect((input) => Right(input), "NOOP4")
  val NOOP5: StackEffect = StackEffect((input) => Right(input), "NOOP5")
  val NOOP6: StackEffect = StackEffect((input) => Right(input), "NOOP6")

  // the empty quotation, as a combinator
  val nil: StackEffect = StackEffect((input) => Right(Data.Output(Quoted.empty :: input.elems)), "OP_NIL")

  val zap: StackEffect = StackEffect((input) => Try(input.elems.tail) match {case Success(r) => Right(Data.Output(r)); case Failure(e) => Left(e.getMessage)}, "OP_ZAP")

  val swap: StackEffect = {
    val action: Data => Either[String,Data] = input => {
      val result = for(
        a <- Try(input.elems.head);
        b <- Try(input.elems.drop(1).head)
      ) yield Data.Output(b :: a :: input.elems.drop(1).tail)
      result match {
        case Success(r) => Right(r)
        case Failure(e) => Left(e.getMessage)
      }
    }
    StackEffect(action, "OP_SWAP")
  }

  val cons: StackEffect = {
    val action: Data => Either[String,Data] = input => {
      (for( a <- Try(input.elems.head.program);
           b <- Try(input.elems.drop(1).head.program)
      ) yield Data.Output(Quoted(Program(Quoted(b) :: a.elems)) :: input.elems.drop(1).tail)) match {
        case Success(r) => Right(r)
        case Failure(e) => Left(e.getMessage)
      }
    }
    StackEffect(action, "OP_CONS")
  }
  import eval._

  val i = StackEffect("OP_I"){
    input => Try(input.elems.head.program).flatMap(_.eval(Data.Input(input.elems.drop(1))))
  }


  val sap: StackEffect = {
    val action: Data => Either[String, Data] = input => {
      for( a <- Try(input.elems.head.program);
           b <- Try(input.elems.drop(1).head.program);
          result <- a.eval(Data.Input(input.elems.drop(2))).flatMap(b.eval(_))
      ) yield result
    }
    StackEffect(action, "OP_SAP")
  }

  val k: StackEffect = StackEffect(
    (input) => input.elems.headOption match {
    case Some(q) => q.program.eval(Data.Input(input.elems.drop(2)))
    case None => Left("stack underflow when trying to evaluate k combinator")
  }, "OP_K")

  val R: StackEffect = StackEffect(k.action, "OP_R") //R is just a synonym for k

  val z: StackEffect = StackEffect(
    (input) => input.elems.drop(1).headOption match {
      case Some(q) => q.program.eval(Data.Input(input.elems.drop(2)))
      case None => Left("stack underflow when trying to evaluate z combinator")
    }, "OP_Z")

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

object eval {
  import ZeroOne._

  implicit class ImplZeroOneAsData(stack: List[ZeroOne]) {
    def asData: Try[Data.Input] = stack.forall(_.isInstanceOf[Quoted]) match {
      case true => Success(Data.Input(stack.map(_.asInstanceOf[Quoted])))
      case false => Failure( new Exception("unable to convert stack to data!"))
    }
  }

  implicit class ImplEvals(p: ZeroOne) {
    def eval(inputStack: Data = Data.empty): Either[String,Data] = p match {
      case Program(elems) => elems.foldLeft(Right(inputStack).asInstanceOf[Either[String,Data]]) {  //eval child elements
        case (Right(stack), elem) => elem.eval(stack)
        case (Left(error), elem) => Left(error + elem.toString) //propagate the error
      }
      case quotedProg: Quoted => Right(Data.Output(quotedProg :: inputStack.elems)) //just push quote onto the stack
      case StackEffect(action,_) => action(inputStack) //pass the stack to the action thereby evaluating it
      case Data.Input(elems) => Right(Data.Output(elems ++ inputStack.elems))//elems.foldLeft(inputStack) {case (stack, elem) => elem.eval(stack)}
      case Data.Output(elems) => Right(Data.Output(elems ++ inputStack.elems)) //elems.foldLeft(inputStack) {case (stack, elem) => elem.eval(stack)}
    }
  }

  implicit class ImplProgramOps(p: Program) {
    def simplify: Program = Program(eval.simplify(p.elems))
  }
  def simplify(elems: List[ZeroOne]): List[ZeroOne] = {
    println(s"simplifying program $elems")
    elems.foldLeft((List.empty[ZeroOne],elems)) {
      case ((stack,continuation), elem: Quoted) =>
        println(s" adding quoted item $elem to stack")
        (elem :: stack, continuation.drop(1))
      case ((stack, continuation), elem: Program) =>
        println(s" simplifying sub program $elem")
        (simplify((elem.elems.reverse ++ stack).reverse), continuation.drop(1))
      case ((stack, continuation), elem: StackEffect) => {
        println(s" simplifying $elem")
        val data = stack.asData
        println(s" stack is $stack, data is $data")
        data.map(d => elem.action(d)) match {
          case Success(Right(output)) =>
            println("success rewriting $elem")
            //Program(output.elems.reverse)
            (output.elems, continuation.drop(1))
          case Success(Left(msg)) =>
            println(s"Left($msg)")
            (continuation.drop(1)++(elem :: stack), continuation.drop(1))
          case _ =>
            println("failed, so returning stack so far plus program")
            (continuation.drop(1)++(elem :: stack), continuation.drop(1))
        }
      }
      case ((stack,continuation),_) =>
        println("OTHER")
        (stack,continuation.drop(1))
    }._1.reverse
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
