package zeroone

import org.scalatest.FunSuite
import ZeroOne._

class ParseSuite extends FunSuite {
  implicit val testMapping = ZeroOne.SymbolMaps.defaultMapping
  test("parse empty program") {
    assert(parse("") == ZeroOne.Program.empty)
  }
  test("parse 1 ==> R") {
    assert(parse("1") == Program(R))
  }
  test("parse 0 ==> [ [[cons] [sap]] [[nil] [swap]] ] [L]") {
    assert(parse("0") == Program(Quoted(Quoted(Quoted(cons),Quoted(sap)), Quoted(Quoted(nil),Quoted(swap))),Quoted(L)))
  }
  test("parse 01 ==> [ [[cons] [sap]] [[nil] [swap]] ] [L] R") {
    assert(parse("01") == Program(Quoted(Quoted(Quoted(cons),Quoted(sap)), Quoted(Quoted(nil),Quoted(swap))),Quoted(L),R))
  }
  test("parse and eval 0 01 == [ [[cons] [sap]] [[nil] [swap]] ]") {
    import eval._
    assert(parse("0 01").eval(Data.empty) == Right(Data.Output( Quoted(Quoted(nil),Quoted(swap)), Quoted(Quoted(cons),Quoted(sap)))) )
  }
  test("parse and simplify 1 == R") {
    import eval._
    assert( parse("1").simplify == Program(R))
  }
  test("parse and symplify 01 == L ") {
    import eval._
    assert( parse("01").simplify == Program(L))
  }

}

class PrintSuite extends FunSuite {

  test("print empty program") {
    assert(ZeroOne.Program.empty.toString == "")
  }
  test("print empty quotation") {
    assert(ZeroOne.Quoted.empty.toString == "[]")
  }
  test("print a simple program") {
    assert(Program(Quoted(Program(NOOP1)),Quoted(Program(NOOP2))).toString == "[NOOP1] [NOOP2]")
  }
  test("print a simple program from data") {
    assert(Program(Data(Quoted(NOOP1), Quoted(NOOP2))).toString == "[NOOP1] [NOOP2]")
  }
}

class EvalSuite extends FunSuite {
  import eval._
  test("eval empty program should not change stack") {
    assert(ZeroOne.Program.empty.eval(Data(Quoted(Program(NOOP1)), Quoted(Program(NOOP2)))) == Right(Data.Output(Quoted(Program(NOOP1)), Quoted(Program(NOOP2)))))
  }
  test("eval nil should push empty quotation on to stack") {
    assert(Program(nil).eval(Data.empty) == Right(Data.Output(Quoted.empty)))
  }
  test("[NOOP1] [NOOP2] NOOP1 == [NOOP1] [NOOP2]") {
    assert( Program(Quoted(Program(NOOP1)), Quoted(Program(NOOP2)), NOOP2).eval(Data.empty) == Right(Data.Output(Quoted(Program(NOOP2)), Quoted(Program(NOOP1)))))
  }
  test("eval from data") {
    assert( Data(Quoted(NOOP1), Quoted(NOOP2)).eval(Data.empty) == Right(Data.Output(Quoted(NOOP1),Quoted(NOOP2))))
  }
  test("[NOOP1] [NOOP2] [NOOP3] zap == [NOOP1] [NOOP2]") {
    assert( Program(Quoted(NOOP1),Quoted(NOOP2), Quoted(NOOP3), zap).eval(Data.empty) == Right(Data.Output(Quoted(NOOP2), Quoted(NOOP1))))
  }
  test("[NOOP1] [NOOP2] zap [NOOP3] == [NOOP1] [NOOP3]") {
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), zap, Quoted(NOOP3)).eval(Data.empty) == Right(Data.Output(Quoted(NOOP3), Quoted(NOOP1))))
  }
  test("[NOOP1] [NOOP2] [zap] i == [NOOP1]") {
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), Quoted(zap), i).eval(Data.empty) == Right(Data.Output(Quoted(NOOP1))))
    assert( Program(Data(Quoted(zap), Quoted(NOOP2), Quoted(NOOP1)), i).eval(Data.empty) == Right(Data.Output(Quoted(NOOP1))))
  }
  test("[zap] [NOOP1] swap == [NOOP1] [zap]") {
    assert( Program(Quoted(zap), Quoted(NOOP1), swap).eval(Data.empty) == Right(Data.Output(Quoted(zap),Quoted(NOOP1))))
  }
  test("[NOOP1] [NOOP2] cons == [[NOOP1] NOOP2]") {
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), cons).eval(Data.empty) == Right(Data.Output(Quoted(Quoted(NOOP1), NOOP2))))
  }
  test("[NOOP1] [NOOP2] [NOOP3] [cons] [zap] sap == [NOOP1] [NOOP2] [NOOP3] zap cons == [[NOOP1] NOOP2]") {
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), Quoted(NOOP3), Quoted(cons), Quoted(zap), sap).eval(Data.empty) == Right(Data.Output(Quoted(Quoted(NOOP1), NOOP2))))
  }
  test("[NOOP1] [NOOP2] [NOOP3] [zap] [cons] sap == [NOOP1] [NOOP2] [NOOP3] cons zap == [NOOP1]") {
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), Quoted(NOOP3), Quoted(zap), Quoted(cons), sap).eval(Data.empty) == Right(Data.Output(Quoted(NOOP1))))
  }
  test("[NOOP1] [NOOP2] [zap] k == [NOOP1]") {
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), Quoted(zap), k).eval(Data.empty) == Right(Data.Output.empty))
    assert( Program(Quoted(NOOP1), Quoted(NOOP2), Quoted(zap), R).eval(Data.empty) == Right(Data.Output.empty))
  }
  test("[NOOP2] [NOOP3] [NOOP1] [zap] [NOOP4] z == [NOOP2] [NOOP3]") {
    assert( Program(Quoted(NOOP2), Quoted(NOOP3), Quoted(NOOP1), Quoted(zap), Quoted(NOOP4), z).eval(Data.empty) == Right(Data.Output(Quoted(NOOP3),Quoted(NOOP2))))
    assert( Program(Quoted(NOOP2), Quoted(NOOP3), Quoted(NOOP1), Quoted(zap), Quoted(NOOP4), L).eval(Data.empty) == Right(Data.Output(Quoted(NOOP3),Quoted(NOOP2))))
  }
}

class DebugSuite extends FunSuite {
  // a debug combinator? "... [E] [D] [C] [... H] [A B ...] step == ... [E] [D] [C] A [... H A] [B ...]"
}