package tip.analysis

import tip.cfg._
import tip.ast.AstNodeData.DeclarationData
import tip.lattices.IntervalLattice._
import tip.lattices._
import tip.solvers._

trait VariableSizeAnalysisWidening extends IntervalAnalysisWidening {
  /**
   * Type values borders
   */
  override val B = Set[Num](
    0, 1,
    Byte.MinValue, Byte.MaxValue,
    Short.MinValue, Short.MaxValue,
    Int.MinValue, Int.MaxValue,
    MInf, PInf
  )

// Everything commented below is just my attempt to create a bitwise lattice, which was unsuccessful
//  private def asInt(num: Num): Option[Int] = num match {
//    case IntNum(i) => Some(i)
//    case PInf => null
//    case MInf => None
//  }
//
//  private def nextPowerOfTwo(n: Num): Num = {
//    if (asInt(n).isEmpty) return 0
//    if (asInt(n) == null) return PInf
//    if (n < 1) 1
//    else Math.pow(2, Math.ceil(Math.log(asInt(n).get) / Math.log(2))).toInt
//  }
//
//  private def prevPowerOfTwo(n: Num): Num = {
//    if (asInt(n).isEmpty) return 0
//    if (asInt(n) == null) return PInf
//    if (n < 1) 0
//    else Math.pow(2, Math.floor(Math.log(asInt(n).get) / Math.log(2))).toInt
//  }
//
//  override def widenInterval(x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element =
//    (x, y) match {
//      case (IntervalLattice.EmptyInterval, _) => y
//      case (_, IntervalLattice.EmptyInterval) => x
//      case ((l1, h1), (l2, h2)) => // let a := min(l1, l2), b := max(h1, h2) in ω([a, b]) = [prevPowerOfTwo(a), nextPowerOfTwo(b)]
//        (prevPowerOfTwo(if (l1 > l2) l2 else l1), nextPowerOfTwo(if (h1 > h2) h1 else h2))
//    }
}

object VariableSizeAnalysis {

  object Intraprocedural {

    /**
     * Variable size analysis, using the worklist solver with init and widening.
     */
    class WorklistSolverWithWidening(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
      extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWidening[CfgNode]
        with VariableSizeAnalysisWidening

    /**
     * Variable size analysis, using the worklist solver with init, widening, and narrowing.
     */
    class WorklistSolverWithWideningAndNarrowing(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
      extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[CfgNode]
        with VariableSizeAnalysisWidening {

      val narrowingSteps = 5
    }
  }
}
