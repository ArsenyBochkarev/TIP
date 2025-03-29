package tip.analysis

import shapeless.{:+:, Inl, Inr}
import tip.ast._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.ast.AstOps.AstOp
import tip.solvers._
import tip.cfg._

abstract class ReachingDefinitionsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(false) {

  // Using AAssignStmt for convenience
  val lattice: MapLattice[CfgNode, PowersetLattice[AAssignStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def removedefs(s: lattice.sublattice.Element, id: AIdentifier) =
    s.filter(as => as.left match {
      case innerId: AIdentifier => id.name != innerId.name
      case _ => true
    })

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case varr: AVarStmt => // [[var x]] = {var x}, however, if we only look at assignments, we don't actually care about this one
            s
          case as: AAssignStmt => // [[x = E]] = removedefs(JOIN(n), x) ∪ {x = E}
            as.left match {
              case id: AIdentifier => lattice.sublattice.lub(removedefs(s, id), Set(as))
              case _ => s
            }
          case _ => s // [[n]] = JOIN(n)
        }
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case _ => s
    }
}

/**
 * Reaching definitions analysis that uses the simple fixpoint solver.
 */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
  extends ReachingDefinitionsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies