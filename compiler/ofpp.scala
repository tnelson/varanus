package edu.brown.simon

import java.math.BigInteger

/*****************************************************************************
Simon OF++ interface
Created by TN in Dec 2015
Modified by TN in May 2016

For now, the toString methods of these classes produce ovs-ofctl command syntax.

I've left some structure out here. For instance, a match is represented by an
equality between terms, without forcing one to be a field, etc.

******************************************************************************/



// An OF++ rule contains a list of matches, a list of actions,
// an optional hard time out, and an optional action-on-timeout list.
// Priority is an *Option* because we may want to defer assigning a
//   concrete priority until we have a list of rules, but must have one before toString() is called.
// Same deal for cookie, although at the moment we don't assign None.
class OFPPRule(val cookie: Option[BigInteger],
			   val m: List[OFPPMatch],
	           val a: List[OFPPAction],
	           val priority: Option[Int],
	           val hardtimeout: Option[Int], // SECONDS
	           val aontimeout: Option[List[OFPPAction]]) {

	// Pass all matches, will condense bit matches (if any)
	def combineBitwiseRegisterMatches(matches: List[OFPPMatch]): List[OFPPMatch] = {
		def folder(acc: Int, m: OFPPMatch): Int = {
			val bitVal = scala.math.pow(m.v.replace("bit", "").toInt, 2).toInt
			println("Combining reg9 bit matches. Had:"+acc+"; m was: +"+m+" bitVal was: "+bitVal+".")
			acc + bitVal
		}
		val nonBits: List[OFPPMatch] = matches.filter({m => !m.v.startsWith("bit")})
		val bits = matches.filter({m => m.v.startsWith("bit")})

		if(bits.size > 0) {
			println("combining bitwise reg9: "+bits+" for "+matches)
			val combinedVal = "0x"+Integer.toHexString(bits.foldLeft(0)(folder));
			new OFPPMatch(EGRESS_SAME_EQ_REGISTER, combinedVal+"/"+combinedVal) :: nonBits
		}
		else
		    nonBits
	}

	def toOVSString(learnlearn: Boolean) = {
		// Only an action set within a learn-learn should be wrapped in {}
		val bracesHuhL = if(learnlearn) "{" else ""
		val bracesHuhR = if(learnlearn) "}" else ""

		val combinedMatches = combineBitwiseRegisterMatches(m)

		combinedMatches.mkString(",")+(if(m.size>0)","else"")+
		"cookie="+(cookie match {case None => "0" case Some(v) => "0x"+v.toString(16)})+","+
		"priority="+(priority match {case None => "0" case Some(v) => v})+
		(hardtimeout match { case None => "," case Some(v) => ",hard_timeout="+v+"," })+
		"actions="+bracesHuhL+
	      (a match { case _::_ => a.mkString(",") case _ => "drop" })+
	      (aontimeout match { case None => "" case Some(v) => ",timeout_act("+v.mkString(",")+")" })+
	      bracesHuhR
	}
}

// A match is just an equality.
class OFPPMatch(val f: String, val v: String) {
	override def toString = f+"="+v
}

// Different rule block specifications:
// use_atomic_table will automatically put the learned rule in the *next* (by atomic-register value) rule block.
// use_rule_table will automatically put the learned rule in the *current* rule block.
sealed trait OFPPRuleBlock
case object UseAtomicTableIngress extends OFPPRuleBlock {
	override def toString = "use_atomic_table=INGRESS"
}
case object UseRuleTable extends OFPPRuleBlock {
	override def toString = "use_rule_table=1"
}
case object UseAtomicTableEgress extends OFPPRuleBlock {
	override def toString = "use_atomic_table=EGRESS"
}
case class UseConstantTable(val t: Int, val mopt: Option[Int]) extends OFPPRuleBlock {
	override def toString = {
		mopt match {
			case Some(m) => "table="+t+",metadata="+m
			case None => "table="+t
		}
	}
}

// Different action types
sealed trait OFPPAction
case class OFPPLearn(tolearn: OFPPRule, block: OFPPRuleBlock, preserveTimeout: Boolean) extends OFPPAction {
	val preserveStr = if(preserveTimeout) "preserve_timeout=1," else ""
	override def toString = "learn_learn("+block.toString+","+preserveStr+tolearn.toOVSString(true)+")"
}
// Delete everything in the current table of this rule
case object OFPPDeleteUseRuleTable extends OFPPAction {
	override def toString = "learn_delete(use_rule_table=1)"
}
case class OFPPDeleteMatched(m: List[OFPPMatch]) extends OFPPAction {
	override def toString = "learn_delete("+m.mkString(",")+")"
}

case object SendToController extends OFPPAction {
		override def toString = "controller"
}

//////////////////
// TODO: used only for Compiler 1.1, not 2.0. Will be removed.
case object IncAtomicTableIngress extends OFPPAction {
	override def toString = "increment_table_id(INGRESS)"
}

case object IncAtomicTableEgress extends OFPPAction {
	override def toString = "increment_table_id(EGRESS)"
}
//////////////////

// Examples from docs
// move:NXM_NX_REG0[0..5]->NXM_NX_REG1[26..31]
// move:NXM_NX_REG0[0..15]->NXM_OF_VLAN_TCI[]
// Defer applies to LHS of arrow. So CopyFromRegister has a defer; for CopyTo caller must produce term string.
case class CopyIntoRegister(val regnum: Int, val startbit: Int, val endbit: Int, val lhs: String) extends OFPPAction {
	override def toString = "move:"+lhs+"->NXM_NX_REG"+regnum+"["+startbit+".."+endbit+"]"
}

/******************************************************************************/

// Thrown if trying to compile a badly formed query
class QueryNotWellFormedException(val wfErrors: List[NotWellFormed]) extends Exception
{
	override def toString = "QueryNotWellFormedException: "+wfErrors.toString
}

// Thrown if we run out of priority space
class OutOfPriorities() extends Exception

/******************************************************************************/
/******************************************************************************/