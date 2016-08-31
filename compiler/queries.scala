package edu.brown.simon

/*****************************************************************************
Simon compiled queries: core types and well-formedness checking
Created by TN October 2015
Modified by TN May 2016 to restrict to forward-chain queries only.
******************************************************************************/

/*
  A term is either an integer constant or an event field identifier
*/
sealed trait Term
case class TConst(v: String) extends Term
case class TField(p: EventID, f: String) extends Term

/*
  A literal is a positive or negative atomic formula over terms; for now, just equality and inequality.
  I have deliberately left out "or" and the usual inductive structure of formulas.
*/
sealed trait Literal
case class LEq(t1: Term, t2: Term) extends Literal
case class LNEq(t1: Term, t2: Term) extends Literal

/*
  An observation can be *matched* by points in the stream of incoming events.
  They may have negative requirements to be matched (e.g., "not see") but still
  match finitely observable event sequences.
*/

sealed trait Observation
// "see v : t | cond within dur"
case class OSee(val v: EventID,
	            val t: EventType,
	            val cond: List[Literal],
	            val dur: Option[Int]) extends Observation {
	override def toString = "OSee("+v+": "+t+","+cond+","+dur+")"
}

// "not see v : t | cond within dur"
// dur is mandatory for "not see" unless type is EgressSame
case class ONotSee(val v: EventID,
	               val t: EventType,
	               val cond: List[Literal],
	               val dur: Option[Int]) extends Observation {
	override def toString = "ONotSee("+v+": "+t+","+cond+","+dur+")"
}
// "not see v1 : t1 | cond1 until v2 : t2 | cond2 within dur"
case class ONotSeeUntil(val v1: EventID,
	                    val cond1: List[Literal],
	                    val v2: EventID,
	                    val t: EventType,
	            		val cond2: List[Literal],
	            		val dur: Option[Int]) extends Observation {
	override def toString = "ONotSeeUntil("+v1+" | "+cond1+"; "+v2+": "+t+","+cond2+"; "+dur+")"
}

/*****************************************************************************/

// Well-formedness errors in queries
sealed trait NotWellFormed

// Identifiers must be ordered let* style
case class NWFLetStar(o: Observation, violations: List[EventID]) extends NotWellFormed
{
	override def toString = "NWFLetStar(o="+o+"       vs="+violations+")"
}

// Identifiers must be ordered let* style, with the added restriction that
// if e.fld is referred to in o_k, it must be matched in each observation between o_k and
// its definition. This is the sufficient syntactic condition we use to compile to
// a constant number of tables. See test cases for examples.
case class NWFForwardChain(o: Observation, violations: List[Term]) extends NotWellFormed
{
	override def toString = "NWFForwardChain(o="+o+"       vs="+violations+")"
}

// Can't duplicate event names across observations
case class NWFReuseEventName(o: Observation) extends NotWellFormed

// Use only valid event fields
//  (validity sometimes depends on context; e.g., "outPt" is invalid on ingress events)

		/*  Re: locPt vs. outPt:

			Ingress: only locPt is available
			Egress, egress-same: only outPt is available.

			For clarity, we force the input query to make this distinction explicitly,
			rather than substituting locPt -> outPt on egress observations.

			While in_port value is technically available in egress events, later versions
			of OVS/OF may modify it in the application tables, making it untrustworthy. Thus,
			if you need to see both in and out ports, just use an ingress/egress-same pairing.
		*/

case class NWFBadFieldUse(o: Observation, violations: List[Term]) extends NotWellFormed

// egress same must follow ingress
case class NWFEgressSameNotAfterIngress(o: Observation) extends NotWellFormed

// Every Eq/NEq term must refer to current event somewhere
case class NWFNonLocalEquality(o: Observation, violations: List[Literal]) extends NotWellFormed

// Priming on variables has semantics: we enforce that p' only occurs in an EgressSame after an Ingress p.
case class NWFBadVariablePriming(viol: EventID, prev: Option[EventID]) extends NotWellFormed
/*****************************************************************************/

/*
	A query is a sequence of observations; a well-formed query must obey the rules above.

	Using a class for this, rather than just typedef, because may have additional fields in future.
*/

class Query(val sequence: List[Observation]) {

	def variablesUsedInCondition(literals: List[Literal]): List[EventID] = {
		literals.foldLeft (List[EventID]()) ((acc,curr) => QueryUtils.variablesUsedInLiteral(curr) ++ acc)
	}

	////////////////////////////////////////////////////
	// Produce NWFEgressSameNotAfterIngress if needed
	//   and NWFBadVariablePriming.
	def egressSameAndPriming(): List[NotWellFormed] = {
		def egressSameHelper(os: List[Observation], priorisposingress: Boolean): List[NotWellFormed] = {
			os match {
				case o :: rest =>
					QueryUtils.getOType(o) match {
						case Ingress(_) if QueryUtils.getOSign(o) => egressSameHelper(rest, true)
						case Ingress(_) => egressSameHelper(rest, false)
						case Egress(_) =>  egressSameHelper(rest, false)
						case EgressSame(_) if priorisposingress => egressSameHelper(rest, false)
						case EgressSame(_) => new NWFEgressSameNotAfterIngress(o) :: egressSameHelper(rest, false)
					}

				case _ => List()
			}
		}
		// EgressSame var must be primed version of the preceding Ingress variable
		def primingHelper(os: List[Observation], optpriorvar: Option[EventID]): List[NotWellFormed] = {
			os match {
				case o :: rest =>
					val v = QueryUtils.getOTriggerVar(o)
					val priorvar = optpriorvar.getOrElse("ERROR") // should always be Some(pv) when used for EgressSame
					QueryUtils.getOType(o) match {
						case Ingress(_) if v.endsWith("'") => NWFBadVariablePriming(v, optpriorvar) :: primingHelper(rest, Some(v))
						case Egress(_) if v.endsWith("'") => NWFBadVariablePriming(v, optpriorvar) :: primingHelper(rest, Some(v))
						case EgressSame(_) if !v.endsWith("'") => NWFBadVariablePriming(v, optpriorvar) :: primingHelper(rest, Some(v))
						case EgressSame(_) if v.replace("'", "") != priorvar => NWFBadVariablePriming(v, optpriorvar) :: primingHelper(rest, Some(v))
						case _ => primingHelper(rest, Some(v))
					}

				case _ => List()
			}
		}

		// Doing both in the same match was getting tangled
		egressSameHelper(sequence, false) ++
		primingHelper(sequence, None)
	}

	////////////////////////////////////////////////////
	// Return set of ways the sequence violates forward-chain let* variable ordering
	def bindingOrder(d: Boolean): List[NotWellFormed] = {
		// helper: check individual observation's binding
		def checkLetStar(v: EventID,
		           bound: List[EventID],
			       o: Observation,
			       cond: List[Literal]) : List[NotWellFormed] = {
			val violations = variablesUsedInCondition(cond).filter(
					varused => ! (v::bound).contains(varused));
			violations match {
			  case List() => List()
			  case _ => List(NWFLetStar(o, violations))
			}
		}
		def checkChain(v: EventID,
		           lastVarIfPos: Option[EventID],    // if prior obs exists and is positive
		           nextLastVarIfPos: Option[EventID], // if prior-prior obs exist and is positive
		           otherVarIfUntil: Option[EventID], // if we're in the blocking part of an until
		           chainedTerms: List[Term],         // terms appearing in prior obs (if any)
			       o: Observation,
			       cond: List[Literal]) : List[NotWellFormed] = {

			def chainViolationFilter(t: Term): Boolean = {
				//println("chainViolationFilter for t="+t+" in cond\n"+cond)

				val lastVarWouldBeOK = lastVarIfPos match {
					// If t refers to prior observation
					case Some(lastv) if QueryUtils.termRefersToEvent(t,lastv) => true
					// or t refers to ingress for prior egresssame
					case Some(lastv) if lastv.endsWith("'") && QueryUtils.termRefersToEvent(t, lastv.replace("'", "")) => true
					case _ => false
				};
				val lastLastVarWouldBeOK = nextLastVarIfPos match {
					// If t is in second ingress in in-egsame-in-egsame sequence, and t refers to first ingress
					//   Must have a primed *lastVarIfPos* inner value
					case Some(llv) if QueryUtils.termRefersToEvent(t,llv) && lastVarIfPos.getOrElse("").endsWith("'") => true
					// If t is in last egsame in an in-egsame-in-egsame sequence, and t refers to first ingress
					case Some(llv) if llv.endsWith("'") && QueryUtils.termRefersToEvent(t, llv.replace("'", "")) => true
					case _ => false
					// Note: we can trust "'" syntax because WFQ check enforces semantic priming
				}
				val otherVarWouldBeOK = otherVarIfUntil match {
					  case Some(otherv) => QueryUtils.termRefersToEvent(t,otherv);
					  case _ => false
				};
				//println("lastVarWouldBeOK: "+lastVarWouldBeOK)
				//println("lastLastVarWouldBeOK: "+lastLastVarWouldBeOK)
				//println("otherVarWouldBeOK: "+otherVarWouldBeOK)

				 // Is a violation if none of: current event field,
				 // prior event field (if positive+exists), chained from previous observation.
				!chainedTerms.contains(t) &&
				!lastVarWouldBeOK &&
				!lastLastVarWouldBeOK &&
				!otherVarWouldBeOK &&
				!QueryUtils.termRefersToEvent(t,v)
			}

			val violations = QueryUtils.fieldTermsUsedInLiterals(cond, None).filter(chainViolationFilter)

			violations match {
			  case List() => List()
			  // Don't return these forward-chaining errors if running
			  // the dynamic-allocation compiler!
			  case _ if !d => List(NWFForwardChain(o, violations))
			  case _ => List()
			}

		}
		// helper: recur on each observation in sequence,
		//         carrying bound variables and chained terms as we go
		def bindingOrderHelper(os: List[Observation],
			                   bound: List[EventID],
			                   lastVarIfPos: Option[EventID],
			                   nextLastVarIfPos: Option[EventID],
			                   chainedTerms: List[Term]) : List[NotWellFormed] = {
			os match {
				case o :: rest =>
					// Careful to distinguish chainedTerms (terms explicitly matched) from the allowance
					// made for the prior observation. Can refer safely to anything in previous observation,
					// (if positive) but otherwise needs to be explicitly matched.
					// Prior observation allowance carries over (via nextLastVarIfPos/lastVarIfPos) if this is an EgressSame (c.f. nat3.sq)
					o match {
						case OSee(v,t,cond,_) =>
							checkLetStar(v, bound, o, cond) ++
							checkChain(v, lastVarIfPos, nextLastVarIfPos, None, chainedTerms, o, cond) ++
							bindingOrderHelper(rest, v :: bound, Some(v), lastVarIfPos, QueryUtils.fieldTermsUsedInLiterals(cond, None))
						case ONotSee(v,_,cond,_) =>
							checkLetStar(v, bound, o, cond) ++
							checkChain(v, lastVarIfPos, nextLastVarIfPos, None, chainedTerms, o, cond) ++
							bindingOrderHelper(rest, v :: bound, None, lastVarIfPos, List())
						case ONotSeeUntil(v1,cond1,v2,t,cond2,_) =>
							// trigger var (second var) in an until can be referred to in precondition part (second part)
							checkLetStar(v1, v2 :: bound, o, cond1) ++
							checkLetStar(v2, bound, o, cond2) ++
							checkChain(v1, lastVarIfPos, nextLastVarIfPos, Some(v2), chainedTerms, o, cond1) ++ // check both conditions
							checkChain(v2, lastVarIfPos, nextLastVarIfPos, None,     chainedTerms, o, cond2) ++
							bindingOrderHelper(rest, v1 :: v2 :: bound, Some(v2), lastVarIfPos, QueryUtils.fieldTermsUsedInLiterals(cond2, None))
					}
				case List() => List()
			}
		}
		// carry last variable and next-to-last variable to support chaining across ingress/egress-same pairs
		bindingOrderHelper(sequence, List(), None, None, List())
	}

	////////////////////////////////////////////////////
	// Return observations that re-use prior event names
	def noReuse(): List[NotWellFormed] = {
		def noReuseHelper(os: List[Observation], sofar: List[EventID]): List[NotWellFormed] = {
			os match {
				case o :: rest =>
					o match {
						case OSee(v,_,_,_) => if(sofar.contains(v)) List(new NWFReuseEventName(o))
								  		  	  else noReuseHelper(rest, v :: sofar)
						case ONotSee(v,_,_,_) => if(sofar.contains(v)) List(new NWFReuseEventName(o))
								  		  	     else noReuseHelper(rest, v :: sofar)
						case ONotSeeUntil(v1,_,v2,_,_,_) => if(sofar.contains(v1)) List(new NWFReuseEventName(o))
															else if(sofar.contains(v2)) List(new NWFReuseEventName(o))
								  		  	  				else noReuseHelper(rest, v1 :: v2 :: sofar)
					}
				case List() => List()
			}
		}
		noReuseHelper(sequence, List())
	}

	////////////////////////////////////////////////////
	// Check for local equalities
	def localEquality(): List[NotWellFormed] = {

		def checkEqLocality(o: Observation, e: EventID, cond: List[Literal]): List[NotWellFormed] = {
			val violations = cond.filter(l => !QueryUtils.variablesUsedInLiteral(l).contains(e))
			violations match {
				case List() => List()
				case _ => List(NWFNonLocalEquality(o, violations))
			}
		}
		def localHelper(os: List[Observation]): List[NotWellFormed] = {
			os match {
				case o :: rest =>
					o match {
						case OSee(v,_,cond,_) =>
							checkEqLocality(o, v, cond) ++ localHelper(rest)
						case ONotSee(v,_,cond, _) =>
						    checkEqLocality(o, v, cond) ++ localHelper(rest)
						case ONotSeeUntil(v1, cond1, v2, _, cond2, _) =>
							checkEqLocality(o, v1, cond1) ++ checkEqLocality(o, v2, cond2) ++ localHelper(rest)
					}
				case List() => List()
			}
		}
		localHelper(sequence)
	}

	////////////////////////////////////////////////////
	// Return observations that mis-use field names

	def getValidNamesForType(t: EventType): List[String] = {
		// These are sanitized field values: underscores removed, etc.
		val validEthFields = List("dltyp", "dlsrc", "dldst", "dlvlan")
		val validARPFields = List("arpop", "arpsha", "arptha", "arpspa", "arpsha")
		val validIPFields = List("nwsrc", "nwdst", "nwproto")
		val validTCPFields = List("tpsrc", "tpdst")

		// Don't allow use of fields that are already bound implicitly by packet type.
		def getValidNamesForPacketType(pt: PacketType): List[String] = {
			pt match {
				case EthPacket => validEthFields
				case ARPPacket => (validEthFields ++ validARPFields).filterNot(x => x == "dltyp")
				case IPPacket => (validEthFields ++ validIPFields).filterNot(x => x == "dltyp")
				case TCPPacket => (validEthFields ++ validIPFields ++ validTCPFields).filterNot(x => x == "dltyp" || x == "nwproto")
			}
		}

		// See comment at declaration of NWFBadFieldUse
		t match {
			case Ingress(pt) => "locpt" :: getValidNamesForPacketType(pt)
			case Egress(pt) => "outpt" :: getValidNamesForPacketType(pt)
			case EgressSame(pt) => "outpt" :: getValidNamesForPacketType(pt)
		}
	}

	def validFieldNames(): List[NotWellFormed] = {
		def validHelper(os: List[Observation], env: Map[EventID, EventType]): List[NotWellFormed] = {

			def validHelper2(o: Observation, env: Map[EventID, EventType], cond: List[Literal]): List[NotWellFormed]  = {
				val terms = QueryUtils.fieldTermsUsedInLiterals(cond, None)
				// For each term, does it use a valid field?
				val issues = terms.filter({
					case TField(v, f) =>
						val sf = QueryUtils.sanitizeFieldName(f)
						env.get(v) match {
							case Some(t) if !getValidNamesForType(t).contains(sf) => true
							case _ => false
						}
					case _ => false
					})

				if(issues.size > 0) List(NWFBadFieldUse(o, issues))
				else List()
			}

			os match {
				case (o @ OSee(v,t,cond,_)) :: rest =>
					val newenv = env + (v -> t)
					validHelper2(o, newenv, cond) ++
					validHelper(rest, newenv)
				case (o @ ONotSee(v,t,cond,_)) :: rest =>
					val newenv = env + (v -> t)
					validHelper2(o, newenv, cond) ++
					validHelper(rest, newenv)
				case (o @ ONotSeeUntil(v1,cond1,v2,t,cond2,_)) :: rest =>
					// When adding separate types in until, don't forget to change this.
					val newenv = env + (v1 -> t) + (v2 -> t)
					validHelper2(o, newenv, cond1) ++
					validHelper2(o, newenv, cond2) ++
					validHelper(rest, newenv)
				case Nil => List()
			}
		}

		validHelper(sequence, Map[EventID, EventType]())
	}

	////////////////////////////////////////////////////
	// Return set of ways the sequence is not well formed. (Union of different criteria)
	def wellFormedCheck(d: Boolean): List[NotWellFormed] = {
		return bindingOrder(d) ++
		       noReuse() ++
		       egressSameAndPriming() ++
		       localEquality() ++
		       validFieldNames()
	}

	def wellFormedCheck(): List[NotWellFormed] = wellFormedCheck(false)

}

/*****************************************************************************
  Helper functions for queries: extracting identifiers used, etc.
*****************************************************************************/

object QueryUtils {

	def sanitizeFieldName(f: String): String = {
		// Resolve some potential confusion
		val pre = f match {
			case "ipsrc" => "nwsrc"
			case "ipdst" => "nwdst"
			case "in_port" => "locpt"
			case "vlan_id" => "dlvlan"
			case _ => f
		}
		// Sanitize
		pre.toLowerCase().replaceAll("_", "")
	}

	def variablesUsedInLiteral(l: Literal): List[EventID] = {
		l match {
			case LEq(t1, t2)   => List(t1,t2) collect {t => t match { case TField(p,f) => p}}
			case LNEq(t1, t2)  => List(t1,t2) collect {t => t match { case TField(p,f) => p}}
		}
	}

	def fieldNamesUsedInLiteral(l: Literal, ofEvent: Option[EventID]): List[String] = {
		def varmatch(p: EventID): Boolean = {
			ofEvent match { case Some(p2) => p == p2 case None => true }
		}

		l match {
			case LEq(t1, t2)   => List(t1,t2) collect {t => t match { case TField(p,f) if varmatch(p) => f}}
			case LNEq(t1, t2)  => List(t1,t2) collect {t => t match { case TField(p,f) if varmatch(p) => f}}
		}
	}

	// Option: pass None if want to gather terms related to all event IDs
	def fieldTermsUsedInLiteral(l: Literal, ofEvent: Option[EventID]): List[Term] = {
		def varmatch(p: EventID): Boolean = {
			ofEvent match { case Some(p2) => p == p2 case None => true }
		}
		l match {
			case LEq(t1, t2)   =>
				List(t1,t2) collect {t => t match { case TField(p,f) if varmatch(p) => t}}
			case LNEq(t1, t2)  =>
				List(t1,t2) collect {t => t match { case TField(p,f) if varmatch(p) => t}}
		}
	}

	def fieldTermsUsedInLiterals(literals: List[Literal], ofEvent: Option[EventID]): List[Term] = {
		literals.foldLeft (List[Term]()) ((acc,l) => fieldTermsUsedInLiteral(l, ofEvent) ++ acc)
	}

	def fieldNamesUsedInLiterals(literals: List[Literal], ofEvent: Option[EventID]): List[String] = {
		literals.foldLeft (List[String]()) ((acc,l) => fieldNamesUsedInLiteral(l, ofEvent) ++ acc)
	}

	def literalsUsedInObservations(os: List[Observation]): List[Literal] = {
		os.foldLeft (List[Literal]()) ((acc, o) => literalsUsedInObservation(o) ++ acc)
	}

	def literalsUsedInObservation(o: Observation): List[Literal] = {
		// This func will return the literals in *BOTH* conditions for an until; beware!
		o match {
			case OSee(_, _, cond, _) => cond
			case ONotSee(_, _, cond, _) => cond
			case ONotSeeUntil(_, cond1, _, _, cond2, _) => (cond1 ++ cond2).distinct
		}
	}

	def termRefersToEvent(t: Term, ev: EventID): Boolean = {
		t match {
			case TField(p, _) if ev == p => true
			case _ => false
		}
	}

	def litRefersToEvent(ev: EventID): (Literal => Boolean) = {
		{l =>
			val vars = variablesUsedInLiteral(l)
			vars.contains(ev)
    	}
    }
    def notLitRefersToEvent(ev: EventID): (Literal => Boolean) = {
    	{l => !litRefersToEvent(ev)(l)}
    }

	def getOType(o: Observation): EventType = {
		o match {
			case OSee(v, t, cond, dur) => t
			case ONotSee(v, t, cond, dur) => t
			case ONotSeeUntil(v1, cond1, v2, t2, cond2, dur) => t2
		}
	}

	def getOTriggerVar(o: Observation): EventID = {
		o match {
			case OSee(v, t, cond, dur) => v
			case ONotSee(v, t, cond, dur) => v
			case ONotSeeUntil(v1, cond1, v2, t2, cond2, dur) => v2
		}
	}

	def getOSign(o: Observation): Boolean = {
		o match {
			case OSee(v, t, cond, dur) => true
			case ONotSee(v, t, cond, dur) => false
			case ONotSeeUntil(v1, cond1, v2, t2, cond2, dur) => true
		}
	}

}

/*****************************************************************************/

object QueryTests {

	def main(args: Array[String]) {
		println("Running Query well-formedness check test cases... no messages is good.")
		println("----------------------------------------------------------------------")

		val qEmpty = new Query(List())
		expectEmpty("qEmpty", qEmpty.wellFormedCheck())

		val qSingle = new Query(List(new OSee("p",Ingress(EthPacket),List(), None)))
		expectEmpty("qSingle", qSingle.wellFormedCheck())

		val qPrimed1 = new Query(List(new OSee("p'",Ingress(EthPacket),List(), None)))
		expectNotEmpty("qPrimed1", qPrimed1.wellFormedCheck())
		val qPrimed2 = new Query(List(new OSee("p",Ingress(EthPacket),List(), None),
        	            	     	  new OSee("q",EgressSame(EthPacket), List(), None)))
		expectNotEmpty("qPrimed2", qPrimed2.wellFormedCheck())
		val qPrimed3 = new Query(List(new OSee("p",Ingress(EthPacket),List(), None),
        	            	     	  new OSee("p'",Egress(EthPacket), List(), None)))
		expectNotEmpty("qPrimed3", qPrimed3.wellFormedCheck())
		val qPrimedOk = new Query(List(new OSee("p",Ingress(EthPacket),List(), None),
        	            	     	  new OSee("p'",EgressSame(EthPacket), List(), None)))
		expectEmpty("qPrimedOk", qPrimedOk.wellFormedCheck())

		// This is OK; no longer have a positive first requirement
		val qSingleTimeout = new Query(List(new ONotSee("p",Ingress(EthPacket), List(), Some(1))))
		expectEmpty("qSingleTimeout", qSingleTimeout.wellFormedCheck())

		val q2Chain = new Query(List(new OSee("p",Ingress(EthPacket),List(), None),
        	            	     	 new OSee("q",Ingress(EthPacket),List(), None)))
		expectEmpty("q2Chain", q2Chain.wellFormedCheck())

		val qUnbound = new Query(List(new OSee("p",Ingress(EthPacket),List(), None),
        	                		  new OSee("q",Ingress(EthPacket),List(LEq(TField("p","dlSrc"), TField("r","dlDst"))), None)))
		expectNotEmpty("qUnbound", qUnbound.wellFormedCheck())

		val qReverseBound = new Query(List(new OSee("q",Ingress(EthPacket),List(LEq(TField("p","dlSrc"), TField("q","dlDst"))), None),
			                               new OSee("p",Ingress(EthPacket),List(), None)))
		expectNotEmpty("qReverseBound", qReverseBound.wellFormedCheck())

		val qReuse = new Query(List(OSee("p",Ingress(EthPacket),List(), None),
                       		    	OSee("p",Ingress(EthPacket),List(), None)))
		expectNotEmpty("qReuse", qReuse.wellFormedCheck())

		val qEgressSame1 = new Query(List(OSee("p1",EgressSame(EthPacket),List(), None),
                       		    	     OSee("p2",Ingress(EthPacket),List(), None)))
		expectNotEmpty("qEgressSame1", qEgressSame1.wellFormedCheck())

		val qEgressSame2 = new Query(List(OSee("p1",Egress(EthPacket),List(), None),
                       		    	     OSee("p2",EgressSame(EthPacket),List(), None)))
		expectNotEmpty("qEgressSame2", qEgressSame2.wellFormedCheck())

		val qEgressSame3 = new Query(List(ONotSee("p1",Ingress(EthPacket),List(), Some(5)),
                       		    	      OSee("p2",EgressSame(EthPacket),List(), None)))
		expectNotEmpty("qEgressSame3", qEgressSame3.wellFormedCheck())

		/////////////////////////////////////////////////
		// Fields must be valid
		val qBadField1 = new Query(List(new OSee("q",Ingress(EthPacket),List(LEq(TConst("5"), TField("q","f1"))), None)))
		expectNotEmpty("qbadField1", qBadField1.wellFormedCheck())
		val qBadField2 = new Query(List(new OSee("q",Ingress(EthPacket),List(LEq(TConst("5"), TField("q","nwSrc"))), None)))
		expectNotEmpty("qbadField2", qBadField2.wellFormedCheck())
		val qBadField3 = new Query(List(new OSee("p",Ingress(IPPacket),List(LEq(TConst("5"), TField("p","nwSrc"))), None),
										new OSee("q",EgressSame(TCPPacket),List(LEq(TConst("5"), TField("q","nwSrc"))), None)))
		expectNotEmpty("qbadField3", qBadField3.wellFormedCheck())
		val qBadField4 = new Query(List(new OSee("q",Ingress(IPPacket),List(LEq(TConst("0x806"), TField("q","dlTyp"))), None)))
		expectNotEmpty("qbadField4", qBadField4.wellFormedCheck())
		// catch bad fields outside of this observation
		val qBadField5 = new Query(List(new OSee("p",Ingress(IPPacket),List(LEq(TConst("5"), TField("p","nwSrc"))), None),
										new OSee("q",Egress(TCPPacket),List(LEq(TField("q", "tpSrc"), TField("p","tpSrc"))), None)))
		expectNotEmpty("qbadField5", qBadField5.wellFormedCheck())
		// don't just use *this* observation to decide what fields are valid (p.locPt is fine, even though it appears in q)
		val qBadField6 = new Query(List(new OSee("p",Ingress(TCPPacket),List(LEq(TConst("5"), TField("p","nwSrc"))), None),
										new OSee("q",Egress(IPPacket),List(LEq(TField("q","outPt"), TField("p","locPt"))), None)))
		expectEmpty("qbadField6", qBadField6.wellFormedCheck())

		/////////////////////////////////////////////////
		// New compiler has stronger requirements: chaining.

		val qForwardChainGood1 = new Query(List(OSee("p1",Ingress(EthPacket),List(), None),
                       		     	            OSee("p2",Ingress(EthPacket),List(LEq(TField("p1","dlSrc"),
                       		    	           	                                      TField("p2","dlSrc"))), None),
                       		    	            OSee("p3",Ingress(EthPacket),List(LEq(TField("p1","dlSrc"),
                       		    	           	                                      TField("p3","dlSrc"))), None)
                       		    	            ))
		expectEmpty("qForwardChainGood1", qForwardChainGood1.wellFormedCheck())

		// Can't skip matching on a field; should say p1.dlDst is a problem here.
		val qForwardChainBad1 = new Query(List(OSee("p1",Ingress(EthPacket),List(), None),
                       		     	           OSee("p2",Ingress(EthPacket),List(LEq(TField("p1","dlSrc"),
                       		    	        	                                     TField("p2","dlSrc"))), None),
                       		    	           OSee("p3",Ingress(EthPacket),List(LEq(TField("p1","dlDst"),
                       		    	                                                 TField("p3","dlSrc"))), None)
                       		    	           ))
		expectNotEmpty("qForwardChainBad1", qForwardChainBad1.wellFormedCheck())
		expectEmpty("qForwardChainBad1 (in dynamic compiler)", qForwardChainBad1.wellFormedCheck(true))

		// Can't refer to fields of negative observations
		val qForwardChainBad2 = new Query(List(OSee("p1",Ingress(EthPacket),List(), None),
                       		     	           ONotSee("p2",Ingress(EthPacket),List(LEq(TField("p1","dlSrc"),
                       		    	        	                             TField("p2","dlSrc"))), Some(5)),
                       		    	           OSee("p3",Ingress(EthPacket),List(LEq(TField("p2","dlSrc"),
                       		    	                                      TField("p3","dlSrc"))), None)
                       		    	           ))
		expectNotEmpty("qForwardChainBad2", qForwardChainBad2.wellFormedCheck())

		// Can't refer to blocking condition of an until
		val qForwardChainBad3 = new Query(List(OSee("p1",Ingress(EthPacket),List(), None),
                       		     	           ONotSeeUntil("p2",List(LEq(TField("p2","dlSrc"),
                       		    	        	                          TConst("5"))),
                       		     	                        "p3",Ingress(EthPacket), List(), Some(5)),
                       		    	           OSee("p4",Ingress(EthPacket),List(LEq(TField("p2","dlSrc"),
                       		    	                                      TField("p4","dlSrc"))), None)
                       		    	           ))
		expectNotEmpty("qForwardChainBad3", qForwardChainBad3.wellFormedCheck())

		// UNTIL: don't get confused by refs in blocking condition to trigger condition
		//  should be able to refer to the *other* var in your own observation
		val qForwardChainGood2 = new Query(List(
	  	  ONotSeeUntil("s",             List(LEq(TField("s", "locpt"), TConst("1")),
	  	  								     LEq(TField("p", "nwsrc"), TField("s", "nwdst")),
	  	               	                     LEq(TField("p", "nwdst"), TField("s", "nwsrc"))),
	  	               "p", Ingress(IPPacket),     List(LEq(TField("p", "locpt"), TConst("2"))), None),
          OSee(        "p'",EgressSame(IPPacket),  List(LEq(TField("p'", "outpt"), TConst("1"))), None)
          ))
		expectEmpty("qForwardChainGood2", qForwardChainGood2.wellFormedCheck())

		// Can't have a literal eq/neq that doesn't refer to the current event (p1.f1 = p2.f1 in p3)
		val qBadTermNoCurrent = new Query(List(OSee("p1",Ingress(EthPacket),List(), None),
                    		     	           OSee("p2",Ingress(EthPacket),List(LEq(TField("p1","dlSrc"),
                       		    	        	                          TField("p2","dlSrc"))), Some(5)),
                       		    	           OSee("p3",Ingress(EthPacket),List(LEq(TField("p2","dlSrc"),
                       		    	                                      TField("p1","dlSrc"))), None)
                       		    	           ))
		expectNotEmpty("qBadTermNoCurrent", qBadTermNoCurrent.wellFormedCheck())

		println("----------------------------------------------------------------------")
	}
}
