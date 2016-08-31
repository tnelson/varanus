package edu.brown.simon

import java.math.BigInteger

/*
	IMPORTANT!

	Both the dynamic table allocator and static tables compilers are still
	in the codebase. To use the dynamic allocator, pass true as the dynamic
	parameter to compileQueryString.
*/

object QueryCompiler {

  	def constPrefix(context: EventType): String = {
  		val tbl = context match {
  			case Ingress(_) => ingressTable
  			case _ => egressTable
  		}
  		val md =  context match {
  			case Ingress(_) => firstIngressMetadata
  			case _ => firstEgressMetadata
  		}
  		//"ovs-ofctl add-flow br0 \"table="+tbl+","
  		"table="+tbl+","+"metadata="+md+","
    }

  	//val constSuffix = "\""
  	val constSuffix = ""
  	val startCookie = new BigInteger("BEEF000000000000", 16) // adjust as needed

  	// Produce ovs-ofctl syntax for this query
  	// Dynamic = true means use the old dynamic-allocation compiler
  	//  (but without forward-chaining WFQ requirement)
  	def compileQueryString(q: Query, dynamic: Boolean): String = {
  		val rules = compileQuery(q, dynamic)
  		val rulesIngress = rules._1
  		val rulesEgress = rules._2

  		// Nothing special about EthPacket here; only outer layer will be used
  		rulesIngress.map({r => constPrefix(Ingress(EthPacket))+r.toOVSString(false)+constSuffix}).mkString("\n") +
  		"\n"+
  		rulesEgress.map({r => constPrefix(Egress(EthPacket))+r.toOVSString(false)+constSuffix}).mkString("\n")
  	}
  	def compileQueryString(q: Query): String = {
  		compileQueryString(q, false)
  	}

  	// Produce structured list of rules for this query
	def compileQuery(q: Query, dynamic: Boolean): (List[OFPPRule],List[OFPPRule]) = {

  		// Check for well-formedness
  		val wfqErrors = q.wellFormedCheck(dynamic)
  		if(!wfqErrors.isEmpty) throw new QueryNotWellFormedException(wfqErrors)

		dynamic match {
  			case true => println("~~ Running dynamic-allocation compiler. ~~")
  			case false => println("~~ Running static tables compiler. ~~")
  		}

  		// Produce set of OFPP rules
  		// non-sequence Parameters:
  		//   depths: distance back to each variable (in/eg-same counts as same distance)
  		//   nextdefer: next number of deferrals in (this is not always max of depths +1 since in/eg-same use the same deferral level)
  		//   currIngress: table to place current level of query in (ingress part if any)
  		//   currEgress: table to place current level of query in (egress part if any)
  		// Start at depth 0, nothing in scope,
  		compileQueryRecur(q.sequence, Map(), 0, 0, 0, dynamic)
	}

	// Helper for compilation: splits results into ingress and egress rule blocks.
	// Will be called recursively by trigger rule construction (hence the name).
	// Return type: INGRESS rules, EGRESS rules. Caller has table ID context.
	def compileQueryRecur(os: List[Observation],
		                  depths: DepthMap,
		                  nextdefer: Int, currIngress: Int, currEgress: Int,
		                  dynamic: Boolean): (List[OFPPRule],List[OFPPRule]) = {

		os match {
			case o :: rest =>

				// This function will pop either 1 or two observations.
				// ingress (without egresssame next): pop off one, return (rules, empty)
				// egress: pop off one, return (empty, rules)
				// ingress (with egresssame next): pop off two, return (rules [ingress half], rules [egress half])

				val otype = QueryUtils.getOType(o)
				val otriggervar = QueryUtils.getOTriggerVar(o)

				val ingressRules = otype match {
					case Ingress(_) => buildRulesFor(o, rest, depths, nextdefer, currIngress, currEgress, dynamic)
					case Egress(_) => List()
					case EgressSame(_) => throw new Exception("should not reach compileQueryRecur w/ EgressSame at head of list: "+o)
				}

				val egressRules = otype match {
					// If this is before an EgressSame, need to concurrently learn ingress+egress
					//     Also, if next otype is an EgressSame, the buildRulesFor call *HERE* is where the
					//     recursion continues into later observations. So need to add this ID to depths.
					case Ingress(_) => rest match {
						case onext :: restrest if isEgressSame(QueryUtils.getOType(onext)) =>
						    buildRulesFor(onext, restrest, depths+(otriggervar->nextdefer), nextdefer, currIngress, currEgress, dynamic)
						case _ => List[OFPPRule]()
					}
					case Egress(_) => buildRulesFor(o, rest, depths, nextdefer, currIngress, currEgress, dynamic)
					case EgressSame(_) => throw new Exception("should not reach compileQueryRecur w/ EgressSame at head of list: "+o)
				}

				// Both lists get separate priority spaces because each go into separate tables
				(assignPriorities(ingressRules, 9999),
			     assignPriorities(egressRules,  9999))

			case _ => throw new Exception("should not get here")
		}
	}

	def buildRulesFor(o: Observation,
		              rest: List[Observation],
		              depths: DepthMap,
		              nextdefer: Int, currIngress: Int, currEgress: Int,
		              dynamic: Boolean): List[OFPPRule] = {
		// General approach is:
		// Create a trigger rule (and any shadow-rules, as needed, to account for negative literals in o)
		//  for until, also create guard rules that learn new shadow-rules as blocking events are seen.

		//println("DEBUG: in buildRulesFor observation: "+o+
		//			"\nnextdefer="+nextdefer+"; currI="+currIngress+"; currE="+currEgress)

		val otype = QueryUtils.getOType(o)
		val otriggervar = QueryUtils.getOTriggerVar(o)

		// One rule for *each* negative literal
		val shadowRules = o match {
			case OSee(v, t, cond, dur) => buildShadowRules(v, otype, cond, dur, depths, nextdefer)
			case ONotSee(v, t, cond, dur) => buildShadowRules(v, otype, cond, dur, depths, nextdefer)
			case ONotSeeUntil(v1, cond1, v2, t2, cond2, dur) => buildShadowRules(v2, otype, cond2, dur, depths, nextdefer)
		}

		val untilGuardRules = o match {
			case OSee(v, t, cond, dur) => List[OFPPRule]()
			case ONotSee(v, t, cond, dur) => List[OFPPRule]()
			case ONotSeeUntil(v1, cond1, v2, t2, cond2, dur) =>
				// Need to provide new depth mapping here so that v2 is in scope
				buildUntilGuardRules(v1, v2, otype, cond1, cond2, dur, rest, depths+(v2 -> nextdefer), nextdefer)
		}

		// One trigger rule for & of positive literals
		val triggerRule = o match {
			// buildTriggerRule recursively calls compileQueryRecur, extends depths map.
			case OSee(v, t, cond, dur) =>
				buildTriggerRule(true, v, t, cond, dur, rest, depths, nextdefer, currIngress, currEgress, dynamic)
			case ONotSee(v, t, cond, dur) =>
				buildTriggerRule(false, v, t, cond, dur, rest, depths, nextdefer, currIngress, currEgress, dynamic)
			case ONotSeeUntil(v1, cond1, v2, t2, cond2, dur) =>
				// No need to give v1 or v2 here. v2 added automatically. v1 goes out of scope.
				buildTriggerRule(true, v2, t2, cond2, dur, rest, depths, nextdefer, currIngress, currEgress, dynamic)
		}

		// This ordering preserves LTL semantics for UNTIL: triggering needs to take priority over learning a new block.
		// Shadow rules for the block-learn are lower priority than triggering as well (here contained in untilGuardRules).
		List.concat(shadowRules, List(triggerRule) ++ untilGuardRules)
	}

	// helper for compilation
	def assignPriorities(rs: List[OFPPRule], nval: Int): List[OFPPRule] = {
		rs match {
			case List() => List()
			case r :: rest =>
				if(nval < 0) throw new OutOfPriorities()
				new OFPPRule(r.cookie, r.m, r.a, Some(nval), r.hardtimeout, r.aontimeout) :: assignPriorities(rest, nval-1)
		}
	}

	def buildMatch2(thisEvent: EventID, lterm: Term, rterm: Term, depths: DepthMap,
		            constrhs: Boolean, egressSameForIngress: Option[EventID]): List[OFPPMatch] = {
		// With ordering resolved, either invoke LHS/RHS or register match
		// Register match only if egresssameingress = Some(x). and RHS = x.fld

		//println("DEBUG buildMatch2: "+thisEvent+" "+lterm+" "+rterm+" "+depths)

		(egressSameForIngress,rterm) match {
			// This is an egress-same matching against corresponding ingress. Need to use special equality match.
			case (Some(ev),_) if QueryUtils.termRefersToEvent(rterm, ev) => // OK to disregard lterm since caller will ensure lterm refers to thisEvent
				(lterm, rterm) match {
					case (TField(_, lf),TField(_,rf)) => List(getEqRegExprMatch(lf, rf)) // guaranteed to be ingress = egress
					case _ => throw new Exception("buildMatch2, egressSameForEgress: "+(lterm,rterm))
				}

			// This is a reference to the ingress before a previous egress-same: need to learn from registers
			// Note that this might be *another* egress-same, so first part of match might be either some or none.
			case (_,TField(vr,fr)) if depths.contains(vr+"'") && vr != thisEvent =>
				registerMap.getOrElse(QueryUtils.sanitizeFieldName(fr), List()).zipWithIndex.map({case (act, i) =>
					new OFPPMatch(protectedMatchLHS(thisEvent, lterm, constrhs),
						          protectedMatchRHS(thisEvent, rterm, depths,   Some(i)))})
			case _ =>
			  List(new OFPPMatch(protectedMatchLHS(thisEvent, lterm, constrhs),
			                     protectedMatchRHS(thisEvent, rterm, depths, None)))
		}
	}

	def buildMatch(thisEvent: EventID, lterm: Term, rterm: Term,
		           depths: DepthMap, egressSameIngress: Option[EventID]): List[OFPPMatch] = {
		// Resolve which term refers to current event and which not. Don't trust ordering in literal to be correct.
		(lterm,rterm) match {
			case (TField(lf, lv), TField(rf, rv)) =>
				if(lf == thisEvent) buildMatch2(thisEvent, lterm, rterm, depths, false, egressSameIngress)
				else                buildMatch2(thisEvent, rterm, lterm, depths, false, egressSameIngress)

			case (TField(lf, lv), TConst(c)) =>
				if(lf == thisEvent) buildMatch2(thisEvent, lterm, rterm, depths, true, egressSameIngress)
				else throw new Exception("buildmatch: belated reference to fields of "+lf+" in "+thisEvent)

			case (TConst(c), TField(rf, rv)) =>
				if(rf == thisEvent) buildMatch2(thisEvent, rterm, lterm, depths, true, egressSameIngress)
				else throw new Exception("buildmatch: belated reference to fields of "+rf+" in "+thisEvent)

			case _ => throw new Exception("buildmatch should not reach here")
		}
	}

	def atomicRuleBlockForType(typ: EventType): OFPPRuleBlock = {
		typ match {
			case Ingress(_) => UseAtomicTableIngress
			case Egress(_) => UseAtomicTableEgress
			case EgressSame(_) => throw new Exception("atomicRuleBlockForType given EgressSame")
		}
	}

	def incAtomicTableForType(typ: EventType): OFPPAction = {
		typ match {
			case Ingress(_) => IncAtomicTableIngress
			case Egress(_) => IncAtomicTableEgress
			case EgressSame(_) => throw new Exception("incAtomicTableForType given EgressSame")
		}
	}


/*
 Reg | Function
----:|----
  0  | `Match tag`
  1  | `Output status (output port)`
2-8  | `Unused`
  9 | `Bits 31-16: dl_src[47:32], Bits 15-0:  dl_dst[47:32] (See
  note)`
 10  | `dl_src[31:0]`
 11  | `dl_dst[31:0]`
 12  | `ip_src (4 bytes)`
 13  | `ip_dst (4 bytes)`
 14  | `tp_src (2 bytes)`
 15  | `tp_dst (2 bytes)`
*/
			// "move:NXM_OF_ETH_TYPE[]->NXM_NX_REG7[0..15]"
			// val regnum: Int, val startbit: Int, val endbit: Int, val lhs: String)

	val registerMap = Map(
			("dlsrc" -> List(new CopyIntoRegister(9, 16, 31, "NXM_OF_ETH_SRC[32..47]"),
				             new CopyIntoRegister(10, 0, 31, "NXM_OF_ETH_SRC[0..31]" ))),
			("dldst" -> List(new CopyIntoRegister(9,  0, 15,  "NXM_OF_ETH_DST[32..47]"),
				             new CopyIntoRegister(11, 0, 31,  "NXM_OF_ETH_DST[0..31]"))),
			("nwsrc" -> List(new CopyIntoRegister(12, 0, 32, "NXM_OF_IP_SRC[]"))),
			("nwdst" -> List(new CopyIntoRegister(13, 0, 32, "NXM_OF_IP_DST[]"))),
			// TODO: NXM matches are protocol-specific. To be robust, will need to know whether UDP or TCP at this point.
			("tpsrc" -> List(new CopyIntoRegister(14, 0, 15, "NXM_OF_TCP_SRC[]"))),
			("tpdst" -> List(new CopyIntoRegister(15, 0, 15, "NXM_OF_TCP_SRC[]")))
		)

	// Store ingress values in pre-set registers for matching on egress.
	// These values are *fixed* and relied on by the OVS extensions!
	def getOutputRegisterActions(fname: String): List[CopyIntoRegister] = {
		registerMap.get(QueryUtils.sanitizeFieldName(fname)) match {

			// This function populates registers on ingress for use in later learns
			// NOT for matching vs. egress-same packet. We have registers 2-6 free
			// to carry more forward.

			case Some(actionList) => actionList
			case None => throw new Exception("getOutputRegisterActions: field unsupported for register stashing: "+fname)
		}
	}


/*
From NDM:

Comparison result map (bits of register 9)

Bit | Comparison
----:|----
  0 | `p.dl_type  ==   p'.dl_type`
  1 | `p.dl_src   ==   p'.dl_src`
  2 | `p.dl_src   ==   p'.dl_dst`
  3 | `p.dl_dst   ==   p'.dl_src`
  4 | `p.dl_dst   ==   p'.dl_dst`
  5 | `p.ip_src   ==   p'.ip_src`
  6 | `p.ip_src   ==   p'.ip_dst`
  7 | `p.ip_dst   ==   p'.ip_src`
  8 | `p.ip_dst   ==   p'.ip_dst`
  9 | `p.tp_src   ==   p'.tp_src`
 10 | `p.tp_src   ==   p'.tp_dst`
 11 | `p.tp_dst   ==   p'.tp_src`
 12 | `p.tp_dst   ==   p'.tp_dst`


As an example, to match on just p.dl_src == p.dl_dst (bit 2), you can write a
rule in the egress table as follows:
table=3, reg9=0x04/0x04, ...

To match on the comparisons p.tcp_src = p.tcp_dst and
p.tcp_dst == p'.tcp_src (bits 10-11), you can match on both comparison results as
follows:
table=3, reg9=0xc00/0xc00

*/

	def getEqRegExprMatch(lf: String, rf: String): OFPPMatch = {
		// lf is field for *EGRESS* (p')
		// rf is field for ingress, stored previously in reg9

		// IMPORTANT: This function produces a match for *this one equality*. If multiple matches are
		// required, caller is responsible for combining them via masks since you can only match
		// on a field once in OVS.

		// Checking in rf,lf order to match format of NDM's writeup (p, p') which is observation order
		(QueryUtils.sanitizeFieldName(rf), QueryUtils.sanitizeFieldName(lf)) match {

 			// These values will not, and are NOT INTENDED to parse properly in OVS.
 			// They are meant to facilitate post-processing to combine matches on register 9.

			case ("dltyp","dltyp") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit0")

			case ("dlsrc","dlsrc") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit1")
			case ("dlsrc","dldst") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit2")
			case ("dldst","dlsrc") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit3")
			case ("dldst","dldst") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit4")

			case ("nwsrc","nwsrc") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit5")
			case ("nwsrc","nwdst") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit6")
			case ("nwdst","nwsrc") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit7")
			case ("nwdst","nwdst") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit8")

			// TODO: NXM matches are protocol-specific. To be robust, will need to know whether UDP or TCP at this point.
			case ("tpsrc","tpsrc") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit9")
			case ("tpsrc","tpdst") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit10")
			case ("tpdst","tpsrc") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit11")
			case ("tpdst","tpdst") => new OFPPMatch(EGRESS_SAME_EQ_REGISTER, "bit12")

			case _ => throw new Exception("getEqRegExpr: invalid pair of fields "+lf+" "+rf+
				                          ". ingress/egress matching is not supported for these fields at this time.")
		}
	}

	def highestDepthID(depths: DepthMap): EventID = {
		val accum = depths.foldLeft ("",-1) ({case ((acck, accv),(k,v)) => if(v > accv) (k, v) else (acck, accv) })
		accum._1
	}

	// Pass label to distinguish different pieces of a rule block
	val LABEL_SHADOW = 1
	val LABEL_TRIGGER = 3
	val LABEL_BLOCKING_TRIGGER = 3
	val LABEL_LEARNED_BLOCK = 9
	def buildCookie(blockdepth : Int, label : Int): Option[BigInteger] = {
		Some(startCookie.add(BigInteger.valueOf((blockdepth * 16) + label)))
	}

	def getFieldRegExpr(fname: String, idx: Int): String = {
		registerMap.get(QueryUtils.sanitizeFieldName(fname)) match {
			case None => throw new Exception("Unknown field name when constructing register reference: "+fname+"; idx: "+idx)
			case Some(acts) if acts.size > idx => "NXM_OF_REG"+acts(idx).regnum+"["+acts(idx).startbit+".."+acts(idx).endbit+"]"
			case _ => throw new Exception("getFieldRegExpr called with index out of bounds: "+fname+","+idx)
		}
	}

	def getFieldExpr(thisEvent: EventID, pname: EventID, fname: String, maybedepths: Option[Map[EventID, Int]],
		             forconst: Boolean, parambits: Option[(Int, Int)],
		             partOfField: Option[Int]): String = {

		// Do NOT use this function if you need a register reference for *matching* in EgressSame.
		// DO use this function if you need a register reference for learning from that EgressSame.

		// This function has to produce OVS syntax for a variety of situations.
		// * LHS vs. RHS of the match: maybedepths is None if LHS of the match, Some if RHS.
		// * LHS needs to have different fieldname syntax (NXM...) if RHS is not a constant

		// * If p is an ingress for a prior egress-same, partOfField will be Some i.
		//   use carry-through registers with deferral as needed (see registerMap).

		//println("DEBUG getFieldExpr: "+thisEvent+" "+pname+" "+fname+" "+partOfField+" "+forconst)

		val bitstr = parambits match { case None => "" case Some((from, to)) => from+".."+to}

		// sanitized, lowercase, underscore removed for easy matching of both forms
		val stx =
			partOfField match {
				case Some(idx) if !forconst => getFieldRegExpr(fname, idx)
				case None => QueryUtils.sanitizeFieldName(fname) match {
			case "dlsrc" => if(!forconst)   { "NXM_OF_ETH_SRC["+bitstr+"]" } else  { "eth_src" }
			case "dldst" => if(!forconst)   { "NXM_OF_ETH_DST["+bitstr+"]" } else  { "eth_dst"}
			case "dltyp" => if(!forconst)   { "NXM_OF_ETH_TYPE["+bitstr+"]" } else { "eth_type" }

			// "nw" for constant; "IP" for learn
			case "nwsrc" => if(!forconst)   { "NXM_OF_IP_SRC["+bitstr+"]" } else   { "nw_src" }
			case "nwdst" => if(!forconst)   { "NXM_OF_IP_DST["+bitstr+"]" } else   { "nw_dst" }
			case "nwproto" => if(!forconst) { "NXM_OF_IP_PROTO["+bitstr+"]" } else { "nw_proto" }

			case "tpsrc" => if(!forconst)   { "NXM_OF_TCP_SRC["+bitstr+"]" } else  { "tcp_src" }
			case "tpdst" => if(!forconst)   { "NXM_OF_TCP_DST["+bitstr+"]" } else  { "tcp_dst" }

			case "locpt" => if(!forconst)   { "NXM_OF_IN_PORT["+bitstr+"]" } else  { "in_port" }
			case "outpt" => if(!forconst)   { "NXM_OF_REG"+outputPortRegister+"["+bitstr+"]" } else  { "reg"+outputPortRegister }

			// https://github.com/ndemarinis/ovs/blob/branch-2.5/lib/meta-flow.h#L642-1705
			case "arpop" => if(!forconst)  { "NXM_OF_ARP_OP["+bitstr+"]" } else  { "arp_op" }
			case "arpspa" => if(!forconst)  { "NXM_OF_ARP_SPA["+bitstr+"]" } else  { "arp_spa" }
			case "arptpa" => if(!forconst)  { "NXM_OF_ARP_TPA["+bitstr+"]" } else  { "arp_tpa" }
			case "arpsha" => if(!forconst)  { "NXM_OF_ARP_SHA["+bitstr+"]" } else  { "arp_sha" }
			case "arptha" => if(!forconst)  { "NXM_OF_ARP_THA["+bitstr+"]" } else  { "arp_tha" }

			// Get vlan id from the TCI field
			case "dlvlan" => if(!forconst)  {
					parambits match { case None => "NXM_OF_VLAN_TCI[0..11]"
				                      case _ => throw new Exception("tried to extract bits from vlan_id field; currently unsupported.") }}
					         else  { "vlan_id" }

			case _ => throw new Exception("should not get here: unknown field name: "+fname)
			}
		}

		maybedepths match {
			case None => stx
			case Some(m) =>
				m.get(pname) match {
					case None => throw new Exception("should not get here; unknown event id in depths: "+pname+". had depths="+maybedepths)
					case Some(depth) =>
						if(depth < 1) stx // reference to first event; no deferral needed
						else stx+"(defer="+depth+")" // defer substitution as many times as needed
				}
		}
	}

	def protectedMatchLHS(thisEvent: EventID, t: Term, isconst: Boolean): String = {
		// isconst here is to inform getFieldExpr whether the *RHS* was constant
		t match {
			case TField(p,f) => getFieldExpr(thisEvent, p, f, None, isconst, None, None)
			case TConst(c) => throw new Exception("should not get here")
		}
	}

	def protectedMatchRHS(thisEvent: EventID, t: Term, depths: DepthMap, partOfField: Option[Int]): String = {
		t match {
			case TField(p,f) => getFieldExpr(thisEvent, p, f, Some(depths), false, None, partOfField)
			case TConst(c) => c
		}
	}

	def makeMatchForType(et: EventType): List[OFPPMatch] = {
		val pt = et match {
				case Ingress(t) => t
				case Egress(t) => t
				case EgressSame(t) => t
			}

		pt match {
			case TCPPacket => List(new OFPPMatch("eth_type", "0x800"),
								   new OFPPMatch("nw_proto", "6"))
			case IPPacket => List(new OFPPMatch("eth_type", "0x800"))
			case ARPPacket => List(new OFPPMatch("eth_type", "0x806"))
			case _ => List()
		}
	}


	/*********************************************************************
	* Shadow-rule and until-blocking construction is the same for both compilers.
	**********************************************************************/

	def buildShadowRule(thisEvent: EventID, thisType: EventType, cond: List[Literal], dur: Option[Int], fterm: Term, vterm: Term, depths: Map[EventID, Int], maxdepth: Int): OFPPRule = {
		// defer assigning priority, but can assign cookie here
		val egressSameForIngress = if(isEgressSame(thisType)) Some(highestDepthID(depths)) else None

		// Specialize the match to the positive matches that the trigger rule will use
		val specializationMatches = (cond collect {case LEq(fterm,vterm) => buildMatch(thisEvent, fterm, vterm, depths, egressSameForIngress) }).flatten

		// Base = specialization + match for the negative literal we're matching on
		val baseMatches = buildMatch(thisEvent, fterm, vterm, depths, egressSameForIngress) ++ specializationMatches

		// Remember to add the register tag match if this is catching an egress-same
		val matches =
		  makeMatchForType(thisType) ++
		  (if(isEgressSame(thisType))
		    new OFPPMatch("reg0", maxdepth.toString) :: baseMatches
		  else
		    baseMatches)

		new OFPPRule(buildCookie(maxdepth, LABEL_SHADOW), matches, List(), None, dur, None)
	}

	def buildShadowRules(thisEvent: EventID, thisType: EventType, cond: List[Literal], dur: Option[Int], depths: Map[EventID, Int], maxdepth: Int): List[OFPPRule] = {
		// For each negative literal in o's condition, produce a matching drop rule
		cond collect {l => l match { case LNEq(fterm,vterm) => buildShadowRule(thisEvent, thisType, cond, dur, fterm, vterm, depths, maxdepth) }}
	}


	// TODO: until makes no sense with egress-same type. should make that clear
	//    (otherwise, might need reg0 = tag match)

	// Called for the "not" portion of a "not see ... until ..."
	// Returns *both* shadow rules and blocking trigger.
	def buildUntilGuardRules(blockingEvent: EventID, triggerEvent: EventID, oType: EventType,
							 blockingCond: List[Literal], triggerCond: List[Literal],
							 dur: Option[Int], rest: List[Observation], depths: DepthMap,
							 maxdepth: Int): List[OFPPRule] = {
		// Shadow for A, then self-learn triggers for A [matching against future-used fields; no future used fields = block everything].

		// Construct shadow rules for blocking event
		val shadowRules = QueryCompiler.buildShadowRules(blockingEvent, oType, blockingCond, dur, depths, maxdepth)

		// First, gather field references to the "not" variable in later observations. Store as p1.x<->p2.y mapping
		// These are interpreted existentially, AFTER being mapped through the equalities in map.
		// For instance, not see x | x.nwSrc = y.nwDst until see y ... produces x.nwSrc -> {y.nwDst}.
		// If we see an x match, its Src is used to block Dst for potential y matches.

		def addToFieldMap(acc: Map[Term, List[Term]], k: Term, vadd: Term): Map[Term, List[Term]] = {
			val maybeVal = acc.get(k)
			maybeVal match {
				case None => acc + (k->List(vadd))
				case Some(lst) => acc + (k->(vadd::lst))
			}
		}

		val existentials = blockingCond.foldLeft (Map[Term, List[Term]]()) (
			(acc, l) => l match { case LEq(TField(e1, f1), TField(e2,f2)) if e1 == blockingEvent => addToFieldMap(acc, TField(e1, f1), TField(e2,f2))
		   	                      case LEq(TField(e1, f1), TField(e2,f2)) if e2 == blockingEvent => addToFieldMap(acc, TField(e2, f2), TField(e1,f1))
		   	                      case _ => acc })

		//println("Debug info: found existentials for blocking: "+blockingEvent+"; trigger:"+triggerEvent+"; => "+existentials)

		// Construct blocking trigger rule. Extract positive matches (we already dealt with negative ones in shadow rules above)
		// First, remove literals that reference triggerEvent
		val currentEventBlockingCond = blockingCond.filter(QueryUtils.notLitRefersToEvent(triggerEvent))
		val blockingMatches = (currentEventBlockingCond collect
  		    {l => l match { case LEq(lterm,rterm) => QueryCompiler.buildMatch(blockingEvent, lterm, rterm, depths, None) }}).flatten

  		// Now construct match template for what will be learned every time blockingMatches is hit.
  		// Use nested fold because each map entry contains a *list* of equalities to match
  		val existentialMatch = existentials.foldLeft (List[OFPPMatch]()) (
  			{case (acc, (TField(kv, kf), vlist)) => vlist.foldLeft (acc) (
  				{case (acc2, TField(vv, vf)) => QueryCompiler.buildMatch(blockingEvent, TField(vv, vf), TField(kv, kf), depths, None)++acc2
  				 case (_, TConst(c)) => throw new Exception("should not get here; existentialMatch vs. constant: "+c)})})

	    // TODO FIX: note the duration of this blocking learned rule will be the FULL duration, not full minus elapsed.
  		val blockingLearn = OFPPLearn(new OFPPRule(QueryCompiler.buildCookie(maxdepth, LABEL_LEARNED_BLOCK),
  												  (makeMatchForType(oType) ++ existentialMatch),
  												  List(), Some(50000), dur, None), UseRuleTable, false)
		val blockingTrigger = new OFPPRule(QueryCompiler.buildCookie(maxdepth, LABEL_BLOCKING_TRIGGER),
			                               (makeMatchForType(oType) ++ blockingMatches), List(blockingLearn), None, dur, None)

		shadowRules ++ List(blockingTrigger)
	}


	/*********************************************************************
	* Stubs that call the appropriate Static or Dynamic compiler functions
	**********************************************************************/

	def buildTriggerRule(sign: Boolean, thisEvent: EventID, thisType: EventType,
		                 cond: List[Literal], dur: Option[Int],
		                 rest: List[Observation], depths: DepthMap,
		                 nextdefer: Int, currIngress: Int, currEgress: Int,
		                 dynamic: Boolean): OFPPRule = {
		  dynamic match {
  			case true =>
  				DynamicQueryCompiler.buildTriggerRule(
  					sign, thisEvent, thisType, cond, dur,
  					rest, depths, nextdefer)
  			case false =>
  				StaticQueryCompiler.buildTriggerRule(
  					sign, thisEvent, thisType, cond, dur,
  					rest, depths, nextdefer, currIngress, currEgress)
  		}
	}

}
