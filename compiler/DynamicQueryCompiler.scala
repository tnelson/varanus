package edu.brown.simon

import java.math.BigInteger

object DynamicQueryCompiler {


	def buildTriggerRule(sign: Boolean, thisEvent: EventID, thisType: EventType,
		                 cond: List[Literal], dur: Option[Int],
		                 rest: List[Observation], depths: DepthMap,
		                 nextdefer: Int): OFPPRule = {

		// Three potential behaviors for this function:
		// (1) NORMAL: match on cond in current packet; action learn next observation
		// (2) INGRESS-BEFORE-EGRESS-SAME: match on cond in current packet; action populate metadata for later match;
		// (3) EGRESS-SAME: match on metadata (p) and current packet (p'); action learn next observation
		// Caller must be aware of context so they can learn (2) and (3) results in the same stage;
		//   calling in context (2) will not recursively invoke (3); (3) must be called separately.

		// TODO: allow propagation of ingress (before egress-same) values to later in query
		//   right now, our register scheme for comparison makes it impossible to recover values of p; only p'.
		//   rare for this to be a problem unless the switch changes the field we care about in transition from p to p'.

		val newdepths = depths+(thisEvent -> nextdefer)

		val egressSameForIngress = thisType match { case EgressSame(_) => Some(QueryCompiler.highestDepthID(depths)) case _ => None }

		// Whether or not this is egress-same, go through buildMatch
  	val normalMatches = (cond collect
  		  {l => l match { case LEq(lterm,rterm) => QueryCompiler.buildMatch(thisEvent, lterm, rterm, depths, egressSameForIngress) }}).flatten

		val blockid = nextdefer.toString

  		// If EgressSame, then match needs to also include reg0=[block]
  		val matches =
  			if(isEgressSame(thisType)) new OFPPMatch("reg0", blockid) :: normalMatches
  		  	else normalMatches

  		// If rest is empty, the action is a send-to-controller. If not, recur and learn those rules.
  		val actions = rest match {
  			case nextO :: _ =>
  				val nextType = QueryUtils.getOType(nextO)

  				if(isEgressSame(nextType)) { // => from well-formedness, know we are building trigger for an ingress

  					// Move code for this block to register 0. do not use depths.size because ingr-egr pairs have same depth.
  					val reg0 = new CopyIntoRegister(0, 0, 31, blockid)

  					// Move fields into their assigned registers; only store fields of this packet that are referenced in nextO.
					val fieldsToMove = QueryUtils.fieldNamesUsedInLiterals(QueryUtils.literalsUsedInObservations(rest), Some(thisEvent))
  					val regks = fieldsToMove.foldLeft (List[OFPPAction]()) ({case (acc, fld) => QueryCompiler.getOutputRegisterActions(fld) ++ acc})

  					reg0 :: regks
  				}
  				else
  				{
  					// Action is a learn. Get (ingress-rules, egress-rules) for the next observation.
  					val tolearn = QueryCompiler.compileQueryRecur(rest, newdepths, nextdefer+1, 0, 0, true); // TRUE for *dynamic*; table counters won't be used

            // Learning over an existing rule should *not* replace the existing timeout for a negative observation
            val preserveTimeout = nextO match {
              case ONotSee(_,_,_,_) => true
              case _ => false
            }

					 // May have to add a virtual table of either, or both, types.
  					val learnIngressActions =
  						if (tolearn._1.size > 0)
  							QueryCompiler.incAtomicTableForType(Ingress(EthPacket)) ::
  							tolearn._1.map({l => OFPPLearn(l, QueryCompiler.atomicRuleBlockForType(Ingress(EthPacket)), preserveTimeout)});
  						else
  							List[OFPPAction]()

  					val learnEgressActions =
  						if (tolearn._2.size > 0)
  							QueryCompiler.incAtomicTableForType(Egress(EthPacket)) ::
  							tolearn._2.map({l => OFPPLearn(l, QueryCompiler.atomicRuleBlockForType(Egress(EthPacket)), preserveTimeout)});
  						else
  							List[OFPPAction]()

  					learnIngressActions ++ learnEgressActions
  				}
  			case _ => List(SendToController)
  		}

  		// If this is a negative observation, actions take place on timeout
  		// defer assigning a priority to this rule
  		if (sign || isEgressSame(thisType)) // Can't detect a "not see" via timer; construct match differently instead
			  new OFPPRule(QueryCompiler.buildCookie(nextdefer, QueryCompiler.LABEL_TRIGGER),
				         (QueryCompiler.makeMatchForType(thisType) ++ matches), actions, None, dur, None)
		  else
  			new OFPPRule(QueryCompiler.buildCookie(nextdefer, QueryCompiler.LABEL_TRIGGER),
  			         (QueryCompiler.makeMatchForType(thisType) ++ matches), List(OFPPDeleteUseRuleTable), None, dur, Some(actions))
	}






}