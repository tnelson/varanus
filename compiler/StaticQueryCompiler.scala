package edu.brown.simon

import java.math.BigInteger

object StaticQueryCompiler {

	def buildTriggerRule(sign: Boolean, thisEvent: EventID, thisType: EventType,
		                 cond: List[Literal], dur: Option[Int],
		                 rest: List[Observation], depths: DepthMap,
		                 nextdefer: Int, currIngress: Int, currEgress: Int): OFPPRule = {

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

  					// Move fields into their assigned registers; only store fields of this packet that are referenced in nextO
            //    (or future observations that must be learned by nextO!)
					val fieldsToMove = QueryUtils.fieldNamesUsedInLiterals(QueryUtils.literalsUsedInObservations(rest), Some(thisEvent))
  					val regks = fieldsToMove.foldLeft (List[OFPPAction]()) ({case (acc, fld) => QueryCompiler.getOutputRegisterActions(fld) ++ acc})

  					reg0 :: regks

  					// Do not recur; caller invokes separately for next step to construct learn chain from there.
  				}
  				else
  				{
  					// Since next is not egress-same, we recur here.
  					// Increment table counters as appropriate. Note that if this is an egress-same,
  					// we need to increment both, since the call for the preceding ingress
  					// (the 'if' branch above) did not get a chance to do so in the recursion.
  					val nextIngress = if(isIngress(thisType) || isEgressSame(thisType)) currIngress +1 else currIngress
  					val nextEgress =  if(isEgress(thisType) || isEgressSame(thisType)) currEgress +1 else currEgress

  					// Action is a learn. Get (ingress-rules, egress-rules) for the next observation.
  					val tolearn = QueryCompiler.compileQueryRecur(rest, newdepths, nextdefer+1, nextIngress, nextEgress, false); // FALSE for *static*

            // Learning over an existing rule should *not* replace the existing timeout for a negative observation
            val preserveTimeout = nextO match {
              case ONotSee(_,_,_,_) => true
              case _ => false
            }

					  // May have to add a virtual table of either, or both, types.
  					val learnIngressActions =
  						if (tolearn._1.size > 0)
  							tolearn._1.map({l => OFPPLearn(l, UseConstantTable(ingressTable, Some(nextIngress)), preserveTimeout)});
  						else
  							List[OFPPAction]()

  					val learnEgressActions =
  						if (tolearn._2.size > 0)
  							tolearn._2.map({l => OFPPLearn(l, UseConstantTable(egressTable, Some(nextEgress)), preserveTimeout)});
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
		else {
      // specialize for bindings we have and matches in this condition
      // Rules with matching matches will be deleted (even if they match against other things as well, like shadow rules do)
      val thisTable = if(isIngress(thisType)) ingressTable else egressTable
      val thisMetadata = if(isIngress(thisType)) currIngress else currEgress
      val delMatches = new OFPPMatch("table", thisTable.toString) ::
                       new OFPPMatch("metadata", thisMetadata.toString) ::
                       normalMatches
  		new OFPPRule(QueryCompiler.buildCookie(nextdefer, QueryCompiler.LABEL_TRIGGER),
  	  	         (QueryCompiler.makeMatchForType(thisType) ++ matches), List(OFPPDeleteMatched(delMatches)), None, dur, Some(actions))
    }
	}

}
