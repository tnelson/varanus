package edu.brown.simon

/*
	A few compiler test cases *without* parser
*/

object QueryCompilerTests {
	def main(args: Array[String]) {
		println("Running Query Compiler Tests... no messages is good.")
		println("----------------------------------------------------------------------")

    	/*
		see p1 . p1.dltyp=0x800, p1.nwProto!=1
		see p2 . p2.dlTyp=0x800, p2.dlsrc = p1.dldst, p3.dldst = p1.dlsrc
		not see p3 . 0x800=p3.dltyp, p3.ipsrc=p1.ipdst, p3.nwproto!=1, p3.dldst=p2.dldst within 5 seconds
    	*/
    	val test1 = new Query(List(
	  	OSee("p1",Ingress(IPPacket),List(LNEq(TField("p1", "nwProto"), TConst("1"))), None),
	  	OSee("p2",Ingress(IPPacket),List(LEq(TField("p2","dlsrc"), TField("p1","dldst")),
		                                 LEq(TField("p2","ipdst"), TField("p1","ipdst"))), None),
      	ONotSee("p3",Ingress(IPPacket),List(LEq(TField("p1","ipdst"), TField("p3","ipsrc")),  // test reversal of terms
	  	                                    LNEq(TField("p3", "nwProto"), TConst("1")), // test negative deeper in
		                                    LEq(TField("p3","dldst"), TField("p2","dldst"))), Some(5))
	    ))

		val result1 = QueryCompiler.compileQueryString(test1)
  		println()
  		println("result1 (testing chained learn):")
  		println(result1)
  		println()

  		/*********************************************************************/
  		// Examples for paper: stateful firewall
  		/*********************************************************************/

  		// (0)
		val swf1 = new Query(List(
	  	  OSee(   "p", Egress(IPPacket),     List(LEq(TField("p", "nwsrc"), TConst("192.0.2.1"))), None)
	    ))
	    println()
	    println("Building SFW 1")
	    val swf1result = QueryCompiler.compileQueryString(swf1)
	    println(swf1result)


  		/*
			(1) Packets arriving at internal port are passed.
				see p: arrive | p.locPt = 1
				see p': egress same | p.locPt != 2
		*/

		val swf2 = new Query(List(
	  	  OSee("p", Ingress(EthPacket),    List(LEq(TField("p", "locpt"), TConst("1"))), None),
          OSee("p'",EgressSame(EthPacket), List(LNEq(TField("p'", "outpt"), TConst("2"))), None)
	    ))
	    println()
	    println("Building SFW 2")
	    val swf2result = QueryCompiler.compileQueryString(swf2)
	    println(swf2result)

		/*
			(2) Packets arriving at external port that are preceded by a corresponding packet
			    at internal port are passed.

                see start: arrive | start.locPt = 1
                see p: arrive within 10 | p.locPt = 2, p.nwSrc = start.nwDst, p.nwDst = start.nwSrc
                see p': egress same | p'.locPt != 1

                Add a within to the second observation to account for timeouts.
		*/
		val swf3 = new Query(List(
	  	  OSee("s",    Ingress(IPPacket),   List(LEq(TField("s", "locpt"), TConst("1"))), None),
          OSee("p",    Ingress(IPPacket),   List(LEq(TField("p", "locpt"), TConst("2")),
	  		                                     LEq(TField("p", "nwsrc"), TField("s", "nwdst")),
	  		                                     LEq(TField("p", "nwdst"), TField("s", "nwsrc"))), Some(10)),
          OSee("p'",EgressSame(IPPacket),List(LNEq(TField("p'", "outpt"), TConst("1"))), None)
          ))
		println()
		println("Building SFW 3")
		val swf3result = QueryCompiler.compileQueryString(swf3)
		println(swf3result)

		val swf3_with_notsee = new Query(List(
	  	  OSee("s",    Ingress(IPPacket),   List(LEq(TField("s", "locpt"), TConst("1"))), None),
          OSee("p",    Ingress(IPPacket),   List(LEq(TField("p", "locpt"), TConst("2")),
	  		                                     LEq(TField("p", "nwsrc"), TField("s", "nwdst")),
	  		                                     LEq(TField("p", "nwdst"), TField("s", "nwsrc"))), Some(10)),
          ONotSee("p'",EgressSame(IPPacket),List(LEq(TField("p'", "outpt"), TConst("1"))), None)
          ))
		println()
		println("Building SFW 3 (with not see egress-same)")
		val swf3result_with_notsee = QueryCompiler.compileQueryString(swf3_with_notsee)
		println(swf3result_with_notsee)

		/*
			(3) Packets arriving at external port without being preceded by a corresponding packet
			    at internal port are dropped.

			NOTE: DELIBERATELY WRONG VERSION W/ EGRESS TO TEST EGRESS CHAINING

			    not see start: arrival | start.locPt = 1, p.nwSrc = start.nwDst, p.nwDst = start.nwSrc
                  until see p: arrival | p.locPt = 2
                see p': egress | p'.locPt = 1
		*/

		val swf4_badegress = new Query(List(
	  	  ONotSeeUntil("s",             List(LEq(TField("s", "locpt"), TConst("1")),
	  	  								     LEq(TField("p", "nwsrc"), TField("s", "nwdst")),
	  	               	                     LEq(TField("p", "nwdst"), TField("s", "nwsrc"))),
	  	               "p", Ingress(IPPacket),    List(LEq(TField("p", "locpt"), TConst("2"))), None),
          OSee(        "p2",Egress(IPPacket),     List(LEq(TField("p2", "outpt"), TConst("1"))), None)
          ))
		println()
		println("Building SFW 4 (with bad egress)")
		val swf4result_badegress = QueryCompiler.compileQueryString(swf4_badegress)
		println(swf4result_badegress)

		// IMPORTANT! Note the use of special "outpt" field for output port. Shortcut.

		val swf4 = new Query(List(
	  	  ONotSeeUntil("s",             List(LEq(TField("s", "locpt"), TConst("1")),
	  	  								     LEq(TField("p", "nwsrc"), TField("s", "nwdst")),
	  	               	                     LEq(TField("p", "nwdst"), TField("s", "nwsrc"))),
	  	               "p", Ingress(IPPacket),     List(LEq(TField("p", "locpt"), TConst("2"))), None),
          OSee(        "p'",EgressSame(IPPacket),  List(LEq(TField("p'", "outpt"), TConst("1"))), None)
          ))
		println()
		println("Building SFW 4")
		val swf4result = QueryCompiler.compileQueryString(swf4)
		println(swf4result)


		// Test placing ingress/egress-same pair between other observations
		// Test mac address transfer
		val testIEchain = new Query(List(
	      OSee("r", Ingress(IPPacket), List(LEq(TField("r", "locpt"), TConst("4"))), None),
	  	  ONotSeeUntil("s",             List(LEq(TField("s", "locpt"), TConst("1")),
	  	  								LEq(TField("p", "nwsrc"), TField("s", "nwdst")),
	  	               	                LEq(TField("p", "nwdst"), TField("s", "nwsrc"))),
	  	               "p",Ingress(IPPacket),List(LEq(TField("p", "locpt"), TConst("2"))), None),
          OSee("p'",EgressSame(IPPacket),List(LEq(TField("p'", "outpt"), TConst("1")),
	  		                            LEq(TField("p", "nwsrc"), TField("p'", "nwsrc")),
	  		                            LEq(TField("p", "dldst"), TField("p'", "dldst"))), None),
          OSee("r2", Egress(IPPacket), List(LEq(TField("r2", "outpt"), TConst("5"))), None)
          ))
		println()
		println("Building testIEchain")
		val testIEchainResult = QueryCompiler.compileQueryString(testIEchain)
		println(testIEchainResult)

		println("----------------------------------------------------------------------")
    }
}
