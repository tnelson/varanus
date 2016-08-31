package edu.brown.simon

import scala.util.parsing.combinator._

// ^^ if parsing on left succeeds, func on right is executed
object QueryParser extends JavaTokenParsers {   // JTP extends RegexParsers

	def ethAddress: Parser[String] =
	  """[0-9a-fA-F]{2}:[0-9a-fA-F]{2}:[0-9a-fA-F]{2}:[0-9a-fA-F]{2}:[0-9a-fA-F]{2}:[0-9a-fA-F]{2}""".r

	def hexValue: Parser[String] =
	  """0x[0-9a-fA-F]+""".r

	def ipgroup: Parser[String] = "25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?".r
	def ipv4Address: Parser[String] =
	  (ipgroup <~ ".") ~ (ipgroup <~ ".") ~ (ipgroup <~ ".") ~ ipgroup ^^ {case g1 ~ g2 ~ g3 ~ g4 => g1+"."+g2+"."+g3+"."+g4}

    def num: Parser[String] = wholeNumber ^^ (_.toString)
    // Allow identifiers to have alphanumeric, underscore, and prime
    def identifier: Parser[String] = """[a-zA-Z0-9_']+""".r ^^ (s => s.toString)

    def fieldterm: Parser[Term] = identifier ~ "." ~ identifier ^^ { case e ~ "." ~ f => new TField(e,f) }
    def numterm: Parser[Term] = num ^^ {case n => new TConst(n)}
    def ipterm: Parser[Term] = ipv4Address ^^ {case n => new TConst(n)}
    def ethterm: Parser[Term] = ethAddress ^^ {case n => new TConst(n)}
    def hexterm: Parser[Term] = hexValue ^^ {case n => new TConst(n)}

    // Order matters; will take the first match it can!
    // If fields are just identifiers, and fieldterm comes first, 192.168. ...will confuse the parser.
    // Same with hexterm
    def term: Parser[Term] = ipterm |
    						 hexterm |
    						 fieldterm |
                             ethterm |
                             numterm


    def eqlit: Parser[Literal] =  term ~ "="  ~ term  ^^ {case lhs ~ "="  ~ rhs => new LEq(lhs, rhs)}
    def neqlit: Parser[Literal] = term ~ "!=" ~ term  ^^ {case lhs ~ "!=" ~ rhs => new LNEq(lhs, rhs)}
    def literal: Parser[Literal] = eqlit | neqlit

    def pred: Parser[List[Literal]] = repsep(literal, ",")

	def ptype: Parser[PacketType] = "tcp" ^^ {_ => TCPPacket} |
									"ip" ^^ {_ => IPPacket} |
									"arp" ^^ {_ => ARPPacket} |
									"eth" ^^ {_ => EthPacket}

    // Again, note ordering: egress same is longer, so it goes first.
    def otype: Parser[EventType] = "ingress" ~> ptype ^^ { pt => Ingress(pt) } |
    							   "arrival" ~> ptype ^^ { pt => Ingress(pt) } | // synonym to avoid confusion
                                   "egress" ~> "same" ~> ptype ^^ { pt => EgressSame(pt) } |
                                   "egress" ~> ptype ^^ { pt => Egress(pt) }


    def within: Parser[Int] = ("within" ~> num) ^^ {n => n.toInt}
    def maybewithin: Parser[Option[Int]] = opt(within)

    def osee:  Parser[Observation] =
      "see" ~>          identifier ~ ":" ~ otype ~ maybewithin ~ "|" ~ pred ^^
        {case id ~ ":" ~ t ~ w ~ "|" ~ p => new OSee(id, t, p, w)}
    def onotsee: Parser[Observation] =
      "not" ~> "see" ~> identifier ~ ":" ~ otype ~ maybewithin ~ "|" ~ pred ^^
        {case id ~ ":" ~ t ~ w ~ "|" ~ p => new ONotSee(id, t, p, w)}
    def ountil: Parser[Observation] =
      "not" ~> "see" ~> identifier ~ "|" ~ pred ~
        "until" ~ "see" ~ identifier ~ ":" ~ otype ~ maybewithin ~ "|" ~ pred ^^
          {case bid ~ "|" ~ bpred ~ "until" ~ "see" ~ tid ~ ":" ~ t ~ w ~ "|" ~ tpred => new ONotSeeUntil(bid, bpred, tid, t, tpred, w)}

    // Note the (?m) multiline flag here.
    def commentLine: Parser[Unit] = "(?m)#.*$".r ^^ { _ => () }
    def maybeComments: Parser[Option[List[Unit]]] = opt(rep(commentLine))
    def observation: Parser[Observation] = osee | onotsee | ountil
    def query: Parser[Query] =
    	maybeComments ~> "query" ~> rep(observation) ^^
    	{case os => new Query(os)}

// TODO : could technically produce "egress same within 5".

/*
    def trans = "(" ~> repsep(trans_spec, ",") <~ ")"


*/
}

object ParserTests {
	def main(args: Array[String]) {
		println("----------------------------------------")
		println("Running parser tests. No errors is good.")

		val x1 = QueryParser.parseAll(QueryParser.num, "142")
		//println(x1)

		// Note this progression: parsing may fail at any non-terminal on the chain up.
		// Bad ordering in an "or" of parsers may cause problems this catches.
		val ip1 = QueryParser.parseAll(QueryParser.ipv4Address, "192.168.0.1")
		//println(ip1)
		val ip2 = QueryParser.parseAll(QueryParser.ipterm, "192.168.0.1")
		//println(ip2)
		val ip3 = QueryParser.parseAll(QueryParser.term, "192.168.0.1")
		//println(ip3)

		val eth1 = QueryParser.parseAll(QueryParser.ethAddress, "ca:FE:ca:FE:01:23")
		//println(eth1)


		val t1 = QueryParser.parseAll(QueryParser.term, "a.fld")
		//println(t1)
		val t2 = QueryParser.parseAll(QueryParser.term, "3")
		//println(t2)

		val l1 = QueryParser.parseAll(QueryParser.literal, "a.fld = b.fld")
		val l2 = QueryParser.parseAll(QueryParser.literal, "a.fld != b.fld")
		val l3 = QueryParser.parseAll(QueryParser.literal, "a.fld != 5")
		//println(l1)
		//println(l2)
		//println(l3)

		val p1 = QueryParser.parseAll(QueryParser.pred, "a.fld != 5,   b.fld=c.fld2,3=7")
		//println(p1)

		val o1 = QueryParser.parseAll(QueryParser.observation, "see x : ingress | x.locPt = 1")
		//println(o1)
		val o1w = QueryParser.parseAll(QueryParser.observation, "see x : ingress within 5 | x.locPt = 1")
		//println(o1w)

		val o2 = QueryParser.parseAll(QueryParser.observation, "not see x : egress | x.locPt = 1")
		//println(o2)
		val o3 = QueryParser.parseAll(QueryParser.observation, "not see x | x.locPt = 1,x.nwSrc=y.nwSrc until see y : ingress | y.locPt=2")
		//println(o3)

		val qs1 = """query
		       not see x | x.locPt = 1,x.nwSrc=y.nwSrc until see y : ingress | y.locPt=2
		       see z: ingress within 10 | z.locPt =  3, z.nwDst = y.nwSrc, z.dlSrc = ca:FE:ca:FE:01:23, z.nwSrc = 192.168.0.1
		      """
		val q1 = QueryParser.parseAll(QueryParser.query, qs1)
		//println(q1)


		val sfw1 = """query
			see p : egress | p.nwSrc = 192.0.2.1
		"""
		val sfw1_parsed = QueryParser.parseAll(QueryParser.query, sfw1)
		//println(sfw1_parsed)

		val sfw2 = """query
			see p: ingress | p.locpt = 1
			see p': egress same | p'.locpt != 2
		"""
		val sfw2_parsed = QueryParser.parseAll(QueryParser.query, sfw2)
		//println(sfw2_parsed)

		val sfw3 = """query
		    see s : ingress | s.locpt = 1
		    see p : ingress | p.locpt = 2, p.nwsrc = s.nwdst, p.nwdst = s.nwSrc
		    see p': egress same | p'.locpt != 1
		"""
		val sfw3_parsed = QueryParser.parseAll(QueryParser.query, sfw3)
		//println(sfw3_parsed)

		val sfw4 = """query
			not see s | s.locpt = 1, s.nwsrc=p.nwdst, s.nwdst=p.nwSrc
			  until see p : ingress | p.locpt = 2
			see p' : egress same | p'.outpt = 1
		"""
		val sfw4_parsed = QueryParser.parseAll(QueryParser.query, sfw4)
		//println(sfw4_parsed)

		println("----------------------------------------")
	}
}
