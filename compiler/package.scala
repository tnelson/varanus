package edu.brown

// Package objects allow top-level definitions
// Note that the last piece of the package path is defined here, in the object name, not in the package declaration.
package object simon {
	type EventID = String

    // Packet types. We can either use case classes (which don't support inheritance)
    //  or write our own extractors. For now, use case classes and remember that
    // "IPPacket" is not an "EthPacket". Use most specific type AT ALL TIMES.
    sealed trait PacketType
    case object EthPacket extends PacketType
    case object IPPacket  extends PacketType
    case object ARPPacket extends PacketType
    case object TCPPacket extends PacketType

    // Observations look for either ingress or egress events with a specific packet type
    sealed abstract class EventType(val ptype: PacketType)
    case class            Ingress(override val ptype: PacketType)  extends EventType(ptype)
    case class            Egress(override val ptype: PacketType) extends EventType(ptype)
    case class            EgressSame(override val ptype: PacketType) extends EventType(ptype)

    // Compiler helper values, OFPP values, etc.
    val ingressTable = 1
    val egressTable = 3
    // Virtual table IDs within actual OpenFlow tables
    val firstIngressMetadata = 0
    val firstEgressMetadata = 0

    val EGRESS_SAME_EQ_REGISTER = "reg9"

    val portFlood = 0xFFFB // OFPP_FLOOD
    val portAll = 0xFFFC // OFPP_FLOOD
    val portDrop = 0xFFFF // OFPP_NONE
    val portController = 0xFFFD // OFPP_CONTROLLER
    val portInPort = 0xFFF8 // OFPP_IN_PORT

    val outputPortRegister = 1

	val REGISTER_OFFSET = 1

    // Keep track of how deep in a variable is, so we can defer the proper number of times.
    type DepthMap = Map[EventID, Int]

    def expectEmpty(msg: String, v: List[Any]) {
        v match {
            case List() => ()
            case x :: rest => println(msg+" failed: " + v)
        }
    }

    def expectNotEmpty(msg: String, v: List[Any]) {
        v match {
            case List()  => println(msg+" failed: " + v)
            case x :: rest => ()
        }
    }

    def isIngress(etype: EventType): Boolean = {
        etype match {
            case Ingress(_) => true
            case _ => false
        }
    }
    def isEgress(etype: EventType): Boolean = {
        etype match {
            case Egress(_) => true
            case _ => false
        }
    }
    def isEgressSame(etype: EventType): Boolean = {
        etype match {
            case EgressSame(_) => true
            case _ => false
        }
    }
}