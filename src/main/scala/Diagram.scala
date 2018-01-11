import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import firrtl.{CircuitState, Transform, UnknownForm}
import firrtl.ir._
import firrtl.passes.Pass

import scala.collection.mutable

object Diagram extends Pass {
  override def name = "Diagram"

  def dumpLoc(loc: Expression, acc: String): String = {
    loc match {
      case r: Reference => r.name + acc
      case s: SubField => dumpLoc(s.expr, acc) + "." + s.name
      case i: SubIndex => dumpLoc(i.expr, acc) + "[" + i.value + "]"
      case s => acc
    }
  }

  def dumpConnect(m: DefModule)(c: Connect): Unit = {
    //    println(c.loc)
    //    println(dumpLoc(c.loc, "") + " <= " + dumpLoc(c.expr, ""))
    if (connections contains m.name) {
      connections(m.name) = connections(m.name) :+ (dumpLoc(c.loc, ""), dumpLoc(c.expr, ""))
    } else {
      connections(m.name) = Seq((dumpLoc(c.loc, ""), dumpLoc(c.expr, "")))
    }
    //println(c.loc + "<=" + c.expr)
  }

  def dumpStmts(m: DefModule)(s: Statement): Statement = {
    s match {
      case s: Connect => dumpConnect(m)(s)
      case s: DefInstance => {
        if (hierarchy contains m.name) {
          hierarchy(m.name) = hierarchy(m.name) :+ (s.module, s.name)
        }
        else
          hierarchy(m.name) = Seq((s.module, s.name))

      }
      case s => {
        s mapStmt dumpStmts(m)
      }
    }
    s
  }

  def dumpPorts(context: PrintWriter)(module: String)(p: Port): Port = {

    context.write(p.name)
    p
  }

  def getField(f: Field, acc: String): String = {
    getType(f.tpe, acc + "." + f.name)
  }

  def getType(t: Type, acc: String): String = {
    t match {
      case b: BundleType => {
        (b.fields map (f => getField(f, acc))).mkString("|")
      }
      case c: UIntType => {
        acc
      }
      case v: VectorType => {
        (for (i <- 0 until v.size) yield getType(v.tpe, acc + "[" + i + "]")).mkString("|")
      }
      case b: Type => acc
    }

  }


  def getPortName(p: Port): String = {
    getType(p.tpe, p.name)
  }

  def dumpModule(m: DefModule): Unit = {
    m mapStmt dumpStmts(m)
    val names = (m.ports map (p => getPortName(p))).mkString("|")
    // ugly hack
    portList(m.name) = names.split("\\|")
  }

  val hierarchy: mutable.Map[String, Seq[(String, String)]] = mutable.HashMap.empty[String, Seq[(String, String)]]
  val portList: mutable.Map[String, Seq[String]] = mutable.HashMap.empty[String, Seq[String]]
  val connections: mutable.Map[String, Seq[(String, String)]] = mutable.HashMap.empty[String, Seq[(String, String)]]

  def sanitize(s: String): String = {
    s.replace(".", "_").replace("[", "_").replace("]", "")
  }

  override def run(c: Circuit): Circuit = {

    c.modules foreach dumpModule

    for (m <- hierarchy.keys) {
      val context: PrintWriter = new PrintWriter(new File(m + ".dot"))
      context.write("digraph G{\n")
      context.write("graph [rankdir=LR, splines=ortho];\nnode [shape=record]; \n")

      //   context.write("\nnode [shape=record];\n")
      val ios = (portList(m).filter(s => s.startsWith("io."))) map (port => "<" + port.split("io.", 2)(1) + ">" + port.split("io.", 2)(1))
      context.write("io [label=\"{io|{")
      context.write(sanitize(ios.mkString("|")) + "}")
      context.write("}\"];\n")
      for (submodule <- hierarchy(m)) {

        context.write(submodule._2 + "[label=\"{" + submodule._2 + "|{")
        val l = for (port <- portList(submodule._1)) yield "<" + port + ">" + port

        context.write(sanitize(l.mkString("|")) + "}")

        context.write("}\"];\n")
      }

      // connections
      for ((what, where) <- connections(m)) {
        var toIsOK: Boolean = false
        var fromIsOK: Boolean = false
        val toModuleName = where.split("\\.", 2)(0)
        val fromModuleName = what.split("\\.", 2)(0)
        var from = "logic"
        var to = "logic"
        if (toModuleName == "clock" || toModuleName == "reset" || toModuleName == "io" || hierarchy(m).exists(item => {
          item._2 == toModuleName
        })) {
          toIsOK = true
        }
        if (fromModuleName == "clock" || fromModuleName == "reset" || fromModuleName == "io" || hierarchy(m).exists(item => {
          item._2 == fromModuleName
        })) {
          fromIsOK = true
        }
        if (fromIsOK) {
          from = sanitize(what.split("\\.", 2).mkString(":"))
        }
        if (toIsOK) {
          to = sanitize(where.split("\\.", 2).mkString(":"))
        }
        context.write(from + " -> " + to + ";\n")

      }

      context.write("}\n")
      context.flush()
      context.close()
    }

    //println(hierarchy)
    //println(portList)
    c
  }
}

object Main extends App {
  if (args.length != 1) {
    println("Usage: diagram fir_file")

  } else {
    val input: String = scala.io.Source.fromFile(args(0)).mkString
    val state = CircuitState(firrtl.Parser.parse(input), UnknownForm)
    val transforms = Seq(Diagram)
    transforms.foldLeft(state) {
      (c: CircuitState, t: Transform) => t.runTransform(c)
    }
  }
}