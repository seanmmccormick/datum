package datum.avrolib

import cats.data.State
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, AlgebraM, scheme}
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.SchemaBuilder

object TestFoo extends App {

  private type Registry[A] = State[Map[Int, AvroSchema], A]

  private val algebra: AlgebraM[Registry, SchemaF, AvroSchema] = AlgebraM {

    //case ValueF(IntType, _) => AvroSchema.create(AvroSchema.Type.INT)

    case ValueF(IntType, _) => State.pure {
      val z = AvroSchema.create(AvroSchema.Type.INT)
      z.addProp("ATTR","NEAT")
      z
    }

    case ObjF(fields, _) =>
      val fingerprint = fields.hashCode()
      State { registry =>
        if (registry.contains(fingerprint)) {
          (registry, registry(fingerprint))
        } else {
          val wat = fields
            .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
              case (acc, (k, v)) =>
                acc.name(k).`type`(v).noDefault()
            }
            .endRecord()
          (registry + (fingerprint -> wat), wat)
        }
      }
  }

  def wurt(s: Schema) = {
    val zz = scheme.cataM(algebra)
    val (foo, bar) = zz(s).run(Map.empty).value
    pprint.pprintln(foo)
    bar
  }


  val ss = obj()("foo" -> value(IntType), "omg" -> value(IntType))

  val ok = wurt(ss)

  println(ok)
  pprint.pprintln(ok.getDoc)
}
