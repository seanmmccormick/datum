//package datum.algebras.defaults
//import datum.patterns.attributes._
//import datum.patterns.data
//import datum.patterns.data.{Data, DataF, EmptyValue, RowValue}
//import datum.patterns.schemas._
//import cats.instances.either._
//import datum.algebras.generic.{AnnotatedFunction, Wurt}
//import qq.droste.data._
//import qq.droste.data.prelude._
//import qq.droste.{Algebra, AlgebraM, scheme}
//
//import scala.collection.mutable
//
//class Defaults(override val modify: Data => Wurt.WithSchema[Data, Data]) extends AnnotatedFunction[Data, Data, Data] {
//  import Defaults._
//
//  /*
//   * This is an Algebra that, given a default-annotated Schema,
//   * can generate a function from Data => Data, replacing missing
//   * values as defined in the schema with the correct default.
//   */
//  override val base: Algebra[AttrF[SchemaF, Data, ?], Data => Data] =
//    Algebra[AttrF[SchemaF, Data, ?], Data => Data] {
//
//      // handle cases for `ObjF`s that can either themselves be missing, or contain missing fields
//      case AttrF(default, ObjF(schemaFields, attributes)) =>
//        inp =>
//          Fix.un[DataF](inp) match {
//
//            // Handle case for when the obj itself is missing and has a default defined
//            case data.EmptyValue if attributes.contains(key) => default
//
//            // Handle case for when the obj is missing, but might have default fields
//            case data.EmptyValue =>
//              val nonEmptyFields = schemaFields.mapValues(fn => fn(data.empty)).filter {
//                case (_, result) if result == data.empty => false
//                case _                                   => true
//              }
//              if (nonEmptyFields.isEmpty) data.empty
//              else data.obj(nonEmptyFields)
//
//            // Handle case for possibly missing fields in the object
//            case data.ObjValue(dataFields) =>
//              val missingFields = schemaFields.filterKeys(k => !dataFields.keySet.contains(k))
//              val defaultFields = missingFields.mapValues(fn => fn(data.empty))
//
//              // Return new data.Obj type with possibly new default fields added
//              data.obj(dataFields ++ defaultFields)
//
//            // Otherwise return the input value
//            case _ => inp
//          }
//
//      // handle cases for `RowF`s that can either themselves be missing, or contain missing fields
//      case AttrF(default, RowF(elements, attributes)) =>
//        inp =>
//          Fix.un[DataF](inp) match {
//            case data.EmptyValue if attributes.contains(key) => default
//
//            case data.RowValue(dataValues) if dataValues.length == elements.length =>
//              val updated = (dataValues zip elements) map {
//                case (inpColumn, schemaColumn) =>
//                  schemaColumn.value(inpColumn)
//              }
//              Fix[DataF](RowValue(updated))
//
//            case _ => inp
//          }
//
//      // Handle case for replacing empty inputs with default value
//      case AttrF(default, schema) if schema.attributes.contains(key) =>
//        inp =>
//          Fix.un[DataF](inp) match {
//            case data.EmptyValue => default
//            case _               => inp
//          }
//
//      // format: off
//
//      // Otherwise, just return the input
//      case _ => inp => inp
//
//      // format: on
//    }
//}
//
//object Defaults {
//  val key: AttributeKey = "default".asAttributeKey
//
//  def default(attribute: Attribute): (AttributeKey, Attribute) = key -> attribute
//
//  def apply(modify: Data => Wurt.WithSchema[Data, Data] = cats.data.Kleisli.pure): Defaults = new Defaults(modify)
//
//  //todo - strip empty headers on columns ???
//  object modifiers {
//    val allowColumnResizingKey: AttributeKey = "allow-column-resizing".asAttributeKey
//
//    private def resizeColumnsFn(inp: Data, values: Vector[Data]): Wurt.WithSchema[Data, Data] = {
//      cats.data.ReaderT {
//        case RowF(elements, attributes)
//            if attributes.get(allowColumnResizingKey).contains(property(true)) && values.length < elements.length =>
//          val results = mutable.Buffer.empty[Data]
//
//          (values zip elements).foreach {
//            case (elem, schemaColumn) if elem == data.empty => results += schemaColumn.value(data.empty)
//            case (elem, _)                                  => results += elem
//          }
//
//          (0 until elements.length - values.length).foreach { idx =>
//            val offset = idx + values.length
//            val fn = elements(offset).value
//            results += fn(data.empty)
//          }
//          data.row(results.toVector)
//
//        case _ => inp
//
//      }
//    }
//
//    val resizeColumns: Data => Wurt.WithSchema[Data, Data] = inp =>
//      Fix.un[DataF](inp) match {
//        case RowValue(values) => resizeColumnsFn(inp, values)
//        case EmptyValue       => resizeColumnsFn(inp, Vector.empty)
//        case _                => cats.data.Kleisli.pure(inp)
//    }
//  }
//}
