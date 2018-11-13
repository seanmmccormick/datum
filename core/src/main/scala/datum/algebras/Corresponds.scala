package datum.algebras
import datum.algebras.generic.ModifiableFunction
import datum.patterns.attributes.Attributed
import datum.patterns.data._
import datum.patterns.schemas._
import qq.droste.Algebra
import qq.droste.data.Fix

class Corresponds(override val modify: Attributed[Boolean] => Attributed[Boolean])
  extends ModifiableFunction[Data, Boolean] {

  private def matchValue(fn: PartialFunction[DataF[Fix[DataF]], Boolean])(value: Data): Boolean =
    fn.applyOrElse[DataF[Fix[DataF]], Boolean](Fix.un[DataF](value), _ => false)

  override val base: Algebra[SchemaF, Data => Boolean] = Algebra {

    case ObjF(schemaFields, _) =>
      Fix.un[DataF](_) match {
        case ObjValue(valueFields) =>
          schemaFields.forall {
            case (key, checkFn) =>
              valueFields.contains(key) && checkFn(valueFields(key))
          }
        case _ => false
      }

    case RowF(schemaColumns, _) =>
      Fix.un[DataF](_) match {
        case RowValue(values) if schemaColumns.length == values.length =>
          schemaColumns.zip(values).forall {
            case (column, data) =>
              val fn = column.value
              fn(data)
          }
        case _ => false
      }

    case UnionF(alternatives, _) =>
      d =>
        alternatives.exists(pred => pred(d))

    case ArrayF(element, _) =>
      Fix.un[DataF](_) match {
        case _ => false
      }

    case ValueF(TextType, _)      => matchValue { case TextValue(_)      => true }
    case ValueF(IntType, _)       => matchValue { case IntValue(_)       => true }
    case ValueF(LongType, _)      => matchValue { case LongValue(_)      => true }
    case ValueF(FloatType, _)     => matchValue { case FloatValue(_)     => true }
    case ValueF(DoubleType, _)    => matchValue { case DoubleValue(_)    => true }
    case ValueF(BooleanType, _)   => matchValue { case BooleanValue(_)   => true }
    case ValueF(DateType, _)      => matchValue { case DateValue(_)      => true }
    case ValueF(InstantType, _)   => matchValue { case InstantValue(_)   => true }
    case ValueF(LocalTimeType, _) => matchValue { case LocalTimeValue(_) => true }
    case ValueF(ZonedTimeType, _) => matchValue { case ZonedTimeValue(_) => true }
    case ValueF(BytesType, _)     => matchValue { case BytesValue(_)     => true }

  }
}
