package datum.transform

import datum.schema._
//import SQL._

object Challenge {

  /*
  json_schema = [name:string,id:int,age:int,school_name:string,school_id:int]
csv_schema = [[name:string,id:int,age:int,school_id:int], [school_id:int, school_name:string]]
json_data
{
    [
        {"abc",12,"tech", 123},
        {"abc",12,"tech1", 345},
        {"xyz",12,"tech", 123},
        {"xyz",12,"cmu", 576},
    ]
}

this should be converted to two csv formats as
table_1
"abc",12,123\n
"abc",12,345\n
"xyz",12,123\n
"xyz",12,576\n

table_2
123,"tech"\n
123,"tech"\n
345,"tech1"\n
576,"cmu"\n
   */

  val inp: Schema = struct(
    "name" -> text,
    "id" -> int,
    "age" -> int,
    "school_name" -> text,
    "school_id" -> int
  )

  val out1: Schema = struct(
    "name" -> text,
    "id" -> int,
    "age" -> int,
    "school_id" -> int
  )

  val out2: Schema = struct(
    "school_id" -> int,
    "school_name" -> text
  )


//  Transform.struct(
//    "country" -> Copy("originCountry" :: asText),
//    "person" -> Transform.struct(
//      "firstName" -> Copy("name" :: asText),
//      "lastName" -> Copy("name" :: asText)
//    )
//  )
}
