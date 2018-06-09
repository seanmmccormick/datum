# Types

Path - eg:
`/foo/bar/:2/baz`

Segment - An element of a `Path`

Schema - Should describe a single "thing" (eg row, case class, record)

Data - Should represent a single "thing" (eg row, case class, record)

Description

Value

Transform

# Meanings

m :: Schema ->  Path -> Desciption

m :: Data ->  Path -> Value

m :: Path -> [ Segments ]

m :: Segment -> ( NamedSegment(string) | IndexedSegment(int) | Root | End {of path} )

m :: Description -> ( type?, optional?, metadata? )

# Operations

transform :: [ (Path {src}, Path {dst}, a -> b) ] -> Data -> Data

lookup :: Data -> Path -> Value
       :: Schema -> Path -> Description
       
update :: (Path -> Value) -> Data -> Data
       :: (Path -> Value) -> Schema -> Schema

generate :: [ (Path {src}, Path {dst}) ] -> Data -> Data

save :: Data -> ()

load :: () -> Data

# Questions

What is a csv? How does it relate to `{ [ ... ] }` (ie a js file as in challenge 1)

? :: Path -> Container Schema
  :: Path -> Container Data
  
foreach :: (Data -> b) -> Container Data -> Container B  