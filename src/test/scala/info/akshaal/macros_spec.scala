/** Akshaal, 2012. http://akshaal.info */

package info.akshaal

import org.specs2._

class MacroSpec extends Specification with matcher.ScalaCheckMatchers {
    def is =
        "This is a specification for macros." ^
            "fields macro should work" ! (recordTest.example1 and personTest.example) ^
            "fields macro should work with arbitrary data" ! recordTest.example2 ^
            "fields macro should support value transformation" ! transTest.example

    import clazz.valueIdentity

    object personTest {
        type FormatFun = Any => Any
        type PrettyArgs = (Option[String], FormatFun)
        class Pretty(aka : Option[String] = None, format : FormatFun = identity) extends annotation.StaticAnnotation

        def pp[X <: AnyRef](fields : clazz.Fields[X, Any, PrettyArgs])(x : X) = {
            fields map {
                case clazz.Field(fieldName, get, (akaOpt, fmtFun)) =>
                    val name = fieldName.replaceAll("([A-Z][a-z]+)", " $1").toLowerCase.capitalize
                    val aka = akaOpt map (" (aka " + _ + ")") getOrElse ""
                    val value = fmtFun(get(x))
                    s"$name$aka: $value"
            } mkString "\n"
        }

        val toUpperCaseFormat = (_ : Any).toString.toUpperCase
        val optionFmt = (_ : Any) match { case x : Option[_] => x getOrElse "" }

        case class Person(
            id : Int,
            @Pretty(aka = Some("nickname")) name : String,
            @Pretty firstName : String,
            @Pretty(None, format = toUpperCaseFormat) secondName : String,
            @Pretty(None, format = optionFmt) twitter : Option[String])
        val personPrettyFields = clazz.fields[Pretty, Person, Any, PrettyArgs]('valueIdentity)
        val ppPerson = pp(personPrettyFields) _

        val person1 = Person(1, "akshaal", "Evgeny", "Chukreev", Some("https://twitter.com/Akshaal"))
        val person2 = Person(2, "BillGates", "Bill", "Gates", Some("https://twitter.com/BillGates"))

        val persons = List(person1, person2)

        def example =
            (persons map ppPerson) must_==
                List(
                    List(
                        "Name (aka nickname): akshaal",
                        "First name: Evgeny",
                        "Second name: CHUKREEV",
                        "Twitter: https://twitter.com/Akshaal").mkString ("\n"),

                    List(
                        "Name (aka nickname): BillGates",
                        "First name: Bill",
                        "Second name: GATES",
                        "Twitter: https://twitter.com/BillGates").mkString ("\n"))
    }

    object recordTest {
        class Attr(title : String, priority : Int = 0) extends annotation.StaticAnnotation

        case class Record(id : Int,
                          @Attr("Name", 1) name : String,
                          @Attr("Weight") weight : Long)

        val annotatedRecordFields = clazz.fields[Attr, Record, Any, (String, Int)]('valueIdentity)

        def fieldInfo2Str(record : Record)(field : clazz.Field[Record, Any, (String, Int)]) : String = {
            val value = field get record
            val (title, priority) = field.args
            s"${field.name}: ${value} ($title, $priority)"
        }

        def example1 = {
            val record = Record(18, "abc", 4)

            annotatedRecordFields.map(fieldInfo2Str(record)).toSet must_== Set(
                "name: abc (Name, 1)",
                "weight: 4 (Weight, 0)"
            )
        }

        def example2 = check { (l : Long, s : String) =>
            val record = Record(18, s, l)

            annotatedRecordFields.map(fieldInfo2Str(record)).toSet must_== Set(
                s"name: $s (Name, 1)",
                s"weight: $l (Weight, 0)"
            )
        }
    }

    object transTest {
        import scala.annotation.meta.getter

        // Mini-framework

        trait MyToStringable[-T] {
            def myToString(t : T) : String
        }

        def myToString[T : MyToStringable](t : T) : String = implicitly[MyToStringable[T]].myToString(t)

        implicit object myToStringableString extends MyToStringable[String] {
            def myToString(t : String) : String = t
        }

        implicit object myToStringableLong extends MyToStringable[AnyVal] {
            def myToString(t : AnyVal) : String = t.toString
        }

        // Adopt mini-framework for fields

        def toStringByMts[T : MyToStringable](t : T, args : Any) : String = myToString(t)

        implicit def myToStrinableFields[T <: AnyRef](implicit fields: clazz.Fields[T, String, _]): MyToStringable[T] = {
            new MyToStringable[T] {
                def myToString(t: T): String = {
                    "{" + (fields map {
                        (field: clazz.Field[T, String, _]) =>
                            field.name + ": " + field.get(t: T)
                    }).mkString(", ") + "}"
                }
            }
        }

        // Test - - - - - - - - -

        @getter class F extends annotation.StaticAnnotation

        case class Record[T](id : Int, name : String, data : T)
        case class Term[T](name : String, value : T)

        implicit def mts1[T : MyToStringable] = clazz.allFields[Record[T], String]('toStringByMts)
        implicit def mts2[T : MyToStringable] = clazz.allFields[Term[T], String]('toStringByMts)

        def example = {
            val record = Record(18, "abc", Term("test", 3))
            val record2 = Record(11, "cba", Term("test", Term("xyz", "x")))

            (myToString(record) must_== "{id: 18, name: abc, data: {name: test, value: 3}}") and
            (myToString(record2) must_== "{id: 11, name: cba, data: {name: test, value: {name: xyz, value: x}}}")
        }
    }
}
