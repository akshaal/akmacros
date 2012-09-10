/** Evgeny Chukreev, 2012. http://akshaal.info */

package info.akshaal

import org.specs2._

class MacroSpec extends Specification with matcher.ScalaCheckMatchers {
    def is =
        "This is a specification for macros." ^
            "fields macro should work" ! (recordTest.example1 and personTest.example) ^
            "fields macro should work with arbitrary data" ! recordTest.example2 ^
            "fields macro should support value transformation" ! transTest.example ^
            "factory macro should support default parameter values" ! factoryTest.onlyDefaultsExample ^
            "factory macro should work when there is no default value" ! factoryTest.noDefaultsExample ^
            "factory macro should throw exception when no value and no defaults" ! factoryTest.exceptionDefaultsExample ^
            "factory macro should handle abstract types" ! factoryTest.complicatedExample

    import clazz.{valueIdentity, castValue}

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
                            field.name + ": " + field.get(t: T).trim
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

    object factoryTest {
        case class Z (a : String = "x", b : Int = 3)
        val castingZFactory = clazz.factory[Any, Z]('castValue)

        case class Kla (x : String = "x", y : Int)
        val castingKlaFactory = clazz.factory[Any, Kla]('castValue)

        def onlyDefaultsExample = castingZFactory(_ => None) must_== Z()

        def noDefaultsExample =
            castingKlaFactory {
                case 'y => Some(10)
                case _ => None
            } must_== Kla(y = 10)

        def overrideDefaultsExample =
            castingKlaFactory {
                case 'y => Some(11)
                case 'x => Some("xxx")
            } must_== Kla(x = "xxx", y = 11)

        def exceptionDefaultsExample =
            castingKlaFactory {
                case 'y => None
                case 'x => Some("abc")
            } must throwAn[NoSuchElementException]

        // Complicated test

        trait S
        case class SValue(x : Any) extends S
        case class SMap(map : Map[Symbol, S]) extends S

        trait MyParseable[T] {
             def myParse(s : S) : T
        }

        implicit object PInt extends MyParseable[Int] {
            def myParse(s : S) : Int = s match {
                case SValue(x) => x.asInstanceOf[Int]
            }
        }

        implicit object PString extends MyParseable[String] {
            def myParse(s : S) : String = s match {
                case SValue(x) => x.asInstanceOf[String]
            }
        }

        def myValueParse[X : MyParseable](value: Option[S], symbol: Symbol): Option[X] =
            value map (myParse[X](_))

        def myParse[T : MyParseable](s : S) : T = implicitly[MyParseable[T]].myParse(s)

        implicit def factoryToParseable[T <: AnyRef](implicit factory : clazz.Factory[S, T]) : MyParseable[T] = {
            new MyParseable[T] {
                def myParse(s : S) : T = {
                    factory {
                        sym =>
                            s match {
                                case SMap(m) => m get sym
                            }
                    }
                }
            }
        }

        // Test

        def test() : Unit = {
            case class CaseA(aaa : Int) extends Case
            implicit def stm = clazz.factory[S, CaseA]('myValueParse)
        }

        sealed trait Case
        case class CaseA(aaa : Int) extends Case
        case class CaseB(bbb : String) extends Case
        case class Term[T](name : String, value : T)
        case class TermImp[T : MyParseable](name : String, value : T)

        implicit def stm1[T : MyParseable] = clazz.factory[S, Term[T]]('myValueParse)
        implicit val stm2 : clazz.Factory[S, CaseA] = clazz.factory[S, CaseA]('myValueParse)
        implicit val stm3 : clazz.Factory[S, CaseB] = clazz.factory[S, CaseB]('myValueParse)
        implicit def stm4[T : MyParseable] : clazz.Factory[S, TermImp[T]] = clazz.factory[S, TermImp[T]]('myValueParse)

        implicit val caseParseable =
            AbstractParseable[Case](itHas('aaa) -> stm2).or(itHas('bbb) -> stm3)

        case class AbstractParseable[T <: AnyRef] (cases : Seq[(SMap => Boolean, clazz.Factory[S, T])]) extends MyParseable[T] {
            def myParse(s : S) : T = s match {
                case smap: SMap =>
                    (cases find (_._1(smap)) map (_._2) map(factoryToParseable(_).myParse(s))).get
            }

            def or(aCase : (SMap => Boolean, clazz.Factory[S, T])) : AbstractParseable[T] =
                new AbstractParseable (cases ++ List(aCase))
        }

        object AbstractParseable {
            def apply[T <: AnyRef] (aCase : (SMap => Boolean, clazz.Factory[S, T])) : AbstractParseable[T] =
                new AbstractParseable (List(aCase))
        }

        def itHas(sym : Symbol) = (smap : SMap) => smap.map.contains(sym)

        val caseA1S = SMap(Map('aaa -> SValue(3)))
        val term1S = SMap(Map('name -> SValue("abc"), 'value -> caseA1S))

        val caseB2S = SMap(Map('bbb -> SValue("x")))
        val term2S = SMap(Map('name -> SValue("mno"), 'value -> caseB2S))

        def complicatedExample =
            (myParse[Term[Case]](term1S) must_== Term(name = "abc", value = CaseA(3))) and
            (myParse[Term[Case]](term2S) must_== Term(name = "mno", value = CaseB("x"))) and
            (myParse[TermImp[Case]](term2S) must_== TermImp(name = "mno", value = CaseB("x")))
    }
}

//  LocalWords:  abc myValueParse mno BillGates cba xyz
