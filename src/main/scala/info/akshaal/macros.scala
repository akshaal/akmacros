/** Evgeny Chukreev, 2012. http://akshaal.info */

package info.akshaal

import language.experimental.macros
import scala.language.existentials
import scala.reflect.macros.Context
import scala.annotation.Annotation

/**
 * Macros for traversing over class elements.
 */
object clazz {
    def valueIdentity[X](value: X, annotationArgs: Any): X = value
    def castValue[X](value: Option[Any], symbol: Symbol): Option[X] = value map (_.asInstanceOf[X])

    /**
     * Class factory type.
     * @tparam F type of value that parameter provider function should return
     * @tparam T type of value that this factory creates
     */
    type Factory[F, T] = Function1[Function1[Symbol, Option[F]], T]

    /**
     * Macro to create a factory for the given type I.
     * @tparam I factory will be generated for this type
     * @tparam F type of values that provider function should return
     * @param apply symbol of the function to use to convert from provider function results into class parameter values
     */
    def factory[F, I <: AnyRef](apply: Symbol): Factory[F, I] = macro factoryImpl[F, I]

    /**
     * Implementation of the factory macro.
     */
    def factoryImpl[F: c.AbsTypeTag, I <: AnyRef: c.AbsTypeTag](c: Context)(apply: c.Expr[Symbol]): c.Expr[Factory[F, I]] = {
        import c.universe._

        val applyFunName =
            apply.tree match {
                case Apply(_, List(Literal(Constant(s)))) => s.toString
                case _ =>
                    c.abort(apply.tree.pos,
                            "factory macro is expected to be used with symbol literal like 'castValue")
            }

        // Workaround for unavailable nme.defaultGetterName (borrowed from compiler internals)
        val DEFAULT_GETTER_INIT_STRING = "$lessinit$greater"
        val DEFAULT_GETTER_STRING = "$default$"
        def defaultGetterName(pos: Int): TermName = {
            newTermName(DEFAULT_GETTER_INIT_STRING + DEFAULT_GETTER_STRING + pos)
        }

        // Materialize types
        val instanceT = implicitly[c.AbsTypeTag[I]].tpe
        val constructor = instanceT.declaration(nme.CONSTRUCTOR).asMethod
        val companionSymbol = instanceT.typeSymbol.companionSymbol

        val paramTreeSets =
            constructor.params.take(1) map {
                paramSet =>
                    paramSet.zipWithIndex map {
                        case (param, paramZeroIndex) =>
                            val paramRealType = param.typeSignature.asSeenFrom(instanceT, instanceT.typeSymbol)

                            val symbolNameExpr = c.literal(param.name.toString.trim)
                            val symbolTree = reify ( scala.Symbol(symbolNameExpr.splice) ).tree
                            val applyFunTree = c.parse(applyFunName)

                            val fCallTree = Apply(Ident(newTermName("f")), List(symbolTree))

                            val applyFunApplicationTree =
                                treeBuild.mkMethodCall(applyFunTree,
                                                       List(paramRealType),
                                                       List(fCallTree, symbolTree))

                            val paramValueTree =
                                if (param.asTerm.isParamWithDefault) {
                                    val defaultGetterTree =
                                        Select(treeBuild.mkAttributedRef(companionSymbol),
                                               defaultGetterName(paramZeroIndex + 1))

                                    Apply (Select (applyFunApplicationTree, newTermName("getOrElse")),
                                           List(defaultGetterTree))
                                } else {
                                    Select (applyFunApplicationTree, newTermName("get"))
                                }

                            paramValueTree
                    }
            }

        val constructorCallTree =
            paramTreeSets.foldLeft(Select(New(treeBuild.mkAttributedRef(instanceT.typeSymbol)),
                                          nme.CONSTRUCTOR) : Tree)(
                treeBuild.mkMethodCall(_: Tree, _)
            )

        val constructorCallExpr = c.Expr[I](constructorCallTree)

        reify {
            new Factory[F, I] {
                def apply(f: scala.Symbol => Option[F]): I = {
                    constructorCallExpr.splice
                }
            }
        }
    }

    /**
     * An object of this class represents a class field.
     * @tparam I type of class the field belongs to
     * @tparam R type of value for the field
     * @tparam A type of annotation arguments (TupleX or None)
     * @param name name of the field
     * @param get function that returns field value of an instance given as argument to the function
     * @param args list of arguments to the annotation found on the field
     */
    case class Field[-I <: AnyRef, +R, +A <: Product](name: String, get: I => R, args: A)

    /**
     * List of fields belonging to the given type.
     * @tparam I Owner of fields
     * @tparam R common super type of field values
     * @tparam A type of annotation arguments (TupleX or None)
     */
    type Fields[-I <: AnyRef, +R, +A <: Product] = List[Field[I, R, A]]

    /**
     * Macro which inspects class 'I' and returns a list of fields annotated with annotation 'Ann' (or Nothing).
     * @tparam I type of class to scan for fields
     * @tparam R common super type of field values
     * @tparam Ann search for field with this annotation
     * @tparam Args type of arguments in the annotation (TupleX or None)
     */
    def fields[Ann, I <: AnyRef, R, Args <: Product](apply: Symbol) = macro fieldsImpl[Ann, I, R, Args]

    /**
     * Macro which inspects class 'I' and returns a list of all fields.
     * @tparam I type of class to scan for fields
     * @tparam R common super type of field values
     */
    def allFields[I <: AnyRef, R](apply: Symbol) = macro fieldsImpl[Any, I, R, None.type]

    /**
     * Implementation of the fields macro.
     */
    def fieldsImpl[Ann: c.AbsTypeTag, I <: AnyRef: c.AbsTypeTag, R: c.AbsTypeTag, Args <: Product: c.AbsTypeTag](c: Context)(apply: c.Expr[Symbol]): c.Expr[Fields[I, R, Args]] = {
        import c.universe._

        // Materialize types
        val instanceT = implicitly[c.AbsTypeTag[I]].tpe
        val annT = implicitly[c.AbsTypeTag[Ann]].tpe

        val applyFunName =
            apply.tree match {
                case Apply(_, List(Literal(Constant(s)))) => s.toString
                case _ =>
                    c.abort(apply.tree.pos,
                        "fields macro is expected to be used with symbol literal like 'valueIdentity")
            }

        // Get annotated fields. Note that hasAnnotation doesn't work for a reason...
        val allFields = annT.toString == "Any"
        val fields =
            if (allFields) {
                instanceT.members filter (member => member.asTerm.isGetter && member.isPublic)
            } else {
                instanceT.members filter (member => member.getAnnotations.exists(_.atp == annT))
            }

        // Fold given expression sequence into a new expression that creates List of expressions at runtime
        def foldIntoListExpr[T: c.AbsTypeTag](exprs: Iterable[c.Expr[T]]): c.Expr[List[T]] =
            exprs.foldLeft(reify { Nil: List[T] }) {
                (accumExpr, expr) =>
                    reify { expr.splice :: accumExpr.splice }
            }

        // For each field, construct expression that will instantiate Field object at runtime
        val fieldExprs =
            for (field <- fields) yield {
                val name = field.name.toString.trim
                val nameExpr = c literal name

                // Construct arguments list expression
                val argsTree = {
                    lazy val argTrees = field.getAnnotations.find(_.atp == annT).get.args

                    if (allFields || argTrees.isEmpty) {
                        Select(Ident(newTermName("scala")), newTermName("None"))
                    } else {
                        val tupleConstTree = Select(Select(Ident(newTermName("scala")),
                            newTermName(s"Tuple${argTrees.size}")),
                            newTermName("apply"))
                        Apply(tupleConstTree, argTrees)
                    }
                }

                val argsExpr = c.Expr[Args](argsTree)

                // Construct expression (x : $I) => x.$name
                val applyFunTree = c.parse(applyFunName)
                val getFunArgTree = ValDef(Modifiers(), newTermName("x"), TypeTree(instanceT), EmptyTree)

                val getFunBodyTree =
                    treeBuild.mkMethodCall(applyFunTree,
                        List(Select(Ident(newTermName("x")), newTermName(name)),
                            argsTree))

                val getFunExpr = c.Expr[I => R](Function(List(getFunArgTree), getFunBodyTree))

                reify {
                    Field[I, R, Args](name = nameExpr.splice, get = getFunExpr.splice, args = argsExpr.splice)
                }
            }

        // Construct expression list like field1 :: field2 :: Field3 ... :: Nil
        foldIntoListExpr(fieldExprs)
    }
}

//  LocalWords:  args TupleX Tuple argTrees nme defaultGetterName
//  LocalWords:  lessinit getOrElse
