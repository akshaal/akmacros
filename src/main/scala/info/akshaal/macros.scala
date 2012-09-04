/** Akshaal, 2012. http://akshaal.info */

package info.akshaal

import language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.Annotation

/**
 * Macros for traversing over class elements.
 */
object clazz {
    def valueIdentity[X] (value : X, annotationArgs : Any) : X = value

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
                        "fields macro is expected to be used with symbol literal like 'nothing or 'myFunction")
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

//  LocalWords:  args TupleX Tuple argTrees
