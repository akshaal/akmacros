/** Akshaal, 2012. http://akshaal.info */

package info.akshaal

/**
 * Macros for traversing over class elements.
 */
object clazz {
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
}

//  LocalWords:  args TupleX Tuple argTrees
