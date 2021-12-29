package felix.math

type Generator = (Char, MobiusTransformation)


extension (c: Char)
    def inv: Char = if c.isLower then c.toUpper else c.toLower

def inverseGenerator(gen: Generator): Generator =
    val (name, op) = gen
    (name.inv, op.inv)

class Group(gens: Map[Char, MobiusTransformation]):
    require(gens.keys.forall(_.isLower))

    val generators = {
        for baseGen <- gens
             gen <- List(baseGen, inverseGenerator(baseGen))
         yield
             gen
    }.toMap

    def apply(gen: Char): MobiusTransformation = generators.get(gen).get

    def apply(word: String): MobiusTransformation =
        val elts = for c <- word yield generators.get(c).get
        elts.fold(MobiusTransformation.identity)((a,b) => a*b)

    def walk(
        depth: Int, 
        initial: String = "", 
    )(op: (String, MobiusTransformation) => Unit) =
        def recurse(depth: Int, prefix: String, elt: MobiusTransformation): Unit =
            if depth <= 0 then op(prefix, elt)
            else {
                op(prefix, elt)
                val skip = if prefix.size > 0 then prefix.last.inv else '.'
                for (c, m) <- generators if c != skip
                do recurse(depth-1, prefix+c, elt*m)
            }
        
        recurse(depth, initial, MobiusTransformation.identity)

object Group:
    def apply(gens: (Char, MobiusTransformation)*): Group =
        new Group(gens.toMap)

