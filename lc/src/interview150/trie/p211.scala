package p211

final class WordDictionary() {
    import scala.collection.mutable
    import scala.annotation.tailrec

    val map = mutable.Map.empty[Char, WordDictionary]
    var terminate = false

    @tailrec
    def addWord(word: String): Unit = word match {
        case "" =>
            terminate = true
        case s if map contains s.head =>
            map(s.head).addWord(s.tail)
        case s =>
            map(s.head) = new WordDictionary()
            map(s.head).addWord(s.tail)
    }

    def search(word: String): Boolean = WordDictionary.search(List((word, this)))
}

object WordDictionary {
    import scala.annotation.tailrec

    @tailrec
    def search(checks: List[(String, WordDictionary)]): Boolean = checks match {
        case Nil => false
        // whenever result = false, check next case
        case ("", dict) :: _ if dict.terminate => true
        case ("", dict) :: tl => search(tl)
        case (s, dict) :: tl if s.head == '.' && dict.map.isEmpty => search(tl)
        case (s, dict) :: tl if s.head == '.' => {
            val newChecks = dict.map.values.map(d => (s.tail, d))
            search(newChecks.toList ++ tl)
        }
        case (s, dict) :: tl if dict.map contains s.head => {
            val newCheck = (s.tail, dict.map(s.head))
            search(newCheck :: tl)
        }
        case _ :: tl => search(tl)
    }
}

object Solution {
    val ops = Array("addWord","addWord","addWord","search","search","addWord","search","addWord","search","addWord","addWord","search","addWord","addWord","search","search","addWord","addWord","search","search","search","addWord","search","addWord","addWord","search","addWord","search","addWord","search","search","search","addWord","search","addWord","search","search","addWord","search","search","search","search","addWord","addWord","search","search","search","addWord","addWord","search","addWord","search","addWord","search","search","search","addWord","addWord","addWord","addWord","search","search","search","search","addWord","search","addWord","addWord","search","addWord","addWord","search","search","addWord","search","search","addWord","addWord","addWord","search","search","addWord","addWord","addWord","search","search","search","search","search","search","search","search","addWord","addWord","addWord","addWord","addWord","search","addWord","addWord","addWord","addWord","search")
    val inputs = Array("xgvk","wykzbvwdsoyfowqicymzd","xajbtjyjuwgoynjgu","wykzbvwdso..owqicymzd","..ha","qsibzxaorktypkfg","xgvk","vbycuvrkbcq","qsibz.aorkty.kfg","sm","fkqclfmvzpzpnbvz","vb..uvrkbcq","jpnneostllnnma","zvmtfg","g..",".kqclfmvzpzpnbvz","lboe","jypzkxnzc","ii..mhdgrif","ln.","zv..fg","qes","ittuggead.lxjey.i","jioqlytzqx","fojsjyiz","a","qkprluekewtsftvbrjndpt","fkqlfmvzp.p.bvz","mwsgyywmmkzmy","g",".pnneostllnnma","bxwqn.nva.shpbb","tcjmitm","xajb.jyjuwgoynjg.","pybk","qolrv","qsibxa.rkty.kfg","poljqcitty","nmp","lboe","vm.f.","kurootufigiiy.v.","qfdabgsvkboyaq","pvreuprpvoycadnsxaajrkh","le.c.de","jsxmeg.cnpigklxtyfcjset","pybk","sv","knmxzabetvqehv","ozh.zke.xy","ziazu","cfzvjmpidlvypukuvxf","ghhelrzgbsmxkrnezif","vqxn..aab","xhyvayva.","xi","fn","tnjcttrsozynjpqhox","qhxcfujxmayzlsrctmsa","fyaaivfrupktdgw","jpnne.stllnnma","..","zv..fg","polq.it.y")
    val results = Array(null,null,null,true,false,null,true,null,true,null,null,true,null,null,false,true,null,null,false,false,true,null,false,null,null,false,null,false,null,false,true,false,null,true,null,false,false,null,false,true,false,false,null,null,false,false,true,null,null,false,null,false,null,false,false,false,null,null,null,null,true,true,true,false,null,true,null,null,false,null,null,true,false,null,false,false,null,null,null,false,false,null,null,null,false,false,false,false,false,false,true,true,null,null,null,null,null,false,null,null,null,null,true,false,false,null,true,false,false,null,true,false,null,false,null,null,true,null,false,null,false,null,null,null,null,null,true,null,null,null,null,null,true,null,false,null,null,null,null,null,null,null,null,null,null,true,true,false,false,true,null,true,true,null,true,null,true,null,true,null,null,false,null,true,null,false,false,null,false,null,null,null,true,null,false,true,false,null,null,false,null,null,true,null,null,null,null,null)
    def main(args: Array[String]) = {
        val d = new WordDictionary()
        (ops zip inputs zip results).foreach {
            case (("addWord", i), null) => d.addWord(i)
            case (("search", ".."), _) => 
            case (("search", w), res) => {
                if (d.search(w) != res) {
                    println(w)
                }
            }
            case _ => ???
        }
    }
}

/**
 * Your WordDictionary object will be instantiated and called as such:
 * val obj = new WordDictionary()
 * obj.addWord(word)
 * val param_2 = obj.search(word)
 */