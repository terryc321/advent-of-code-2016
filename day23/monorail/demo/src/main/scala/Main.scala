
class foo {

  var a : Int = 0
  var b : Int = 0
  var c : Int = 0
  var d : Int = 0
  var i : Int = 0

  def v0 : Any => Any = {a = 1 ; return v1 }; // cpy 1 a
  def v1 () : Any = {b = 1 ; return v2 } ; // cpy 1 b
  def v2 () : Any = {d = 26 ; return  v3} ; //cpy 26 d
  def v3 () : Any = {if c != 0 then return v5 else return v4} ;  // jnz c 2
  def v4 () : Any = {return v9} ;  // jnz 1 5
  def v5 () : Any = {c = 7  ; return v6} ; // cpy 7 c
  def v6 () : Any = {d = d + 1 ; return v7 } ; // inc d
  def v7 () : Any = {c = c - 1 ; return v8 } ; // dec c
  def v8 () : Any = {if c != 0 then return v6  else return v9 } ;  // jnz c -2
  def v9 () : Any = {c = a ; return v10} ;  // cpy a c
  def v10 () : Any = {a = a + 1 ; return v11 } ;  // inc a
  def v11 () : Any = {b = b - 1 ; return v12 } ;  // dec b
  def v12 () : Any = {if b != 0 then return v10 else return v13 } ;  // jnz b -2
  def v13 () : Any = {b = c ; return v14} ;  // cpy c b
  def v14 () : Any = {d = d - 1 ; return v15} ;  // dec d
  def v15 () : Any = {if d != 0 then return v9 else return v16 } ;  // jnz d -6
  def v16 () : Any = {c = 18 ; return v17 } ;  // cpy 18 c
  def v17 () : Any = {d = 11 ; return v18 } ;  // cpy 11 d
  def v18 () : Any = {a = a + 1 ; return v19 } ;  // inc a
  def v19 () : Any = {d = d - 1 ; return v20 } ;  // dec d
  def v20 () : Any = {if d != 0 then return v18 else return v21 } ;  // jnz d -2
  def v21 () : Any = {c = c - 1 ; return v22 } ;  // dec c
  def v22 () : Any = {if c != 0 then return v17 else return vDone } ;  // jnz c -5
  def vDone () : Any = {}

  def reset () : Unit = {
    a = 0
    b = 0
    c = 0
    d = 0
  }

  def run () : Unit = {
    reset()
    v0()
    println(s"a = $a")
    println(s"b = $b")
    println(s"c = $c")
    println(s"d = $d")
  }
}

@main
def hello(): Unit = {
  val f = new foo()
  f.run()
}
