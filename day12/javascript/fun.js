

  var a = 0
  var b = 0
  var c = 0
  var d = 0
  var i = 0

function v0 () {a = 1 ; return v1 }; // cpy 1 a
function v1 () {b = 1 ; return v2 } ; // cpy 1 b
function v2 () {d = 26 ; return  v3} ; //cpy 26 d
function v3 () {if (c != 0) {return v5} else {return v4}} ;  // jnz c 2
function v4 () {return v9} ;  // jnz 1 5
function v5 () {c = 7  ; return v6} ; // cpy 7 c
function v6 () {d = d + 1 ; return v7 } ; // inc d
function v7 () {c = c - 1 ; return v8 } ; // dec c
function v8 () {if (c != 0) {return v6} else {return v9} } ;  // jnz c -2
function v9 () {c = a ; return v10} ;  // cpy a c
function v10 () {a = a + 1 ; return v11 } ;  // inc a
function v11 () {b = b - 1 ; return v12 } ;  // dec b
function v12 () {if (b != 0) {return v10} else {return v13 }} ;  // jnz b -2
function v13 () {b = c ; return v14} ;  // cpy c b
function v14 () {d = d - 1 ; return v15} ;  // dec d
function v15 () {if (d != 0) {return v9} else {return v16} } ;  // jnz d -6
function v16 () {c = 18 ; return v17 } ;  // cpy 18 c
function v17 () {d = 11 ; return v18 } ;  // cpy 11 d
function v18 () {a = a + 1 ; return v19 } ;  // inc a
function v19 () {d = d - 1 ; return v20 } ;  // dec d
function v20 () {if (d != 0) {return v18} else {return v21 }} ;  // jnz d -2
function v21 () {c = c - 1 ; return v22 } ;  // dec c
function v22 () {if (c != 0) {return v17} else {return vDone }} ;  // jnz c -5
function vDone () {return "done"}

function reset () {
    a = 0
    b = 0
    c = 0
    d = 0
  }

// compensate for javascript not having tail recursion
// we trampoline each
// ok for static programs
function run () {
    reset()    
    let fn = v0;
    while (typeof(fn) === 'function'){
	fn = fn();
    }
    console.log("---------------------")
    console.log("a = ",a)
    console.log("b = ",b)
    console.log("c = ",c)
    console.log("d = ",d)
    console.log("---------------------")
}

function run2 () {
    reset()
    c = 1
    let fn = v0;
    while (typeof(fn) === 'function'){
	fn = fn();
    }
    console.log("---------------------")
    console.log("a = ",a)
    console.log("b = ",b)
    console.log("c = ",c)
    console.log("d = ",d)
    console.log("---------------------")
}


run()
run2()

