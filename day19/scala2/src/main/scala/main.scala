
/*

 doubly linked list implementation in scala 

 */


class Node :
  var prev : Node = EmptyNode
  var next : Node = EmptyNode
  var index : Int = 0
  var value : Int = 1
  def delete () = {
    if (prev != EmptyNode){ 
      prev.next = next
    }
    if (next != EmptyNode){
      next.prev = prev
    }
  }
  def foo () = {
    5
  }

// needed a singleton empty object
object EmptyNode extends Node 

def makeCircularList (s : Int) = {
  var hd : Node = EmptyNode
  var tl : Node = EmptyNode
  var a : Int = s
  var i : Int = 1
  while a > 0 do
   a = a - 1
   var tmp : Node = Node()
   tmp.value = 1
   tmp.index = i
   if ( tl != EmptyNode ) {
     tmp.prev = tl
     tl.next = tmp
     tl = tmp
   }
   else {
     hd = tmp
     tl = tmp
   }
   i = i + 1
  tl.next = hd
  hd
}

def showList (in : Node ) = {
  var p : Node = in
  var iter : Int = 1
  if (p != EmptyNode){
    print(" (")
    print(p.index)
    print(",")
    print(p.value)
    print(") ")
    p = p.next
    while (p != in)
      iter = 0
      print(" (")
      print(p.index)
      print(",")
      print(p.value)
      print(") ")
      p = p.next
    println("")  
  }
}

def showNode(p : Node) = {
  print(" (")
  print(" ivpn:")
  print(p.index)
  print(",")
  print(p.value)
  print(",")
  print(p.prev)
  print(",")
  print(p.next)
  print(") ")  
}


def demo(in : Node , size : Int) = {
  var p : Node = in
  var last : Node = EmptyNode
  var i : Int = size
  while(i > 1){
    
    //showNode(p)
    //showList(p)
    var next : Node = p.next
    p.value = p.value + next.value
    next.value = 0

    //println(s"p before.next = ${p.next}")
    //next.delete()
    //println(s"p after.next = ${p.next}")
    p.next = next.next
    next.next.prev = p

    p = p.next
    //p.next = next.next
    //next.next.prev = p
    last = p
    i = i - 1    
  }
  //showNode(last)
  showList(last)
}


// how figure out who is opposite ? 
def demo2(in : Node , size : Int) = {
  var p : Node = in
  var last : Node = EmptyNode
  var i : Int = size
  while(i > 1){

    var skip = i / 2
    var tmp = p
    while (skip > 0){
      tmp = tmp.next
      skip = skip - 1
    }
    //println(s"taking from elf ${tmp.index}")

    p.value = p.value + tmp.value
    tmp.prev.next = tmp.next
    tmp.next.prev = tmp.prev
    
    // next elf
    p = p.next
    last = p
    i = i - 1    
  }
  showList(last)
}


@main
def main() = {
  println("hello world!")
  var size : Int = 5
  var hd : Node = makeCircularList(size)
  // demo(hd,size)
  // 
  // size = 3005290
  // hd = makeCircularList(size)
  // demo(hd,size)

  size = 5
  hd = makeCircularList(size)
  demo2(hd,size)

  size = 3005290
  hd = makeCircularList(size)
  demo2(hd,size)
  
  // println("node.foo is ")
  // println(5); // + node.foo()
  // println("whats happening")
  //println(node.foo() + node.foo())

  println("all done!")

}


