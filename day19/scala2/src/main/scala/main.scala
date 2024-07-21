
/*


circular list implementation
insert ()
remove ()
 */

class Circ :
  var hd : Node = EmptyNode
  var tl : Node = EmptyNode
  var cur : Node = EmptyNode
  var size : Int = 0

  // insert after current cur
  def insert (i : Int) = {
    if (tl == EmptyNode){
      val tmp = Node()
      tmp.value = i
      tmp.index = size
      tmp.player = size + 1
      tmp.prev = tmp
      tmp.next = tmp
      cur = tmp
      hd = tmp
      tl = tmp
    }
    else {
      val tmp = Node()
      tmp.value = i
      tmp.index = size
      tmp.player = size + 1

      tmp.prev = tl     
      tl.next = tmp

      tmp.next = hd
      tl = tmp
      cur = tl
    }
    size = size + 1
  }


  def remove () = {
    if (cur != EmptyNode){
      if (cur.next == cur){ // one item only
        hd = EmptyNode
        tl = EmptyNode
        cur = EmptyNode
      }
      else {
        val next = cur.next
        val prev = cur.prev
        prev.next = cur.next
        next.prev = cur.prev
        cur = next
        size = size - 1      
      }
      // should have removed this element from 
    }
  }

  def forward () = {
    if (cur != EmptyNode){
      cur = cur.next
    }
  }

  def skip () = {
    if (cur != EmptyNode){
      cur = cur.next
    }
  }


  def backward () = {
    if (cur != EmptyNode){
      cur = cur.prev
    }
  }

  def showNode(p : Node) = {
    print(" (")
    print(" ivpn:")
    print(p.index)
    print(",")
    print(p.player)
    print(",")
    print(p.value)
    print(",")
    print(p.prev)
    print(",")
    print(p.next)
    println(") ")
  }

  def showList () = {
    // showing list does not move cur CURSOR 
    var p : Node = cur
    var iter : Int = 1
    if (p != EmptyNode){
      print(" (")
      print(p.index)
      print(",")
      print(p.player)
      print(",")
      print(p.value)
      print(") ")
      p = p.next
      while (p != cur){
        iter = 0
        print(" (")
        print(p.index)
        print(",")
        print(p.player)
        print(",")
        print(p.value)
        print(") ")
        p = p.next
      } // while
      println("")
    } // p != empty node
  }//

  def makeCircularList (s : Int) = {
    while (size < s){
      insert(1)
    }
    cur = hd
  }

  def reset() = {
    cur = hd
  }


  def traverse(j : Int) = {
    // inbuilt reset
    var p : Node = hd 
    var i : Int = 0
    // traverse to index floor(size / 2)
    while ( p.index != j ) {
      p = p.next
    }
    //println("traversal done. -> ")
    // set current node
    cur = p
  }

/*
 remove skip
 remove remove skip 
in : Node , size : Int
 */
  def fish() = {
    var p : Node = hd 
    var j : Int = size / 2
    var i : Int = 0
    // traverse to index floor(size / 2)
    while ( p.index != j ) {
      p = p.next
    }
    println("traversal done. -> ")
    // set current node
    cur = p

    remove()
    while ( cur.next != cur) {
      skip()
      remove() // remove will leave one item in circular list
      if ( cur.next != cur){
        remove()
      }
    }
    println(s"size is = ${size}")
    // the surviving single node should be the answer
    showList()
  }


 

class Node :
  var prev : Node = EmptyNode
  var next : Node = EmptyNode
  var index : Int = 0
  var value : Int = 1
  var player : Int = 0
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
  //showList(last)
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
  //showList(last)
}


def test () = {
  var size : Int = 3005290
  size = 5 
  //var hd : Node = makeCircularList(size)
  val c : Circ = new Circ()
  println("initialised")
  c.showList()

  println("add 10")
  c.insert(10)
  c.showList()
  println("------------")

  println("add 20")
  c.insert(20)
  c.showList()
  println("------------")

  println("add 30")
  c.insert(30)
  c.showList()
  println("------------")
  
  println("reset")
  c.reset()
  println("------------")

  println("current node ")
  c.showNode(c.cur) 
  println("------------")

  println("remove 1")
  c.remove()
  c.showList()
  println("------------")

  println("current node ")
  c.showNode(c.cur) 
  println("------------")
  println("remove 2")
  c.remove()
  c.showList()
  println("------------")

  println("current node ")
  c.showNode(c.cur) 
  println("------------")
  println("remove 3")
  c.remove()
  c.showList()
  println("------------")

  println("add 10")
  c.insert(10)
  c.showList()
  println("------------")

  println("add 20")
  c.insert(20)
  c.showList()
  println("------------")

  println("add 30")
  c.insert(30)
  c.showList()
  println("------------")
  
  println("reset")
  c.reset()
  println("------------")


  println(s"make circ list of size ${size}")
  c.makeCircularList(size)
  println(s"constructed circular list of size ${c.size}")
  c.showList()
  println("reset")
  c.reset()
  
  println("showlist")
  c.showList()
  println("remove")
  c.remove()
  println("showlist")
  c.showList()
  //c.fish()

}

def test2 () = {
  var size : Int = 3005290
  size = 5

  val d : Circ = new Circ()
  d.makeCircularList(size)
  println(s"constructed circular list of size ${d.size}")
  d.showList()

  d.reset()
  d.showNode(d.cur)
  d.skip()
  d.showNode(d.cur)

  d.remove()
  d.showNode(d.cur)
  d.showList()
  

}

def test3 () = {
  var size : Int = 3005290
  size = 5

  val d : Circ = new Circ()
  d.makeCircularList(size)
  println(s"constructed circular list of size ${d.size}")
  d.traverse(size/2)

 // 3 5 1 4  leaving 2 winner
  d.showNode(d.cur)
  d.showList()

  println("should remove player 3")
  d.remove()   
  d.showList()

  println("should remove player 5")
  d.skip()     
  d.showList()
  d.remove()   
  d.showList()

  println("should remove player 1")
  d.remove()   
  d.showList()
  d.skip()    
  d.showList()
  println("should remove player 4")
  d.remove()   
  d.showList()
 

}

def test5 () = {
  var size : Int = 5

  val d : Circ = new Circ()
  d.makeCircularList(size)
  println(s"constructed circular list of size ${d.size}")
  d.traverse(size/2)

  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.showList()


// 
// size is = 1
//  (613805,613806,1)  ..... PLAYER 613806 is the winner ... but alas REJECTED answer...
//  (613805,613806,1) 
// all done!
// [success] Total time: 0 s, completed 21 Jul 2024, 05:28:33

}

def test6 () = {
  var size : Int = 5

  val d : Circ = new Circ()
  d.makeCircularList(size)
  println(s"constructed circular list of size ${d.size}")
  d.traverse(size/2)

  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.skip()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.skip()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.skip()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.skip()
  d.showList()

  d.showNode(d.cur)
  d.remove()
  d.skip()
  d.showList()


// 
// size is = 1
//  (613805,613806,1)  ..... PLAYER 613806 is the winner ... but alas REJECTED answer...
//  (613805,613806,1) 
// all done!
// [success] Total time: 0 s, completed 21 Jul 2024, 05:28:33

}


def solution () = {
  var size : Int = 3005290
  //size = 5

  val d : Circ = new Circ()
  d.makeCircularList(size)
  println(s"constructed circular list of size ${d.size}")
  d.traverse(size/2)

  d.fish()
  d.showList()


// 
// size is = 1
//  (613805,613806,1)  ..... PLAYER 613806 is the winner ... but alas REJECTED answer...
//  (613805,613806,1) 
// all done!
// [success] Total time: 0 s, completed 21 Jul 2024, 05:28:33

// [info] compiling 1 Scala source to /home/terry/code/advent-of-code/advent-of-code-2016/day19/scala2/target/scala-3.4.2/classes ...
// [info] done compiling
// [info] running main 
// hello world!
// constructed circular list of size 3005290
// traversal done. -> 
// size is = 0
//  (1410968,1410969,1) 
//  (1410968,1410969,1) 
//              ^^^ ------- the winning ELF ... ATTEMPT no 2 ...... REJECTED again ??
// all done!
// [success] Total time: 0 s, completed 21 Jul 2024, 05:38:10


}



@main
def main() = {
  println("hello world!")

  //test5() 
  //test6()

  //test4()
  solution()

  // demo(hd,size)
  // //
  // size = 3005290
  // hd = makeCircularList(size)
  // demo(hd,size)
  //
  // size = 5
  // hd = makeCircularList(size)
  // demo2(hd,size)

  // size = 3005290
  // hd = makeCircularList(size)
  // demo2(hd,size)
  
  println("all done!")

}


