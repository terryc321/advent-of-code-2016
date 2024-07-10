

--[[
   this is a lua solution comment
   keyword landmines
    and       break     do        else      elseif
    end       false     for       function  if
    in        local     nil       not       or
    repeat    return    then      true      until
   while

   function !=(x,y)
   return not( x == y)
   end
   
--]]

a = 0
b = 0
c = 0
d = 0

function reset()
   a = 0
   b = 0
   c = 0
   d = 0
end

function v0 ()
   a = 1
   return v1()
end

function v1 ()
   b = 1
   return v2()
end

function v2()
   d = 26
   return v3()
end

function v3()
   if not (c == 0)
   then return v5()
   else return v4()
   end
end
   
function v4()
  return v9()
end

function v5()
   c = 7
   return v6()
end
function v6()
   d = d + 1
   return v7()
end

function v7()
   c = c -1
   return v8()
end

function v8()
   if not(c == 0)
   then return v6()
   else return v9()
   end
end

function v9()
   c = a
   return v10()
end

function v10()
   a = a + 1
   return v11()
end

function v11()
   b = b - 1
   return v12()
end

function v12()
   if not (b == 0)
   then return v10()
   else return v13()
   end
end

function v13()
   b = c
   return v14()
end

function v14()
   d = d - 1
   return v15()
end

function v15()
   if not(d == 0)
   then return v9()
   else return v16()
   end
end

function v16()
   c = 18
   return v17()
end

function v17()
   d = 11
   return v18()
end

function v18()
   a = a + 1
   return v19()
end

function v19()
   d = d - 1
   return v20()
end

function v20()
   if not(d == 0)
   then return v18()
   else return v21()
   end
end

function v21()
   c = c - 1
   return v22()
end

function v22()
   if not(c ==0)
   then return v17()
   else return vDone()
   end   
end

function vDone()
   return true
end


function run()
   reset()
   v0()
   print("\nRun 1 \n")
   print("a = " , a)
   print("b = " , b)
   print("c = " , c)
   print("d = " , d)   
end

function run2()
   reset()
   c = 1
   v0()
   print("\nRun 2 with c = 1 \n")
   print("a = " , a)
   print("b = " , b)
   print("c = " , c)
   print("d = " , d)   
end


run()
run2()

--[[

   in order for tail call recursion to take place
   lua needs explicit return statement


Run 1 

a = 	318009
b = 	196418
c = 	0
d = 	0

Run 2 with c = 1 

a = 	9227663
b = 	5702887
c = 	0
d = 	0

real	0m0.402s
user	0m0.401s
sys	0m0.001s

   v = {}
   v[0] = v0
   v[1] = v1
   v[2] = v2 ... etc..
   
   
--]]

