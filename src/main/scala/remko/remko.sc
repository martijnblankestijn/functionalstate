var y = 0

def f(x: Int): Int = {
  y = y + x + x
  y
}

f(f(2))

f(f(2)) == 4 + 4 + 4 != f(4)

def g(x: Int): Int = x + x
g(g(2)) == g(4)


// alternatief
var y1 = 1
def f1(x: Int): Int = {
  y1 = y1 + x
  y1
}
f1(2)
f1(f1(2))

