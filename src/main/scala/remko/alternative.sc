// variable only needed for function with side effects
var y = 1

// suppose I have a function like this (only the signature)
// just a very function with an easy signature that takes an Int and returns an Int
def f(x: Int): Int = {
    // do not show the implementation
     y = y + x
    y
}

// and I invoke it like below and get as a return 3 
f(2)
f(2)

// what do I expect if I invoke it like this
f(f(2))
// yes of course I would expect 4
// and of course reading Uncle Bob's clean code I would shout why not call the function addOne
// but we have our functional mindset now, so we keep the f. 
// And yes the next function we will call g ;-)

// and we would go on
f(f(f(2)))