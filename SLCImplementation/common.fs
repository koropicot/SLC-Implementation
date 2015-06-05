module common

//to avoid FunScript bug
type Tuple<'a, 'b> = Tuple of 'a * 'b
type Tuple3<'a, 'b, 'c> = Tuple3 of 'a * 'b * 'c