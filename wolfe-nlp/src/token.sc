def test[T](t:Seq[T]) = t.size
def test(t:Seq[Int]) = t.size
def test(t:Seq[Double]) = t.size
def test(x:Int) = x
test(Array(1,2,3))