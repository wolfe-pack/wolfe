

type T[S] = Array[Seq[S]]


trait Test{

  def changeMeAndReturn:this.type
}

class Blah extends Test {
  def changeMeAndReturn = this
}

val blah = new Blah
blah.changeMeAndReturn.changeMeAndReturn.changeMeAndReturn