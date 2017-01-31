
trait Functor[F[_]] {
  def fmap[A, B](fa : F[A])(f : A => B): F[B]
}


/*
translated from Haskell

fix :: ((a -> b) -> a -> b) -> a -> b
fix f = f (fix f)

data Fix f = In { out :: f (Fix f) }

cata :: Functor f => (f a -> a) -> (Fix f -> a) -> Fix f -> a
cata psi f = psi . fmap f . out

cataRec :: Functor f => (f a -> a) -> Fix f -> a
cataRec psi = fix (cata psi)

 */

case class Fix[F[_]](out: F[Fix[F]])

def fix[A, B](f : (A => B) => (A => B)): (A => B) = f(fix(f))


def cata[A, F[_]](psi: F[A] => A)(f: Fix[F] => A)(t: Fix[F])(implicit fc: Functor[F]): A = {
  psi(fc.fmap(t.out)(f))
}

def cataRec[A, F[_]](psi: F[A] => A)(t: Fix[F])(implicit fc: Functor[F]): A = {
  fix( cata(psi))(t)
}



type Exp = Fix[ExpF]


sealed trait ExpF[+A]
case class Var[A](name: String) extends ExpF[A]
case class App[A](fun: A, arg: A) extends ExpF[A]
case class Lam[A](arg: String, body: A) extends ExpF[A]
case class Lit[A](value: Int) extends ExpF[A]
case class Let[A](v: String, exp: A, body: A) extends ExpF[A]


implicit val ExprFFunctor = new Functor[ExpF] {  //an endofunctor F
  override def fmap[A, B](fa: ExpF[A])(f: (A) => B): ExpF[B] = fa match {
    case Lam(arg, body) => Lam(arg, f(body))
    case App(fun, arg) => App(f(fun), f(arg))
    case Let(v, e, b) => Let(v, f(e),f(b))
    case Lit(v) => Lit(v)
    case Var(n) => Var(n)

  }
}

def vAr(n: String): Exp = Fix[ExpF](Var(n))
def let(v: String, e: Exp, b: Exp): Exp = Fix[ExpF](Let(v, e, b))
def lit(v: Int): Exp = Fix[ExpF](Lit(v))
def app(f: Exp, a: Exp): Exp = Fix[ExpF](App(f, a))
def lam(a: String, b: Exp): Exp = Fix[ExpF](Lam(a, b))


val sample = lam("x", let("y", lit(42), app( app(vAr("add"), vAr("x")), vAr("y"))))

val converted = ???

/*
Exercise:
Convert sample Let into Lambdas (let x = v in b <==> (x -> b) v)
using catamorphism (cataRec)
*/

