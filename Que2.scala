object Subtraction extends App
{
    val x= new Rational(3,4)
    val y= new Rational(5,8)
    val z= new Rational(2,7)

    val s1=x.sub(y)
    val s2=s1.sub(z)
    println(x)
    println(y)
    println(z)
    println(s2)

}
class Rational(n:Int,d:Int)
{
    require(d!= 0, "denominator must be non-zero")
    def numer=n/gcd(n,d)
    def denom=d/gcd(n,d)

    private def gcd(a:Int,b:Int):Int= if(b==0) a else gcd(b,a%b)
    def sub(r:Rational)=new Rational(this.numer*r.denom-this.denom*r.numer,this.denom*r.denom)
    override def toString= numer + "/" + denom
}