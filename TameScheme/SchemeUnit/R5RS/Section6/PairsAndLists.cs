using System;
using csUnit;

namespace SchemeUnit.R5RS.Section6
{
	/// <summary>
	/// Tests from section 6.3.2 ('Pairs and Lists') of R5RS
	/// </summary>
	[TestFixture("R5RS section 6.3")]
	public class PairsAndLists : R5RS
	{
		public PairsAndLists()
		{
		}

		#region pair?

		[Test("pair?")]
		public void Pair1()
		{
			Assert.True((bool)Evaluate("(pair? '(a . b))"));
		}

		[Test("pair?")]
		public void Pair2()
		{
			Assert.True((bool)Evaluate("(pair? '(a b c))"));
		}

		[Test("pair?")]
		public void Pair3()
		{
			Assert.False((bool)Evaluate("(pair? '())"));
		}

		[Test("pair?")]
		public void Pair4()
		{
			Assert.False((bool)Evaluate("(pair? '#(a b))"));
		}
		#endregion

		#region cons

		[Test("cons")]
		public void Cons1()
		{
			Assert.Equals(Parse("(a)"), Evaluate("(cons 'a '())"));
		}

		[Test("cons")]
		public void Cons2()
		{
			Assert.Equals(Parse("((a) b c d)"), Evaluate("(cons '(a) '(b c d))"));
		}

		[Test("cons")]
		public void Cons3()
		{
			Assert.Equals(Parse("(\"a\" b c)"), Evaluate("(cons \"a\" '(b c))"));
		}

		[Test("cons")]
		public void Cons4()
		{
			Assert.Equals(Parse("(a . 3)"), Evaluate("(cons 'a 3)"));
		}

		[Test("cons")]
		public void Cons5()
		{
			Assert.Equals(Parse("((a b) . c)"), Evaluate("(cons '(a b) 'c)"));
		}

		#endregion

		#region car

		[Test("car")]
		public void Car1()
		{
			Assert.Equals(Parse("a"), Evaluate("(car '(a b c))"));
		}

		[Test("car")]
		public void Car2()
		{
			Assert.Equals(Parse("(a)"), Evaluate("(car '((a) b c d))"));
		}

		[Test("car")]
		public void Car3()
		{
			Assert.Equals(Parse("1"), Evaluate("(car '(1 . 2))"));
		}

		[Test("car"), ExpectedException(typeof(Tame.Scheme.Exception.RuntimeException))]
		public void Car4()
		{
			Evaluate("(car '())");
		}

		#endregion

		#region cdr

		[Test("cdr")]
		public void Cdr1()
		{
			Assert.Equals(Parse("(b c d)"), Evaluate("(cdr '((a) b c d))"));
		}

		[Test("cdr")]
		public void Cdr2()
		{
			Assert.Equals(2, Evaluate("(cdr '(1 . 2))"));
		}

		[Test("cdr"), ExpectedException(typeof(Tame.Scheme.Exception.RuntimeException))]
		public void Cdr3()
		{
			Evaluate("(cdr '())");
		}

		#endregion

		#region set-car!

		[Test("set-car!")]
		public void SetCar1()
		{
			Evaluate("(define (f) (list 'not-a-constant-list))");
			Evaluate("(set-car! (f) 3)");
		}

		[Test("set-car!"), ExpectedException(typeof(Tame.Scheme.Exception.RuntimeException))]
		public void SetCar2()
		{
			Evaluate("(define (g) '(constant-list))");
			Evaluate("(set-car! (g) 3))");
		}

		#endregion

		#region list?

		[Test("list?")]
		public void IsList1()
		{
			Assert.True((bool)Evaluate("(list? '(a b c))"));
		}

		[Test("list?")]
		public void IsList2()
		{
			Assert.True((bool)Evaluate("(list? '())"));
		}

		[Test("list?")]
		public void IsList3()
		{
			Assert.False((bool)Evaluate("(list? '(a . b))"));
		}

		[Test("list?")]
		public void IsList4()
		{
			Assert.False((bool)Evaluate("(let ((x (list 'a))) (set-cdr! x x) (list? x))"));
		}

		#endregion

		#region list

		[Test("list")]
		public void List1()
		{
			Assert.Equals(Parse("(a 7 c)"), Evaluate("(list 'a (+ 3 4) 'c)"));
		}

		[Test("list")]
		public void List2()
		{
			Assert.Null(Evaluate("(list)"));
		}

		#endregion

		#region length

		[Test("length")]
		public void Length1()
		{
			Assert.Equals(3, Evaluate("(length '(a b c))"));
		}

		[Test("length")]
		public void Length2()
		{
			Assert.Equals(3, Evaluate("(length '(a (b) (c d e)))"));
		}

		[Test("length")]
		public void Length3()
		{
			Assert.Equals(0, Evaluate("(length '())"));
		}

		#endregion

		#region append

		[Test("append")]
		public void Append1()
		{
			Assert.Equals(Parse("(x y)"), Evaluate("(append '(x) '(y))"));
		}

		[Test("append")]
		public void Append2()
		{
			Assert.Equals(Parse("(a b c d)"), Evaluate("(append '(a) '(b c d))"));
		}

		[Test("append")]
		public void Append3()
		{
			Assert.Equals(Parse("(a (b) (c))"), Evaluate("(append '(a (b)) '((c)))"));
		}

		[Test("append")]
		public void Append4()
		{
			Assert.Equals(Parse("(a b c . d)"), Evaluate("(append '(a b) '(c . d))"));
		}

		[Test("append")]
		public void Append5()
		{
			Assert.Equals(Parse("a"), Evaluate("(append '() 'a)"));
		}

		#endregion

		#region reverse

		[Test("reverse")]
		public void Reverse1()
		{
			Assert.Equals(Parse("(c b a)"), Evaluate("(reverse '(a b c))"));
		}

		[Test("reverse")]
		public void Reverse2()
		{
			Assert.Equals(Parse("((e (f)) d (b c) a)"), Evaluate("(reverse '(a (b c) d (e (f))))"));
		}

		#endregion

		#region list-ref

		[Test("list-ref")]
		public void ListRef1()
		{
			Assert.Equals(Parse("c"), Evaluate("(list-ref '(a b c d) 2)"));
		}

		#endregion

		#region member

		[Test("memq")]
		public void Member1()
		{
			Assert.Equals(Parse("(a b c)"), Evaluate("(memq 'a '(a b c))"));
		}

		[Test("memq")]
		public void Member2()
		{
			Assert.Equals(Parse("(b c)"), Evaluate("(memq 'b '(a b c))"));
		}

		[Test("memq")]
		public void Member3()
		{
			Assert.Equals(false, Evaluate("(memq 'a '(b c d))"));
		}

		[Test("memq")]
		public void Member4()
		{
			Assert.Equals(false, Evaluate("(memq (list 'a) '(b (a) c))"));
		}

		[Test("member")]
		public void Member5()
		{
			Assert.Equals(Parse("((a) c)"), Evaluate("(member (list 'a) '(b (a) c))"));
		}

		[Test("memq")]
		public void Member6()
		{
			Evaluate("(memq 101 '(100 101 102))");			// Unspecified (but should succeed)
		}

		[Test("memv")]
		public void Member7()
		{
			Assert.Equals(Parse("(101 102)"), Evaluate("(memv 101 '(100 101 102))"));
		}

		#endregion

		#region assoc

		[Test("assq")]
		public void Assoc1()
		{
			Evaluate("(define e '((a 1) (b 2) (c 3)))");
			Assert.Equals(Parse("(a 1)"), Evaluate("(assq 'a e)"));
		}

		[Test("assq")]
		public void Assoc2()
		{
			Evaluate("(define e '((a 1) (b 2) (c 3)))");
			Assert.Equals(Parse("(b 2)"), Evaluate("(assq 'b e)"));
		}

		[Test("assq")]
		public void Assoc3()
		{
			Evaluate("(define e '((a 1) (b 2) (c 3)))");
			Assert.Equals(false, Evaluate("(assq 'd e)"));
		}

		[Test("assq")]
		public void Assoc4()
		{
			Assert.Equals(false, Evaluate("(assq (list 'a) '(((a)) ((b)) ((c))))"));
		}

		[Test("assoc")]
		public void Assoc5()
		{
			Assert.Equals(Parse("((a))"), Evaluate("(assoc (list 'a) '(((a)) ((b)) ((c))))"));
		}

		[Test("assq")]
		public void Assoc6()
		{
			Evaluate("(assq 5 '((2 3) (5 7) (11 13)))");					// Unspecified
		}

		[Test("assv")]
		public void Assoc7()
		{
			Assert.Equals(Parse("(5 7)"), Evaluate("(assv 5 '((2 3) (5 7) (11 13)))"));
		}

		#endregion
	}
}
