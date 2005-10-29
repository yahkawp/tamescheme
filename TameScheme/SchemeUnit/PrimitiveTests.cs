using System;

using csUnit;

using Tame.Scheme.Data;
using Tame.Scheme.Runtime;
using Tame.Scheme.Runtime.Parse;
using Tame.Scheme.Syntax;

namespace SchemeUnit
{
	/// <summary>
	/// Tests out various scheme primitives
	/// </summary>
	[TestFixture("Syntax")]
	public class PrimitiveTests
	{
		public PrimitiveTests()
		{
		}

		Interpreter terp = new Interpreter();

		[Test("if")]
		public void BasicIf()
		{
			Assert.Equals(1, terp.Evaluate("(if #t 1 2)"));
			Assert.Equals(2, terp.Evaluate("(if #f 1 2)"));
			Assert.Equals(Unspecified.Value, terp.Evaluate("(if #f 1)"));
		}

		[Test("if")]
		public void ComplexIf()
		{
			terp.TopLevelEnvironment["x"] = 4;
			terp.TopLevelEnvironment["y"] = 0;

			Assert.Equals(1, terp.Evaluate("(let ((y x)) (if (> y 3) 1 (if (< y 3) 2 3)))"));
			Assert.Equals(0, terp.TopLevelEnvironment["y"]);
		}

		[Test("let")]
		public void BasicLet()
		{
			Assert.Equals(1, terp.Evaluate("(let ((x 1) (y 2)) x)"));
		}

		[Test("let")]
		public void LetOrdering()
		{
			terp.TopLevelEnvironment["x"] = 3;
			terp.TopLevelEnvironment["y"] = 4;

			Assert.Equals(3, terp.Evaluate("(let ((x y) (y x)) y)"));
			Assert.Equals(4, terp.Evaluate("(let ((x y) (y x)) x)"));
		}

		[Test("let")]
		public void LetStar()
		{
			terp.TopLevelEnvironment["x"] = 3;
			terp.TopLevelEnvironment["y"] = 4;

			Assert.Equals(4, terp.Evaluate("(let* ((x y) (y x)) x)"));
			Assert.Equals(4, terp.Evaluate("(let* ((x y) (y x)) y)"));
		}

		[Test("let")]
		public void LetRec()
		{
			terp.TopLevelEnvironment["x"] = 3;
			terp.TopLevelEnvironment["y"] = 4;

			Assert.Equals(Unspecified.Value, terp.Evaluate("(letrec ((x y) (y x)) y)"));
			Assert.Equals(Unspecified.Value, terp.Evaluate("(letrec ((x y) (y x)) x)"));
		}

		[Test("lambda")]
		public void Lambda()
		{
			Assert.Equals(4, terp.Evaluate("((lambda (x) x) 4)"));
		}

		[Test("lambda")]
		public void LambdaComplex()
		{
			Assert.Equals(new Symbol("lambda-complex"), terp.Evaluate("(define lambda-complex (lambda (x) (if (> x 3) 1 (if (< x 3) 2 3))))"));
			Assert.Equals(2, terp.Evaluate("(lambda-complex 1)"));
			Assert.Equals(1, terp.Evaluate("(lambda-complex 4)"));
			Assert.Equals(3, terp.Evaluate("(lambda-complex 3)"));
		}

		[Test("lambda")]
		public void LambdaLet()
		{
			Assert.Equals(4, terp.Evaluate("((lambda (x) (let ((y x)) y)) 4)"));
		}

		[Test("lambda")]
		public void LambdaLetExternal()
		{
			terp.TopLevelEnvironment["x"] = 3;

			Assert.Equals(new Symbol("lambda-let-external"), terp.Evaluate("(define lambda-let-external (let ((x 4)) (lambda (y) (+ x y))))"));
			Assert.Equals(8, terp.Evaluate("(lambda-let-external 4)"));
		}

		[Test("lambda")]
		public void LambdaHigherOrder()
		{
			Assert.Equals(new Symbol("lambda-higher"), terp.Evaluate("(define lambda-higher (lambda (x) (lambda (y) (+ x y))))"));
			Assert.Equals(5, terp.Evaluate("((lambda-higher 2) 3)"));

			Assert.Equals(new Symbol("lambda-higher-2"), terp.Evaluate("(define lambda-higher-2 (lambda-higher 2))"));
			Assert.Equals(new Symbol("lambda-higher-5"), terp.Evaluate("(define lambda-higher-5 (lambda-higher 5))"));

			Assert.Equals(4, terp.Evaluate("(lambda-higher-2 2)"));
			Assert.Equals(7, terp.Evaluate("(lambda-higher-5 2)"));
		}

		[Test("lambda")]
		public void LambdaImproper()
		{
			Assert.Equals(terp.ParseScheme("(1 2 3)"), terp.Evaluate("((lambda y y) 1 2 3)"));
			Assert.Equals(terp.ParseScheme("(3 4 5)"), terp.Evaluate("((lambda (x y . z) z) 1 2 3 4 5)"));
		}

		[Test("define")]
		public void BasicDefine()
		{
			Assert.Equals(new Symbol("x"), terp.Evaluate("(define x 1)"));
			Assert.Equals(1, terp.Evaluate("x"));
			Assert.Equals(1, terp.TopLevelEnvironment["x"]);
		}

		[Test("define")]
		public void BlockLetDefine()
		{
			terp.TopLevelEnvironment["x"] = 123;
			terp.TopLevelEnvironment["y"] = 123;

			Assert.Equals(15, terp.Evaluate("(let ((x 5)) (define y (+ 5 x)) (+ y x))"));

			Assert.Equals(123, terp.TopLevelEnvironment["x"]);
			Assert.Equals(123, terp.TopLevelEnvironment["y"]);
		}

		[Test("define")]
		public void BlockLambdaDefine()
		{
			terp.TopLevelEnvironment["x"] = 123;
			terp.TopLevelEnvironment["y"] = 123;

			Assert.Equals(15, terp.Evaluate("((lambda (x) (define y (+ 5 x)) (+ y x)) 5)"));

			Assert.Equals(123, terp.TopLevelEnvironment["x"]);
			Assert.Equals(123, terp.TopLevelEnvironment["y"]);
		}

		[Test("begin")]
		public void Begin()
		{
			Assert.Equals(5, terp.Evaluate("(begin 1 2 3 4 5)"));
		}

		[Test("quote")]
		public void Quote()
		{
			terp.TopLevelEnvironment["x"] = 123;
			terp.TopLevelEnvironment["y"] = 123;

			Assert.Equals(terp.ParseScheme("((x y) x () 4 undefined-symbol #(23 x))"), terp.Evaluate("'((x y) x () 4 undefined-symbol #(23 x))"));
		}
	}
}
