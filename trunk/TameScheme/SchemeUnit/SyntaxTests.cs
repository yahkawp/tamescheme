using System;

using csUnit;

using Tame.Scheme.Data;
using Tame.Scheme.Runtime;
using Tame.Scheme.Runtime.Parse;
using Tame.Scheme.Syntax;

namespace SchemeUnit
{
	/// <summary>
	/// A series of tests to check syntax matching
	/// </summary>
	[TestFixture]
	public class SyntaxTests
	{
		public SyntaxTests()
		{ }

		private Interpreter terp = new Interpreter();

		// These tests test the define-syntax operation, so are dependent on much of the rest of the system working

		[Test]
		public void BasicSyntax()
		{
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(define-syntax basic-syntax (syntax-rules () ((basic-syntax) 1)))")), new Symbol("basic-syntax"));
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(basic-syntax)")), 1);
		}

		[Test]
		public void BasicLiteral()
		{
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(define-syntax basic-literal (syntax-rules (lit) ((basic-literal lit x lit) x)))")), new Symbol("basic-syntax"));
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(basic-literal lit 5 lit)")), 5);
		}

		[Test]
		public void BasicEllipsises()
		{
			terp.Evaluate(terp.ParseScheme("(define-syntax basic-ellipsises (syntax-rules () ((basic-ellipsises a ...) (+ a ...))))"));
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(basic-ellipsises 1 2 3 4)")), 1+2+3+4);
		}

		[Test]
		public void ManyEllipsises()
		{
			terp.Evaluate(terp.ParseScheme("(define-syntax many-ellipsises (syntax-rules () ((many-ellipsises ((a ...) b) ...) '(((a ...) b) ...))))"));
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(many-ellipsises ((1 2 3) 4) (() 5) ((4 5) 6) (() 8))")), terp.ParseScheme("(((1 2 3) 4) (() 5) ((4 5) 6) (() 8))"));
		}

		[Test]
		public void BasicImproper()
		{
			// (You'll probably note that some scheme interpreters can't handle improper syntax like this
			terp.Evaluate(terp.ParseScheme("(define-syntax basic-improper (syntax-rules () ((basic-improper . x) x)))"));
			Assert.Equals(terp.Evaluate(terp.ParseScheme("(basic-improper + 1 2)")), 1+2);
		}

		[Test]
		public void TempBindingLet()
		{
			terp.Evaluate("(define-syntax temp-binding (syntax-rules () ((temp-binding a b) (let ((x a) (y b)) (+ y a)))))");
			terp.Evaluate("(define x 1)");
			terp.Evaluate("(define y 2)");

			// Bit daft, but if temp binding fails we get (let ((x y) (y x)) (+ y y)), which is 4, and if it succeeds, we should get ... (+ temp y) ..., ie 3
			Assert.Equals(terp.Evaluate("(temp-binding y x)"), 3);
		}

		[Test]
		public void TempBindingLetRec()
		{
			terp.Evaluate("(define-syntax temp-binding (syntax-rules () ((temp-binding a b) (letrec ((x a) (y b)) (+ y a)))))");
			terp.Evaluate("(define x 1)");
			terp.Evaluate("(define y 2)");

			Assert.Equals(terp.Evaluate("(temp-binding y x)"), 3);
		}

		[Test]
		public void TempBindingLetStar()
		{
			terp.Evaluate("(define-syntax temp-binding (syntax-rules () ((temp-binding a b) (let* ((x a) (y b)) (+ y a)))))");
			terp.Evaluate("(define x 1)");
			terp.Evaluate("(define y 2)");

			Assert.Equals(terp.Evaluate("(temp-binding y x)"), 3);
		}

		// The R5RS macro definition for cond
		static string condDefinition = "(define-syntax cond (syntax-rules (else =>) ((cond (else result1 result2 ...)) (begin result1 result2 ...)) ((cond (test => result)) (let ((temp test)) (if temp (result temp)))) ((cond (test => result) clause1 clause2 ...) (let ((temp test)) (if temp (result temp) (cond clause1 clause2 ...)))) ((cond (test)) test) ((cond (test) clause1 clause2 ...) (let ((temp test)) (if temp temp (cond clause1 clause2 ...)))) ((cond (test result1 result2 ...)) (if test (begin result1 result2 ...))) ((cond (test result1 result2 ...) clause1 clause2 ...) (if test (begin result1 result2 ...) (cond clause1 clause2 ...)))))";

		[Test]
		public void SchemeCond()
		{
			// More involved: check out some simple cont statements
			terp.Evaluate(condDefinition);

			// Checks if x is less than 3 (1), greater than 3 (2) or equal to 3 (3)
			string simpleTest = "(cond ((> 3 x) 1) ((< 3 x) 2) (else 3))";

			terp.TopLevelEnvironment["x"] = 2;
			Assert.Equals(terp.Evaluate(simpleTest), 1);
			terp.TopLevelEnvironment["x"] = 4;
			Assert.Equals(terp.Evaluate(simpleTest), 2);
			terp.TopLevelEnvironment["x"] = 3;
			Assert.Equals(terp.Evaluate(simpleTest), 3);
		}

		[Test]
		public void SchemeCondCapture()
		{
			// More involved: check out some simple cont statements
			terp.Evaluate(condDefinition);

			// Checks for correct syntax matches when => is captured
			Assert.Equals(terp.Evaluate("(let ((=> #f)) (cond (#t => 'ok)))"), new Symbol("ok"));
		}
	}
}
