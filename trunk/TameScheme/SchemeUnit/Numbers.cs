using System;

using Tame.Scheme.Data;
using Tame.Scheme.Data.Number;
using Tame.Scheme.Runtime;

namespace SchemeUnit
{
    [TestFixture("Numbers")]
    public class Numbers
    {
        public Numbers() { }

        Interpreter terp = new Interpreter();

        #region Number type assertions

        #region IsNumber

        // All number types should be complex?

        [Test("IsNumber")]
        public void IsNumberComplex()
        {
            Assert.True((bool)terp.Evaluate("(number? 1.0+2.2i)"));
        }

        [Test("IsNumber")]
        public void IsNumberRationalComplex()
        {
            Assert.True((bool)terp.Evaluate("(number? 1+2i)"));
        }

        [Test("IsNumber")]
        public void IsNumberReal()
        {
            Assert.True((bool)terp.Evaluate("(number? 1.2)"));
        }

        [Test("IsNumber")]
        public void IsNumberRational()
        {
            Assert.True((bool)terp.Evaluate("(number? 1/2)"));
        }

        [Test("IsNumber")]
        public void IsNumberDecimal()
        {
            Assert.True((bool)terp.Evaluate("(number? #e1.2)"));
        }

        [Test("IsNumber")]
        public void IsNumberLong()
        {
            Assert.True((bool)terp.Evaluate("(number? 1)"));
        }

        [Test("IsNumber")]
        public void IsNumberString()
        {
            Assert.False((bool)terp.Evaluate("(number? \"string\")"));
        }

        [Test("IsNumber")]
        public void IsNumberSymbol()
        {
            Assert.False((bool)terp.Evaluate("(number? 'asymbol)"));
        }

        [Test("IsNumber")]
        public void IsNumberBoolean()
        {
            Assert.False((bool)terp.Evaluate("(number? #f)"));
        }

        [Test("IsNumber")]
        public void IsNumberPair()
        {
            Assert.False((bool)terp.Evaluate("(number? '(1 2 3))"));
        }

        #endregion

        #region IsComplex

        // All number types should be complex?

        [Test("IsComplex")]
        public void IsComplexComplex()
        {
            Assert.True((bool)terp.Evaluate("(complex? 1.0+2.2i)"));
        }

        [Test("IsComplex")]
        public void IsComplexRationalComplex()
        {
            Assert.True((bool)terp.Evaluate("(complex? 1+2i)"));
        }

        [Test("IsComplex")]
        public void IsComplexReal()
        {
            Assert.True((bool)terp.Evaluate("(complex? 1.2)"));
        }

        [Test("IsComplex")]
        public void IsComplexRational()
        {
            Assert.True((bool)terp.Evaluate("(complex? 1/2)"));
        }

        [Test("IsComplex")]
        public void IsComplexDecimal()
        {
            Assert.True((bool)terp.Evaluate("(complex? #e1.2)"));
        }

        [Test("IsComplex")]
        public void IsComplexLong()
        {
            Assert.True((bool)terp.Evaluate("(complex? 1)"));
        }

        [Test("IsComplex")]
        public void IsComplexString()
        {
            Assert.False((bool)terp.Evaluate("(complex? \"string\")"));
        }

        [Test("IsComplex")]
        public void IsComplexSymbol()
        {
            Assert.False((bool)terp.Evaluate("(complex? 'asymbol)"));
        }

        [Test("IsComplex")]
        public void IsComplexBoolean()
        {
            Assert.False((bool)terp.Evaluate("(complex? #f)"));
        }

        [Test("IsComplex")]
        public void IsComplexPair()
        {
            Assert.False((bool)terp.Evaluate("(complex? '(1 2 3))"));
        }

        #endregion

        #region IsReal

        // Complex numbers are not real?

        [Test("IsReal")]
        public void IsRealComplex()
        {
            Assert.False((bool)terp.Evaluate("(real? 1.0+2.2i)"));
        }

        [Test("IsReal")]
        public void IsRealRationalComplex()
        {
            Assert.False((bool)terp.Evaluate("(real? 1+2i)"));
        }

        [Test("IsReal")]
        public void IsRealReal()
        {
            Assert.True((bool)terp.Evaluate("(real? 1.2)"));
        }

        [Test("IsReal")]
        public void IsRealRational()
        {
            Assert.True((bool)terp.Evaluate("(real? 1/2)"));
        }

        [Test("IsReal")]
        public void IsRealDecimal()
        {
            Assert.True((bool)terp.Evaluate("(real? #e1.2)"));
        }

        [Test("IsReal")]
        public void IsRealLong()
        {
            Assert.True((bool)terp.Evaluate("(real? 1)"));
        }

        [Test("IsReal")]
        public void IsRealString()
        {
            Assert.False((bool)terp.Evaluate("(real? \"string\")"));
        }

        [Test("IsReal")]
        public void IsRealSymbol()
        {
            Assert.False((bool)terp.Evaluate("(real? 'asymbol)"));
        }

        [Test("IsReal")]
        public void IsRealBoolean()
        {
            Assert.False((bool)terp.Evaluate("(real? #f)"));
        }

        [Test("IsReal")]
        public void IsRealPair()
        {
            Assert.False((bool)terp.Evaluate("(real? '(1 2 3))"));
        }

        #endregion

        #region IsRational

        // rational? should have the same behaviour as real?

        [Test("IsRational")]
        public void IsRationalComplex()
        {
            Assert.False((bool)terp.Evaluate("(rational? 1.0+2.2i)"));
        }

        [Test("IsRational")]
        public void IsRationalRationalComplex()
        {
            Assert.False((bool)terp.Evaluate("(rational? 1+2i)"));
        }

        [Test("IsRational")]
        public void IsRationalComplex2()
        {
            Assert.True((bool)terp.Evaluate("(rational? 1.0+0i)"));
        }

        [Test("IsRational")]
        public void IsRationalRationalComplex2()
        {
            Assert.True((bool)terp.Evaluate("(rational? 1+0i)"));
        }

        [Test("IsRational")]
        public void IsRationalReal()
        {
            Assert.True((bool)terp.Evaluate("(rational? 1.2)"));
        }

        [Test("IsRational")]
        public void IsRationalRational()
        {
            Assert.True((bool)terp.Evaluate("(rational? 1/2)"));
        }

        [Test("IsRational")]
        public void IsRationalDecimal()
        {
            Assert.True((bool)terp.Evaluate("(rational? #e1.2)"));
        }

        [Test("IsRational")]
        public void IsRationalLong()
        {
            Assert.True((bool)terp.Evaluate("(rational? 1)"));
        }

        [Test("IsRational")]
        public void IsRationalString()
        {
            Assert.False((bool)terp.Evaluate("(rational? \"string\")"));
        }

        [Test("IsRational")]
        public void IsRationalSymbol()
        {
            Assert.False((bool)terp.Evaluate("(rational? 'asymbol)"));
        }

        [Test("IsRational")]
        public void IsRationalBoolean()
        {
            Assert.False((bool)terp.Evaluate("(rational? #f)"));
        }

        [Test("IsRational")]
        public void IsRationalPair()
        {
            Assert.False((bool)terp.Evaluate("(rational? '(1 2 3))"));
        }

        #endregion

        #region IsInteger

        // All number types should be integer?

        [Test("IsInteger")]
        public void IsIntegerComplex()
        {
            Assert.False((bool)terp.Evaluate("(integer? 1.0+2.2i)"));
        }

        [Test("IsInteger")]
        public void IsIntegerComplex2()
        {
            Assert.True((bool)terp.Evaluate("(integer? 1.0+0i)"));
        }

        [Test("IsInteger")]
        public void IsIntegerRationalComplex()
        {
            Assert.False((bool)terp.Evaluate("(integer? 1+2i)"));
        }

        [Test("IsInteger")]
        public void IsIntegerRationalComplex2()
        {
            Assert.True((bool)terp.Evaluate("(integer? 1+0i)"));
        }

        [Test("IsInteger")]
        public void IsIntegerReal()
        {
            Assert.False((bool)terp.Evaluate("(integer? 1.2)"));
        }

        [Test("IsInteger")]
        public void IsIntegerReal2()
        {
            Assert.True((bool)terp.Evaluate("(integer? 1.0)"));
        }

        [Test("IsInteger")]
        public void IsIntegerRational()
        {
            Assert.False((bool)terp.Evaluate("(integer? 1/2)"));
        }

        [Test("IsInteger")]
        public void IsIntegerRational2()
        {
            Assert.True((bool)terp.Evaluate("(integer? 2/1)"));
        }

        [Test("IsInteger")]
        public void IsIntegerDecimal()
        {
            Assert.False((bool)terp.Evaluate("(integer? #e1.2)"));
        }

        [Test("IsInteger")]
        public void IsIntegerDecimal2()
        {
            Assert.True((bool)terp.Evaluate("(integer? #e1.0)"));
        }

        [Test("IsInteger")]
        public void IsIntegerLong()
        {
            Assert.True((bool)terp.Evaluate("(integer? 1)"));
        }

        [Test("IsInteger")]
        public void IsIntegerString()
        {
            Assert.False((bool)terp.Evaluate("(integer? \"string\")"));
        }

        [Test("IsInteger")]
        public void IsIntegerSymbol()
        {
            Assert.False((bool)terp.Evaluate("(integer? 'asymbol)"));
        }

        [Test("IsInteger")]
        public void IsIntegerBoolean()
        {
            Assert.False((bool)terp.Evaluate("(integer? #f)"));
        }

        [Test("IsInteger")]
        public void IsIntegerPair()
        {
            Assert.False((bool)terp.Evaluate("(integer? '(1 2 3))"));
        }

        #endregion

        #endregion

        #region Exactness

        [Test("IsExact")]
        public void IsExactComplex()
        {
            Assert.False((bool)terp.Evaluate("(exact? 1.0+2.0i)"));
            Assert.True((bool)terp.Evaluate("(inexact? 1.0+2.0i)"));
        }

        [Test("IsExact")]
        public void IsExactRationalComplex()
        {
            Assert.True((bool)terp.Evaluate("(exact? 1/2+2i)"));
            Assert.False((bool)terp.Evaluate("(inexact? 1/2+2i)"));
        }

        [Test("IsExact")]
        public void IsExactReal()
        {
            Assert.False((bool)terp.Evaluate("(exact? 1.0)"));
            Assert.True((bool)terp.Evaluate("(inexact? 1.0)"));
        }

        [Test("IsExact")]
        public void IsExactDecimal()
        {
            Assert.True((bool)terp.Evaluate("(exact? #e1.1)"));
            Assert.False((bool)terp.Evaluate("(inexact? #e1.1)"));
        }

        [Test("IsExact")]
        public void IsExactRational()
        {
            Assert.True((bool)terp.Evaluate("(exact? 1/2)"));
            Assert.False((bool)terp.Evaluate("(inexact? 1/2)"));
        }

        [Test("IsExact")]
        public void IsExactLong()
        {
            Assert.True((bool)terp.Evaluate("(exact? 1)"));
            Assert.False ((bool)terp.Evaluate("(inexact? 1)"));
        }

        #endregion
    }
}
