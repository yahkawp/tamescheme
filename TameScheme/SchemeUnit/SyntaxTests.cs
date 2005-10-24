using System;

using csUnit;

using Tame.Scheme.Data;
using Tame.Scheme.Runtime;
using Tame.Scheme.Runtime.Parse;
using Tame.Scheme.Syntax;

#if false
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

		static string testPattern = "((a b ...) ...)";
		static string testMatch = "((1 2 3) (4 5) (6) (7) (8 9 10))";

		public object ParseSomeScheme(string scheme)
		{
			Assert.NotNull(scheme);

			TokenReader schemeReader = new TokenReader(new System.IO.StringReader(scheme));
			Parser schemeParser = new Parser();

			return schemeParser.Parse(schemeReader);
		}

		[Test]
		public void EllipsisDepth()
		{
			// Some simple tests of the ellipsisDepth object
			SyntaxEnvironment.EllipsisDepth depth1 = new SyntaxEnvironment.EllipsisDepth();
			SyntaxEnvironment.EllipsisDepth depth2 = new SyntaxEnvironment.EllipsisDepth();

			depth1.StartToRepeat();
			depth2.StartToRepeat();
			depth1.NextElement();
			depth2.NextElement();
			depth1.StartToRepeat();
			depth2.StartToRepeat();
			depth1.NextElement();
			depth2.NextElement();
			depth1.NextElement();
			depth2.NextElement();

			Assert.Equals(2, depth1.Depth);
			Assert.Equals(2, depth2.Depth);

			// Check that equal depths are indeed equal
			Assert.Equals(depth1.GetHashCode(), depth2.GetHashCode(), "EllipsisDepth hash codes for equal objects do not match");
			Assert.True(depth1.Equals(depth1), "depth1 must equal itself");
			Assert.True(depth2.Equals(depth2), "depth2 must equal itself");
			Assert.True(depth1.Equals(depth2), "depth1 must equal depth2");
			Assert.True(depth2.Equals(depth1), "depth2 must equal depth1");

			// Check that cloning works
			depth2 = new SyntaxEnvironment.EllipsisDepth(depth1);
			Assert.Equals(2, depth2.Depth);
			Assert.Equals(depth1.GetHashCode(), depth2.GetHashCode(), "EllipsisDepth hash codes for cloned objects do not match");
			Assert.True(depth1.Equals(depth1), "depth1 must equal itself");
			Assert.True(depth2.Equals(depth2), "depth2 must equal itself");
			Assert.True(depth1.Equals(depth2), "depth1 must equal depth2 (cloned)");
			Assert.True(depth2.Equals(depth1), "depth2 (cloned) must equal depth1");

			// Check that diferent depths have different values
			depth2.StartToRepeat();
			depth1.FinishRepeating();

			Assert.Equals(1, depth1.Depth);
			Assert.Equals(3, depth2.Depth);

			// Assert.NotEquals(depth1.GetHashCode(), depth2.GetHashCode(), "Non-equal Ellipsis depths have the same hash code");
			Assert.False(depth1.Equals(depth2), "depth1 must not equal depth2 when different");
			Assert.False(depth2.Equals(depth1), "depth2 must not equal depth1 when different");
		}

		[Test]
		public void SimpleMatching()
		{
			// Build the test pattern and the test list to match against
			object testPatternParsed = ParseSomeScheme(testPattern);
			object testMatchParsed = ParseSomeScheme(testMatch);

			Assert.True(testPatternParsed is Pair, "Parser failed: Failed to produce a pair from the test pattern");
			Assert.True(testMatchParsed is Pair, "Parser failed: Failed to produce a pair from the scheme to match against");

			// Build a syntax matcher from the result
			SyntaxElement simpleMatch = SyntaxElement.MakeElementFromScheme(testPatternParsed, new System.Collections.Hashtable());
			Assert.NotNull(simpleMatch, "Failed to generate a SyntaxElement from the test Pattern");

			// Attempt to match against the syntax
			SyntaxEnvironment matchEnvironment = new SyntaxEnvironment();
			Assert.True(simpleMatch.Match(testMatchParsed, out matchEnvironment), "Failed to match the test scheme against the test pattern");
			Assert.NotNull(matchEnvironment, "Failed to produce a syntax environment from the test scheme");

			// a must have the values 1 4 6 7 8
			System.Collections.IList aValues = matchEnvironment["a"];
			Assert.NotNull(aValues, "Failed to find any values for the pattern variable 'a'");
			Assert.True(aValues.Count == 5, "The symbol 'a' should have matched exactly 5 other symbols");
			Assert.True(aValues[0] is long, "aValues[0] must be an integer (is a " + aValues[0].GetType() + ")");
			Assert.True((long)aValues[0] == 1, "aValues[0] != 1");
			Assert.True((long)aValues[1] == 4, "aValues[1] != 4");
			Assert.True((long)aValues[2] == 6, "aValues[2] != 6");
			Assert.True((long)aValues[3] == 7, "aValues[3] != 7");
			Assert.True((long)aValues[4] == 8, "aValues[4] != 8");
		}

		[Test]
		public void EllipsisDepthSyntax()
		{
			// Build the test pattern and the test list to match against
			object testPatternParsed = ParseSomeScheme(testPattern);
			object testMatchParsed = ParseSomeScheme(testMatch);

			Assert.True(testPatternParsed is Pair, "Parser failed: Failed to produce a pair from the test pattern");
			Assert.True(testMatchParsed is Pair, "Parser failed: Failed to produce a pair from the scheme to match against");

			// Build a syntax matcher from the result
			SyntaxElement simpleMatch = SyntaxElement.MakeElementFromScheme(testPatternParsed, new System.Collections.Hashtable());
			Assert.NotNull(simpleMatch, "Failed to generate a SyntaxElement from the test Pattern");

			// Attempt to match against the syntax
			SyntaxEnvironment matchEnvironment;
			Assert.True(simpleMatch.Match(testMatchParsed, out matchEnvironment), "Failed to match the test scheme against the test pattern");
			Assert.NotNull(matchEnvironment, "Failed to produce a syntax environment from the test scheme");

			// Attempt to retrieve the 2nd value of b in the 5th list
			SyntaxEnvironment.EllipsisDepth eDepth = new SyntaxEnvironment.EllipsisDepth();

			// Try against the very first match of the 'a' symbol
			Assert.True(matchEnvironment.Contains(new Symbol("a").SymbolNumber, eDepth), "Syntax matching failed: the symbol 'a' was never matched");
			Assert.Equals(1, (long)matchEnvironment.GetRepeatedMatch(new Symbol("a").SymbolNumber, eDepth), "Syntax matching failed: the first value for 'a' must be 1");

			// Move to the fifth pattern
			eDepth.StartToRepeat();
			eDepth.NextElement();
			eDepth.NextElement();
			eDepth.NextElement();
			eDepth.NextElement();

			// Move to the second repetition therein
			eDepth.StartToRepeat();
			eDepth.NextElement();

			// Check the value exists
			Assert.True(matchEnvironment.Contains(new Symbol("b").SymbolNumber, eDepth), "Syntax matching failed: The second value of b in the fifth list does not exist");

			// Retrieve the value
			long secondBValueInFifthList = (long)matchEnvironment.GetRepeatedMatch(new Symbol("b").SymbolNumber, eDepth);

			Assert.Equals(10, secondBValueInFifthList, "Syntax matching failed: The second value of b in the fifth element should be 10");
		}

		[Test]
		public void FindBinding()
		{
			// Build the test pattern and the test list to match against
			object testPatternParsed = ParseSomeScheme(testPattern);

			Assert.True(testPatternParsed is Pair, "Parser failed: Failed to produce a pair from the test pattern");

			// Build a syntax matcher from the result
			System.Collections.Hashtable literals = new System.Collections.Hashtable();
			literals.Add(new Symbol("a"), true);
			SyntaxElement simpleMatch = SyntaxElement.MakeElementFromScheme(testPatternParsed, literals);

			// 'a' should not be a bound symbol, but 'b' should be
			Assert.True(simpleMatch.ContainsBoundSymbol("b"), "The symbol 'b' was not bound, when it should be");
			Assert.False(simpleMatch.ContainsBoundSymbol("a"), "The symbol 'a' was bound, when it should not be");
		}
	}
}
#endif