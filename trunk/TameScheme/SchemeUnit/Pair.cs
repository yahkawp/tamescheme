using System;

using csUnit;

using Tame.Scheme.Data;
using Tame.Scheme.Runtime;

namespace SchemeUnit
{
	/// <summary>
	/// Some tests for Pairs
	/// </summary>
	[TestFixture("Data types")]
	public class PairTest
	{
		public PairTest()
		{
		}

		Interpreter terp = new Interpreter();

		[Test]
		public void TestLoop()
		{
			Pair shortList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 10)");
			Pair lastElement = shortList;
			Pair loopElement = shortList;

			Assert.False(shortList.HasLoop());

			for (int x=0; x<9; x++) lastElement = (Pair)lastElement.Cdr;
			for (int x=0; x<3; x++) loopElement = (Pair)loopElement.Cdr;

			// Make a loop
			lastElement.Cdr = loopElement;

			Assert.True(shortList.HasLoop());
		}

		[Test]
		public void TestImproper()
		{
			Pair shortList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 10)");

			Assert.False(shortList.IsImproper());

			Pair improperList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 . 10)");

			Assert.True(improperList.IsImproper());
		}

		[Test]
		public void TestImproperWithLoop()
		{
			Pair shortList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 10)");
			Pair lastElement = shortList;
			Pair loopElement = shortList;

			for (int x=0; x<9; x++) lastElement = (Pair)lastElement.Cdr;
			for (int x=0; x<3; x++) loopElement = (Pair)loopElement.Cdr;

			// Make a loop
			lastElement.Cdr = loopElement;

			Assert.False(shortList.IsImproper());
		}

		[Test]
		public void TestLoopOrImproper()
		{
			Pair shortList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 10)");
			Pair lastElement = shortList;
			Pair loopElement = shortList;

			Assert.False(shortList.IsImproperOrLoop());
			
			Pair improperList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 . 10)");
			Assert.True(improperList.IsImproperOrLoop());

			for (int x=0; x<9; x++) lastElement = (Pair)lastElement.Cdr;
			for (int x=0; x<3; x++) loopElement = (Pair)loopElement.Cdr;

			// Make a loop
			lastElement.Cdr = loopElement;

			Assert.True(shortList.IsImproperOrLoop());
		}

		[Test]
		public void FindLoopHead()
		{
			Pair shortList = (Pair)terp.ParseScheme("(1 2 3 4 5 6 7 8 9 10)");
			Pair lastElement = shortList;
			Pair loopElement = shortList;

			Assert.Equals(null, shortList.LoopHead());

			for (int x=0; x<9; x++) lastElement = (Pair)lastElement.Cdr;
			for (int x=0; x<3; x++) loopElement = (Pair)loopElement.Cdr;

			Assert.Equals(10, lastElement.Car);
			Assert.Equals(4, loopElement.Car);

			// Make a loop
			lastElement.Cdr = loopElement;

			Assert.Equals(4, shortList.LoopHead().Car);
			Assert.ReferenceEquals(loopElement, shortList.LoopHead());
		}
	}
}
