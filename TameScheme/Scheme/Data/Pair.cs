// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Scheme 'Pair' class                                                Pair.cs |
// +----------------------------------------------------------------------------+
// | Copyright (c) 2005 Andrew Hunter                                           |
// |                                                                            |
// | Permission is hereby granted, free of charge, to any person obtaining a    |
// | copy of this software and associated documentation files (the "Software"), |
// | to deal in the Software without restriction, including without limitation  |
// | the rights to use, copy, modify, merge, publish, distribute, sublicense,   |
// | and/or sell copies of the Software, and to permit persons to whom the      |
// | Software is furnished to do so, subject to the following conditions:       |
// |                                                                            |
// | The above copyright notice and this permission notice shall be included in |
// | all copies or substantial portions of the Software.                        |
// |                                                                            |
// | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR |
// | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   |
// | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    |
// | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER |
// | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    |
// | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        |
// | DEALINGS IN THE SOFTWARE.                                                  |
// +----------------------------------------------------------------------------+

using System;
using System.Collections;

namespace Tame.Scheme.Data
{
	/// <summary>
	/// Class representing a scheme pair
	/// </summary>
	/// <remarks>Implements the IList interface, but note that pairs that form part of improper or self-referential lists will produce bad results.</remarks>
	public sealed class Pair : ICollection, IList
	{
		/// <summary>
		/// Constructs a pair (())
		/// </summary>
		public Pair()
		{
		}

		/// <summary>
		/// Constructs a pair with a given car and cdr
		/// </summary>
		/// <param name="car">The car object for this pair</param>
		/// <param name="cdr">The cdr object for this pair</param>
		public Pair(object car, object cdr)
		{
			this.Car = car;
			this.Cdr = cdr;
		}

		public static Pair ListFromCollection(ICollection collection)
		{
			// null if there's nothing in the collection
			if (collection == null || collection.Count <= 0) return null;

			// Create a pair otherwise
			return new Pair(collection);
		}

		/// <summary>
		/// Constructs a pair with the contents of a collection (ie, create a list from the collection)
		/// </summary>
		/// <param name="collection">The collection to create a list from. This collection must have at least one object for this constructor to work.</param>
		public Pair(ICollection collection)
		{
			Pair thePair = this;

			// The collection must have at least one object for this to be valid
			// TODO: make this a proper scheme exception
			if (collection.Count <= 0) throw new System.Exception("In order to create a list from a collection, the collection must have at least one entry");

			// Add each object in the collection to this pair
			lock (collection.SyncRoot)
			{
				int count = collection.Count;

				IEnumerator collectionEnum = collection.GetEnumerator();

				for (int x=0; x<count; x++)
				{
					collectionEnum.MoveNext();
					thePair.Car = collectionEnum.Current;

					if (x == count-1) break;

					thePair.Cdr = new Pair();
					thePair = (Pair)thePair.Cdr;
				}
			}
		}

		/// <summary>
		/// Constructs a pair with the contents of a collection (ie, create a list from the collection)
		/// </summary>
		/// <param name="collection">The collection to create a list from. This collection must have at least one object for this constructor to work.</param>
		/// <param name="offset">The offset in the collection to start building the pair from</param>
		public Pair(IList collection, int offset)
		{
			Pair thePair = this;

			// The collection must have at least one object for this to be valid
			// TODO: make this a proper scheme exception
			if (collection.Count <= 0) throw new System.Exception("In order to create a list from a collection, the collection must have at least one entry");

			// Add each object in the collection to this pair
			lock (collection.SyncRoot)
			{
				int count = collection.Count;

				for (int x=offset; x<count; x++)
				{
					thePair.Car = collection[x];

					if (x == count-1) break;

					thePair.Cdr = new Pair();
					thePair = (Pair)thePair.Cdr;
				}
			}
		}

		#region Variables

		// The Car object for this pair
		public object Car = null;

		// The Cdr object for this pair
		public object Cdr = null;

		#endregion

		#region Convienience functions

		internal Pair PairAtIndex(int index)
		{
			if (index < 0) throw new IndexOutOfRangeException("Attempt to access a negative index while using the indexer for a Pair");
			Pair thePair = this;

			while (index > 0)
			{
				object nextPair = thePair.Cdr;

				if (nextPair is Pair)
				{
					thePair = (Pair)thePair.Cdr;
				}
				else
				{
					thePair = null;
					if (index == 1) throw new IndexOutOfRangeException("Pair represents an improper list");
				}
				if (thePair == null) throw new IndexOutOfRangeException("Attempt to access beyond the end of a Pair list");
				index--;
			}

			return thePair;
		}

		public object this[int index]
		{
			get
			{
				return PairAtIndex(index).Car;
			}
			set
			{
				PairAtIndex(index).Car = value;
			}
		}

		/// <summary>
		/// Determines if this Pair represents a list containing a loop.
		/// </summary>
		/// <returns>true if this list is self-referential, false otherwise</returns>
		public bool HasLoop()
		{
			if (this.Cdr == null || !(this.Cdr is Pair)) return false;

			// Uses the tortoise & hare algorithm
			Pair slow = this;
			Pair fast = (Pair)this.Cdr;

			while (fast != null)
			{
				// We only check type for the fast value (as this will not change by the time the slow value gets there)
				if (slow == fast) return true;
				if (fast.Cdr == null) return false;
				if (!(fast.Cdr is Pair)) return false;

				fast = (Pair)fast.Cdr;

				if (slow == fast) return true;
				if (!(fast.Cdr is Pair)) return false;

				fast = (Pair)fast.Cdr;

				slow = (Pair)slow.Cdr;
			}

			return false;
		}

		/// <summary>
		/// Determines if this Pair represents an improper list
		/// </summary>
		/// <returns>True if this pair represents an improper list</returns>
		public bool IsImproper()
		{
			if (this.Cdr == null) return false;
			if (!(this.Cdr is Pair)) return true;

			// Uses the tortoise & hare algorithm
			Pair slow = this;
			Pair fast = (Pair)this.Cdr;

			for (;;)
			{
				// We only check type for the fast value (as this will not change by the time the slow value gets there)
				if (slow == fast) return false;
				if (fast.Cdr == null) return false;
				if (!(fast.Cdr is Pair)) return true;

				fast = (Pair)fast.Cdr;

				if (slow == fast) return false;
				if (fast.Cdr == null) return false;
				if (!(fast.Cdr is Pair)) return true;

				fast = (Pair)fast.Cdr;

				slow = (Pair)slow.Cdr;
			}
		}

		/// <summary>
		/// Returns true if this list is not a 'pure' list: if it is either improper or contains a loop
		/// </summary>
		/// <returns>True if this list contains a loop or is improper</returns>
		public bool IsImproperOrLoop()
		{
			if (this.Cdr == null) return false;
			if (!(this.Cdr is Pair)) return true;

			// Uses the tortoise & hare algorithm
			Pair slow = this;
			Pair fast = (Pair)this.Cdr;

			for (;;)
			{
				// We only check type for the fast value (as this will not change by the time the slow value gets there)
				if (slow == fast) return true;
				if (fast.Cdr == null) return false;
				if (!(fast.Cdr is Pair)) return true;

				fast = (Pair)fast.Cdr;

				if (slow == fast) return true;
				if (fast.Cdr == null) return false;
				if (!(fast.Cdr is Pair)) return true;

				fast = (Pair)fast.Cdr;

				slow = (Pair)slow.Cdr;
			}
		}

		private Pair Midpoint(Pair from, Pair to)
		{
			// returns the pointer to the middle element (round toward front of list) 
			// --- we can implement this with a pointer+double speed pointer walk

			// (Assumes there is a loop)

			Pair slow = from;
			Pair fast = from;

			while (fast != to)
			{
				fast = (Pair)fast.Cdr;
				if (fast == to) break;

				fast = (Pair)fast.Cdr;
				slow = (Pair)slow.Cdr;
			}

			return slow;
		}

		private bool Find(Pair from, Pair what, Pair to)
		{
			// returns true if we encounter f in a walk from e to g, otherwise false.

			Pair thisItem = from;

			if (to == what) return true;
			while (thisItem != to)
			{
				if (thisItem == what) return true;
				thisItem = (Pair)thisItem.Cdr;
			}

			return false;
		}

		/// <summary>
		/// If this pair contains a loop, finds the first element that exists in the loop
		/// </summary>
		/// <returns>The element that begins the loop, or null if there is not a loop</returns>
		/// <remarks>It is more efficient to call this and check for a null return than it is to call HasLoop() beforehand if you need
		/// to determine if something contains a loop, and if it does, where that loop begins.</remarks>
		public Pair LoopHead()
		{
			// Found an O(n) algorithm for this at http://discuss.fogcreek.com/techInterview/default.asp?cmd=show&ixPost=710
			// (This is Alex Harris's algorithm - I think I agree with his reasoning that this is a O(n) operation)

			Pair x = new Pair(null, this);					// An element outside the loop
			Pair y = this;									// (Not necessarily an element inside the loop)

			// Change y to an element inside the loop
			Pair slow = x;
			Pair fast = this;

			for(;;)
			{
				// We only check type for the fast value (as this will not change by the time the slow value gets there)
				if (slow == fast) { y = slow; break; }
				if (!(fast.Cdr is Pair)) return null;
				if (fast.Cdr == null) return null;

				fast = (Pair)fast.Cdr;

				if (slow == fast) { y = slow; break; }
				if (!(fast.Cdr is Pair)) return null;
				if (fast.Cdr == null) return null;

				fast = (Pair)fast.Cdr;

				slow = (Pair)slow.Cdr;
			}

			// Find the loop head
			Pair a = x;
			Pair b = (Pair)y.Cdr;
			Pair c = y;

			for (;;)
			{
				Pair t = Midpoint(a, c);
				if (Find(b, t, c))
					c = t;
				else
					a = (Pair)t.Cdr;

				t = Midpoint(b, c);

				if (Find(a, t, c))
					c = t;
				else
					b = (Pair)t.Cdr;

				if (a == b) return a;
				if (a == c || b == c) return c;
			}
		}

		#endregion

		public override bool Equals(object obj)
		{
			// TODO: a danger: what if this.Car introduces a loop? This is an infinite loop at the moment.
			if (obj == null) 
				return false;
			if (!(obj is Pair)) 
				return false;

			Pair current = this;
			Pair compareTo = (Pair)obj;

			Pair loopHead = LoopHead();
			Pair compareLoopHead = compareTo.LoopHead();
			bool visitedLoopHead = false;

			// If one object contains a loop, so must the other
			if ((loopHead == null || compareLoopHead == null) && loopHead != compareLoopHead) 
				return false;

			while (current != null)
			{
				if (current == loopHead)
				{
					// The pair we're comparing to must also loop here
					if (compareTo != compareLoopHead) 
						return false;

					// Stop if we've already been here
					if (visitedLoopHead)
					{
						break;
					}
					else
					{
						visitedLoopHead = true;
					}
				}

				// Cars must be equal (DANGER: CAR LOOPS ARE NOT ACCOUNTED FOR YET)
				if (current.Car == null)
				{
					if (current.Car != null) 
						return false;
				}
				else if (!current.Car.Equals(compareTo.Car))
				{
					return false;
				}

				// Move on
				if (current.Cdr == null)
				{
					if (compareTo.Cdr == null)
						return true;
					else
						return false;
				}
				else if (current.Cdr is Pair)
				{
					if (!(compareTo.Cdr is Pair)) 
						return false;

					current = (Pair)current.Cdr;
					compareTo = (Pair)compareTo.Cdr;
				}
				else
				{
					if (compareTo.Cdr is Pair) 
						return false;

					return current.Cdr.Equals(compareTo.Cdr);
				}
			}

			// Both pairs are equal
			return true;
		}

		public override string ToString()
		{
			string res = "";

			Pair current = this;

			Pair loopHead = LoopHead();					// The first item in the loop if this pair contains one
			bool visitedLoopHead = false;				// set to true after the first time we visit loopHead

			while (true)
			{
				if (current.Car == null)
					res += "()";
				else
					res += current.Car.ToString();

				if (current.Cdr == null)
				{
					// We're done
					break;
				}
				else if (current.Cdr is Pair)
				{
					// The Cdr is a pair: move on
					res += " ";
					current = (Pair)current.Cdr;

					if (current == loopHead && visitedLoopHead)
					{
						// Notify that this contains a loop
						res += "#[...]";
					}

					if (current == loopHead) visitedLoopHead = true;
				}
				else
				{
					// The Cdr is not a pair add a '. foo' style thing and finish
					res += " . " + current.Car.ToString();
					current = null;
					break;
				}
			}

			// We've constructed everything except the brackets
			return "(" + res + ")";
		}

		#region ICollection Members

		public bool IsSynchronized
		{
			get
			{
				return false;
			}
		}

		public int Count
		{
			get
			{
				int count = 0;
				Pair currentPair = this;
				Pair slowPair = currentPair;

				while (currentPair != null)
				{
					currentPair = (Pair)currentPair.Cdr;
					count++;

					if (currentPair == null) break;
					if (currentPair == slowPair) throw new NotSupportedException("A list with a loop is uncountable");

					currentPair = (Pair)currentPair.Cdr;
					count++;

					if (currentPair == slowPair) throw new NotSupportedException("A list with a loop is uncountable");

					slowPair = (Pair)slowPair.Cdr;
				}

				return count;
			}
		}

		public void CopyTo(Array array, int index)
		{
			Pair currentPair = this;

			if (HasLoop()) throw new NotSupportedException("A Pair that contains a loop cannot be copied into an array");

			while (currentPair != null)
			{
				array.SetValue(currentPair.Car, index++);

				currentPair = (Pair)currentPair.Cdr;
			}
		}

		public object SyncRoot
		{
			get
			{
				// Pairs have to be globally synchronised at the moment
				return typeof(Pair);
			}
		}

		#endregion

		#region IEnumerable Members

		private class PairEnumerator : IEnumerator
		{
			public PairEnumerator(Pair firstPair)
			{
				this.firstPair = firstPair;
				this.currentPair = null;
				this.loopHead = firstPair.LoopHead();
			}

			Pair firstPair, currentPair, loopHead;

			bool visitedLoopHead = false;
			bool visitedImproperElement = false;

			#region IEnumerator Members

			public void Reset()
			{
				currentPair = null;
				visitedLoopHead = false;
				visitedImproperElement = false;
			}

			public object Current
			{
				get
				{
					if (visitedImproperElement) return currentPair.Cdr;
					return currentPair.Car;
				}
			}

			public bool MoveNext()
			{
				if (visitedImproperElement) return false;
				if (firstPair == null || (currentPair != null && currentPair.Cdr == null))
					return false;
				if (currentPair != null && visitedLoopHead && currentPair.Cdr == loopHead)
					return false;
	
				if (currentPair == null) 
				{
					currentPair = firstPair;
				}
				else
				{
					if (currentPair.Cdr is Pair)
						currentPair = (Pair)currentPair.Cdr;
					else
						visitedImproperElement = true;
				}

				if (currentPair == loopHead) visitedLoopHead = true;

				return true;
			}

			#endregion
		}

		public IEnumerator GetEnumerator()
		{
			return new PairEnumerator(this);
		}

		#endregion

		#region IList Members

		// While we implement this interface, a Pair does not fully represent a .NET IList (it's a slightly different container model)

		public bool IsReadOnly
		{
			get
			{
				return false;
			}
		}

		/// <summary>
		/// Removes the Pair at the specified index.
		/// </summary>
		/// <param name="index">The index of the object to remove.</param>
		/// <remarks>
		/// Removing the object at index 0 may have 'odd' side-effects (this effectively copies the object at index 1 over this one).
		/// You cannot remove the object at index 0 if this is the only object in the list. Use the Cdr for preference instead of trying
		/// to remove the first object if at all possible.
		/// </remarks>
		public void RemoveAt(int index)
		{
			// Can't remove before index 0
			if (index < 0) throw new IndexOutOfRangeException("Attempt to remove a negative index from a Pair");

			if (index == 0)
			{
				// Check for error conditions
				if (Cdr == null) throw new IndexOutOfRangeException("Cannot remove index 0 of a Pair if its Cdr is null");
				if (!(Cdr is Pair)) throw new IndexOutOfRangeException("Cannot remove index 0 of a pair if its Cdr is not also a Pair");

				// Replace ourself with the following object
				Pair following = (Pair)Cdr;

				Car = following.Car;
				Cdr = following.Cdr;
			}
			else
			{
				// At this point, index is > 0, so we remove by altering the Cdr of this element:
				Pair preceeding = PairAtIndex(index-1);

				// Check that there's a following element, and that it's also a Pair
				if (preceeding.Cdr == null) throw new IndexOutOfRangeException("Attempt to remove an element beyond the end of a list");
				if (!(preceeding.Cdr is Pair)) throw new IndexOutOfRangeException("Cannot remove the final element of an improper list");

				// Remove the element
				preceeding.Cdr = ((Pair)preceeding.Cdr).Cdr;
			}
		}

		/// <summary>
		/// Inserts an object as the Car of a new Pair at the given index
		/// </summary>
		/// <param name="index"></param>
		/// <param name="value"></param>
		/// <remarks>As with all IList operations, the effect may be different to what you expect if the index is 0</remarks>
		public void Insert(int index, object value)
		{
			// Can't remove before index 0
			if (index < 0) throw new IndexOutOfRangeException("Attempt to insert before a negative index from a Pair");

			if (index == 0)
			{
				// Create a new pair that's a copy of this one
				Pair newPair = new Pair(Car, Cdr);

				// Change this one to point to the new one and contain the object
				Cdr = newPair;
				Car = value;
			}
			else
			{
				// Get the preceeding Pair
				Pair preceeding = PairAtIndex(index-1);

				// Create a new pair after preceeding
				preceeding.Cdr = new Pair(value, preceeding.Cdr);
			}
		}

		/// <summary>
		/// Removes the first occurance of the given object from the list formed by this Pair.
		/// </summary>
		/// <remarks>Does nothing if the object is not in the list. May fail if the object is the only one in the list. Removal is by copying.</remarks>
		public void Remove(object value)
		{
			Pair pairToRemove = PairContaining(value);

			if (pairToRemove != null) pairToRemove.RemoveAt(0);
		}

		/// <summary>
		/// Returns true if the given object is in the list formed by this pair.
		/// </summary>
		/// <remarks>Infinite loop if the list is self-referential. Exception if the list is improper.</remarks>
		public bool Contains(object value)
		{
			return PairContaining(value)!=null;
		}

		/// <summary>
		/// Lists formed by Pairs cannot be cleared. Do not call this function.
		/// </summary>
		public void Clear()
		{
			throw new NotSupportedException("Pairs cannot sensibly be cleared");
		}

		/// <summary>
		/// Determines the Pair which is part of the list that this Pair forms and has the given object as a Car.
		/// </summary>
		public Pair PairContaining(object value)
		{
			Pair thisPair = this;
			int currentIndex = 0;

			while (thisPair != null)
			{
				if (thisPair.Car == value) break;

				thisPair = (Pair)thisPair.Cdr;
				currentIndex++;
			}

			return thisPair;
		}

		/// <summary>
		/// Determines the index of the element of the list with the given value as a Car
		/// </summary>
		public int IndexOf(object value)
		{
			// Search for the pair with the given object as the Car
			Pair thisPair = this;
			int currentIndex = 0;

			while (thisPair != null)
			{
				if (thisPair.Car == value) break;

				thisPair = (Pair)thisPair.Cdr;
				currentIndex++;
			}

			if (thisPair != null)
				return currentIndex;
			else
				return -1;
		}

		/// <summary>
		/// Adds an element to the end of the list
		/// </summary>
		/// <param name="value">The value to add</param>
		/// <returns>The index of the new element</returns>
		public int Add(object value)
		{
			Pair lastPair = this;
			int count = 0;

			while (lastPair.Cdr != null) 
			{
				lastPair = (Pair)lastPair.Cdr;
				count++;
			}

			lastPair.Cdr = new Pair(value, null);

			return count+1;
		}

		/// <summary>
		/// Whether or not this is a fixed size IList (Pairs are never fixed size)
		/// </summary>
		public bool IsFixedSize
		{
			get
			{
				return false;
			}
		}

		#endregion
	}
}
