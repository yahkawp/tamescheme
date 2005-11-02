using System;
using System.Collections;
using System.Text;
using System.Reflection;

namespace SchemeUnit
{
    /// <summary>
    /// Minimal unit testing class.
    /// </summary>
    /// <remarks>
    /// We used to use csUnit, but that only works under VS.NET 2003, which is less than ideal.
    /// These classes are not as flexible, but should work under both VS and Mono.
    /// </remarks>
    class Assert
    {
        #region Running the tests

        static void Wipe(int count)
        {
            string wipe = "";
            string spaces = "";

            for (int x = 0; x < count; x++)
            {
                wipe += '\b';
                spaces += " ";
            }

            Console.Out.Write(wipe);
            Console.Out.Write(spaces);
            Console.Out.Write(wipe);
        }

        class FixtureComparison : IComparer
        {
            public FixtureComparison() { }

            #region IComparer Members

            private string Fixture(object x)
            {
                if (x == null) return "";

                TestFixtureAttribute fixtureAttr = (TestFixtureAttribute)Attribute.GetCustomAttribute((Type)x, typeof(TestFixtureAttribute));

                if (fixtureAttr == null) return "";
                if (fixtureAttr.AttributeName == null) return "";

                return fixtureAttr.AttributeName;
            }

            public int Compare(object x, object y)
            {
                return Fixture(x).CompareTo(Fixture(y));
            }

            #endregion
        }

        [STAThread]
        static void Main(string[] args)
        {
            // Counts
            int tests = 0;
            int success = 0;
            int errors = 0;
            int failures = 0;

            // Short introduction
            Console.Out.WriteLine("TameScheme Test Suite");

            // Work out the list of classes with the TestFixture attribute applied
            ArrayList testClasses = new ArrayList();

            foreach (Type t in Assembly.GetExecutingAssembly().GetTypes())
            {
                TestFixtureAttribute fixtureAttr = (TestFixtureAttribute)Attribute.GetCustomAttribute(t, typeof(TestFixtureAttribute));

                if (fixtureAttr != null)
                {
                    testClasses.Add(t);
                }
            }

            // Sort according to the fixture
            testClasses.Sort(new FixtureComparison());

            string lastFixture = null;

            // For each class, run the tests
            foreach (Type t in testClasses)
            {
                TestFixtureAttribute fixtureAttr = (TestFixtureAttribute)Attribute.GetCustomAttribute(t, typeof(TestFixtureAttribute));

                // Display the name of this fixture if necessary
                if (lastFixture == null || !lastFixture.Equals(fixtureAttr.AttributeName))
                {
                    lastFixture = fixtureAttr.AttributeName;

                    if (lastFixture != null)
                    {
                        Console.Out.WriteLine("* " + lastFixture);
                    }
                }

                // Display the name of the class
                Console.Out.WriteLine("| * Running tests in " + t.ToString());

                // Create the class
                object thisClass = null;

                try
                {
                    thisClass = t.GetConstructor(new Type[0]).Invoke(new object[0]);
                }
                catch (TargetInvocationException e)
                {
                    tests++;
                    errors++;

                    Console.Out.WriteLine("Failed to instantiate class");
                    Console.Out.WriteLine(e.GetBaseException().ToString());
                }

                if (thisClass == null) continue;

                // For each method that has a [Test] attribute set
                MethodInfo[] methods = t.GetMethods();

                foreach (MethodInfo methodInfo in methods)
                {
                    TestAttribute testAttr = (TestAttribute)Attribute.GetCustomAttribute(methodInfo, typeof(TestAttribute));
                    ExpectedExceptionAttribute expected = (ExpectedExceptionAttribute)Attribute.GetCustomAttribute(methodInfo, typeof(ExpectedExceptionAttribute));

                    if (testAttr != null)
                    {
                        int linePos = 0;                                // Adds some formatting
                        Console.Out.Write("| | * "); linePos += 6;

                        // Write the test attribute name
                        if (testAttr.AttributeName != null)
                        {
                            Console.Out.Write(testAttr.AttributeName); linePos += testAttr.AttributeName.Length;
                            Console.Out.Write(" "); linePos += 1;
                        }

                        // Write the test method name
                        Console.Out.Write(methodInfo.Name); linePos += methodInfo.Name.Length;
                        Console.Out.Write(" "); linePos += 1;

                        // Write some dots
                        for (int dot = 0; dot < 60 - linePos; dot++)
                            Console.Out.Write(".");
                        Console.Out.Write(" ");

                        Console.Out.Flush();

                        // Run the test
                        try
                        {
                            tests++;
                            methodInfo.Invoke(thisClass, new object[0]);

                            if (expected == null)
                            {
                                // We succeed if there was no expected exception
                                success++;

                                Wipe(61);
                                // Console.Out.WriteLine("[ OK ]");
                            }
                            else
                            {
                                // An exception was expected: this is a failure
                                failures++;

                                Console.Out.WriteLine("[ Failure ]");
                                Console.Out.WriteLine("Was expecting the exception " + expected.ExceptionType.ToString());
                            }
                        }
                        catch (Exception e)
                        {
                            if (e is TargetInvocationException)
                            {
                                e = ((TargetInvocationException)e).GetBaseException();
                            }

                            if (e is UnitFailure)
                            {
                                // This is a failure
                                failures++;

                                Console.Out.WriteLine("[ Failure ]");

                                Console.Out.WriteLine(e.Message);
                                Console.Out.WriteLine(e.ToString());
                            }
                            else if (expected != null && expected.ExceptionType.Equals(e.GetType()))
                            {
                                // This exception was expected
                                success++;

                                Wipe(61);
                                // Console.Out.WriteLine("[ OK ]");
                            }
                            else
                            {
                                // This exception was not expected
                                errors++;

                                Console.Out.WriteLine("[ Error ]");
                                Console.Out.WriteLine("Got exception " + e.GetType().ToString());
                                if (expected != null) Console.Out.WriteLine("(Was expecting " + expected.ExceptionType.ToString() + ")");
                                Console.Out.WriteLine(e.ToString());
                            }
                        }
                    }
                }
            }

            Console.Out.WriteLine("\nSUMMARY");
            Console.Out.WriteLine("  Tests run : " + tests);
            Console.Out.WriteLine("  Succeeded : " + success);
            Console.Out.WriteLine("  Failed    : " + failures);
            Console.Out.WriteLine("  Errors    : " + errors);

            if (tests == success)
            {
                Console.Out.WriteLine("All tests passed");
            }
            else
            {
                Console.Out.WriteLine("Testing failed");
#if !NoTestWait
                Console.In.ReadLine();
#endif
            }
        }

        #endregion

        #region Unit testing functions

        private class UnitFailure : Exception
        {
            public UnitFailure(string message)
                : base(message)
            { }
        }

        static public void True(bool shouldBeTrue)
        {
            if (!shouldBeTrue) throw new UnitFailure("Value should have been true");
        }

        static public void False(bool shouldBeFalse)
        {
            if (shouldBeFalse) throw new UnitFailure("Value should have been false");
        }

        static public new void Equals(object expected, object actual)
        {
            // Objects are the same if they refer to the same location
            if (expected == actual) return;

            // Report any differences involving null
            if (expected == null)
            {
                throw new UnitFailure("Got " + actual.ToString() + ", but was expecting null");
            }
            if (actual == null)
            {
                throw new UnitFailure("Got null, but was expecting " + expected.ToString());
            }

            // If the types are different, try to cast them so that they are the same
            if (expected.GetType() != actual.GetType())
            {
                try
                {
                    expected = Convert.ChangeType(expected, actual.GetType());
                }
                catch (Exception)
                {
                    throw new UnitFailure("Got " + actual.ToString() + ", but was expecting " + expected.ToString());
                }
            }

            // Test for equality
            if (!object.Equals(expected, actual))
            {
                throw new UnitFailure("Got " + actual.ToString() + ", but was expecting " + expected.ToString());
            }
        }

        static public void Null(object shouldBeNull)
        {
            Equals(null, shouldBeNull);
        }

        #endregion
    }

    #region Attributes

    class TestAttribute : Attribute
    {
        public TestAttribute()
        { }

        public TestAttribute(string attributeName)
        {
            this.attributeName = attributeName;
        }

        string attributeName = null;

        public string AttributeName { get { return attributeName; } }
    }

    class TestFixtureAttribute : Attribute
    {
        public TestFixtureAttribute()
        { }

        public TestFixtureAttribute(string attributeName)
        {
            this.attributeName = attributeName;
        }

        string attributeName = null;

        public string AttributeName { get { return attributeName; } }
    }

    class ExpectedExceptionAttribute : Attribute
    {
        public ExpectedExceptionAttribute(Type exceptionType)
        {
            this.exceptionType = exceptionType;
        }

        Type exceptionType = null;

        public Type ExceptionType { get { return exceptionType; } }
    }

    #endregion
}
