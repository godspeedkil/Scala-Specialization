package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s1000neg = singletonSet(-1000)
    val s1000 = singletonSet(1000)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect works correctly") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val i1 = intersect(u1, u2)
      assert(contains(i1, 2), "Intersect 1")
      assert(!contains(i1, 1) && !contains(i1, 3),
        "Intersect exclusion 1")
    }
  }

  test("diff works correctly") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val d1 = diff(u1, u2)
      assert(contains(d1, 1), "Diff 1")
      assert(!contains(d1, 2) && !contains(d1, 3),
        "Diff Exclusion 1")
    }
  }

  test("filter works correctly") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val f1 = filter(u1, s2)
      assert(contains(f1, 2), "Filter 1")
      assert(!contains(f1, 1) && !contains(f1, 3),
        "Filter 2")
    }
  }

  test("forall works correctly") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(u1, s3)
      val u3 = union(s1000neg, s1000)
      assert(forall(u2, { x: Int => x < 4 }), "Forall 1")
      assert(forall(u2, { x: Int => x > 0 }), "Forall 2")
      assert(!forall(u2, { x: Int => x != 1 }), "Forall Exclusion 1")
      assert(forall(u3, { x: Int => x >= -1000 }), "Test limits 1")
      assert(forall(u3, { x: Int => x <= 1000 }), "Test limits 2")
    }
  }

  test("exists works correctly") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(u1, s3)
      val u3 = union(s1000neg, s1000)
      assert(exists(u2, { x: Int => x == 2 }), "Exists 1")
      assert(!exists(u2, { x: Int => x == 4 }), "Exists Exclusion 1")
      assert(exists(u3, { x: Int => x == -1000 }) && exists(u3,
        { x: Int => x == 1000 }), "Exists limits 1")
    }
  }

  test("map works correctly") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s1000neg, s1000)
      val u3 = union(u1, u2)
      val m1 = map(u3, { x: Int => x + 2})
      assert(contains(m1, 3), "Map 1")
      assert(contains(m1, 4), "Map 2")
      assert(contains(m1, -998), "Map 3")
      assert(contains(m1, 1002), "Map 4")
      assert(!contains(m1, 1), "Map Exclusion 1")
      assert(!contains(m1, 2), "Map Exclusion 2")
    }
  }
}
