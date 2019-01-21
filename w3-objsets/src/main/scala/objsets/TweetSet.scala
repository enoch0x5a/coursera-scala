package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def filter(p: Tweet => Boolean): TweetSet

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList
  def descendingByRetweetAcc(acc: TweetSet): TweetSet
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  def incl(tweet: Tweet, fn: (Tweet, Tweet) => Boolean, save_duplicates: Boolean): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def toList(revert: Boolean): TweetList
}

class Empty extends TweetSet {
  def filter(p: Tweet => Boolean): TweetSet = new Empty

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet =
    incl(tweet, (elem, tweet) => true, false)

  def incl(x: Tweet, fn: (Tweet, Tweet) => Boolean, save_duplicates: Boolean): TweetSet =
    new NonEmpty(x, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def union(other: TweetSet): TweetSet = other

  def toList(revert: Boolean): TweetList = Nil

  def descendingByRetweet() = Nil

  def descendingByRetweetAcc(acc: TweetSet): TweetSet = new Empty

  def mostRetweeted: Tweet =
    throw new java.util.NoSuchElementException("List is Empty")
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val new_acc = left.filterAcc(p, acc) union right.filterAcc(p, acc)

    if (p(elem)) new_acc.incl(elem)
    else acc
  }

  def toList(revert: Boolean): TweetList =
    if (revert) (left.toList(revert).push(elem)).push(right.toList(revert))
    else (right.toList(revert).push(elem)).push(left.toList(revert))

  def descendingByRetweetAcc(acc: TweetSet): TweetSet = {
    val fn = (elem: Tweet, t: Tweet) => elem.retweets > t.retweets
    acc.incl(elem, fn, true)
  }

  def descendingByRetweet: TweetList = {
    left.descendingByRetweetAcc(new Empty).toList(true)
      .push(elem)
      .push(right.descendingByRetweetAcc(new Empty).toList(true))
  }

  def mostRetweeted: Tweet =
    descendingByRetweet.head

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    incl(x, (t1:Tweet, t2:Tweet) => t1.text > t2.text, false)
  }

  def incl(x: Tweet, fn: (Tweet, Tweet) => Boolean, save_duplicates: Boolean): TweetSet = {
    if (x == elem)
      if (save_duplicates) new NonEmpty(elem, left.incl(x, fn, save_duplicates), right)
      else this
    else if(fn(elem, x)) new NonEmpty(elem, left.incl(x, fn, save_duplicates), right)
    else new NonEmpty(elem, left, right.incl(x))
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (tw.text > elem.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }


  def union(other: TweetSet): TweetSet =
    new NonEmpty(elem, left union other, right)
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def push(t: Tweet): TweetList =
    new Cons(head, tail.push(t))

  def push(list: TweetList): TweetList = {
    def pushAcc(list: TweetList, acc: TweetList): TweetList =
      if (list.isEmpty) acc
      else pushAcc(list.tail, acc.push(list.head))

    if (list.isEmpty) this
    else pushAcc(list, Nil)
  }

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  override def push(tweet: Tweet) = new Cons(tweet, Nil)
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val google_inclusion = (t: Tweet) => google.exists( (e) => t.text.contains(e))
  val apple_inclusion = (t: Tweet) => apple.exists( (e) => t.text.contains(e))

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(google_inclusion)
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(apple_inclusion)

  /**
  * A list of all tweets mentioning a keyword from either apple or google,
  * sorted by the number of retweets.
  */
  lazy val trending: TweetList = {
    val sets = googleTweets union appleTweets
    sets.descendingByRetweet
  }
}

object Main extends App {
  // Print the trending tweets
  // GoogleVsApple.trending foreach println
  // TweetReader.allTweets foreach println
  TweetReader.allTweets.descendingByRetweet foreach println
}
