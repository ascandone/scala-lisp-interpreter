package collection.mutable

import scala.reflect.ClassTag

class ArrayStack[A: ClassTag](private var size: Int = 1024) {
  private val array = new Array[A](size)
  private var pointer = -1;

  def push(value: A): Unit = {
    // TODO check stack overflow
    this.pointer += 1
    array(this.pointer) = value
  }

  def pop(): A = {
    val value = this.peek()
    array(this.pointer) = null.asInstanceOf[A]
    this.pointer -= 1
    value
  }

  def peek(): A =
    if (this.pointer >= 0) {
      array(this.pointer)
    } else {
      throw new Exception("Out of bounds")
    }

  def length(): Int =
    this.pointer + 1

  def set() {
    ???
  }

  def get() {
    ???
  }


}
