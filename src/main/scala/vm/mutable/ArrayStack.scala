package vm.mutable

import scala.reflect.ClassTag

class ArrayStack[A: ClassTag](private var size: Int = 1024) {
  private val array = new Array[A](size)
  private var pointer = -1;

  def withPointer(startingPointer: Int, ops: => Unit): Unit = {
    val initialPointer = pointer
    pointer = startingPointer - 1
    ops
    pointer = initialPointer
  }

  def push(value: A): Unit = {
    // TODO check stack overflow
    pointer += 1
    array(pointer) = value
  }

  def pop(): A = {
    val value = peek()
    array(pointer) = null.asInstanceOf[A]
    pointer -= 1
    value
  }

  def peek(): A =
    if (pointer >= 0) {
      array(pointer)
    } else {
      throw new Exception(s"Out of bounds ($pointer)")
    }

  def length(): Int =
    pointer + 1

  def set(index: Int, value: A): Unit =
    array(index) = value

  def get(index: Int): A =
    array(index)

  def toArray: Array[A] = array.slice(0, pointer + 1)
}
