package com.bbn.fp.basic

object AnonymousClasses extends App {

  abstract class Animal(name: String) {
    def eat: String
  }

  trait Habit {
    def animalType: String
  }

  class Dog extends Animal("Canine") with Habit {
    def eat: String = "crunch crunch"

    def animalType: String = "Domestic"
  }

  val giraffe: Animal = new Dog {
    override def eat: String = "shcumck shcumck"

  }
  // AnonymousClasses$$anon$1
  println(giraffe.getClass)


}
