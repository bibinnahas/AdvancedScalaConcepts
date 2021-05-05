package com.bbn.fp.basic

class Animal(name: String, location: String) {
  val creatureType = "wild/domestic"

  def eat = s"$name makes sounds like crunch nom shmick while eating"

  def noise = s"$name makes sounds like bark roar shmick and is seen in $location"
}

class Dog(override val creatureType: String, name: String, location: String) extends Animal(name, location) {
  override def eat = s"$name makes sounds like crunch crunch while eating"

}

object PolymorphicCalls {

  def main(args: Array[String]): Unit = {
    val k9: Animal = new Dog("K9", "Canine", "India")
    println(k9.eat)
    println(k9.creatureType)
    println(k9.noise)

  }
}