package com.pokemon

import okhttp3.{OkHttpClient, Request}

import scala.util.control.NonFatal
import spray.json._
import DefaultJsonProtocol._

class PokedexGET(name: String, type1: String, type2: String, attack: Int, defense: Int, spattack: Int, spdefense: Int, legendary: Boolean) {
  // Constructor parameters are promoted to members
  // val name = _name
  // val type1 = _type1
  // val type2 = _type2
  // val attack = _attack
  // val defense = _defense
  // val spattack = _spattack
  // val spdefense = _spdefense
  // val legendary = _legendary

  // toString redefinition to return the value of an instance instead of its memory addres
  override def toString = s"$name\n$type1\n$type2\n Attaque: $attack\nDefense: $defense\nAttaque speciale: $spattack\nDefense speciale: $spdefense\n Is legendary: $legendary)"
}

object PokemonGET {
  // Constructor that allows the omission of the `new` keyword
  def main(name: String, type1: String, type2: String, attack: Int, defense: Int, spattack: Int, spdefense: Int, legendary: Boolean): PokedexGET =
    new PokedexGET(name, type1, type2, attack, defense, spattack, spdefense, legendary)
}
object PokedexGETALL {

  def main(args: Array[String]): Unit = println("Hello pokédex")
}
object PokedexWHEREIS {

  def main(args: Array[String]): Unit = println("Hello pokédex")
}
object PokedexMATCH {

  def main(args: Array[String]): Unit = println("Hello pokédex")
}
