import play.api.libs.json._

import scala.io.StdIn.readLine

object Main extends App {

  val testMode: Boolean = true // Passer cette variable à true pour pouvoir tester les commanndes avec le pokemon Dracaufeau (sans passer par scala.io.Source.fromURL)

  def toStringFormatted(value: JsValue): String = {
    var s = value.toString().substring(1, value.toString().length - 1)
    s = s.substring(0, 1).toUpperCase() + s.substring(1)
    s
  }

  def testDoubleType(file: JsValue): JsValue = {
    try {
      val type2 = (file \ "types" \ 1 \ "type" \ "name").get
      type2
    } catch {
      case _: NoSuchElementException => JsString("null")
    }
  }

  var jsonresponse = ""
  println("Bienvenue sur le Pokédex !\n\nLes commandes possibles sont:\n\n- GET <pokemon-name>\n- GETALL <region?> <pokemon-type1?> <generation?> <legendary?>\n- WHEREIS <pokemon-name>\n- MATCH <pokemon-name>")

  val url = "https://pokeapi.co/api/v2/"

  print("Entrez votre commande (GET/GETALL/WHEREIS/MATCH): ")
  val cmd = readLine().toUpperCase()

  if (cmd == "GET") {
    try {
      print("Entrez le pokemon souhaité: ")
      if (testMode) printf("(no matter, it will be charizard) : ") // PASSER LA VARIABLE "testMode" A "false" POUR POUVOIR CHOISIR LE POKEMON
      val pokemon = readLine().toLowerCase()
      val http1 = url + "pokemon/" + pokemon

      var jsonPokemon: JsValue =
        if (!testMode)
          Json.parse(scala.io.Source.fromURL(http1)("UTF-8").mkString)
        else
          Json.parse(scala.io.Source.fromFile("src/main/scala/pokemon-charizard.json")("UTF-8").mkString)

      val name = (jsonPokemon \ "name").get
      val type1 = (jsonPokemon \ "types" \ 0 \ "type" \ "name").get
      val type2 = testDoubleType(jsonPokemon)
      val attack = (jsonPokemon \ "stats" \ 0 \ "base_stat").get
      val defense = (jsonPokemon \ "stats" \ 1 \ "base_stat").get
      val special_attack = (jsonPokemon \ "stats" \ 2 \ "base_stat").get
      val special_defense = (jsonPokemon \ "stats" \ 3 \ "base_stat").get
      val http2 = url + "pokemon-species/" + pokemon

      var jsonPokemonSpecies: JsValue =
        if (!testMode)
          Json.parse(scala.io.Source.fromURL(http2)("UTF-8").mkString)
        else
          Json.parse(scala.io.Source.fromFile("src/main/scala/pokemon-species-charizard.json")("UTF-8").mkString)

      val is_legendary = (jsonPokemonSpecies \ "is_legendary").get

      val gen_count = (jsonPokemon \\ "game_indices").headOption match {
        case Some(JsArray(values)) => values.length
        case _ => 0
      }
      var genList = ""
      for (i <- 0 until gen_count) {
        genList += toStringFormatted((jsonPokemon \ "game_indices" \ i \ "version" \ "name").get) + " "
      }

      val move_count = (jsonPokemon \\ "moves").headOption match {
        case Some(JsArray(values)) => values.length
        case _ => 0
      }
      var moveList = ""
      for (i <- 0 until move_count) {
        moveList += toStringFormatted((jsonPokemon \ "moves" \ i \ "move" \ "name").get) + " "
      }

      printf("Name : " + toStringFormatted(name) + "\n")
      printf("First Type : " + toStringFormatted(type1) + "\n")
      if (toStringFormatted(type2) != "null") printf("Second Type : " + toStringFormatted(type2) + "\n")
      printf("Attack : " + attack.toString() + "\n")
      printf("Defense : " + defense.toString() + "\n")
      printf("SP Attack : " + special_attack.toString() + "\n")
      printf("SP Defense : " + special_defense.toString() + "\n")
      printf("Is it a legendary ? : " + is_legendary.toString() + "\n")
      printf("The list of generations in which it appears (Total of " + gen_count + ") : " + genList + "\n")
      printf("Its list of move (Total of " + move_count + ") : " + moveList + "\n")

    } catch {
      case e: Exception => println("Pas d'informations sur ce type de Pokemon (existent-ils vraiment ?)")
    }
  }
  else if (cmd == "GETALL") {
    try {
      print("Vous souhaitez chercher par 'type', 'generation' ou si le pokemon est 'legendaire' ou non ?\n Entrez type/generation/legendaire: ")
      val search = readLine()
      if (search == "type") {
        try {
          print("Quel type de pokemon cherchez-vous ?")
          val typee = readLine()
          val http = url + "type/" + typee
          val result = scala.io.Source.fromURL(http).mkString
          println(result)
          println(http)
        } catch {
          case e: Exception => println("Pas d'informations sur ce type de Pokemon (existent-ils vraiment ?)")
        }
      }
      else if (search == "generation") {
        try {
          print("Quelle generation de pokemon vous interesse ?")
          val gen = readLine()
          val http = url + "generation/" + gen
          val result = scala.io.Source.fromURL(http).mkString
          println(result)
          println(http)
        } catch {
          case e: Exception => println("Pas d'informations sur cette generation de Pokemon (existent-ils vraiment ?)")
        }
      }
      else if (search == "legendaire") {
        try {
          print("Cherchez vous les pokemon legendaire ? (O/N)")
          val legendary = readLine()
          // val http = url+"generation/"+legendary
          // val result = scala.io.Source.fromURL(http).mkString
          // println(result)
          // println(http)
          print("Commande indisponible pour le moment...")
        } catch {
          case e: Exception => println("Pas d'informations sur ces Pokemons (existent-ils vraiment ? o_O)")
        }
      }
    } catch {
      case e: Exception => println("Pas d'informations ou erreur de saisie")
    }
  }
  else if (cmd == "WHEREIS") {
    try {
      print("Entrez le pokemon souhaité: ")
      val pokemon = readLine()
      val http = url + "pokemon/" + pokemon + "/encounters"
      val result = scala.io.Source.fromURL(http).mkString
      println(result)
      println(http)
    } catch {
      case e: Exception => println("Pas d'informations sur ce Pokemon (existe-il vraiment ?)")
    }
  }
  else if (cmd == "MATCH") {
    try {
      print("Commande indisponible pour le moment...")
    } catch {
      case e: Exception => println("Pas d'informations sur ce Pokemon (existe-il vraiment ?)")
    }
  }
  else {
    val urlnext = "error/"
  }

  // val i = cmd match {
  //   case "GET" => val urlnext = "pokemon/"
  //   // case "GETALL" =>
  //   case "WHEREIS" => val urlnext = "pokemon-habitat/"
  //   // case "MATCH" =>
  // }


}
