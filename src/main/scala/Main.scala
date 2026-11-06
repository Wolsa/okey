// Okey – Task 1.1: Spielfeld als String auf der Konsole

val cellWidth = 4   // Breite einer Zelle, z. B. "B 12"
val rackSlots = 15  // Plätze pro Reihe auf der Istaka

// Ein Stein, z. B. tile("R", 7) -> "R 7 "
def tile(color: String, number: Int): String =
  s"$color $number".padTo(cellWidth, ' ')

// Falscher Joker (nimmt den Wert des Jokers an)
def falseJoker: String = "FJ".padTo(cellWidth, ' ')

// Leeres Feld
def emptyCell: String = " " * cellWidth

// Trennlinie: +----+----+ ... +
def line(cells: Int = rackSlots): String =
  ("+" + "-" * cellWidth) * cells + "+"

// Eine Reihe Steine, rechts mit leeren Feldern aufgefüllt
def row(tiles: List[String], cells: Int = rackSlots): String =
  tiles.padTo(cells, emptyCell).mkString("|", "|", "|")

// Istaka mit 2 Reihen
def rack(top: List[String], bottom: List[String], cells: Int = rackSlots): String =
  List(line(cells), row(top, cells), line(cells), row(bottom, cells), line(cells))
    .mkString("\n")

// Kopfzeile mit Anzeigestein, Joker und Anzahl Steine im Stapel
def header(indicator: String, joker: String, stack: Int): String =
  s"Anzeigestein: [${indicator.trim}]   Joker: ${joker.trim}   Stapel: $stack"

// Oberster Stein auf dem Abwurfstapel jedes Spielers
def discards(piles: List[(String, String)]): String =
  piles.map((name, top) => s"$name [$top]").mkString("Abwurf:  ", "   ", "")

// Gesamtes Spielfeld
def field(
    indicator: String,
    joker: String,
    stack: Int,
    piles: List[(String, String)],
    currentPlayer: String,
    top: List[String],
    bottom: List[String]
): String =
  List(
    header(indicator, joker, stack),
    "",
    discards(piles),
    "",
    s"$currentPlayer ist dran:",
    rack(top, bottom)
  ).mkString("\n")

@main def run(): Unit =
  val top = List(tile("R", 1), tile("R", 2), tile("R", 3), emptyCell,
                 tile("B", 11), tile("B", 12), tile("B", 13), emptyCell, falseJoker)
  val bottom = List(tile("Y", 5), tile("S", 5), tile("B", 5), emptyCell,
                    tile("S", 9), tile("Y", 10))

  val piles = List(
    ("Spieler 1", emptyCell),
    ("Spieler 2", tile("B", 3)),
    ("Spieler 3", tile("Y", 10)),
    ("Spieler 4", emptyCell)
  )

  println(field(tile("R", 7), tile("R", 8), 48, piles, "Spieler 1", top, bottom))