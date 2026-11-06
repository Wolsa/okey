
// Okey – Task 1.2: Worksheet mit ersten Datenstrukturen
// Speichern (Cmd + S) -> Metals zeigt die Ergebnisse direkt neben jeder Zeile an.

// ---------- Farben ----------
enum Color(val short: String):
  case Red    extends Color("R")
  case Blue   extends Color("B")
  case Yellow extends Color("Y")
  case Black  extends Color("S")

Color.values
Color.Red.short
 
// ---------- Steine ----------
sealed trait Tile

case class NumberTile(color: Color, value: Int) extends Tile:
  require(value >= 1 && value <= 13, s"Wert muss zwischen 1 und 13 liegen, war $value")
  override def toString: String = s"${color.short} $value"

case object FalseJoker extends Tile:
  override def toString: String = "FJ"

val red7 = NumberTile(Color.Red, 7)
red7.color
red7.value
red7

val copyTile = red7.copy(color = Color.Blue)
copyTile

FalseJoker

// Ungültiger Stein wirft eine Exception:
scala.util.Try(NumberTile(Color.Red, 14))

// ---------- Joker aus dem Anzeigestein ----------
// Joker = nächsthöhere Zahl in derselben Farbe, nach 13 kommt 1
def jokerFor(indicator: NumberTile): NumberTile =
  NumberTile(indicator.color, indicator.value % 13 + 1)

jokerFor(red7)                           // R 8
jokerFor(NumberTile(Color.Black, 13))    // S 1

// ---------- Kompletter Steinsatz (106 Steine) ----------
val allTiles: List[Tile] =
  (for
    _     <- 1 to 2
    color <- Color.values
    value <- 1 to 13
  yield NumberTile(color, value)).toList ++ List(FalseJoker, FalseJoker)

allTiles.size                                    // 106
allTiles.count(_ == FalseJoker)                  // 2
allTiles.count(_ == NumberTile(Color.Red, 7))    // 2

// ---------- Spieler ----------
case class Player(name: String, rack: List[Tile] = Nil):
  def tileCount: Int = rack.size
  def add(tile: Tile): Player = copy(rack = rack :+ tile)
  def remove(tile: Tile): Player = copy(rack = rack.diff(List(tile)))

val p = Player("Batuhan")
p.tileCount
val p2 = p.add(red7).add(FalseJoker)
p2.rack
p2.remove(red7).rack
p.rack   // unverändert: Player ist immutable

// ---------- Austeilen ----------
// Fester Seed, damit beim Testen immer dieselbe Verteilung rauskommt
val shuffled = scala.util.Random(42).shuffle(allTiles)

// Erster Spieler bekommt 15 Steine, alle anderen 14
def deal(tiles: List[Tile], names: List[String]): (List[Player], List[Tile]) =
  names.zipWithIndex.foldLeft((List.empty[Player], tiles)) {
    case ((players, remaining), (name, index)) =>
      val count = if index == 0 then 15 else 14
      val (hand, rest) = remaining.splitAt(count)
      (players :+ Player(name, hand), rest)
  }

val (players, rest) = deal(shuffled, List("Spieler 1", "Spieler 2", "Spieler 3", "Spieler 4"))

players.map(_.tileCount)   // List(15, 14, 14, 14)
rest.size                  // 49

// Anzeigestein: erster normaler Stein vom Stapel (kein falscher Joker)
val indicator = rest.collectFirst { case t: NumberTile => t }.get
val stack     = rest.diff(List(indicator))
val joker     = jokerFor(indicator)

stack.size                 // 48

// Ist ein Stein der Joker?
def isJoker(tile: Tile): Boolean = tile == joker
players.head.rack.filter(isJoker)