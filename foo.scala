sealed trait Shape {
  def sides: Int;
  def perimeter: Double;
  def area: Double;
  def colour: Colour;
}

case class Circle(radius: Double, colour: Colour) extends Shape {
  val sides = 1;
  val perimeter = 2 * math.Pi * radius;
  val area = math.Pi * radius * radius;
}

case class Rectangle(
  width: Double,
  height: Double,
  colour: Colour) extends Shape {
  val sides = 4;
  val perimeter = 2 * (width + height);
  val area = width * height;
}

case class Square(size: Double, colour: Colour) extends Shape {
  val sides = 4;
  val perimeter = 4 * size;
  val area = size * size;
}

object Draw {
  def apply(shape: Shape): String = shape match {
    case Rectangle(width, height, colour) =>
      s"A ${Draw(colour)} rectangle of width ${width}cm and height ${height}cm"

    case Square(size, colour) =>
      s"A ${Draw(colour)} square of size ${size}cm"

    case Circle(radius, colour) =>
      s"A ${Draw(colour)} circle of radius ${radius}cm"
  }

  def apply(colour: Colour): String = colour match {
    case Red => "red"
    case Yellow => "yellow"
    case Pink => "pink"
    case colour => if(colour.isLight) "light" else "dark"
  }
}

sealed trait Colour {
  def red: Double;
  def green: Double;
  def blue: Double;

  def isLight = (red + green + blue) / 3.0 > 0.5;
  def isDark = !isLight;
}

final case object Red extends Colour {
  val red = 1;
  val green = 0;
  val blue = 0;
}

final case object Yellow extends Colour {
  val red = 1;
  val green = 1;
  val blue = 0;
}

final case object Pink extends Colour {
  val red = 1;
  val green = 0.75;
  val blue = 1;
}

final case class CustomColour(
  red: Double,
  green: Double,
  blue: Double) extends Colour;

object divide {
  def apply(dividend: Int, divisor: Int): DivisionResult
}
