import util.control.Breaks._
val input = """
4 4
*...
....
.*..
....
3 5
**...
.....
.*...
0 0
"""

println(input)

var inputList = List.fromString(input, '\n')
println(inputList.size)

// Can not do this because i is immutable
// for (i <- 0 to inputList.size - 1) 
// {
//   var headerList = List.fromString(inputList.first, ' ')
//   var height = headerList.first 
//   i = i + height
//   println(i);
//   println(inputList(i));
// }

/**
 * ClassName
 */
class Field(
  var height: Int, 
  var width: Int, 
  var first: Int, 
  var last: Int, 
  var input: List[String]
  ) 
{
  var body = new Array[Array[String]](height, width)
  var counter = 0;
  for (i <- first to last )
  {
    var line = input(i).split("").drop(1)
    println(i + ":" + input(i))
    
    body(counter) = line
    counter = counter + 1;
  }
  
  var body2 = new Array[Array[String]](height, width)
  for (i <- 0 to body.size - 1)
  {
    for (j <- 0 to body(i).size - 1)
    { 
      var clue = 0;
      if (body(i)(j) == "*") 
      {
        body2(i)(j) = "*"
      }else{
        for(x <- -1 to 1)
        {
          for(y <- -1 to 1)
          { 
            var adjX = i + x
            var adjY = j + y
            if (withinBoundary(adjX, adjY)) 
            {
              if (body(adjX)(adjY) == "*" ) 
                clue = clue + 1
            }
          }
        }
        body2(i)(j) = clue.toString
      }
    }
  }
  
  def printHeader = println("height:" + height + " width:" + width + " bodyFirst:" + first + " bodyLast:" + last)

  def printOriginalBody = {
    for (i <- first to last)
      println(input(i))
  }
  def printArrayBody = printArray(body)
  def printArrayBody2 = printArray(body2)
  
  def printArray(array: Array[Array[String]]) = {
    for (i <- 0 until array.size)
    {
      array(i).foreach((a: String) => print(a))
      print("\n")
    }
  }
  
  def withinBoundary(x :Int, y :Int) = {
    if (x < 0 || y < 0 || x > height - 1 || y > width - 1 ) 
    {
       false
    }
    else{
       true
    }
  }
}

var counter :Int = 0;
do
{
  var headerList = List.fromString(inputList(counter), ' ')
  var height :Int = headerList(0).toInt;
  var width :Int = headerList(1).toInt;
  if (height == 0 && width == 0) 
  {
    println("EOF")
    break
  }
  var bodyFirst :Int = counter + 1
  var bodyLast :Int =  counter + height
  println("Creating new field")
  var field = new Field(height, width, bodyFirst, bodyLast, inputList);
  field.printHeader;
  println("ORIGINAL")
  field.printOriginalBody;
  println("ARRAY")
  field.printArrayBody;
  println("ARRAY2")
  field.printArrayBody2;
  counter = counter + height + 1
}while(counter < inputList.size )
