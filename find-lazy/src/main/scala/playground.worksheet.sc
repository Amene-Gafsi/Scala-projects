import find.cs214.open

val food = open("example-dir/food")

food.path()

food.hasChildren()

val fruits = food.firstChild()

fruits.path()

fruits.hasNextSibling()

fruits.nextSibling().path()
