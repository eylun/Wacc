import parsley.Parsley, Parsley._
import parsley.character.{char, string, digit}
import parsley.implicits.character.{charLift, stringLift}

object Main extends App {
    // Using something from Parsely here
	val hello: Parsley[Unit] = void('h' *> ("ello" <|> "i") *> " world!")
	hello.parse("hello world!")
	hello.parse("hi world!")
	hello.parse("hey world!")
	println("Hello world!")
}
