all : RustParser.class

clean :
	rm *.class
	rm *.java

RustParser.class : RustParser.java
	javac Rust*.java

RustParser.java : Rust.g4 xidstart.g4 xidcont.g4
	java -jar /usr/local/lib/antlr-4.0-complete.jar Rust.g4
