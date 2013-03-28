rust-antlr
==========

An ANTLR grammar for Rust

To try it out:

- install antlr 4 jar, put it in your classpath
- run ```make```
- try ```java org.antlr.v4.runtime.misc.TestRig Rust tts -tree -encoding UTF-8 < foo.rs```

... this should generate a "tree" representation of the parse tree for foo.rs.
