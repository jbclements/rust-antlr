rust-antlr
==========

An ANTLR grammar for Rust

The ANTLR grammar works in two modes. First, it can parse whole files as
sequences of "token trees". Second, it can parse them as Rust programs. It's
not worth getting into the differences here; suffice it to say that parsing
as token trees essentially just tests the lexer, and makes sure that the
delimiters match.

The token-tree parsing appears to successfully parse all readily-available
Rust code.

The full parser still has some bugs. I think it's close, but it's not yet
finished.


To try it out:

- install antlr 4 jar, put it in your classpath
- run ```make```
- try ```java org.antlr.v4.runtime.misc.TestRig Rust tts -encoding UTF-8 < foo.rs```

or, if you want to parse using the full program parser, replace this last line with

- try ```java org.antlr.v4.runtime.misc.TestRig Rust prog -encoding UTF-8 < foo.rs```

This generates no output on success, and error output on failure.

If you want to see the tree produced by the parser, you can pass the "-tree" argument.
