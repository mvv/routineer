Routineer
=========
Routineer is a Scala library that provides an internal DSL for declaring HTTP
routes. Routes are constructed from strings and patterns and the type of the
handling function is automatically inferred:

	get("prefix" /> * /> "middle" /> (IntP >>> PositiveP[Int])) {
	    (req // Your request type,
	     str // Inferred to be a String,
	     i   // Inferred to be an Int) =>
	  ...
	}

Installation
------------
Install [Simple Build Tool](http://code.google.com/p/simple-build-tool), run

	$ sbt update publish-local publish-local-maven

If you plan to use Routineer with [Scalaz](http://code.google.com/p/scalaz),
run

	$ sbt "project routineer-scalaz" update publish-local publish-local-maven

Usage
-----

There is no documentation at the moment, look at the
[example](https://github.com/mvv/routineer/blob/master/examples/servlet/src/ExampleServlet.scala)

