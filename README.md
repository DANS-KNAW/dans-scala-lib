dans-scala-lib
==============

Library with generic extensions for Scala-based DANS modules


DESCRIPTION
-----------

The DANS development team uses Scala as its main programming language. 
We aim to maintain a commons style of coding. Some of the coding idioms
recur often. In cases where these patterns are not captured already by
a good, third party, open source library we put them in `dans-scala-lib`.

The objects, classes and functions in this library are intended to be 
useful for all of our projects. The library only declares a minimal number
of dependencies.


INSTALLATION
------------

To use this libary in a Maven-based project:

1. Include in your `pom.xml` a declaration for the DANS maven repository:

        <repositories>
            <!-- possibly other repository declartions here ... -->
            <repository>
                <id>DANS</id>
                <releases>
                    <enabled>true</enabled>
                </releases>
                <url>http://maven.dans.knaw.nl/</url>
            </repository>
        </repositories>

2. Include a dependency on this library. The version should of course be
   set to the latest version (or left out, if it is managed by an ancestor `pom.xml`).

        <dependency>
            <groupId>nl.knaw.dans.lib</groupId>
            <artifactId>dans-scala-lib</artifactId>
            <version>1.0</version>
        </dependency>
