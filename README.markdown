README
======

etap is a collection of Erlang modules that provide a TAP testing harness. These modules allow developers to create extensive and comprehensive tests covering many aspects of application and module development. This includes simple assertions, exceptions, the application behavior and event web requests. This library was originally written by Jeremy wall.

As per the TAP wiki:

> TAP, the Test Anything Protocol, is a simple text-based interface between testing modules in a test harness. TAP started life as part of the test harness for Perl but now has implementations in C/C++, Python, PHP, Perl and probably others by the time you read this. 

These modules are not meant to compete with eunit, but to offer a more general testing facility that isn't provides by eunit.

    http://en.wikipedia.org/wiki/Test_Anything_Protocol
    http://testanything.org/wiki/index.php/Main_Page

BUILD & INSTALL
===============

To build this library, from the root directory execute the `make` command. You should also execute the `make test` command to verify that the library functions correctly on your system.

    $ make
    $ make test

If you choose to run the `make test` command then please be sure to `make clean` after to remove any of the temporary beam files created by the tests in the `t/` directory.

The included tests cover the basic functionality of the etap modules. They can also be used as a reference when writing your own tests. 

To install etap you need to create the `etap/bin/` directory in your current Erlang library and copy all of the .beam files created by the `make` file.

    $ sudo mkdir -p /usr/lib/erlang/lib/etap-0.3/ebin
    $ make clean && make
    $ sudo cp ebin/*.beam /usr/lib/erlang/lib/etap-0.3/ebin/

SUPPORTED FUNCTIONALITY
=======================

There are a number of proposals listed on the TAP wiki that are not supported by this library. Please be aware of this when creating your tests.

 * NOT SUPPORTED: TAP diagnostic syntax
 * LIMITED SUPPORTED: TAP meta information
 * NOT SUPPORTED: TAP logging syntax
 * NOT SUPPORTED: Test groups
 * NOT SUPPORTED: Test blocks
 * NOT SUPPORTED: SKIP
 * NOT SUPPORTED: TODO
 * LIMITED SUPPORTED: TAP datetime

CREDITS
=======

2007-2008 Jeremy Wall
2008 Nick Gerakines
