xml-to-json
===========

Simple command line tool for converting XML files to JSON

Currently the tool processes XMLs according to lossy rules designed to produce sensibly minimal output. If you need to convert without losing information at all consider something like the XSLT offered by the [jsonml project](http://www.jsonml.org/). This tool produces json output similar to the [xml2json-xslt project](http://code.google.com/p/xml2json-xslt/).

Performance
-----------

The speed on a core-i5 machine is about 1.8MB of xml / sec, with a 100MB XML file resulting in a 56MB json output.

Usage
-----

Just run the tool with the filename as a single argument, and direct the stdout to a file or a pipe:

> xml-to-json myfile.xml > myfile.js

