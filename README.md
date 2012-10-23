xml-to-json
===========

Simple command line tool for converting XML files to JSON

Currently the tool processes XMLs according to lossy rules designed to produce sensibly minimal output. If you need to convert without losing information at all consider something like the XSLT offered by the [jsonml project](http://www.jsonml.org/). This tool produces json output similar (but not identical) to the [xml2json-xslt project](http://code.google.com/p/xml2json-xslt/).

Example output
--------------

Input file:

```
<?xml version="1.0"?>
<!DOCTYPE Test>
<Tests>
  <Test Name="The First Test">
    <Description>
Just a dummy
<!-- comment -->
Xml file.
    </Description>
  </Test>
  <Test Name="Second"/>
</Tests>
```

JSON output (formatted for readability - actual output a single line):

```
{
	"Tests" : { 
		"Test" : [
			{ "Name" : "The First Test", 
			  "Description" : {
				  "Format" : "FooFormat",
				  "value"  : "Just a dummy\n\nXml file."
              }
			},
			{ "Name" : "Second" }
		]
	}
}
```





Performance
-----------

The speed on a core-i5 machine is about 1.8MB of xml / sec, with a 100MB XML file resulting in a 56MB json output.

A few simple tests have shown this to be at least twice as fast as [jsonml's xlst-based converter](http://www.jsonml.org/xml/) (however, the outputs are not similar, as stated above).
 
Usage
-----

Basic usage
~~~~~~~~~~~
Just run the tool with the filename as a single argument, and direct the stdout to a file or a pipe:

> xml-to-json myfile.xml > myfile.js

Advanced
~~~~~~~~
Use the `--help` option to see the full command line options.