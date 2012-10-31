# xml-to-json

Fast & easy command line tool for converting XML files to JSON.


## Contents
* [Overview](#overview)
* [Installation](#installation)
* [Usage](#usage)
* [Example output](#example-output)
* [Performance](#performance)


## Overview

The output is designed to be easy to store and process using JSON-based databases, such as [mongoDB](http://www.mongodb.org/) and [CouchDB](http://couchdb.apache.org/). In fact, the original motivation for xml-to-json was to store and query a large (~10GB) XML-based dataset, using an off-the-shelf scalable JSON database.

Currently the tool processes XMLs according to lossy rules designed to produce sensibly minimal output. If you need to convert without losing information at all consider something like the XSLT offered by the [jsonml project](http://www.jsonml.org/). Unlike jsonml, this tool - xml-to-json - produces json output similar (but not identical) to the [xml2json-xslt project](http://code.google.com/p/xml2json-xslt/).

### Implementation Notes

xml-to-json is implemented in [Haskell](http://www.haskell.org). Currently the implementation is minimal - for example, the core translation functionality is not exported as a library. If you want to use it as a library, open an issue on this project (or better yet - do it and submit a pull request).

As of this writing, xml-to-json uses [hxt](http://hackage.haskell.org/package/hxt) with the [expat](http://expat.sourceforge.net/)-based [hxt-expat](http://hackage.haskell.org/package/hxt-expat) parser. The pure Haskell parsers for hxt [all seem to have memory issues](http://stackoverflow.com/q/2292729/562906) which hxt-expat doesn't.

## Installation

**Note for Windows users**: [Download pre-built exe here](https://github.com/downloads/sinelaw/xml-to-json/xml-to-json.zip). I managed to compile using [cygwin](http://www.cygwin.org). Use the [xml-to-json branch for cygwin](https://github.com/sinelaw/xml-to-json/tree/cygwin) if you're trying to build for windows. The main difference is lack of curl support.

To install the **release version**: Since xml-to-json is implemented in Haskell, "all you need to do" is install the latest [Haskell platform](http://www.haskell.org/platform/) for your system, and then run:

```
cabal update
cabal install xml-to-json
```

To install **from source**: Clone this repository locally, and then (assuming you have [Haskell platform](http://www.haskell.org/platform/) installed) run `cabal install`:

```
cd xml-to-json
cabal install
```

## Usage

### Basic usage

Just run the tool with the filename as a single argument, and direct the stdout to a file or a pipe:

> xml-to-json myfile.xml > myfile.js


### Advanced

Use the `--help` option to see the full command line options.

Here's a (possibly outdated) snapshot of the `--help` output:

```
Usage: <program> [OPTION...] files...
  -h      --help                      Show this help
  -t TAG  --tag-name=TAG              Start conversion with nodes named TAG (ignoring all parent nodes)
  -s      --skip-roots                Ignore the selected nodes, and start converting from their children
                                      (can be combined with the 'start-tag' option to process only children of the matching nodes)
  -a      --as-array                  Output the resulting objects in a top-level JSON array
  -m      --multiline                 When using 'as-array' output, print each of top-level json object on a seperate line.
                                      (If not using 'as-array', this option will be on regardless, and output is always line-seperated.)
          --no-collapse-text=PATTERN  For elements with tag matching regex PATTERN only:
	  			      Don't collapse elements that only contain text into a simple string property.
                                      Instead, always emit '.value' properties for text nodes, even if an element contains only text.
                                      (Output 'schema' will be more stable.)
          --no-ignore-nulls           Don't ignore nulls (and do output them) in the top level of output objects
```

## Example output

Input file:

```
<?xml version="1.0"?>
<!DOCTYPE Test>
<Tests>
  <Test Name="The First Test">
    <SomeText>Some simple text</SomeText>
    <SomeMoreText>More text</SomeMoreText>
    <Description Format="FooFormat">
Just a dummy
<!-- comment -->
Xml file.
    </Description>
  </Test>
  <Test Name="Second"/>
</Tests>
```

JSON output using default settings:
```
{"Tests":{"Test":[{"Name":"The First Test","SomeText":"Some simple text","Description":{"Format":"FooFormat","value":"Just a dummy\n\nXml file."}},{"Name":"Second"}]}}
```

Formatted for readability (not the actual output):

```
{
   "Tests":{
      "Test":[
         {
            "Name":"The First Test",
	    "SomeMoreText":"More text",
            "SomeText":"Some simple text",
            "Description":{
               "Format":"FooFormat",
               "value":"Just a dummy\n\nXml file."
            }
         },
         {
            "Name":"Second"
         }
      ]
   }
}
```

Note that currently xml-to-json does not retain the order of elements / attributes.
 
Using the various options you can control various aspects of the output such as:

* At which top-level nodes the conversion starts to work (-t)
* Whether to wrap the output in a top-level JSON array,
* Whether or not to collapse simple string elements, such as the <SomeText> element in the example, into a simple string property (you can specify a regular expression pattern to decide which nodes to collapse)
* And more...

Use the `--help` option to see a full list of options.


## Performance

For large XML files, the speed on a core-i5 machine is about 2MB of xml / sec, with a 100MB XML file resulting in a 56MB json output. It took about 10 minutes to process 1GB of xml data. The main performance limit is memory - only one single-threaded process was running since every single large file (tens of megabytes) consumes a lot of memory - about 50 times the size of the file.

A few simple tests have shown this to be at least twice as fast as [jsonml's xlst-based converter](http://www.jsonml.org/xml/) (however, the outputs are not similar, as stated above).

Currently the program processes files serially. If run in parallel on many small XML files (<5MB) the performance becomes cpu-bound and processing may be *much* faster, depending on the architecture. A simple test showed performance of about 5MB / sec (on the same core-i5).


