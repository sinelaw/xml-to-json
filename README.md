# xml-to-json

Fast & easy command line tool for converting XML files to JSON.

The output is designed to be easy to store and process using JSON-based databases, such as [mongoDB](http://www.mongodb.org/) and [CouchDB](http://couchdb.apache.org/). In fact, the original motivation for xml-to-json was to store and query a large (~10GB) XML-based dataset, using an off-the-shelf scalable JSON database.

Currently the tool processes XMLs according to lossy rules designed to produce sensibly minimal output. If you need to convert without losing information at all consider something like the XSLT offered by the [jsonml project](http://www.jsonml.org/). Unlike jsonml, this tool - xml-to-json - produces json output similar (but not identical) to the [xml2json-xslt project](http://code.google.com/p/xml2json-xslt/).

### Implementation Notes

xml-to-json is implemented in [Haskell](http://www.haskell.org). Currently the implementation is minimal - for example, the core translation functionality is not exported as a library. If you want to use it as a library, open an issue on this project (or better yet - do it and submit a pull request).

As of this writing, xml-to-json uses [hxt](http://hackage.haskell.org/package/hxt) with the [expat](http://expat.sourceforge.net/)-based [hxt-expat](http://hackage.haskell.org/package/hxt-expat) parser. The pure Haskell parsers for hxt [all seem to have memory issues](http://stackoverflow.com/q/2292729/562906) which hxt-expat doesn't.

## Contents

* [Usage](#usage)
* [Example output](#example-output)
* [Performance](#performance)

## Usage

### Basic usage

Just run the tool with the filename as a single argument, and direct the stdout to a file or a pipe:

> xml-to-json myfile.xml > myfile.js


### Advanced

Use the `--help` option to see the full command line options.

Here's a (possibly outdated) snapshot of the `--help` output:

```
Usage: xml-to-json [OPTION...] files...
  -h      --help          Show this help
  -t TAG  --tag-name=TAG  Start conversion with nodes named TAG (ignoring all parent nodes)
  -s      --skip-roots    Ignore the selected nodes, and start converting from their children
                          (can be combined with the 'start-tag' option to process only children of the matching nodes)
  -m      --multiline     Output each of the top-level converted json objects on a seperate line
  -n      --ignore-nulls  Ignore nulls (do not output them) in the top-level output objects
  -a      --as-array      Output the resulting objects in a top-level JSON array
```

## Example output

Input file:

```
<?xml version="1.0"?>
<!DOCTYPE Test>
<Tests>
  <Test Name="The First Test">
    <Description Format="FooFormat">
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





## Performance

For large XML files, the speed on a core-i5 machine is about 2MB of xml / sec, with a 100MB XML file resulting in a 56MB json output. It took about 10 minutes to process 1GB of xml data. The main performance limit is memory - only one single-threaded process was running since every single large file (tens of megabytes) consumes a lot of memory - about 50 times the size of the file.

A few simple tests have shown this to be at least twice as fast as [jsonml's xlst-based converter](http://www.jsonml.org/xml/) (however, the outputs are not similar, as stated above).

Currently the program processes files serially. If run in parallel on many small XML files (<5MB) the performance becomes cpu-bound and processing may be *much* faster, depending on the architecture.

