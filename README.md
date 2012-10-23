# xml-to-json

Fast & easy command line tool for converting XML files to JSON.

The output is designed to be easy to store and process using JSON-based databases, such as [mongoDB](http://www.mongodb.org/) and [CouchDB](http://couchdb.apache.org/). In fact, the original motivation for xml-to-json was to store and query a large (~10GB) XML-based dataset, using an off-the-shelf scalable JSON database.

Currently the tool processes XMLs according to lossy rules designed to produce sensibly minimal output. If you need to convert without losing information at all consider something like the XSLT offered by the [jsonml project](http://www.jsonml.org/). This tool produces json output similar (but not identical) to the [xml2json-xslt project](http://code.google.com/p/xml2json-xslt/).


## Usage

### Basic usage

Just run the tool with the filename as a single argument, and direct the stdout to a file or a pipe:

> xml-to-json myfile.xml > myfile.js


### Advanced

Use the `--help` option to see the full command line options.

Here's a (possibly outdated) snapshot of the `--help` output:

`
Usage: xml-to-json [OPTION...] files...
  -h      --help          Show this help
  -t TAG  --tag-name=TAG  Start conversion with nodes named TAG (ignoring all parent nodes)
  -s      --skip-roots    Ignore the selected nodes, and start converting from their children
                          (can be combined with the 'start-tag' option to process only children of the matching nodes)
  -m      --multiline     Output each of the top-level converted json objects on a seperate line
  -n      --ignore-nulls  Ignore nulls (do not output them) in the top-level output objects
  -a      --as-array      Output the resulting objects in a top-level JSON array
`

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

The speed on a core-i5 machine is about 1.8MB of xml / sec, with a 100MB XML file resulting in a 56MB json output.

A few simple tests have shown this to be at least twice as fast as [jsonml's xlst-based converter](http://www.jsonml.org/xml/) (however, the outputs are not similar, as stated above).
 