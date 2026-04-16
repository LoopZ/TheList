## Contributing to _The List_

Interested in helping improve _The List_, then this is the document for you. It will help you understand how the project is structered and maintaned. Along with any rules and guidelines you will need to know.

## Project structure

There are two main directories in the root of the project called [utilities](utilities) and [source](source). 

* [utilities](utilities) - contains the source files for the programs we use to manage _The List_.
* [source](source) - contains the many bits and pieces of _The List_ broken down for more suitable editing using _GitHub_.

## The source directory

Under this path, there are a couple configuration files and some **group** directories. The [_Release.txt](source/_Release.txt) file simply contains version of the release we are working towards. The [_Mapping.txt](source/_Mapping.txt) file contains settings and other information needed for [makelist](utilities/makelist) to compile _The List_ into is release format. 

With the exception of the [Miscellaneous](source/Miscellaneous) directory, the other sub-directories are _groups_ which contain the files that become a _LST_ file as defined in the [_Mapping.txt](source/_Mapping.txt) file when _The List_ is compiled. The files in the [Miscellaneous](source/Miscellaneous) are not processed and are simply copied into a release.

## Group sub-directories

Every _group_ sub-directory contains at least one _Comment Section_ file. These files always start with an **underscore** character. Generally, these files are included as-is into the appropriate _LST_ file based on the order as defined in the [_Mapping.txt](source/_Mapping.txt). 

Most _groups_ _(like [Interrupt List](source/Interrupt%20List) and [Ports List](source/Ports%20List))_ can contain numerious sub-directories and files used for the items in a _LST_ file. The directory and file names for those items is not important to the program that compiles _The List_. Their names are only for our convience when finding and editing an entry for _The List_. Those items can be broken down into any level of sub-paths which we find suitable to our needs in maintaning the project. 

_Note: All files are required to have a `.txt` file extention and must be encoded in standard ASCII using code page 437. UTF-8 and other text encoding schemes are not supported at this time._
