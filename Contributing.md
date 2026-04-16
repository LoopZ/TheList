## Contributing to _The List_

Interested in helping improve _The List_, then this is the document for you. It will help you understand how the project is structered and maintaned. Along with any rules or guidelines, you will need to know.

## Project structure

There are two main directories in the root of the project called [utilities](utilities) and [source](source). 

* [utilities](utilities) - contains the source files for the programs we use to manage _The List_.
* [source](source) - contains the many bits and pieces of _The List_ broken down for editing on _GitHub_.

## The source directory

Under this path, there are a couple configuration files and some **group** directories. The [_Release.txt](source/_Release.txt) file simply contains version of the release we are working towards. The [_Mapping.txt](source/_Mapping.txt) file contains settings and other information needed for [makelist](utilities/makelist) to compile _The List_ into is release format. 

With the exception of the [Miscellaneous](source/Miscellaneous) directory, the other sub-directories are _groups_ which contain the files that become a _LST_ file as defined in the [_Mapping.txt](source/_Mapping.txt) file when _The List_ is compiled. The files in the [Miscellaneous](source/Miscellaneous) are not processed and are simply copied into a release.

## Group sub-directories

Every _group_ sub-directory contains at least one _Comment Section_ file. These files always start with an **underscore** character. Generally, these files are included as-is into the appropriate _LST_ file based on the order as defined in the [_Mapping.txt](source/_Mapping.txt). 

Most _groups_ _(like [Interrupt List](source/Interrupt%20List) and [Ports List](source/Ports%20List))_ can contain numerious sub-directories and files used for the items in a _LST_ file. The directory and file names for those items is not important to the program that compiles _The List_. Their names are only for our convience when finding and editing an entry for _The List_. Those items can be broken down into any level of sub-paths which we find suitable to our needs in maintaning the project. 

_Note: All files are required to have a `.txt` file extention and must be encoded in standard ASCII using code page 437. UTF-8 and other text encoding schemes are not supported at this time._

## List item entries

The individual list item entry files each contain a header that has important information that is needed when compiling the list files. The header starts end ends with two vertical bars comprised of 80 dashes (minus characters). While there are several possible fields, only the **Unique ID** field is absolutely mandatory. 

* **Unique ID** - Manditory field. It contains the ID which will be used for the section break in the compiled _LST_ file. For example, _Interrupt 21h function 4Ch_ has a _Unique ID_ of `214C`.
  
  However, sometimes there is insufficient uniqueness for an ID to distinguish it from another entry. For such items, 
the ID can be suffixed with a **sort-as** to better identify the entry and guarantee the order in a _LST_ file.

  For example, there are be multiple entries for _Interrupt 21h function 40h_. These entries have _Unique IDs_ like `2140-sort-as-2140++` and `2140-sort-as-2140PC/TCP` to always maintain their the same order in _The List_. Additional text after the _Unique ID_ is prohibited. 

* **Sort As** - Optional field. This is an alternative method to the _sort-as_ suffix for providing additional uniqueness. Using one of the previously mentioned entries as an example, you could enter `Unique ID: 2140` and `Sort as: 2140PC/TCP`. Additional text after the _Sorting ID_ is prohibited.

  Either method of providing a _Sorting ID_ is equally acceptable. However, do not use both the `-sort-as-` suffix and the `Sort As` field in the same entry.

* **Category** - Recommended field. This is a single character field as listed in the [_Categories.txt](source/Interrupt%20List/_Categories.txt) file or `n/a`. While not required, additional text is recommended after the _category identifier_ in order to ease identification of the category. Entries are restricted to only a single _category_ per file.

* **Flag** - Recommended field. Like the _Category_ field, this is a single character field as listed in the [_Flags.txt](source/Interrupt%20List/_Flags.txt) file or `n/a`. While not required, additional text is recommended after the _flag identifier_ in order to ease identification of the flag. You may include several _Flag_ fields in the header if needed.

### _(tbd)_

