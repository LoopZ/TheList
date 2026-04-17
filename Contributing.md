## Contributing to _The List_

If you're interested in helping improve _The List_, then this is the document for you. It will help you understand how the project is structured and maintained. Along with any rules or guidelines that you will need to know.

## Project structure

There are two main directories in the root of the project called [utilities](utilities) and [source](source).

* [utilities](utilities) - contains the source text files for the programs we use to manage _The List_.
* [source](source) - contains the many bits and pieces of _The List_ broken down for editing using _git_.

## The source directory

Under this path, there are a couple configuration files and some **group** directories. The [_Release.txt](source/_Release.txt) file simply contains the version of the release we are working towards. The [_Mapping.txt](source/_Mapping.txt) file contains settings and other information needed for [makelist](utilities/makelist) to compile _The List_ into its release format.

With the exception of the [Miscellaneous](source/Miscellaneous) directory, the other sub-directories are _groups_ which contain the files that become a _LST_ file as defined in the [_Mapping.txt](source/_Mapping.txt) file when _The List_ is compiled. The files in the [Miscellaneous](source/Miscellaneous) directory are not processed and are simply copied into a release.

## Group sub-directories

Every _group_ sub-directory contains at least one _Comment Section_ file. The filenames of these files always start with an **underscore** character. Generally, these files are included as-is into the appropriate _LST_ file based on the order as defined in the [_Mapping.txt](source/_Mapping.txt) file.

Most _groups_ _(like [Interrupt List](source/Interrupt%20List) and [Ports List](source/Ports%20List))_ can contain numerous sub-directories and files used for the items in a _LST_ file. The directory and file names for those items are not important to the program that compiles _The List_. Their names are only for our convenience when finding and editing an entry for _The List_. Those items can be broken down into any level of sub-paths which we find suitable to our needs in maintaining the project. (When compiling, the unique IDs or sort-as IDs (described below) are used to determine the order of entries.)

_Note: All files are required to have a `.txt` filename extension and must be encoded in standard ASCII using code page 437. UTF-8 and other text encoding schemes are not supported at this time._

## List item entries

### List item headers

The individual list item entry files each contain a header that has important information that is needed when compiling the list files. The header starts and ends with two horizontal bars comprised of 80 dashes (minus characters). While there are several possible fields, only the **Unique ID** field is absolutely mandatory.

* **Unique ID** - Mandatory field. It contains the ID which will be used for the section break in the compiled _LST_ file. For example,
[Interrupt 21h function 4Ch](source/Interrupt%20List/INT%2021%20DOS%20Function%20Calls/INT%20214C%20DOS%202%20EXIT%20TERMINATE%20WITH%20RETURN%20CODE.txt)
has a _Unique ID_ of `214C`. An entry has to have exactly one _Unique ID_.

  However, sometimes there is insufficient uniqueness for an ID to distinguish it from another entry. For such items,
the ID can be suffixed with a **-sort-as-** ID to better identify the entry and guarantee the order in a _LST_ file.

  For example, there are multiple entries for _Interrupt 21h function 40h_. These entries have _Unique IDs_ like `2140-sort-as-2140++` and `2140-sort-as-2140PC/TCP` to always maintain the same order in _The List_. Additional text after the _Unique ID_ is prohibited.

* **Sort As** - Optional field. This is an alternative method to the _-sort-as-_ suffix for providing additional uniqueness. Using one of the previously mentioned entries as an example, you could enter `Unique ID: 2140` and `Sort As: 2140PC/TCP`. Additional text after the _Sort As ID_ is prohibited.

  Either method of providing a _Sort As ID_ is equally acceptable. However, do not use both the `-sort-as-` suffix and the `Sort As` field in the same entry.

* **Category** - Recommended field. This is a single character field as listed in the [_Categories.txt](source/Interrupt%20List/_Categories.txt) file or `-` if no category is used. Note that the _category identifier_ is case-sensitive. While not required, additional text is recommended after the _category identifier_ in order to ease identification of the category. Entries are restricted to only a single _category_ per file. The _category identifier_ is entered into the section break line of the compiled _LST_ file, after the first 8 dashes. After the _category identifier_ another dash follows, then the list-as ID (from the _Unique ID_).

* **Flag** - Recommended field. Like the _Category_ field, this is a single character field as listed in the [_Flags.txt](source/Interrupt%20List/_Flags.txt) file or `n/a` if no flag is applicable. Like the _category identifier_, the _flag identifier_ is case-sensitive. While not required, additional text is recommended after the _flag identifier_ in order to ease identification of the flag. You may include several _Flag_ fields in the header if needed.

### List item contents

After the second 80-dash separator line that ends the header,
the contents of the list item follow.
All contents are copied exactly into the _LST_ file during compilation,
except that leading and trailing empty lines are skipped
and linebreaks are normalised to CR LF.
Content lines should always fit in fewer than 80 columns.
Tab stops are expected at every 8th column.

#### Summary base line

The first non-empty line is known as the summary base.

For an entry like [INT 21/AH=4Ch](source/Interrupt%20List/INT%2021%20DOS%20Function%20Calls/INT%20214C%20DOS%202%20EXIT%20TERMINATE%20WITH%20RETURN%20CODE.txt),
the summary starts with `INT 21`.
After this there may be a blank-separated field listing
all flag letters, if any.
(If multiple flags are used, there are no blanks
between the individual flag letters.)
Note that all used flags must be listed here again
because the header _Flag_ fields do not affect the compiled _LST_ file.

After this there's a blank and a dash and another blank.
The remainder of the line is the _Name_ of the entry.
It may start with one or multiple component or system identifiers separated
from the remaining _Name_ by another dash each.
Words other than proper nouns or identifiers are typically in all-caps.

The summary base does not include register values for the entry.

#### Input conditions

After the summary base line, input conditions may follow,
mostly listing register values to set up to call this entry's function.
All input conditions are indented with a tab
at the very beginning of the line.
[ecm's IntList](https://pushbx.org/ecm/web/#projects-intlist)
expects input registers to be in one of the following formats
to pick them up for its dynamic summary
and as register states for hyperlink destinations:

  * `<TAB><all-caps register name> = <hexadecimal value>` for a single value
  * `<TAB><reg name> = <hex> subfn <hex>` for register value and subfunction value
  * `<TAB><reg name> = <hex> ('<letters>')` for magic values, where the letters match the ASCII interpretation of the hexadecimal value in NASM or MASM order
  * `<TAB><reg name> = <hex> / <hex>` for multiple values
  * `<TAB><reg name> = <hex>..| to |-<hex>` for a value range
  * `<TAB><reg name> = subfunction|type of load|what to return in BH|origin of move` for a type register
  * Line starting with `---<NONDASH>`: Skipped
  * Line starting with `<NONTAB>`: End of dynamic summary buildup

Type registers enter a sub-mode where matches occur as follows:

  * `<TAB><4 BLANKs><hex>...` (trail ignored) gives one value for the type register
  * Line starting with `<TAB>` followed by different whitespace than exactly `<4 BLANKs>`: Skipped
  * Line starting with `<TAB><NONWHITESPACE>`: End of type register sub-mode
  * Line starting with `<NONTAB>`: End of dynamic summary buildup

#### Indented sections

After the input conditions, indented sections may follow.
These start with section names that are not indented with contents
indented by one tab or more.
Typically, sentences or paragraphs start with a 1-tab indentation
and continued paragraph parts use `<TAB><2 BLANKs>` indentation.
The section names are:

  * Return:
  * Desc:
  * Notes:
  * BUGS:
  * Program:
  * InstallCheck:
  * Range:

There is a special section name _SeeAlso:_.
Every line belonging to this section type starts with `SeeAlso:` followed
by one blank.
This section lists only references to other entries or tables.

#### Tables

After the named sections, tables may be listed.
Tables are always started by an empty line then an unindented line.
Near the beginning of a table definition,
the text `(Table Annnn)` always is used,
in which `A` is alphanumeric (all-caps) and `n` are decimal digits.

#### Hyperlinks

[IntList](https://pushbx.org/ecm/web/#projects-intlist)
uses the following patterns to match references as hyperlinks
within entries after the summary base line:

  * Name matches are matched as `"<N NONDOUBLEQUOTEs>"`, name matches are allowed after all references except for tables. Note that there must not be any whitespace before the opening doublequote.
  * Register values are matched as `<all-caps register name>=<hex>` without any whitespace
  * Multiple register values are chained with slashes `/`

The following patterns are turned into references:

  * Interrupts are matched as `INT<BLANK><hex>`
  * Interrupts with registers are matched as `INT<BLANK><hex>/<regvalues>`
  * Register-only references (only valid in INTERRUP.LST) are matched as `<regvalues>` where the first register must be one of AH, AL, or AX
  * In SeeAlso: lines, register-only references may occur for other first registers too
  * Tables are matched as `#<1 all-caps ALPHANUM><4 DECIMAL DIGITs>`
  * Memory is matched as `MEM<BLANK><hex>:<hex>` or `MEM<BLANK><hex>`
  * Far calls are matched as `@<hex>:<hex>`
  * Port ranges are matched as `PORT<BLANK><hex>-<hex>`
  * Single ports are matched as `PORT<BLANK><hex>`

After an interrupt with registers or register-only reference match,
an immediately following `,<hex>`
(with the comma not preceeded or followed by any whitespace)
is matched as a reference to the same call as the prior match
except the last register's value is replaced by the specified number.
