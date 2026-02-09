(*

  PasExt.pp is a unit for very common basic functionality in both console and
  desktop applications which not provided by the standard PFC units. Most of
  the routines are very simple and are provided to prevent the need to
  constantly re-implement them in a new program.

*)

{
   Copyright (c) 2025-2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit PasExt;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils;

type
  { LogMessage Verbosity Level }
  TVerbosity = (vbCritical, vbMinimal, vbNormal, vbVerbose, vbExcessive);
  { procedure type for custom output of LogMessage }
  TLogWriterProc = procedure (Message : String);

  { Options for DirScan function.
    dsFiles         include Files in list.
    dsDirectories   include Directories in list.
    dsRecursive     recursively search all sub-directories.
    dsHidden        include . hidden files and/or folders.
  reserved for future options:
    dsLinks         include file and/or directory links.
    dsDeadLinks     include broken links.
    dsCaseless      match files/dirs recardless of case.
    dsSystem        include system files/directories.
  }
  TDirScanOptions = set of (dsFiles, dsDirectories, dsLinks, dsDeadLinks,
    dsCaseless, dsRecursive, dsHidden, dsSystem);

  { Options for SystemPath function.
    spExecutable   = Location of executable.
    spApplication  = Location of application package or executable.
    spResources    = Location of application support files.
    spWorking      = Location of application working directory.
    spHome         = Location of user's home directory.
    spData         = Location of user specific data files.
    spDownloads    = Location for user download files.
  }
  TSystemPaths = (spExecutable, spApplication, spResources, spHome, spData,
    spWorking, spDownloads);

  { TArrayOfBoolean is an array of Boolean; }
  TArrayOfBoolean = array of Boolean;
  { TArrayOfByte is an array of Byte; }
  TArrayOfByte = array of Byte;
  { TArrayOfWord is an array of Word; }
  TArrayOfWord = array of Word;
  { TArrayOfInteger is an array of Integer; }
  TArrayOfInteger = array of Integer;
  { TArrayOfLongInt is an array of LongInt; }
  TArrayOfLongInt = array of LongInt;
  { TArrayOfChar is a TCharArray; Same as an array of Char; }
  TArrayOfChar = TCharArray;
  { TArrayOfChar is a TArrayOfChar; Same as an array of Char; }
  TASCIIzString = TArrayOfChar;
  { TArrayOfString is a TStringArray; Same as an array of String; }
  TArrayOfString = TStringArray;
  { TArrayOfRawByteString is an array of RawByteString; }
  TArrayOfRawByteString = array of RawByteString;
  { TArrayOfUnicodeString is an array of UnicodeString; }
  TArrayOfUnicodeString = array of UnicodeString;
  { TArrayOfAnsiString is an array of AnsiString; }
  TArrayOfAnsiString = array of AnsiString;
  { TArrayOfPointers is an array of Pointer; }
  TArrayOfPointers = array of Pointer;
  { TArrayOfInt8 is an array of Int8; Same as an array of ShortInt; }
  TArrayOfInt8 = array of Int8;
  { TArrayOfInt16 is an array of Int16; Same as an array of SmallInt; }
  TArrayOfInt16 = array of Int16;
  { TArrayOfInt32 is an array of Int32; Same as an array of LongInt; }
  TArrayOfInt32 = array of Int32;
  { TArrayOfInt64 is an array of Int64; }
  TArrayOfInt64 = array of Int64;
  { TArrayOfUInt8 is an array of UInt8; Same as an array of Byte; }
  TArrayOfUInt8 = array of UInt8;
  { TArrayOfUInt16 is an array of UInt16; Same as an array of Word; }
  TArrayOfUInt16 = array of UInt16;
  { TArrayOfUInt32 is an array of UInt32; Same as an array of Cardinal/LongWord; }
  TArrayOfUInt32 = array of UInt32;
  { TArrayOfUInt64 is an array of UInt64; Same as an array of QWord; }
  TArrayOfUInt64 = array of UInt64;

const
  { CR is a constant for ASCII value 13 (aka 0x0D). A Carriage Return character. }
  CR          = #$0d;
  { LF is a constant for ASCII value 10 (aka 0x0A). A Line Feed character. }
  LF          = #$0a;
  { CRLF is a constant for both CR + LF. The common file line ending in most
   DOS and Windows text files. }
  CRLF        = CR + LF;
  { SPACE is a constant for ASCII value 32 (aka 0x20). A Space character. }
  SPACE       = #$20;
  { BACKSPACE is a constant for ASCII value 8 (aka 0x08). A BAckspace character. }
  BACKSPACE   = #$08;
  { TAB is a constant for ASCII value 9 (aka 0x09). A Tab character. }
  TAB         = #$09;
  { AMPERSAND is a constant for ASCII value 38 (aka 0x26). A Amphersand (&) character. }
  AMPERSAND   = #$26;
  { LESSTHAN is a constant for ASCII value 60 (aka 0x3C). A Lesser Than (<) character. }
  LESSTHAN    = #$3c;
  { GREATERTHAN is a constant for ASCII value 62 (aka 0x3E). A Greater Than (>) character. }
  GREATERTHAN = #$3e;
  { COMMA is a constant for ASCII value 44 (aka 0x2C). A Comma (,) character. }
  COMMA       = #$2c;
  { COLON is a constant for ASCII value 58 (aka 0x3A). A Colon (:) character. }
  COLON       = #$3a;
  { SEMICOLON is a constant for ASCII value 59 (aka 0x3B. A Semi-colon (;) character. }
  SEMICOLON   = #$3b;
  { UNDERSCORE is a constant for ASCII value 95 (aka 0x5F. A Underscore (_) character. }
  UNDERSCORE  = #$5f;
  { PERIOD is a constant for ASCII value 46 (aka 0x2E. A Period (.) character. }
  PERIOD      = #$2e;
  { QUOTESINGLE is a constant for ASCII value 39 (aka 0x27. A Single Quote (') character. }
  QUOTESINGLE = #$27;
  { QUOTEDOUBLE is a constant for ASCII value 34 (aka 0x22. A Double Quote (") character. }
  QUOTEDOUBLE = #$22;
  { HYPHEN is a constant for ASCII value 45 (aka 0x2D. A Hyphen (-) character which is technically a Minus Symbol. }
  HYPHEN      = #$2d;
  { EQUAL is a constant for ASCII value 61 (aka 0x3D. A Period (=) character. }
  EQUAL       = #$3d;
  { DELETECHAR is a constant for ASCII value 127 (aka 0x7F. A Delete character. }
  DELETECHAR  = #$7f;

  { TAB1 is a constant for a single TAB character. Same as the TAB constant. }
  TAB1        = TAB;
  { TAB2 is a constant for a series of 2 consecutive TAB characters. }
  TAB2        = TAB1 + TAB;
  { TAB3 is a constant for a series of 3 consecutive TAB characters. }
  TAB3        = TAB2 + TAB;
  { TAB4 is a constant for a series of 4 consecutive TAB characters. }
  TAB4        = TAB3 + TAB;
  { TAB5 is a constant for a series of 5 consecutive TAB characters. }
  TAB5        = TAB4 + TAB;
  { TAB6 is a constant for a series of 6 consecutive TAB characters. }
  TAB6        = TAB5 + TAB;
  { TAB7 is a constant for a series of 7 consecutive TAB characters. }
  TAB7        = TAB6 + TAB;
  { TAB8 is a constant for a series of 8 consecutive TAB characters. }
  TAB8        = TAB7 + TAB;
  { SPACE1 is a constant for a single SPACE character. Same as the SPACE constant. }
  SPACE1      = SPACE;
  { SPACE2 is a constant for a series of 2 consecutive SPACE characters. }
  SPACE2      = SPACE1 + SPACE;
  { SPACE3 is a constant for a series of 3 consecutive SPACE characters. }
  SPACE3      = SPACE2 + SPACE;
  { SPACE4 is a constant for a series of 4 consecutive SPACE characters. }
  SPACE4      = SPACE3 + SPACE;
  { SPACE5 is a constant for a series of 5 consecutive SPACE characters. }
  SPACE5      = SPACE4 + SPACE;
  { SPACE6 is a constant for a series of 6 consecutive SPACE characters. }
  SPACE6      = SPACE5 + SPACE;
  { SPACE7 is a constant for a series of 7 consecutive SPACE characters. }
  SPACE7      = SPACE6 + SPACE;
  { SPACE8 is a constant for a series of 8 consecutive SPACE characters. }
  SPACE8      = SPACE7 + SPACE;

  MaxInteger  = MaxLongInt;

  { Additional directory search attribute to match anything including active
  and dead symlinks. }
  faAnything  = MaxLongInt or faAnyFile;

  {$IFDEF WINDOWS}
    PathDelimiter = '\';
  {$ELSE}
    PathDelimiter = '/';
  {$ENDIF}

  { Command line switch character. }
  SwitchChar : RawByteString = HYPHEN;

  { Directory searching WildCard }
  WildCard = '*';

  { Verbosity Level of LogMessage for text output to log }
  VerboseLevel : TVerbosity = vbNormal;

  { The maximum number of links in a chain to follow when trying to resolve the
    final target of a link before giving up. This is a simple way to prevent
    becoming stuck in an endless loop if a link in the chain happens to point
    back to a previous link in the chain. I think 35 is a reasonable maximum. }
  MaxLinkDepth : integer = 35;

var
  { Variable to procedure which displays or logs a message when the current
    VerboseLevel is greater or equal to the current VerbosityLevel. By default,
    tis function is set to a routine which outputs text to the console. }
   LogWriter : TLogWriterProc;

   { Path of the program executable }
   AppExecPath : RawByteString;
   { Path of the console executable or desktop application package on macOS. }
   AppBasePath : RawByteString;
   { The location of files needed by the application. Generally, this is the
     same directory as the executable. The primary exceptions are for macOS
     application bundles which is inside the bundle in the Resources directory. }
   AppDataPath : RawByteString;
   { The User's Home Directory }
   UserHomePath : RawByteString;
   (* The default location for application related user files. It varies widely
     based on the Operating System. Under macOS, Linux and Unix, it is basically
     the same as "${HOME}/.${$0##*/}/". On Windows, it is based on the
     LOCALAPDATA environment variable *)
   UserDataPath : RawByteString;
   { Current working directory for the user. At startup, this is set to the
     users home directory and can be used in an application to maintain a
     consistent filesystem directory location for dialogs. }
   UserWorkPath : RawByteString;
   { The user's language identifier string. }
   UserLanguage : RawByteString;
   { The user's time-zone string. }
   UserTimeZone : RawByteString;

(* Logging Functions *)

  procedure LogMessage(Verbosity : TVerbosity; Message : String); overload;
  procedure LogMessage(Verbosity : TVerbosity; Message : String;
    Data : TArrayOfByte; BitWidth : integer = 0); overload;
  procedure LogMessage(Verbosity : TVerbosity; Message : String;
    Data : TArrayOfWord); overload;
  procedure LogMessage(Verbosity : TVerbosity; Message : String;
    Data : TArrayOfInt64); overload;

(* Unicode and UTF-8 related functions *)

  { Return true if any Unicode characters are detected in S.
    Warning: Some languages (like Cyrillic based) always return True. Even if
    the actual text is not encoded as Unicode or UTF-8. A more accurate method
    is to use the UTF8ToValues and convert the string to values. If the
    resulting array and string have different lengths, then the string is
    encoded as Unicode/UTF-8. }
  function isUnicode(const S : RawByteString) : boolean;
  { Return true if any Unicode characters are detected in S.
    Warning: Some languages (like Cyrillic based) always return True. Even if
    the actual text is not encoded as Unicode or UTF-8. A more accurate method
    is to use the UTF8ToValues and convert the string to values. If the
    resulting array and string have different lengths, then the string is
    encoded as Unicode/UTF-8. }
  function isUnicode(const S : UnicodeString) : boolean;

  { Return S as a UnicodeString. If Unicode Characters are present, raise an exception. }
  function asUnicode(const S :RawByteString) : UnicodeString;
  { Return S as a RawByteString. If Unicode Characters are present, raise an exception. }
  function asRawByte(const S :UnicodeString) : RawByteString;

  { Converts a UTF-8 String into an array of its character values. Returns True
    if there were no encoding errors in the string. If there were encoding
    errors, it will return False and still try to convert the the String giving
    those characters a value of -1. }
  function UTF8ToValues(const UTF : UTF8String; out Values : TArrayOfInt32) : boolean; overload;
  { Converts a UTF-8 String stored in a RawByteString into an array of its
    character values. Returns True if there were no encoding errors in the
    string. If there were encoding errors, it will return False and still
    try to convert the the String giving those characters a value of -1. }
  function UTF8ToValues(const S : RawByteString; out Values : TArrayOfInt32) : boolean; overload;
  function UTF8ToValues(const A : TArrayOfByte; out Values : TArrayOfInt32) : boolean; overload;

  { Converts an array of character values into an UTF-8 String. If there were
    no values outside of range 0 - 0x10ffff, it will return True. Otherwise,
    it will return False while assigning such characters teh BadChar value. }
  function ValuesToUTF8(const Values : TArrayOfInt32; out UTF : UTF8String;
    BadChar : Int32 = 0 ) : boolean;

(* Some very simple convenience functions and procedures *)

  { convert a TArrayOfByte to a RawByteString }
  function ToString(const V : TArrayOfByte) : RawByteString; overload;
  { convert a string to an array of bytes }
  function ToBytes(const S : RawByteString) : TArrayOfByte; overload;
  { convert a string to an array of bytes }
  function ToBytes(const S : UnicodeString) : TArrayOfByte; overload;

  { Appends one string onto another. V:=V+A; }
  procedure Cat(var V : RawByteString; const A : RawByteString); overload;
  { Appends one string onto another. V:=V+A; }
  procedure Cat(var V : UnicodeString; const A : UnicodeString); overload;
  { Appends one string onto another. V:=V+A; }
  procedure Cat(var V : AnsiString; const A : AnsiString); overload;

  { Appends one string onto then end of an array of strings. V:=V+A; }
  procedure Cat(var V : TArrayOfRawByteString; const A : RawByteString); overload;
  { Appends one string onto then end of an array of strings. V:=V+A; }
  procedure Cat(var V : TArrayOfUnicodeString; const A : UnicodeString); overload;
  { Appends one string onto then end of an array of strings. V:=V+A; }
  procedure Cat(var V : TArrayOfAnsiString; const A : AnsiString); overload;

  { Appends one array of strings onto then end of another array of strings. V:=V+A; }
  procedure Cat(var V : TArrayOfRawByteString; const A : TArrayOfRawByteString); overload;
  { Appends one array of strings onto then end of another array of strings. V:=V+A; }
  procedure Cat(var V : TArrayOfUnicodeString; const A : TArrayOfUnicodeString); overload;
  { Appends one array of strings onto then end of another array of strings. V:=V+A; }
  procedure Cat(var V : TArrayOfAnsiString; const A : TArrayOfAnsiString); overload;

  { Appends a boolean to an array of booleans }
  procedure Cat(var V : TArrayOfBoolean; const A : Boolean); overload;
  { Appends a byte to an array of bytes }
  procedure Cat(var V : TArrayOfByte; const A : Byte); overload;
  { Appends a word to an array of words }
  procedure Cat(var V : TArrayOfWord; const A : Word); overload;
  { Appends a Integer to an array of Integers }
  procedure Cat(var V : TArrayOfInteger; const A : Integer); overload;
  { Appends a LongInt to an array of LongInts }
  procedure Cat(var V : TArrayOfLongInt; const A : LongInt); overload;
  { Appends a Character to an array of Chars }
  procedure Cat(var V : TArrayOfChar; const A : Char); overload;
  { Appends a String to an array of Strings }
  procedure Cat(var V : TArrayOfString; const A : String); overload;
  { Appends a Pointer to an array of Pointers }
  procedure Cat(var V : TArrayOfPointers; const A : Pointer); overload;
  { Appends a Int8 to an array of Int8s }
  procedure Cat(var V : TArrayOfInt8; const A : Int8); overload;
  { Appends a Int16 to an array of Int16s }
  procedure Cat(var V : TArrayOfInt16; const A : Int16); overload;
  { Appends a Int32 to an array of Int32s }
  procedure Cat(var V : TArrayOfInt32; const A : Int32); overload;
  { Appends a Int64 to an array of Int64s }
  procedure Cat(var V : TArrayOfInt64; const A : Int64); overload;
  { Appends a UInt8 to an array of UInt8s }
  procedure Cat(var V : TArrayOfUInt8; const A : UInt8); overload;
  { Appends a UInt16 to an array of UInt16s }
  procedure Cat(var V : TArrayOfUInt16; const A : UInt16); overload;
  { Appends a UInt32 to an array of UInt32s }
  procedure Cat(var V : TArrayOfUInt32; const A : UInt32); overload;
  { Appends a UInt64 to an array of UInt64s }
  procedure Cat(var V : TArrayOfUInt64; const A : UInt64); overload;

  { Returns the length of the longest string in a TStringList.
    If ReturnIndex is True, the Index of the First Longest string is returned.
    If the TStringList is empty, -1 will be rewturned. }
  function Longest(const S : TStringList; ReturnIndex : boolean = false) : integer; overload;
  { Returns the length of the longest string in a TArrayOfRawByteString.
    If ReturnIndex is True, the Index of the First Longest string is returned.
    If the TArrayOfRawByteString is empty, -1 will be rewturned. }
  function Longest(const S : TArrayOfRawByteString; ReturnIndex : boolean = false) : integer; overload;
  { Returns the length of the longest string in a TArrayOfUnicodeString.
    If ReturnIndex is True, the Index of the First Longest string is returned.
    If the TArrayOfUnicodeString is empty, -1 will be rewturned. }
  function Longest(const S : TArrayOfUnicodeString; ReturnIndex : boolean = false) : integer; overload;
  { Returns the length of the longest string in a TArrayOfAnsiString.
    If ReturnIndex is True, the Index of the First Longest string is returned.
    If the TArrayOfAnsiString is empty, -1 will be rewturned. }
  function Longest(const S : TArrayOfAnsiString; ReturnIndex : boolean = false) : integer; overload;

  { When TF is TRUE, return T. Otherwise, return F as result. }
  function WhenTrue(TF : Boolean; const T : RawByteString;
    const F : RawByteString = '') : RawByteString; overload;
  { When TF is TRUE, return T. Otherwise, return F as result. }
  function WhenTrue(TF : Boolean; const T : UnicodeString;
    const F : UnicodeString = '') : UnicodeString; overload;
  { When TF is TRUE, return T. Otherwise, return F as result. }
  function WhenTrue(TF : Boolean; T : Integer; F : Integer = 0) : integer; overload;

  function BoolStr(TF : Boolean; T : String = 'True'; F : String = 'False') : String; overload;

  { When count is 1 return Single string. Otherwise, return Multiple String. }
  function Plural(Value : Int64; Multiple : RawByteString;
    Single: RawByteString = '' ) : RawByteString; overload;
  { When count is 1 return Single string. Otherwise, return Multiple String. }
  function Plural(Value : Int64; Multiple: UnicodeString;
    Single : UnicodeString = '') : UnicodeString; overload;

  { swap values for A and B }
  procedure Exchange(var A, B : Char); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : UTF8String); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : AnsiString); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : RawByteString); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : UnicodeString); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : Pointer); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : TObject); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : Int8); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : UInt8); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : Int16); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : UInt16); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : Int32); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : UInt32); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : Int64); overload;
  { swap values for A and B }
  procedure Exchange(var A, B : UInt64); overload;

  { Compare two arrays. If they are identical result is 0. The first unequal
    element will result in either -1 or +1. If you provide a value for Count,
    then only that number of elements will be tested. If Count is zero, all
    array elements will be tested and also wether or not the arrays are equal
    length. Length is tested after each element is compared. Once the elements
    are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfByte; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfWord; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfInteger; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfLongInt; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfInt8; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfInt16; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfInt32; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfInt64; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfUInt8; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfUInt16; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfUInt32; Count : integer = 0) : integer; overload;
  { Compare two arrays. If they are identical result is 0. The first unequal
     element will result in either -1 or +1. If you provide a value for Count,
     then only that number of elements will be tested. If Count is zero, all
     array elements will be tested and also wether or not the arrays are equal
     length. Length is tested after each element is compared. Once the elements
     are compared, if a array is longer or shorter will result in -1 or +1. }
  function Compare(var A, B : TArrayOfUInt64; Count : integer = 0) : integer; overload;
  { Compare two variables. If they are identical result is 0. The first unequal
     byte will result in either -1 or +1. }
  function Compare(var A, B; Size : integer) : integer; overload;

  { return the first character of a string. If string is empty, #0 is returned. }
  function FirstChar(S : RawByteString) : Char; overload;

(* String Padding functions *)

  { returns a RawByteString string of multiple S that is at least MinLength
    of characters.  If Crop is true, returned RawByteString will be cropped
    to MinLength. }
  function StringOf(S : RawByteString; MinLength : integer;
    Crop : boolean = true) : RawByteString; overload;
  { returns a UnicodeString of multiple S that is at least MinLength of
    characters. If Crop is true, returned UnicodeString will be cropped
    to MinLength. }
  function StringOf(S : UnicodeString; MinLength : integer;
    Crop : boolean = true) : UnicodeString; overload;
  { returns S that has Padding added to the front until it is at least
    MinLength. If S is already at least MinLength, no padding will
    be inserted to the front of S. }
  function LeftPad(S : RawByteString; MinLength : integer;
    Padding : RawByteString = SPACE) : RawByteString; overload;
  { returns S that has Padding added to the front until it is at least
    MinLength. If S is already at least MinLength, no padding will
    be inserted to the front of S. }
  function LeftPad(S : UnicodeString; MinLength : integer;
    Padding : UnicodeString = SPACE) : UnicodeString; overload;
  { returns S that has Padding added to the tail until it is at least
    MinLength. If S is already at least MinLength, no padding will
    be appended to the end of S. }
  function RightPad(S : RawByteString; MinLength : integer;
    Padding : RawByteString = SPACE) : RawByteString; overload;
  { returns S that has Padding added to the tail until it is at least
    MinLength. If S is already at least MinLength, no padding will
    be appended to the end of S. }
  function RightPad(S : UnicodeString; MinLength : integer;
    Padding : UnicodeString = SPACE) : UnicodeString; overload;
  { When S is less than MinLength, Padding will be added to the end
    of S and then to the front of S until it is at least MinlLength. }
  function CenterPad(S : RawByteString; MinLength : integer;
    Padding : RawByteString = SPACE) : RawByteString; overload;
  { When S is less than MinLength, Padding will be added to the end
    of S and then to the front of S until it is at least MinlLength. }
  function CenterPad(S : UnicodeString; MinLength : integer;
    Padding : UnicodeString = SPACE) : UnicodeString; overload;
  { return Value as a RawByteString that is at least MinLength by
    inserting 0 in front when needed. }
  function ZeroPad(Value : LongInt; MinLength : integer) : RawByteString; overload;
  function ZeroPad(S : RawByteString; MinLength : integer) : RawByteString; overload;

(* String cutting related functions *)

  { Returns portion of a S up to the Delim. If the Delim is not present,
  the entire string is returned. The returned string plus the Delim is removed
  from S. }
  function PopDelim(var S : RawByteString; Delim: RawByteString = SPACE): RawByteString; overload;
  { Returns portion of a S up to the Delim. If the Delim is not present,
  the entire string is returned. The returned string plus the Delim is removed
  from S. }
  function PopDelim(var S : UnicodeString; Delim: UnicodeString = SPACE): UnicodeString; overload;
  { Returns portion of a S up to the Delim. If the Delim is not present,
   the entire string is returned. The returned string plus the Delim is removed
   from S. }
  function PopDelim(var S : AnsiString; Delim: AnsiString = SPACE): AnsiString; overload;

  { Returns specific fields in a RawByteString seperated by Delim. If Last is -1, then
  all remaining fields starting with First are returned. }
  function CutDelim(S, Delim : RawByteString; First : integer; Last : integer = -1) : RawByteString; overload;
  { Returns specific fields in a RawByteString seperated by Delim. If Last is -1, then
   all remaining fields starting with First are returned. }
  function CutDelim(S, Delim : UnicodeString; First : integer; Last : integer = -1) : UnicodeString; overload;

  { returns the portion of a string between BeginStr and EndStr. If either
    BeginStr or EndStr are not present, the result will be a null string. }
  function Excise(const S, BeginStr, EndStr : RawByteString; CaseSpecific : boolean = true) : RawByteString; overload;
  { returns the portion of a string between BeginStr and EndStr. If either
    BeginStr or EndStr are not present, the result will be a null string. }
  function Excise(const S, BeginStr, EndStr : UnicodeString; CaseSpecific : boolean = true) : UnicodeString; overload;

  { Returns True if a S begins with SubStr }
  function HasLeading(const S, SubStr : RawByteString; CaseSpecific : boolean = true) : boolean; overload;
  { Returns True if a S begins with SubStr }
  function HasLeading(const S, SubStr : UnicodeString; CaseSpecific : boolean = true) : boolean; overload;

  { Returns True if a S ends with SubStr }
  function HasTrailing(const S, SubStr : RawByteString; CaseSpecific : boolean = true) : boolean; overload;
  { Returns True if a S ends with SubStr }
  function HasTrailing(const S, SubStr : UnicodeString; CaseSpecific : boolean = true) : boolean; overload;

  { Returns True if a S begins with StartStr and ends with EndStr }
  function HasEnds(const S, StartStr, EndStr : RawByteString; CaseSpecific : boolean = true) : boolean; overload;
  { Returns True if a S begins with StartStr and ends with EndStr }
  function HasEnds(const S, StartStr, EndStr : UnicodeString; CaseSpecific : boolean = true) : boolean; overload;

  { Returns S without a leading SubStr when present. Or, entire S. }
  function ExcludeLeading(const S, SubStr : RawByteString; CaseSpecific : boolean = true) : RawByteString; overload;
  { Returns S without a leading SubStr when present. Or, entire S. }
  function ExcludeLeading(const S, SubStr : UnicodeString; CaseSpecific : boolean = true) : UnicodeString; overload;

  { Returns S without a trailing SubStr when present. Or, entire S. }
  function ExcludeTrailing(const S, SubStr : RawByteString; CaseSpecific : boolean = true) : RawByteString; overload;
  { Returns S without a trailing SubStr when present. Or, entire S. }
  function ExcludeTrailing(const S, SubStr : UnicodeString; CaseSpecific : boolean = true) : UnicodeString; overload;

  { Returns S adding leading SubStr when not present.  }
  function IncludeLeading(const S, SubStr : RawByteString; CaseSpecific : boolean = true) : RawByteString; overload;
  { Returns S adding leading SubStr when not present.  }
  function IncludeLeading(const S, SubStr : UnicodeString; CaseSpecific : boolean = true) : UnicodeString; overload;

  { Returns S adding trailing SubStr when not present. }
  function IncludeTrailing(const S, SubStr : RawByteString; CaseSpecific : boolean = true) : RawByteString; overload;
  { Returns S adding trailing SubStr when not present. }
  function IncludeTrailing(const S, SubStr : UnicodeString; CaseSpecific : boolean = true) : UnicodeString; overload;

  { Returns S without leading StartStr either trailing EndStr.  }
  function ExcludeEnds(const S, StartStr, EndStr : RawByteString; CaseSpecific : boolean = true) : RawByteString; overload;
  { Returns S without leading StartStr either trailing EndStr.  }
  function ExcludeEnds(const S, StartStr, EndStr : UnicodeString; CaseSpecific : boolean = true) : UnicodeString; overload;

  { glue the elements of a RawByteString array together using Delim and return
    a RawByteString }
  function Implode(Strs : TArrayOfRawByteString; Delim : RawByteString = CR) : RawByteString; overload;
  { glue the elements of a UnicodeString array together using Delim and return
    a UnicodeString }
  function Implode(Strs : TArrayOfUnicodeString; Delim : UnicodeString = CR) : UnicodeString; overload;
  { glue the Items of a TStringList together using Delim and return a RawByteString }
  function Implode(Strs : TStringList; Delim : RawByteString = CR) : RawByteString; overload;

  { breaks RawByteString into an array of RawByteString at each Delim }
  function Explode(S : RawByteString; Delim : RawByteString = CR):TArrayOfRawByteString; overload;
  { breaks UnicodeString into an array of UnicodeString at each Delim }
  function Explode(S : UnicodeString; Delim : UnicodeString = CR):TArrayOfUnicodeString; overload;
  { breaks RawByteString into a new TStringList at each Delim. Caller must destroy the
    created TStringList. }
  function Explode(S : RawByteString; Delim : RawByteString = CR):TStringList; overload;
  { breaks RawByteString into an existing TStringList at each Delim. Any existing
    items in TStringList will be cleared. Caller must Create and Destroy the
    TStringList. }
  procedure Explode(S : RawByteString; var Strs : TStringList; Delim : RawByteString = CR); overload;

(* Miscellaneous string functions *)

  { Converts the first letter of each word to Upper Case. It does not convert
    other characters to Lower Case. A word is any Group of Letters separated
    by a non-letter. }
  function TitleCase(S : RawByteString) : RawByteString; overload;
  { Converts the first letter of each word to Upper Case. It does not convert
    other characters to Lower Case. A word is any Group of Letters separated
    by a non-letter. }
  function TitleCase(S : UnicodeString) : UnicodeString; overload;
  { Returns only the letters contained in a string. }
  function AlphaOnly(S : RawByteString) : RawByteString; overload;
  { Returns only the letters contained in a string. }
  function AlphaOnly(S : UnicodeString) : UnicodeString; overload;

  { returns a randomly generated string of Count length consisting of CharSet. If CharSet
  is not provided, then the default set '0123456789abcdefghijkmnpqrstuvwxyz' is used. The
  default set does not include lowercase L or O. }
  function RandomStr(Count : Integer; CharSet : RawByteString = '') : RawByteString; overload;
  { returns a randomly generated string of Count length consisting of CharSet. If CharSet
  is not provided, then the default set '0123456789abcdefghijkmnpqrstuvwxyz' is used. The
  default set does not include lowercase L or O. }
  function RandomStr(Count : Integer; CharSet : UnicodeString) : UnicodeString; overload;

  { escape text for usage in HTML by converting & < > and " to HTML entities }
  function EscapeHTML(const S : RawByteString) : RawByteString; overload;
  { escape text for usage in HTML by converting & < > and " to HTML entities }
  function EscapeHTML(const S : UnicodeString) : UnicodeString; overload;

  { opposite of EscapeHTML function }
  function UnEscapeHTML(const S : RawByteString) : RawByteString; overload;
  { opposite of EscapeHTML function }
  function UnEscapeHTML(const S : UnicodeString) : UnicodeString; overload;

  { convert line endings to an expected standard like CRLF, CR or LF }
  function NormalizeLineEndings(const S : RawByteString; Ending :
    RawByteString = LF) : RawByteString; overload;
  { convert line endings to an expected standard like CRLF, CR or LF }
  function NormalizeLineEndings(const S : UnicodeString; Ending :
    UnicodeString = LF) : UnicodeString; overload;

    { return true if Wild matches string }
  function WildMatch(Wild, S : RawByteString) : boolean; overload;
  { return true if Wild matches string, provides the Match portion of the string }
  function WildMatch(Wild, S : RawByteString; out Match : RawByteString) : boolean; overload;

(* File related functions *)

  { returns True if a FileName or it's path are not compliant with the DOS 8.3
  file naming convention. }
  function RequireLFN(FileName : RawByteString) : boolean; overload;
  { returns True if a FileName or it's path are not compliant with the DOS 8.3
  file naming convention. }
  function RequireLFN(FileName : UnicodeString) : boolean; overload;

  { Return a file or directory using the case-specific version on the filesystem.
  If the file does not exist and MustExist is true, a null RawByteString will be returned.
  If the file does not exist and MustExist is false, the existing portion will
  have its letter case adjusted to match the file name and the rest will remain
  as provided to the function. }
  function FileCase(FileName : RawByteString; MustExist : boolean = true) : RawByteString; overload;
  { Return a file or directory using the case-specific version on the filesystem.
  If the file does not exist and MustExist is true, a null RawByteString will be returned.
  If the file does not exist and MustExist is false, the existing portion will
  have its letter case adjusted to match the file name and the rest will remain
  as provided to the function. }
  function FileCase(FileName : UnicodeString; MustExist : boolean = true) : UnicodeString; overload;

  { Return a file or directory within a specific path using the case-specific
  version on the filesystem. The Parent portion is not adjusted or returned by
  the function. If the file does not exist and MustExist is true, a null RawByteString
  will be returned. If the file does not exist and MustExist is false, the
  existing portion will have its letter case adjusted to match the file name
  and the rest will remain as provided to the function. }
  function FileCase(FileName, Parent : RawByteString; MustExist : boolean = true) : RawByteString; overload;
  { Return a file or directory within a specific path using the case-specific
  version on the filesystem. The Parent portion is not adjusted or returned by
  the function. If the file does not exist and MustExist is true, a null RawByteString
  will be returned. If the file does not exist and MustExist is false, the
  existing portion will have its letter case adjusted to match the file name
  and the rest will remain as provided to the function. }
  function FileCase(FileName, Parent : UnicodeString; MustExist : boolean = true) : UnicodeString; overload;

  { Return the FileName without the Path or Extension }
  function ExtractFileBase(const FileName : RawByteString) : RawByteString; overload;
  { Return the FileName without the Path or Extension }
  function ExtractFileBase(const FileName : UnicodeString) : UnicodeString; overload;

  { return the size of a file. }
  function FileGetSize(FileName:RawByteString; out Size : Int64; FollowLinks : boolean=true) : boolean; overload;
  { return the size of a file. }
  function FileGetSize(FileName:UnicodeString; out Size : Int64; FollowLinks : boolean=true) : boolean; overload;

  { load file from disk into array, returns 0 or error code }
  function FileLoad(const FileName : RawByteString; out Data : TArrayOfByte) : integer; overload;
  { load file from disk into array, returns 0 or error code  }
  function FileLoad(const FileName : UnicodeString; out Data : TArrayOfByte) : integer; overload;

  { save a file to disk from an array, returns 0 or error code  }
  function FileSave(const FileName : RawByteString; const Data : TArrayOfByte) : integer; overload;
  { save a file to disk from an array, returns 0 or error code  }
  function FileSave(const FileName : UnicodeString; const Data : TArrayOfByte) : integer; overload;

  { copy a disk file from Src to Dst. Will fail if Dst file already exists and
  Overwrite is False. Returns 0 if file was copied. This function does not
  duplicate file permissions or ownership for the copied file. }
  function FileCopy(Src, Dst: RawByteString; Overwrite: boolean = true): integer; overload;
  { copy a disk file from Src to Dst. Will fail if Dst file already exists and
  Overwrite is False. Returns 0 if file was copied. This function does not
  duplicate file permissions or ownership for the copied file. }
  function FileCopy(Src, Dst: UnicodeString; Overwrite: boolean = true): integer; overload;
  { move a disk file from Src to Dst. Will fail if Dst file already exists and
  Overwrite is False. An attempt is made to simply "rename" the existing file
  to the new location. If that fails, it will then try to copy the file and
  then delete the original. Returns 0 if file was moved. This function will not
  preserve file permissions or ownership if a copy was required to perform the
  move. }
  function FileMove(Src, Dst: RawByteString; Overwrite: boolean = true): integer; overload;
  { move a disk file from Src to Dst. Will fail if Dst file already exists and
  Overwrite is False. An attempt is made to simply "rename" the existing file
  to the new location. If that fails, it will then try to copy the file and
  then delete the original. Returns 0 if file was moved. This function will not
  preserve file permissions or ownership if a copy was required to perform the
  move. }
  function FileMove(Src, Dst: UnicodeString; Overwrite: boolean = true): integer; overload;

  { binary compare two files. If both files exist and have identical contents
  then returns 0. If either file does not exist or an error occurs, then -1
  is returned. If the files have different contents, then 1 is returned if
  FileA is newer (or same) timestamp. 2 is returned if FileB is newer than
  FileA. }
  function FileCompare(FileA, FileB: RawByteString): integer; overload;
  { binary compare two files. If both files exist and have identical contents
  then returns 0. If either file does not exist or an error occurs, then -1
  is returned. If the files have different contents, then 1 is returned if
  FileA is newer (or same) timestamp. 2 is returned if FileB is newer than
  FileA. }
  function FileCompare(FileA, FileB: UnicodeString): integer; overload;

  { return the CRC32 checksum of a file. }
  function FileCRC32(FileName : RawByteString) : LongInt; overload;
  { return the CRC32 checksum of a file. }
  function FileCRC32(FileName : UnicodeString) : LongInt; overload;
  { return the MD5 hash of a file. }
  function FileMD5(FileName: RawByteString): RawByteString; overload;
  { return the MD5 hash of a file. }
  function FileMD5(FileName: UnicodeString): UnicodeString; overload;
  { return the SHA1 hash of a file. }
  function FileSHA1(FileName: RawByteString): RawByteString; overload;
  { return the SHA1 hash of a file. }
  function FileSHA1(FileName: UnicodeString): UnicodeString; overload;

  { return the next unused file name based on the one provided by possibly
  adding a Divider and incremented number to the file name. }
  function FileIterative(FileName : RawByteString; MinPad : integer = 0;
     AlwaysEnum : boolean = false; Divider : RawByteString = '-') : RawByteString; overload;
  { return the next unused file name based on the one provided by possibly
  adding a Divider and incremented number to the file name. }
  function FileIterative(FileName : UnicodeString; MinPad : integer = 0;
     AlwaysEnum : boolean = false; Divider : UnicodeString = '-') : UnicodeString; overload;

(* Directory related functions *)

  { return a specific file system path with trailing delimiter.
    spExecutable   = Location of executable.
    spApplication  = Location of application package or executable.
    spResources    = Location of application support files.
    spWorking      = Location of application working directory.
    spHome         = Location of user's home directory.
    spData         = Location of user specific data files.
    spDownloads    = Location for user download files.
  }
  function SystemPath(PathType : TSystemPaths) : RawByteString;

  { read directory contents and return a list of files and/or directories }
  function DirScan(FileSpec : RawByteString; Options : TDirScanOptions =
    [dsFiles, dsSystem]) : TArrayOfRawByteString; overload;
  { read directory contents and return a list of files and/or directories }
  function DirScan(FileSpec : UnicodeString; Options : TDirScanOptions =
    [dsFiles, dsSystem]) : TArrayOfUnicodeString; overload;

  { read directory contents and return a list of files and/or directories }
  procedure DirScan(FileSpec : RawByteString; out List : TArrayOfRawByteString;
    Options : TDirScanOptions = [dsFiles, dsSystem]); overload;
  { read directory contents and return a list of files and/or directories }
  procedure DirScan(FileSpec : UnicodeString; out List : TArrayOfUnicodeString;
    Options : TDirScanOptions = [dsFiles, dsSystem]); overload;
  { read directory contents and return a list of files and/or directories }
  procedure DirScan(FileSpec : RawByteString; var List : TStringList; Options
    : TDirScanOptions = [dsFiles, dsSystem]); overload;

  { Create a directory and any required parents directories }
  function CreateTree(Directory : RawByteString; CanExist : boolean = false) : boolean; overload;
  { Create a directory and any required parents directories }
  function CreateTree(Directory : UnicodeString; CanExist : boolean = false) : boolean; overload;
  { Deletes a directory and any existing files or subdirectories }
  function DeleteTree(Directory: RawByteString) : boolean; overload;
  { Deletes a directory and any existing files or subdirectories }
  function DeleteTree(Directory: UnicodeString) : boolean; overload;

  { Create a unique temporary directory . If the program is terminated and
  the temporary directory has not been deleted, it will be removed at that time.
  Returns the path of the temporary directory. If Parent is not specified,
  the system's user temp path will be used as the Parent. }
  function MkTempDir (Parent : RawByteString = ''): RawByteString; overload;
  { Create a unique temporary directory . If the program is terminated and
  the temporary directory has not been deleted, it will be removed at that time.
  Returns the path of the temporary directory. If Parent is not specified,
  the system's user temp path will be used as the Parent. }
  function MkTempDir (Parent : UnicodeString = ''): UnicodeString; overload;
  { Delete a temporary directory and any files or directories it contains. }
  procedure RmTempDir(TempDir : RawByteString); overload;
  { Delete a temporary directory and any files or directories it contains. }
  procedure RmTempDir(TempDir : UnicodeString); overload;

(* Filesystem Symlinks Related. At this time only macOS, Linux and Unix *)

  { return true if a file or directory item is a symlink }
  function IsLink(FileName : RawByteString) : boolean; overload;

  { returns the target of a link. if FinalTarget is true, it will
    return the final item in a link chain. If the link or chain is broken,
    it will return an empty string. }
  function LinkTarget(FileName : RawByteString; FinalTarget:Boolean=True) : RawByteString; overload;

  { returns the target of a link. }
  function ReadLink(FileName : RawByteString) : RawByteString; overload;

  { returns true if any part of a link chain is broken. }
  function IsDeadLink(FileName : RawByteString) : boolean; overload;

  { returns true if the link can be resolved to a file or directory. }
  function LinkExists(FileName : RawByteString) : boolean; overload;

  { create a new file system link }
  function CreateLink(Target, Link : RawByteString) : boolean; overload;

{ Nibbling on Bits }

  { Return the number of bytes required for a given number of bits. }
  function BitSize(Bits : word) : word; overload;

  { Count the number of set bits in a byte. }
  function BitCount(V : Byte; MaxBits : integer = -1) : integer; overload;
  { Count the number of set bits in a Word. }
  function BitCount(V : Word; MaxBits : integer = -1) : integer; overload;
  { Count the number of set bits in a DWord. }
  function BitCount(V : DWord; MaxBits : integer = -1) : integer; overload;
  { Count the number of set bits in an array of bytes. }
  function BitCount(A : TArrayOfByte; MaxBits : integer = -1) : integer; overload;

  { Return a packed array of bytes where each entry only uses a specified
  number of bits. An Item may consist of multiple bytes. }
  function BitPack(Data : TArrayOfByte; BitWidth : integer) : TArrayOfByte; overload;

  { Return an unpacked array of bytes that was packed by the BitPack function.
    Since an array that has been packed can have upto 7 unused bits at the end,
    you will likely need to use MaxBytes to fix the size of the array,
    especially when BitWidth is less than 8 bits. }
  function BitUnPack(Data : TArrayOfByte; BitWidth : integer;
    MaxBytes : integer = -1) : TArrayOfByte; overload;

{ A simply procedure to suppress compiler warning messages about unused parameters.
  This is generally not needed. But, sometimes Event Handler Methods do not need
  all the parameters passed to them. I don't like useless warning messages. }
  procedure IgnoreParameter(const Parameters);
{ A simple procedure to suppress compiler warning messages about unintialized
  variables. Normally, you don't want to fake initializing a variable. But,
  when dealing with older low level functions, they may return data through
  a 'var' parameter. Often, those parameters do not require initialization. }
  procedure PseudoInit(out Parameters);


implementation

(* Platform Specific Functiomns ------------------------------------------- *)

{$IFDEF WINDOWS}
uses CRC, MD5, SHA1, GetText, Windows;

{ TODO 5 -cDevel Implement IsLink function for Windows }
function IsLink(FileName : RawByteString) : boolean;
begin
  IgnoreParameter(FileName);
  IsLink:=False;
  raise Exception.Create('function "IsLink" not implemented for Windows.');
end;

{ TODO 5 -cDevel Implement LinkTarget function for Windows }
function LinkTarget(FileName : RawByteString; FinalTarget:Boolean=true) : RawByteString;
begin
  IgnoreParameter(FileName);
  IgnoreParameter(FinalTarget);
  LinkTarget:='';
  raise Exception.Create('function "LinkTarget" not implemented for Windows.');
end;

{ TODO 5 -cDevel Implement ReadLink function for Windows }
function ReadLink(FileName : RawByteString) : RawByteString;
begin
  IgnoreParameter(FileName);
  ReadLink:='';
   raise Exception.Create('function "ReadLink" not implemented for Windows.');
end;

{ TODO 5 -cDevel Implement IsDeadLink function for Windows }
function IsDeadLink(FileName : RawByteString) : boolean;
begin
  IgnoreParameter(FileName);
  IsDeadLink:=False;
  raise Exception.Create('function "IsDeadLink" not implemented for Windows.');
end;

{ TODO 5 -cDevel Implement LinkExists function for Windows }
function LinkExists(FileName : RawByteString) : boolean;
begin
  IgnoreParameter(FileName);
  LinkExists:=False;
  raise Exception.Create('function "LinkExists" not implemented for Windows.');
end;

{ TODO 5 -cDevel Implement CreateLink function for Windows }
function CreateLink(Target, Link : RawByteString) : boolean;
begin
  IgnoreParameter([Target, Link]);
  CreateLink:=False;
  raise Exception.Create('function "CreateLink" not implemented for Windows.');
end;

{$ELSE}

uses CRC, MD5, SHA1, baseUnix, Unix, Process
  {$IFDEF DARWIN}, MacOSAll {$ENDIF};

function IsLink(FileName : RawByteString) : boolean;
var
  S : Stat;
begin
  PseudoInit(S);
  IsLink := False;
  if fpLStat(FileName, S) = 0 then
    IsLink:=fpS_ISLNK(S.st_mode);
end;

function LinkTarget(FileName : RawByteString; FinalTarget:Boolean=True) : RawByteString;
var
  S : Stat;
  E : integer;
begin
  PseudoInit(S);
  // Set the maximum number of links in a chain.
  E := MaxLinkDepth;
  while E > 0 do begin
    Dec(E);
    LinkTarget := fpReadLink(FileName);
    // don't want final target of possible multiple link chain }
    if not FinalTarget then Break;
    // dead link check
    if LinkTarget='' then Break;
    // adjust path to be relative to previous when needed
    if LinkTarget[1] <> PathDelimiter then begin
      LinkTarget := ExtractFilePath(FileName) + LinkTarget;
    end;
    FileName:=LinkTarget;
    // check if it is valid
    if fpLStat(LinkTarget, S) <> 0 then begin
      LinkTarget:='';
      Break;
    end;
    // is it the end of chain?
    if not fpS_ISLNK(S.st_mode) then break;
  end;
end;

function ReadLink(FileName : RawByteString) : RawByteString;
begin
  ReadLink:=fpReadLink(FileName);
end;

function IsDeadLink(FileName : RawByteString) : boolean;
begin
  IsDeadLink := IsLink(FileName) and (LinkTarget(FileName, True)='');
end;

function LinkExists(FileName : RawByteString) : boolean;
begin
  LinkExists := IsLink(FileName) and (LinkTarget(FileName, True)<>'');
end;

function CreateLink(Target, Link : RawByteString) : boolean;
begin
  CreateLink:=fpSymLink(PChar(Target), PChar(Link)) = 0;
end;

{$ENDIF}

{$PUSH}
{ could be '$WARN 5024 off', but might change in later versions of FPC  }
{$HINTS OFF}
procedure IgnoreParameter(const Parameters);
begin
  { This procedure does nothing at all. }
end;

procedure PseudoInit(out Parameters);
begin
  { This procedure does nothing at all. }
end;
{$POP}

(* ------------------------------------------------------------------------ *)

const
  ArrayGrowFactor = 16;
  TransferBufSize = 32*1024;

var
   TempItems : TStringList;

procedure DoneUnit(Aborted:boolean);
var
  I : integer;
begin
  // Maybe, update to only show message when Clean-Up is required.
  if Aborted then begin
    WriteLn('Emergency shutdown clean-up.');
    if ExitCode=0 then ExitCode:=1;
  end;
  if Assigned(TempItems) then begin
    for I := TempItems.Count - 1 downto 0 do
      RmTempDir(TempItems[I]);
    FreeAndNil(TempItems);
  end;
end;

procedure HandleSigInt(aSignal: LongInt); cdecl;
begin
  if aSignal=2 then
    Writeln(' - Abort!');
  DoneUnit(True);
  Halt(ExitCode);
end;

procedure Finalize;
begin
  DoneUnit(False);
end;

procedure Initialize;
var
  {$if defined(darwin)}
    lbuf :  StringPtr;
  {$else}
    ltmp : String;
  {$endif}
begin
  Randomize;
  TempItems:=nil;
  // PathDelimiter:=IncludeTrailingPathDelimiter('');
  AppExecPath:=ExtractFilePath(ExpandFileName(ParamStr(0)));
  {$if defined(windows)}
    SwitchChar := '/';
    // WildCard := '*';
    AppBasePath:=AppExecPath;
    AppDataPath:=AppExecPath;
    UserHomePath := IncludeTrailingPathDelimiter(
      SysUtils.GetEnvironmentVariable('HOMEDRIVE') +
      SysUtils.GetEnvironmentVariable('HOMEPATH'));
    UserDataPath  := IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('LOCALAPPDATA')) +
      ExcludeTrailing(ExtractFileName(ParamStr(0)), '.EXE', false));
    ltmp:='';
    GetLanguageIDs(AnsiString(UserLanguage), ltmp);
    UserTimeZone:='GMT';
  {$elseif defined(darwin)}
    FpSignal(SigInt, @HandleSigInt); // Install Control-C Handler
    // SwitchChar := '-';
    // WildCard := '*';
    AppBasePath:=ExtractFilePath(ExcludeTrailing(AppExecPath,
      '.app/Contents/MacOS/'));
    if AppBasePath=AppExecPath then
      AppDataPath:=AppExecPath
    else
      AppDataPath:=ExcludeTrailing(AppExecPath,'MacOS/') + 'Resources/';
    UserHomePath:=IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('HOME'));
    UserDataPath:=IncludeTrailingPathDelimiter(UserHomePath + '.' + ExtractFileName(ParamStr(0)));
    { MultiApp changes this to:
    UserDataPath:=UserHomePath +
     'Library' + PathDelimiter +
     'Application Support' + PathDelimiter +
     APP_IDENTIFIER + PathDelimiter +
     APP_PRODUCTVERSION + PathDelimiter;
    }
    UserTimeZone:=UpperCase(tzname[tzdaylight]);
    lbuf := New(StringPtr);
    if CFStringGetPascalString(CFLocaleGetIdentifier(CFLocaleCopyCurrent),
      lbuf, Sizeof(lbuf^), 0) then UserLanguage := String(lbuf^);
    Dispose(lbuf);
  {$elseif defined(linux) or defined(unix)}
    FpSignal(SigInt, @HandleSigInt); // Install Control-C Handler
    // SwitchChar := '-';
    // WildCard := '*';
    AppBasePath:=AppExecPath;
    AppDataPath:=AppExecPath;
    UserHomePath:=IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('HOME'));
    UserDataPath:=IncludeTrailingPathDelimiter(UserHomePath + '.' + ExtractFileName(ParamStr(0)));
    UserTimeZone:=UpperCase(tzname[tzdaylight]);
    ltmp := CutDelim(GetEnvironmentVariable('LANG'), '.', 1,1);
    if ltmp <> '' then UserLanguage := ltmp;
  {$else}
    {$ERROR platform not supported }
  {$endif}
  UserWorkPath:=UserHomePath;
end;

procedure LogMessage(Verbosity: TVerbosity; Message: String);
begin
  if Verbosity <= VerboseLevel then LogWriter(Message);
end;

procedure LogBits(Verbosity: TVerbosity; Message: String; Data: TArrayOfByte;
  BitWidth : integer);
var
  I, B, P, M, C, Z, R : Integer;
  S : String;
begin
  LogMessage(Verbosity, Message + ' (' + IntToStr(Length(Data)) + ' byte' +
    WhenTrue(Length(Data) <> 1, 's') + ')');
  for I := 1 to Length(Message) do
    if (Message[I] <> TAB) and (Message[I] <> SPACE) then begin
      SetLength(Message, I - 1);
      Break;
    end;
  Cat(Message, TAB);
  Z:=Length(IntToStr(Length(Data) * 8 div BitWidth - 1));
  M := Length(Data);
  if M = 0 then Exit;
  LogMessage(Verbosity, Message+'(From Low to High, not as binary values!)');
  I:=0;
  B:=0;
  R:=0;
  repeat
    S:='';
    P:=0;
    C:=0;
    While (P < BitWidth) do begin
      Cat(S,WhenTrue(Data[I] and (1 shl B) <> 0, '1', '0'));
      Inc(P);
      Inc(B);
      Inc(C);
      if B > 7 then begin
        B:=0;
        Inc(I);
        if I >= M then Break;
        if P < BitWidth then Cat(S, ' - ');
      end;
    end;
    LogMessage(Verbosity, Message+'{' + ZeroPad(R,Z) + ':' + IntToStr(C) + '} '+TAB+S);
    Inc(R);
  until I >= M;
end;

procedure LogArray(Verbosity: TVerbosity; Message: String; Data: TArrayOfByte
  );
var
  I : Integer;
  S : String;
begin
  S:='';
  for I := 0 to Length(Data) - 1 do begin
    if S <> '' then S := S + ', ';
    S:=S + HexStr(Data[I], SizeOf(Data[I]) * 2);
  end;
  S:=Message + ' [' + S + ']:' + IntToStr(Length(Data));
  LogMessage(Verbosity, S);
end;

procedure LogMessage(Verbosity: TVerbosity; Message: String; Data: TArrayOfByte;
  BitWidth : integer );
begin
  if BitWidth > 0 then
    LogBits(Verbosity, Message, Data, BitWidth)
  else
    LogArray(Verbosity, Message, Data);
end;

procedure LogMessage(Verbosity: TVerbosity; Message: String; Data: TArrayOfWord
  );
var
  I : Integer;
  S : String;
begin
  S:='';
  for I := 0 to Length(Data) - 1 do begin
    if S <> '' then S := S + ', ';
    S:=S + HexStr(Data[I], SizeOf(Data[I]) * 2);
  end;
  S:=Message + ' [' + S + ']:' + IntToStr(Length(Data));
  LogMessage(Verbosity, S);
end;

procedure LogMessage(Verbosity: TVerbosity; Message: String; Data: TArrayOfInt64
  );
var
  I : Integer;
  S : String;
begin
  S:='';
  for I := 0 to Length(Data) - 1 do begin
    if S <> '' then S := S + ', ';
    S:=S + HexStr(Data[I], SizeOf(Data[I]) * 2);
  end;
  S:=Message + ' [' + S + ']:' + IntToStr(Length(Data));
  LogMessage(Verbosity, S);
end;

function isUnicode(const S: RawByteString): boolean;
{$IFNDEF FIX_UTF8}
var
  I : integer;
{$ENDIF}
begin
  {$IFNDEF FIX_UTF8}
  isUnicode:=True;
  for I := 1 to Length(S) do
    if Byte(S[I]) and $80 = $80 then Exit;
  isUnicode:=False;
  {$ELSE}
    isUnicode:=Length(S) <> Length(UnicodeString(S));
  {$ENDIF}
end;

function isUnicode(const S: UnicodeString): boolean;
begin
  {$IFNDEF FIX_UTF8}
  isUnicode:=isUnicode(RawByteString(S));
  {$ELSE}
    isUnicode:=Length(S) <> Length(RawByteString(S));
  {$ENDIF}
end;

function asUnicode(const S: RawByteString): UnicodeString;
{$IFNDEF FIX_UTF8}
var
  X : RawByteString;
{$ENDIF}
begin
  {$IFNDEF FIX_UTF8}
  //  check for typecast error
  X:=RawByteString(UnicodeString(S));
  if (X<>S) or isUnicode(S) then
  {$ELSE}
  if isUnicode(S) then
  {$ENDIF}
    raise Exception.Create('string "' + S + '" cannot be safely converted to Unicode.');
  asUnicode:=UnicodeString(S);
end;

function asRawByte(const S: UnicodeString): RawByteString;
{$IFNDEF FIX_UTF8}
var
  X : UnicodeString;
{$ENDIF}
begin
  {$IFNDEF FIX_UTF8}
  //  check for typecast error
  X:=UnicodeString(RawByteString(S));
  if (X<>S) or isUnicode(S) then
  {$ELSE}
  if isUnicode(S) then
  {$ENDIF}
    raise Exception.Create('string "' + RawByteString(S) +
    '" cannot be safely converted from Unicode.');
  asRawByte:=RawByteString(S);
end;

const
  CodePointMasks : array[1..4] of record A, O : byte end = (
    (A:$7f; O:$80),
    (A:$1f; O:$c0),
    (A:$0f; O:$e0),
    (A:$0f; O:$f0)
  );

{ TODO 5 -cDevel Restrict U+D800–U+DFFF from UTF-8, they are reserved for UTF-16 }
function UTF8ToValues(const S: RawByteString; out Values: TArrayOfInt32): boolean;
var
  IU, IV, CP, L, CL, CM : integer;
  CV, CT : Int32;
begin
  Result:=True;
  IU:=1;
  IV:=0;
  Values:=[];
  L := Length(S);
  SetLength(Values, L);
  while IU <= L do begin
    CV:=Byte(S[IU]);
    if (CV and $80) = 0 then begin // High bit not set, standard ASCII
      {
      if CV = $7f then begin
        // It is likely followed by value x2302 (226 140 130).
        // Browsers either ignore it or display a Box. Then, the x2302 "house"
        // symbol sometimes follows. Technically it is a "Delete" Control
        // Character.
        CV:=$7f;
      end;
      }
      Values[IV]:=CV;
      Inc(IU);
      Inc(IV);
      Continue;
    end;
    // Number of set bits + 0 equals number of total bytes.
    if (CV and $f0) = $f0 then       // 1111????, technically 0xf8 and 11110???
      CL := 4
    else if (CV and $f0) = $e0 then  // 1110????
      CL := 3
    else if (CV and $e0) = $c0 then  // 110?????
      CL := 2
    else begin                       // 10??????
      // I dont think this is legal/possible for the first character.
      // So, treat it as an encoding error. Otherwise, mask would be $3f.
      // Values[IV]:=CV and $3f;
      Values[IV]:=-1;
      Inc(IU);
      Inc(IV);
      Result:=False;
      Continue;
    end;
    CV:=0;
    CP:=0;
    CM:=CodePointMasks[CL].A;
    if CL + IU > L then begin
      // String is not long enough.
      CL:=1;
      CV:=-1;
      Result:=False;
    end else
      while CP < CL do begin
        CT:=Byte(S[IU+CP]);
        if (CP > 0) and (CT and $c0 <> $80) then begin
          // After the first byte, all bytes are masked with 10??????
          // So if the mask is missing, then encoding error.
          CV:=-1;
          CL:=1;
          Result:=False;
          Break;
        end;
        CV:=(CV shl 6) + (CT and CM);
        CM := $3f;
        Inc(CP);
      end;
    Values[IV]:=CV;
    Inc(IV);
    Inc(IU, CL);
  end;
  SetLength(Values, IV);
end;

function UTF8ToValues(const UTF: UTF8String; out Values: TArrayOfInt32
  ): boolean;
begin
  Result:=UTF8ToValues(RawByteString(UTF), Values);
end;

function UTF8ToValues(const A: TArrayOfByte; out Values: TArrayOfInt32
  ): boolean;
var
  S : RawByteString;
begin
  S:=ToString(A);
  Result:=UTF8ToValues(S, Values);
end;

function ValuesToUTF8(const Values: TArrayOfInt32; out UTF: UTF8String;
  BadChar : Int32 = 0 ): boolean;
var
  IU, IV, CP, CL : integer;
  CV : Int32;
  A, O : Byte;
  S : RawByteString;
begin
  if (BadChar < 0) or (BadChar > $10ffff) then
    BadChar:=0;
  Result:=True;
  IU:=0;
  S:='';
  SetLength(S, Length(Values) * 4);
  for IV := 0 to High(Values) do begin
    CV:=Values[IV];
    if (CV < 0) or (CV > $10ffff) then begin
      // Previous Error. Or, Too big.
      Result:=False;
      CV:=BadChar;
    end;
    if CV > $ffff then
      CL := 4
    else
    if CV > $07ff then
      CL := 3
    else
    if CV > $007f then
      CL := 2
    else begin
      S[IU+1]:=Char(CV);
      Inc(IU);
      Continue;
    end;
    CP := CL;
    A := $3f;
    O := $80;
    while CP > 0 do begin
      if CP = 1 then begin
        A := CodePointMasks[CL].A;
        O := CodePointMasks[CL].O;
      end;
      S[IU+CP] := Char((CV and A) or O);
      CV := CV shr 6;
      Dec(CP);
    end;
    Inc(IU, CL);
  end;
  SetLength(S, IU);
  UTF:=UTF8String(S);
end;

procedure LogToConsole(Message: String);
begin
  {$IFDEF WINDOWS}
  Message:=NormalizeLineEndings(Message, CRLF);
  {$ENDIF}
  WriteLn(Message);
end;

function ToString(const V: TArrayOfByte): RawByteString;
var
  I : Integer;
begin
  Result:='';
  SetLength(Result, Length(V));
  // FPC complains about using the following as "not inlined"
  // Move(V[0], Result[1], Length(V));
  // So, just do it as this instead. Not going to be as fast. But, whatever.
  for I := 0 to Length(V) - 1 do
    Result[1 + I]:=Char(V[I]);
end;

function ToBytes(const S: RawByteString): TArrayOfByte;
var
  I : Integer;
begin
  Result:=[];
  SetLength(Result, Length(S));
  // FPC complains about using the following as "not inlined"
  // Move(V[0], Result[1], Length(V));
  // So, just do it as this instead. Not going to be as fast. But, whatever.
  for I := 0 to Length(S) - 1 do
    Result[I]:=Byte(S[I + 1]);
end;

function ToBytes(const S: UnicodeString): TArrayOfByte;
begin
  Result:=ToBytes(RawByteString(S));
end;

procedure Cat(var V: RawByteString; const A: RawByteString);
begin
  V:=V+A;
end;

procedure Cat(var V: UnicodeString; const A: UnicodeString);
begin
  V:=V+A;
end;

procedure Cat(var V: AnsiString; const A: AnsiString);
begin
  V:=V+A;
end;

procedure Cat(var V: TArrayOfRawByteString; const A: RawByteString);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfUnicodeString; const A: UnicodeString);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfAnsiString; const A: AnsiString);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfRawByteString; const A: TArrayOfRawByteString);
var
  P, I : integer;
begin
  I := Low(A);
  P := High(V) + 1;
  SetLength(V, Length(V) + Length(A));
  while I <= High(A) do begin
    V[P]:=A[I];
    Inc(I);
    Inc(P);
  end;
end;

procedure Cat(var V: TArrayOfUnicodeString; const A: TArrayOfUnicodeString);
var
  P, I : integer;
begin
  I := Low(A);
  P := High(V) + 1;
  SetLength(V, Length(V) + Length(A));
  while I <= High(A) do begin
    V[P]:=A[I];
    Inc(I);
    Inc(P);
  end;
end;

procedure Cat(var V: TArrayOfAnsiString; const A: TArrayOfAnsiString);
var
  P, I : integer;
begin
  I := Low(A);
  P := High(V) + 1;
  SetLength(V, Length(V) + Length(A));
  while I <= High(A) do begin
    V[P]:=A[I];
    Inc(I);
    Inc(P);
  end;
end;

procedure Cat(var V: TArrayOfBoolean; const A: Boolean);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfByte; const A: Byte);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfWord; const A: Word);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfInteger; const A: Integer);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfLongInt; const A: LongInt);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfChar; const A: Char);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfString; const A: String);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfPointers; const A: Pointer);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfInt8; const A: Int8);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfInt16; const A: Int16);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfInt32; const A: Int32);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfInt64; const A: Int64);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfUInt8; const A: UInt8);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfUInt16; const A: UInt16);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfUInt32; const A: UInt32);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

procedure Cat(var V: TArrayOfUInt64; const A: UInt64);
begin
  SetLength(V, Length(V) + 1);
  V[High(V)]:=A;
end;

function Longest(const S: TStringList; ReturnIndex: boolean): integer;
var
  L, I, R : Integer;
begin
  R:=-1;
  Result:=-1;
  for I := 0 to S.Count - 1 do begin
    L:=Length(S[I]);
    if L > Result then begin
      Result:=L;
      R:=I;
    end;
  end;
  if ReturnIndex then
    Result:=R;
end;

function Longest(const S: TArrayOfRawByteString; ReturnIndex: boolean): integer;
var
  L, I, R : Integer;
begin
  R:=-1;
  Result:=-1;
  for I := Low(S) to High(S) do begin
    L:=Length(S[I]);
    if L > Result then begin
      Result:=L;
      R:=I;
    end;
  end;
  if ReturnIndex then
    Result:=R;
end;

function Longest(const S: TArrayOfUnicodeString; ReturnIndex: boolean): integer;
var
  L, I, R : Integer;
begin
  R:=-1;
  Result:=-1;
  for I := Low(S) to High(S) do begin
    L:=Length(S[I]);
    if L > Result then begin
      Result:=L;
      R:=I;
    end;
  end;
  if ReturnIndex then
    Result:=R;
end;

function Longest(const S: TArrayOfAnsiString; ReturnIndex: boolean): integer;
var
  L, I, R : Integer;
begin
  R:=-1;
  Result:=-1;
  for I := Low(S) to High(S) do begin
    L:=Length(S[I]);
    if L > Result then begin
      Result:=L;
      R:=I;
    end;
  end;
  if ReturnIndex then
    Result:=R;
end;

function WhenTrue(TF: Boolean; const T: RawByteString; const F: RawByteString
  ): RawByteString;
begin
  if TF then
    Result:=T
  else
    Result:=F;
end;

function WhenTrue(TF: Boolean; const T: UnicodeString; const F: UnicodeString
  ): UnicodeString;
begin
  if TF then
    Result:=T
  else
    Result:=F;
end;

function WhenTrue(TF: Boolean; T: Integer; F: Integer): integer;
begin
  if TF then
    Result:=T
  else
    Result:=F;
end;

function BoolStr(TF: Boolean; T: String; F: String): String;
begin
  if TF then
    Result:=T
  else
    Result:=F;
end;

function Plural(Value: Int64; Multiple: RawByteString; Single: RawByteString): RawByteString;
begin
  if Value = 1 then
    Result:=Single
  else
    Result:=Multiple;
end;

function Plural(Value: Int64; Multiple: UnicodeString; Single: UnicodeString): UnicodeString;
begin
  if Value = 1 then
    Result:=Single
  else
    Result:=Multiple;
end;

procedure Exchange(var A, B: Char);
var
  C : Char;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: UTF8String);
var
  C : UTF8String;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: AnsiString);
var
  C : AnsiString;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: RawByteString);
var
  C : RawByteString;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: UnicodeString);
var
  C : UnicodeString;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: Pointer);
var
  C : Pointer;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: TObject);
var
  C : TObject;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: Int8);
var
  C : Int8;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: UInt8);
var
  C : UInt8;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: Int16);
var
  C : UInt16;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: UInt16);
var
  C : UInt16;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: Int32);
var
  C : UInt32;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: UInt32);
var
  C : UInt32;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: Int64);
var
  C : UInt64;
begin
  C:=A;
  A:=B;
  B:=C;
end;

procedure Exchange(var A, B: UInt64);
var
  C : UInt64;
begin
  C:=A;
  A:=B;
  B:=C;
end;

function Compare(var A, B: TArrayOfByte; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfWord; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfInteger; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfLongInt; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfInt8; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfInt16; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfInt32; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfInt64; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfUInt8; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfUInt16; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfUInt32; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B: TArrayOfUInt64; Count: integer): integer;
var
  I : integer;
  C : integer;
begin
  Result:=0;
  C := Length(A);
  if Length(B) < Length(A) then C:=Length(B);
  if (Count > 0) then C := Count;
  for I := 0 to C - 1 do
    if A[I] < B[I] then begin
      Result := -1;
      Exit;
    end else if A[I] > B[I] then begin
      Result := 1;
      Exit;
    end;
  if Count <> 0 then Exit;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1;
end;

function Compare(var A, B; Size: integer): integer;
begin
  Result:=Compare(TArrayOfByte(A),TArrayOfByte(B), Size);
end;

function FirstChar(S: RawByteString): Char;
begin
  if Length(S) = 0 then
    Result:=#0
  else
    Result:=S[1];
end;

function StringOf(S: RawByteString; MinLength: integer; Crop: boolean
  ): RawByteString;
begin
  if S = '' then S := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do
    Result := Result + S;
  if Crop then
    Result:=Copy(Result, 1, MinLength);
end;

function StringOf(S: UnicodeString; MinLength: integer; Crop: boolean
  ): UnicodeString;
begin
  if S = '' then S := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do
    Result := Result + S;
  if Crop then
    Result:=Copy(Result, 1, MinLength);
end;

function LeftPad(S: RawByteString; MinLength: integer;
  Padding: RawByteString): RawByteString;
begin
  if Padding = '' then Padding := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do
    Result := Padding+Result;
end;

function LeftPad(S: UnicodeString; MinLength: integer; Padding: UnicodeString
  ): UnicodeString;
begin
  if Padding = '' then Padding := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do
    Result := Padding+Result;
end;

function RightPad(S: RawByteString; MinLength: integer;
  Padding: RawByteString): RawByteString;
begin
  if Padding = '' then Padding := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do
    Result := Result+Padding;
end;

function RightPad(S: UnicodeString; MinLength: integer; Padding: UnicodeString
  ): UnicodeString;
begin
  if Padding = '' then Padding := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do
    Result := Result+Padding;
end;

function CenterPad(S: RawByteString; MinLength: integer;
  Padding: RawByteString): RawByteString;
begin
  if Padding = '' then Padding := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do begin
    Result := Result+Padding;
    if Length(Result) < MinLength then
      Result := Padding+Result;
  end;
end;

function CenterPad(S: UnicodeString; MinLength: integer; Padding: UnicodeString
  ): UnicodeString;
begin
  if Padding = '' then Padding := SPACE;
  Result:=S;
  While (Length(Result) < MinLength) do begin
    Result := Result+Padding;
    if Length(Result) < MinLength then
      Result := Padding+Result;
  end;
end;

function ZeroPad(Value: LongInt; MinLength: integer): RawByteString;
begin
  Result := LeftPad(IntToStr(Value), MinLength, '0');
end;

function ZeroPad(S: RawByteString; MinLength: integer): RawByteString;
begin
  Result:=S;
  While Length(Result) < MinLength do
    Result:='0' + Result;
end;

function PopDelim(var S: RawByteString; Delim: RawByteString): RawByteString;
var
  P : integer;
begin
  P := Pos(Delim, S);
  if P <= 0 then P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
  Delete(S, 1, P - 1 + Length(Delim));
end;

function PopDelim(var S: UnicodeString; Delim: UnicodeString): UnicodeString;
var
  P : integer;
begin
  P := Pos(Delim, S);
  if P <= 0 then P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
  Delete(S, 1, P - 1 + Length(Delim));
end;

function PopDelim(var S : AnsiString; Delim: AnsiString = SPACE): AnsiString;
var
  P : integer;
begin
  P := Pos(Delim, S);
  if P <= 0 then P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
  Delete(S, 1, P - 1 + Length(Delim));
end;

function CutDelim(S, Delim: RawByteString; First: integer; Last: integer
  ): RawByteString;
var
  I : integer;
begin
  CutDelim:='';
  I := 1;
  While (I < First) and (S <> '') do begin
    Inc(I);
    PopDelim(S,Delim);
  end;
  if Last = -1 then begin
    CutDelim:=S;
    Exit;
  end;
  While (S <> '') and (I <= Last)  do begin
    Inc(I);
    if CutDelim <> '' then
      CutDelim:=CutDelim+Delim+PopDelim(S)
    else
      CutDelim:=PopDelim(S,Delim);
  end;
end;

function CutDelim(S, Delim: UnicodeString; First: integer; Last: integer
  ): UnicodeString;
var
  I : integer;
begin
  CutDelim:='';
  I := 1;
  While (I < First) and (S <> '') do begin
    Inc(I);
    PopDelim(S,Delim);
  end;
  if Last = -1 then begin
    CutDelim:=S;
    Exit;
  end;
  While (S <> '') and (I <= Last)  do begin
    Inc(I);
    if CutDelim <> '' then
      CutDelim:=CutDelim+Delim+PopDelim(S)
    else
      CutDelim:=PopDelim(S,Delim);
  end;
end;

function Excise(const S, BeginStr, EndStr: RawByteString; CaseSpecific: boolean
  ): RawByteString;
var
  B, E : integer;
begin
  Result:='';
  if CaseSpecific then begin
    B:=Pos(BeginStr, S);
    if B=0 then Exit;
    Inc(B, Length(BeginStr));
    E:=Pos(EndStr, S, B);
    if E=0 then Exit;
    Result:=Copy(S, B, E - B);
  end else begin
    B:=Pos(UpperCase(BeginStr), UpperCase(S));
    if B=0 then Exit;
    Inc(B, Length(BeginStr));
    E:=Pos(UpperCase(EndStr), UpperCase(S), B);
    if E=0 then Exit;
    Result:=Copy(S, B, E - B);
  end;
end;

function Excise(const S, BeginStr, EndStr: UnicodeString; CaseSpecific: boolean
  ): UnicodeString;
var
  B, E : integer;
begin
  Result:='';
  if CaseSpecific then begin
    B:=Pos(BeginStr, S);
    if B=0 then Exit;
    Inc(B, Length(BeginStr));
    E:=Pos(EndStr, S, B);
    if E=0 then Exit;
    Result:=Copy(S, B, E - B);
  end else begin
    B:=Pos(UpperCase(BeginStr), UpperCase(S));
    if B=0 then Exit;
    Inc(B, Length(BeginStr));
    E:=Pos(UpperCase(EndStr), UpperCase(S), B);
    if E=0 then Exit;
    Result:=Copy(S, B, E - B);
  end;
end;

function HasLeading(const S, SubStr: RawByteString; CaseSpecific : boolean): boolean;
begin
  if CaseSpecific then
    HasLeading:=Copy(S, 1, Length(SubStr)) = SubStr
  else
    HasLeading:=Lowercase(Copy(S, 1, Length(SubStr))) = LowerCase(SubStr)
end;

function HasLeading(const S, SubStr: UnicodeString; CaseSpecific : boolean): boolean;
begin
  if CaseSpecific then
    HasLeading:=Copy(S, 1, Length(SubStr)) = SubStr
  else
    HasLeading:=Lowercase(Copy(S, 1, Length(SubStr))) = LowerCase(SubStr)
end;

function HasTrailing(const S, SubStr: RawByteString; CaseSpecific : boolean): boolean;
begin
  if CaseSpecific then
    HasTrailing:=Copy(S, Length(S) - Length(SubStr) + 1) = SubStr
  else
    HasTrailing:=Lowercase(Copy(S, Length(S) - Length(SubStr) + 1)) = LowerCase(SubStr);
end;

function HasTrailing(const S, SubStr: UnicodeString; CaseSpecific : boolean): boolean;
begin
  if CaseSpecific then
    HasTrailing:=Copy(S, Length(S) - Length(SubStr) + 1) = SubStr
  else
    HasTrailing:=Lowercase(Copy(S, Length(S) - Length(SubStr) + 1)) = LowerCase(SubStr);
end;

function HasEnds(const S, StartStr, EndStr: RawByteString; CaseSpecific : boolean): boolean;
begin
  HasEnds := (Length(S) >= Length(StartStr) + Length(EndStr)) and
    HasLeading(S, StartStr, CaseSpecific) and HasTrailing(S, EndStr, CaseSpecific);
end;

function HasEnds(const S, StartStr, EndStr: UnicodeString; CaseSpecific : boolean): boolean;
begin
  HasEnds := (Length(S) >= Length(StartStr) + Length(EndStr)) and
    HasLeading(S, StartStr, CaseSpecific) and HasTrailing(S, EndStr, CaseSpecific);
end;

function ExcludeLeading(const S, SubStr : RawByteString; CaseSpecific : boolean) : RawByteString;
begin
  if HasLeading(S, SubStr, CaseSpecific) then
    ExcludeLeading:=Copy(S, Length(SubStr) + 1)
  else
    ExcludeLeading:=S;
end;

function ExcludeLeading(const S, SubStr : UnicodeString; CaseSpecific : boolean) : UnicodeString;
begin
  if HasLeading(S, SubStr, CaseSpecific) then
    ExcludeLeading:=Copy(S, Length(SubStr) + 1)
  else
    ExcludeLeading:=S;
end;

function ExcludeTrailing(const S, SubStr : RawByteString; CaseSpecific : boolean) : RawByteString;
begin
  if HasTrailing(S, SubStr, CaseSpecific) then
    ExcludeTrailing:=Copy(S, 1, Length(S) - Length(SubStr))
  else
    ExcludeTrailing:=S;
end;

function ExcludeTrailing(const S, SubStr : UnicodeString; CaseSpecific : boolean) : UnicodeString;
begin
  if HasTrailing(S, SubStr, CaseSpecific) then
    ExcludeTrailing:=Copy(S, 1, Length(S) - Length(SubStr))
  else
    ExcludeTrailing:=S;
end;

function IncludeLeading(const S, SubStr: RawByteString; CaseSpecific: boolean
  ): RawByteString;
begin
  if HasLeading(S, SubStr, CaseSpecific) then
    IncludeLeading:=S
  else
    IncludeLeading:=SubStr + S;
end;

function IncludeLeading(const S, SubStr: UnicodeString; CaseSpecific: boolean
  ): UnicodeString;
begin
  if HasLeading(S, SubStr, CaseSpecific) then
    IncludeLeading:=S
  else
    IncludeLeading:=SubStr + S;

end;

function IncludeTrailing(const S, SubStr: RawByteString; CaseSpecific: boolean
  ): RawByteString;
begin
  if HasTrailing(S, SubStr, CaseSpecific) then
    IncludeTrailing:=S
  else
    IncludeTrailing:=S + SubStr;
end;

function IncludeTrailing(const S, SubStr: UnicodeString; CaseSpecific: boolean
  ): UnicodeString;
begin
  if HasTrailing(S, SubStr, CaseSpecific) then
    IncludeTrailing:=S
  else
    IncludeTrailing:=S + SubStr;
end;

function ExcludeEnds(const S, StartStr, EndStr : RawByteString; CaseSpecific : boolean) : RawByteString;
begin
  ExcludeEnds:=ExcludeTrailing(ExcludeLeading(S, StartStr, CaseSpecific),
    EndStr, CaseSpecific);
end;

function ExcludeEnds(const S, StartStr, EndStr : UnicodeString; CaseSpecific : boolean) : UnicodeString;
begin
  ExcludeEnds:=ExcludeTrailing(ExcludeLeading(S, StartStr, CaseSpecific),
    EndStr, CaseSpecific);
end;

function Implode(Strs: TArrayOfRawByteString; Delim: RawByteString
  ): RawByteString;
var
  I : integer;
begin
  Result:='';
  for I := Low(Strs) to High(Strs) do begin
    Result:=Result + Strs[I];
    if I < High(Strs) then
      Result:=Result+Delim;
  end;
end;

function Implode(Strs: TArrayOfUnicodeString; Delim: UnicodeString
  ): UnicodeString;
var
  I : integer;
begin
  Result:='';
  for I := Low(Strs) to High(Strs) do begin
    Result:=Result + Strs[I];
    if I < High(Strs) then
      Result:=Result+Delim;
  end;
end;

function Implode(Strs: TStringList; Delim: RawByteString): RawByteString;
var
  I : integer;
begin
  Result:='';
  for I := 0 to Strs.Count - 1 do begin
    Result:=Result + Strs[I];
    if I < Strs.Count - 1 then
      Result:=Result+Delim;
  end;
end;

function Explode(S: RawByteString; Delim: RawByteString): TArrayOfRawByteString;
var
  I : Integer;
begin
  I := 0;
  Result:=[];
  While Length(S) > 0 do begin
    if I = Length(Result) then
      SetLength(Result, Length(Result) + ArrayGrowFactor);
    Result[I] := PopDelim(S, Delim);
    Inc(I);
  end;
  SetLength(Result, I);
end;

function Explode(S: UnicodeString; Delim: UnicodeString): TArrayOfUnicodeString;
var
  I : Integer;
begin
  I := 0;
  Result:=[];
  While Length(S) > 0 do begin
    if I = Length(Result) then
      SetLength(Result, Length(Result) + ArrayGrowFactor);
    Result[I] := PopDelim(S, Delim);
    Inc(I);
  end;
  SetLength(Result, I);
end;

function Explode(S: RawByteString; Delim: RawByteString): TStringList;
begin
  Result:=TStringList.Create;
  Explode(S, Result, Delim);
end;

procedure Explode(S: RawByteString; var Strs: TStringList; Delim: RawByteString
  );
begin
  While Length(S) > 0 do
    Strs.Add(PopDelim(S, Delim));
end;

function TitleCase(S: RawByteString): RawByteString;
var
  I : integer;
  T, F : boolean;
begin
  F:=True;
  for I := 1 to Length(S) do begin
    T:=LowerCase(S[I]) = UpCase(S[I]);
    if F and (not T) then
      S[I]:=UpCase(S[I]);
    F:=T;
  end;
  Result:=S;
end;

function TitleCase(S: UnicodeString): UnicodeString;
var
  I : integer;
  T, F : boolean;
begin
  F:=True;
  for I := 1 to Length(S) do begin
    T:=LowerCase(S[I]) = UpCase(S[I]);
    if F and (not T) then
      S[I]:=UpCase(S[I]);
    F:=T;
  end;
  Result:=S;
end;

function AlphaOnly(S: RawByteString): RawByteString;
var
  I : integer;
begin
  Result:='';
  for I := 1 to Length(S) do
    if LowerCase(S[I]) <> UpCase(S[I]) then
      Cat(Result, S[I]);
end;

function AlphaOnly(S: UnicodeString): UnicodeString;
var
  I : integer;
begin
  Result:='';
  for I := 1 to Length(S) do
    if LowerCase(S[I]) <> UpCase(S[I]) then
      Cat(Result, S[I]);
end;

function RandomStr(Count: Integer; CharSet: RawByteString): RawByteString;
var
  R : integer;
begin
  RandomStr:='';
  if CharSet = '' then
    CharSet:='0123456789abcdefghijkmnpqrstuvwxyz';  // No L or O.
  while Count > 0 do begin
    R:=Random(Length(CharSet)) + 1;
    RandomStr:=RandomStr + CharSet[R];
    Dec(Count);
  end;
end;

function RandomStr(Count: Integer; CharSet: UnicodeString): UnicodeString;
var
  R : integer;
begin
  RandomStr:='';
  if CharSet = '' then
    CharSet:='0123456789abcdefghijkmnpqrstuvwxyz';  // No L or O.
  while Count > 0 do begin
    R:=Random(Length(CharSet)) + 1;
    RandomStr:=RandomStr + CharSet[R];
    Dec(Count);
  end;
end;

function EscapeHTML(const S: RawByteString): RawByteString;
begin
  Result:=StringReplace(S, AMPERSAND, '&amp;', [rfReplaceAll]);
  Result:=StringReplace(Result, LESSTHAN, '&lt;', [rfReplaceAll]);
  Result:=StringReplace(Result, GREATERTHAN, '&gt;', [rfReplaceAll]);
  Result:=StringReplace(Result, QUOTEDOUBLE, '&quot;', [rfReplaceAll]);
end;

function EscapeHTML(const S: UnicodeString): UnicodeString;
begin
  Result:=UnicodeString(EscapeHTML(RawByteString(S)));
end;

function UnescapeHTML(const S: RawByteString): RawByteString;
begin
  Result:=StringReplace(S, '&quot;', QUOTEDOUBLE, [rfReplaceAll]);
  Result:=StringReplace(Result, '&gt;', GREATERTHAN, [rfReplaceAll]);
  Result:=StringReplace(Result, '&lt;', LESSTHAN, [rfReplaceAll]);
  Result:=StringReplace(Result, '&amp;', AMPERSAND, [rfReplaceAll]);
end;

function UnescapeHTML(const S: UnicodeString): UnicodeString;
begin
  Result:=UnicodeString(UnescapeHTML(RawByteString(S)));
end;

function NormalizeLineEndings(const S: RawByteString; Ending: RawByteString
  ): RawByteString;
begin
  case Ending of
    LF : begin
      Result:=StringReplace(S, CRLF, LF,[rfReplaceAll]);
      Result:=StringReplace(Result, CR, LF,[rfReplaceAll]);
    end;
    CR : begin
      Result:=StringReplace(S, CRLF, CR,[rfReplaceAll]);
      Result:=StringReplace(Result, LF, CR,[rfReplaceAll]);
    end;
    CRLF : begin
      Result:=StringReplace(S, CRLF, LF,[rfReplaceAll]);
      Result:=StringReplace(Result, CR, LF,[rfReplaceAll]);
      Result:=StringReplace(Result, LF, CRLF,[rfReplaceAll]);
    end;
  else
    Result:=StringReplace(S, CRLF, LF,[rfReplaceAll]);
    Result:=StringReplace(Result, CR, LF,[rfReplaceAll]);
    Result:=StringReplace(Result, LF, Ending,[rfReplaceAll]);
  end;
end;

function NormalizeLineEndings(const S: UnicodeString; Ending: UnicodeString
  ): UnicodeString;
begin
  Result:=UnicodeString(NormalizeLineEndings(UnicodeString(S), UnicodeString(Ending)));
end;

function WildMatch(Wild, S: RawByteString): boolean;
var
  M : RawByteString;
begin
  WildMatch := WildMatch(Wild, S, M);
end;

function WildMatch(Wild, S: RawByteString; out Match: RawByteString): boolean;
// Ported from my DOS QStrings Library. LOL.
// Modified to return the matched string, instead of True/False.

{ far from perfect, but good enough for now. :-) }

  function SubMatchWC(AWild, AStr :RawByteString) : boolean;
  var
      I : Integer;
  begin
      SubMatchWC:= False;
      if Length(AWild) <> Length(AStr) then exit;
      for I := 1 to Length(AWild) do
          if (AWild[I] <> AStr[I]) and (AWild[I] <> '?') then exit;
      SubMatchWC := True;
  end;

  function QPos(ASub, AStr : RawByteString) : integer;
  var
      I : integer;
  begin
      QPos := 0;
      for I := 1 to Length(AStr) - Length(ASub) + 1 do
          if SubMatchWC(ASub, Copy(AStr, I, Length(ASub))) then begin
              QPos := I;
              Break;
          end;
  end;

var
    PW, PS : integer;
    X : integer;
    WM: RawByteString;
begin
    X := 0;
    WildMatch:=True;
    Match := '';
    if Wild = S then begin
       Match:=S;
       Exit;
    end;
    if Wild = '' then begin
      WildMatch:=False;
      Exit;
    end;
    WM:='';
    repeat
        Inc(X);
        if Wild[1] = '*' then begin
            // WriteLn('[A-',AWild, '/',AStr,']');
            While (Wild<> '') and (Wild[1] = '*') do Delete(Wild, 1,1);
            if Wild = '' then begin
              Match:=WM + S;
              Exit;
            end;
            PW := Pos('*', Wild);
            if PW < 1 then PW := Length(Wild)+ 1;
            // WriteLn(PW, ';', Copy(AWild, 1, PW -1), ';', AStr);
            PS := QPos(Copy(Wild, 1, PW -1), S);
            { WriteLn(PS); }
            if PS < 1 then Break;
            WM:=WM+Copy(S, 1, PS - 1);
            Delete(S, 1, PS - 1);
        end;
        // WriteLn('[B-',AWild, '/',AStr,']');
        if SubMatchWC(Wild, S) then Exit;
        PW := Pos('*', Wild) - 1;
        { WriteLn(PW); }
        if PW < 1 then PW := Length(Wild) + 1;
        if not SubMatchWC(Copy(Wild,1, PW), Copy(S, 1, PW)) then Break;
        Delete(Wild, 1, PW);
        WM:=WM+'*';
        Delete(S, 1, PW);
        // WriteLn('[C-',AWild, '/',AStr,']');
    until (Wild = '') or (S = '') or (X = 1000);
    // WriteLn('[D-',AWild, '/',AStr,']');
    WildMatch:=((Wild = '*') or (Wild = '')) and (S = '');
    if WildMatch then
      Match:=WM;
end;

function IsLFNPart(Part : RawByteString) : boolean;
var
  N, E : RawByteString;
  I : Integer;
begin
  IsLFNPart:=True;
  N := UpperCase(ExtractFileBase(Part));
  E := UpperCase(ExcludeLeading('.', ExtractFileExt(Part)));
  I := Length(N);
  if (I = 0) or (I > 8) then Exit; // valid 1-8
  I := Length(E);
  if (I > 3) then Exit; // (Ext (no dot), valid 0-3
  Part:=N+E;
  for I := 1 to Length(Part) do
    if not (Part[I] in [#$30..#$39,#$40..#$5A,#$20,#$21,#$23..#$29,#$2D,
      #$5E..#$60,#$7B,#$7D,#$7E]) then
      Exit;
  IsLFNPart:=False;
end;

function RequireLFN(FileName: RawByteString): boolean;
var
  S : RawByteString;
begin
  RequireLFN:=False;
  While (RequireLFN = False) and (FileName <> '') do begin
    S := PopDelim(FileName, PathDelimiter);
    RequireLFN:=IsLFNPart(S);
  end;
end;

function RequireLFN(FileName: UnicodeString): boolean;
begin
  Result:=RequireLFN(RawByteString(FileName));
end;

function FileCase(FileName: UnicodeString; MustExist: boolean): UnicodeString;
begin
  FileCase:=FileCase(FileName, asUnicode(''), MustExist);
end;

function FileCase(FileName: RawByteString; MustExist: boolean): RawByteString;
begin
  FileCase:=FileCase(FileName, '', MustExist);
end;

function FileCase(FileName, Parent: RawByteString; MustExist: boolean
  ): RawByteString;
var
   E: Integer;
   H: RawByteString;
   O: RawByteString;
   T: RawByteString;
   R: TSearchRec;
begin
  H:=FileName;
  FileName:=Lowercase(FileName);
  if Parent <> '' then
    Parent:=IncludeTrailingPathDelimiter(Parent);
  O:='';
  While FileName<>'' do begin
    T:=LowerCase(PopDelim(FileName, PathDelim));
    E:=FindFirst(Parent + O + WildCard, faAnything, R);
    while E = 0 do begin
      if LowerCase(R.Name) = T then begin
        if R.Attr and faDirectory = faDirectory then
          O:=O+IncludeTrailingPathDelimiter(R.Name)
        else
          O:=O+R.Name;
        Break;
      end;
      E:=FindNext(R);
    end;
    SysUtils.FindClose(R);
    if E <> 0 then begin
      if MustExist then
        FileCase:=''
      else
        FileCase:=O + Copy(H, Length(O) + 1);
      Exit;
    end;
  end;
  FileCase:=O;
end;

function FileCase(FileName, Parent: UnicodeString; MustExist: boolean
  ): UnicodeString;
var
   E: Integer;
   H: UnicodeString;
   O: UnicodeString;
   T: UnicodeString;
   R: TUnicodeSearchRec;
begin
  H:=FileName;
  FileName:=Lowercase(FileName);
  if Parent <> '' then
    Parent:=IncludeTrailingPathDelimiter(Parent);
  O:='';
  While FileName<>'' do begin
    T:=LowerCase(PopDelim(FileName, PathDelim));
    E:=FindFirst(Parent + O + asUnicode(WildCard), faAnything, R);
    while E = 0 do begin
      if LowerCase(R.Name) = T then begin
        if R.Attr and faDirectory = faDirectory then
          O:=O+IncludeTrailingPathDelimiter(R.Name)
        else
          O:=O+R.Name;
        Break;
      end;
      E:=FindNext(R);
    end;
    SysUtils.FindClose(R);
    if E <> 0 then begin
      if MustExist then
        FileCase:=''
      else
        FileCase:=O + Copy(H, Length(O) + 1);
      Exit;
    end;
  end;
  FileCase:=O;
end;

function ExtractFileBase(const FileName: RawByteString): RawByteString;
var
  E : RawByteString;
begin
  Result:=ExtractFileName(FileName);
  E := ExtractFileExt(Result);
  if E <> '' then
    Result:=Copy(Result, 1, Length(Result) - Length(E));
end;

function ExtractFileBase(const FileName: UnicodeString): UnicodeString;
var
  E : UnicodeString;
begin
  Result:=ExtractFileName(FileName);
  E := ExtractFileExt(Result);
  if E <> '' then
    Result:=Copy(Result, 1, Length(Result) - Length(E));
end;

{$PUSH}{$I-}
function FileGetSize(FileName: RawByteString; out Size : Int64; FollowLinks
  : boolean=true) : boolean;
var
  R : Integer;
  S : TSearchRec;
begin
  Result:=false;
  Size:=-1;
  if FollowLinks then begin
    if IsLink(FileName) then
      FileName:=LinkTarget(FileName, True);
  end;
  if FileName = '' then Exit;
  R := FindFirst(FileName, faAnything, S);
  if R <> 0 then Exit;
  SysUtils.FindClose(S);
  Size:=S.Size;
  Result:=True;
end;

function FileGetSize(FileName: UnicodeString; out Size: Int64; FollowLinks
  : boolean): boolean;
begin
  Result:=FileGetSize(RawByteString(FileName), Size, FollowLinks);
end;

function FileLoad(const FileName: RawByteString; out Data: TArrayOfByte
  ): integer;
var
   F : File;
   R, E : integer;
begin
  Data:=[];
  System.Assign(F, FileName);
  Reset(F, 1);
  R := IOResult;
  if R = 0 then begin
    if FileSize(F) >= MaxInteger then
      R := 8
    else begin
      SetLength(Data, FileSize(F));
      BlockRead(F, Data[0], Length(Data));
    end;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if R <> 0 then
    SetLength(Data, 0);
end;

function FileLoad(const FileName: UnicodeString; out Data: TArrayOfByte
  ): integer;
begin
  Result:=FileLoad(RawByteString(FileName),Data);
end;

function FileSave(const FileName: RawByteString; const Data: TArrayOfByte
  ): integer;
var
   F : File;
   R, E : integer;
begin
  System.Assign(F, FileName);
  Rewrite(F,1);
  R := IOResult;
  if R = 0 then begin
    BlockWrite(F, Data[0], Length(Data));
    R := IOResult;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
end;

function FileSave(const FileName: UnicodeString; const Data: TArrayOfByte
  ): integer;
begin
  Result:=FileSave(RawByteString(FileName),Data);
end;

function FileCopy(Src, Dst: RawByteString; Overwrite: boolean): integer;
var
  B : array[1..TransferBufSize] of byte;
  SF, DF : File;
  SC, DC : boolean;
  R, W : LongInt;
  DT : TDateTime;
begin
  PseudoInit(B);
  PseudoInit(R);
  PseudoInit(W);
  FileCopy:=-1;
  if (not Overwrite) and FileExists(Dst) then Exit;
  SC:=False;
  DC:=False;
  try
    Assign(SF, Src);
    Reset(SF, 1);
    if IOResult<>0 then
      raise Exception.Create('could open source file');
    SC := True;
    Assign(DF, Dst);
    Rewrite(DF,1);
    if IOResult<>0 then
      raise Exception.Create('could open destinaton file');
    DC:=True;
    repeat
      BlockRead(SF, B, Sizeof(B), R);
      if IOResult <> 0 then
        raise Exception.Create('error reading source file');
      if R <> 0 then begin
        BlockWrite(DF, B, R, W);
        if (R<>W) or (IOResult <> 0) then
          raise Exception.Create('error writing destination file');
      end;
    until (R = 0);
    Close(SF);
    IOResult;
    SC:=False;
    Close(DF);
    IOResult;
    DC:=False;
    if not FileAge(Src, DT) then
      raise Exception.Create('error reading source timestamp');
    if FileSetDate(Dst, DateTimeToFileDate(DT)) <> 0 then
      raise Exception.Create('error writing destination timestamp');
    FileCopy:=0;
  except
    if SC then begin
      Close(SF);
      IOResult;
    end;
    if DC then begin
      Close(DF);
      IOResult;
    end;
    if FileExists(Dst) then
      SysUtils.DeleteFile(Dst);
  end;
end;

function FileCopy(Src, Dst: UnicodeString; Overwrite: boolean): integer;
begin
  Result:=FileCopy(RawByteString(Src), RawByteString(Dst), Overwrite);
end;

function FileMove(Src, Dst: RawByteString; Overwrite: boolean): integer;
begin
  FileMove:=-1;
  if (not Overwrite) and FileExists(Dst) then Exit;
  if not RenameFile(Src, Dst) then begin
    if FileCopy(Src, Dst, true) <> 0 then Exit;
    if not SysUtils.DeleteFile(Src) then begin
       SysUtils.DeleteFile(Dst);
       Exit;
    end;
  end;
  FileMove:=0;
end;

function FileMove(Src, Dst: UnicodeString; Overwrite: boolean): integer;
begin
  Result:=FileMove(RawByteString(Src), RawByteString(Dst), Overwrite);
end;

function FileCompare(FileA, FileB: RawByteString): integer;
var
  B1, B2 : array [1..TransferBufSize] of byte;
  R1, R2 : LongInt;
  F1, F2 : File;
  O1, O2 : Boolean;
  A1, A2 : LongInt;
  S1, S2 : Int64;
  DT : TDateTime;
  Match : Boolean;
  I : integer;

begin
  PseudoInit(B1);
  PseudoInit(R1);
  PseudoInit(B2);
  PseudoInit(R2); // If it were R2D2, things may be different.
  FileCompare:=-1;
  if not (FileExists(FileA) and FileExists(FileB)) then Exit;
  if not FileAge(FileA, DT, True) then Exit;
  A1:=DateTimeToFileDate(DT);
  if not FileAge(FileB, DT, True) then Exit;
  A2:=DateTimeToFileDate(DT);
  if not FileGetSize(FileA, S1, True) then Exit;
  if not FileGetSize(FileB, S2, True) then Exit;
  if S1 <> S2 then begin
    Match:=False;
  end else begin
    FileCompare:=0;
    Match:=True;
    O1:=False;
    O2:=False;
    try
      Assign(F1, FileA);
      Reset(F1,1);
      if IOResult <> 0 then
        Raise Exception.Create('error opening file ' + FileA);
      O1:=True;
      Assign(F2, FileB);
      Reset(F2,1);
      if IOResult <> 0 then
        Raise Exception.Create('error opening file ' + FileB);
      O2:=True;
      repeat
        BlockRead(F1, B1, Sizeof(B1), R1);
        if IOResult <> 0 then
          raise Exception.Create('error reading file ' + FileA);
        BlockRead(F2, B2, Sizeof(B2), R2);
        if IOResult <> 0 then
          raise Exception.Create('error reading file ' + FileB);
        if R1 <> R2 then // should never happen.
          raise Exception.Create('file read mismatch');
        for I := 1 to R1 do
          if B1[I] <> B2[I] then begin
             Match:=False;
             Break;
          end;
      until (R1 = 0) or (Not Match);
    except
      FileCompare:=-1;
    end;
    try
      if O2 then Close(F2);
    finally
      if O1 then Close(F1);
    end;
  end;
  if not Match then begin
    if A1 >= A2 then
      FileCompare:=1
    else
      FileCompare:=2;
  end;
end;

function FileCompare(FileA, FileB: UnicodeString): integer;
begin
  Result:=FileCompare(RawByteString(FileA),RawByteString(FileB));
end;

function FileCRC32(FileName : RawByteString) : LongInt;
var
  B : array[0..TransferBufSize-1] of byte;
  F : File;
  C : LongInt;
begin
  PseudoInit(B);
  PseudoInit(C);
  FileCRC32 := crc.crc32(0, nil, 0);
  Assign(F, FileName);
  Reset(F, 1);
  if IOResult = 0 then begin
    C := SizeOf(B);
    while (C = SizeOf(B)) do begin
      BlockRead(F, B, SizeOf(B), C);
      if IOResult <> 0 then begin
        FileCRC32:=0;
        Break;
      end;
      if C > 0 then begin
        FileCRC32 := crc.crc32(FileCRC32, B, C);
      end;
    end;
    Close(F);
  end else
    FileCRC32 := 0;
end;

function FileCRC32(FileName: UnicodeString): LongInt;
begin
  Result:=FileCRC32(RawByteString(FileName));
end;

function FileMD5(FileName: RawByteString): RawByteString;
begin
  FileMD5:='';
  try
    FileMD5:=MD5Print(MD5File(FileName));
  finally
  end;
end;

function FileMD5(FileName: UnicodeString): UnicodeString;
begin
  Result:=UnicodeString(FileMD5(RawByteString(FileName)));
end;

function FileSHA1(FileName: RawByteString): RawByteString;
begin
  FileSHA1:='';
  try
    FileSHA1:=SHA1Print(SHA1File(FileName));
  finally
  end;
end;

function FileSHA1(FileName: UnicodeString): UnicodeString;
begin
  Result:=UnicodeString(FileSHA1(RawByteString(FileName)));
end;

function FileIterative(FileName: RawByteString; MinPad: integer;
  AlwaysEnum: boolean; Divider: RawByteString): RawByteString;
var
  P, N, E : RawByteString;
  I : integer;
begin
  // Could improve performance with binary search of next available number.
  I := 0;
  P := ExtractFilePath(FileName);
  N := ExtractFileBase(FileName);
  E := ExtractFileExt(FileName);
  if AlwaysEnum then
    FileName:=P + N + Divider + ZeroPad(I,MinPad) + E;
  while FileExists(FileName) or DirectoryExists(FileName) do begin
    Inc(I);
    FileName:=P + N + Divider + ZeroPad(I,MinPad) + E;
  end;
  FileIterative:=FileName;
end;

function FileIterative(FileName: UnicodeString; MinPad: integer;
  AlwaysEnum: boolean; Divider: UnicodeString): UnicodeString;
var
  P, N, E : UnicodeString;
  I : integer;
begin
  // Could improve performance with binary search of next available number.
  I := 0;
  P := ExtractFilePath(FileName);
  N := ExtractFileBase(FileName);
  E := ExtractFileExt(FileName);
  if AlwaysEnum then
    FileName:=P + N + Divider + UnicodeString(ZeroPad(I,MinPad)) + E;
  while FileExists(FileName) or DirectoryExists(FileName) do begin
    Inc(I);
    FileName:=P + N + Divider + UnicodeString(ZeroPad(I,MinPad)) + E;
  end;
  FileIterative:=FileName;
end;

function SystemPath(PathType: TSystemPaths): RawByteString;
begin
  case PathType of
    spExecutable   : Result:=AppExecPath;
    spApplication  : Result:=AppBasePath;
    spResources    : Result:=AppDataPath;
    spWorking      : Result:=UserWorkPath;
    spHome         : Result:=UserHomepath;
    spData         : Result:=UserDatapath;
    spDownloads    : Result:=IncludeTrailingPathDelimiter(UserHomePath) + 'Downloads';
  else
    Result:=UserHomepath;
  end;
  Result:=IncludeTrailingPathDelimiter(Result);
end;

function DirScan(FileSpec: RawByteString; Options: TDirScanOptions
  ): TArrayOfRawByteString;
var
  Path,
  Spec  : RawByteString;
  Count : integer;

  procedure Add(const S : RawByteString);
  begin
    if Count = Length(Result) then
      SetLength(Result, Length(Result) + ArrayGrowFactor);
    Result[Count] := S;
    Inc(Count);
  end;

  procedure Scan(Sub : RawByteString);
  var
    Search : TSearchRec;
    Error : integer;
  begin
    // Recurse all sub-directiories when requested
    if dsRecursive in Options then begin
      Error := FindFirst(Path + Sub + WildCard, faAnything, Search);
      while (Error = 0) do begin
        if (Search.Attr and faDirectory = faDirectory) and
        (Search.Name <> '.') and (Search.Name <> '..') then begin
          if (Search.Name[1] <> '.') or (dsHidden in Options) then
            Scan(Sub + IncludeTrailingPathDelimiter(Search.Name));
        end;
        Error := FindNext(Search);
      end;
      SysUtils.FindClose(Search);
    end;
    // Add items from this directory
    Error := FindFirst(Path + Sub + Spec, faAnything, Search);
    while (Error = 0) do begin
      if (Search.Name[1] = '.') and (dsHidden in Options = false) then begin
        // Ignore Hidden DOT files
      end else if (Search.Name = '.') or (Search.Name = '..') then begin
        // ignore
      end else if (Search.Attr and faDirectory = faDirectory) then begin
        if dsDirectories in Options then
          Add(Sub + Search.Name);
      end else if dsFiles in Options then
        Add(Sub + Search.Name);
      Error := FindNext(Search);
    end;
    SysUtils.FindClose(Search);
  end;

begin
  Count:=0;
  Result:=[];
  Path:=ExtractFilePath(ExpandFileName(FileSpec));
  Spec:=ExtractFileName(FileSpec);
  if Spec='' then
    Spec:=WildCard;
  Scan('');
  SetLength(Result, Count);
end;

function DirScan(FileSpec: UnicodeString; Options: TDirScanOptions
  ): TArrayOfUnicodeString;
var
  Path,
  Spec  : UnicodeString;
  Count : integer;

  procedure Add(const S : UnicodeString);
  begin
    if Count = Length(Result) then
      SetLength(Result, Length(Result) + ArrayGrowFactor);
    Result[Count] := S;
    Inc(Count);
  end;

  procedure Scan(Sub : UnicodeString);
  var
    Search : TUnicodeSearchRec;
    Error : integer;
  begin
    // Recurse all sub-directiories when requested
    if dsRecursive in Options then begin
      Error := FindFirst(Path + Sub + UnicodeString(WildCard), faAnything, Search);
      while (Error = 0) do begin
        if (Search.Attr and faDirectory = faDirectory) and
        (Search.Name <> '.') and (Search.Name <> '..') then begin
          if (Search.Name[1] <> '.') or (dsHidden in Options) then
            Scan(Sub + IncludeTrailingPathDelimiter(Search.Name));
        end;
        Error := FindNext(Search);
      end;
      SysUtils.FindClose(Search);
    end;
    // Add items from this directory
    Error := FindFirst(Path + Sub + Spec, faAnything, Search);
    while (Error = 0) do begin
      if (Search.Name[1] = '.') and (dsHidden in Options = false) then begin
        // Ignore Hidden DOT files
      end else if (Search.Name = '.') or (Search.Name = '..') then begin
        // ignore
      end else if (Search.Attr and faDirectory = faDirectory) then begin
        if dsDirectories in Options then
          Add(Sub + Search.Name);
      end else if dsFiles in Options then
        Add(Sub + Search.Name);
      Error := FindNext(Search);
    end;
    SysUtils.FindClose(Search);
  end;

begin
  Count:=0;
  Result:=[];
  Path:=ExtractFilePath(ExpandFileName(FileSpec));
  Spec:=ExtractFileName(FileSpec);
  if Spec='' then
    Spec:=UnicodeString(WildCard);
  Scan('');
  SetLength(Result, Count);
end;

procedure DirScan(FileSpec: RawByteString; out List: TArrayOfRawByteString;
  Options: TDirScanOptions);
begin
  List:=DirScan(FileSpec, Options);
end;

procedure DirScan(FileSpec: UnicodeString; out List: TArrayOfUnicodeString;
  Options: TDirScanOptions);
begin
  List:=DirScan(FileSpec, Options);
end;

procedure DirScan(FileSpec: RawByteString; var List: TStringList;
  Options: TDirScanOptions);
var
  Items : TArrayOfRawByteString;
  I : Integer;
begin
  Items:=DirScan(FileSpec, Options);
  List.Clear;
  for I := Low(Items) to High(Items) do
    List.Add(Items[I]);
end;

function CreateTree(Directory: RawByteString; CanExist: boolean): boolean;
var
  EP, MP, DP, TP : RawByteString;
begin
  CreateTree:=CanExist;
  if DirectoryExists(Directory) then Exit;
  Directory:=Trim(StringReplace(Directory, PathDelimiter + '.' + PathDelimiter,
   PathDelimiter, [rfReplaceAll]));
  if Directory = PathDelimiter then exit;
  CreateTree:=False;
  EP:='';
  DP:='';
  MP:='';
  While Directory <> '' do begin
    TP:=PopDelim(Directory, PathDelimiter);
    if (TP = '') then begin
      if (EP = '') then
        TP:='/'
       else
         continue;
    end;
    if DirectoryExists(EP + TP) then begin
      EP:=EP + IncludeTrailingPathDelimiter(TP);
      Continue;
    end;
    MP:=MP+TP;
    if not CreateDir(EP + MP) then begin
      if DP <> '' then
        DeleteTree(EP + DP);
      Exit;
    end;
    if DP = '' then DP:=MP;
    MP:=IncludeTrailingPathDelimiter(MP);
  end;
  CreateTree:=True;
end;

function CreateTree(Directory: UnicodeString; CanExist: boolean): boolean;
begin
  Result:=CreateTree(RawByteString(Directory), CanExist);
end;

function DeleteTree(Directory: RawByteString): boolean;
var
  S : TSearchRec;
  R : Integer;
begin
  DeleteTree:=False;
  if Directory='' then exit;
  if FileExists(Directory) then begin
    DeleteTree:=SysUtils.DeleteFile(Directory);
    Exit;
  end;
  Directory:=IncludeTrailingPathDelimiter(Directory);
  // Using MaxLongInt for search attribute. Broken links do not show up
  // in the search with only faAnyfile. (At least on macOS)
  R:=FindFirst(Directory + WildCard,faAnything, S);
  While R = 0 do begin
    if (S.Name <> '.') and (S.Name <> '..') then begin
      if IsLink(Directory + S.Name) then begin
         if not SysUtils.DeleteFile(Directory+S.Name) then exit;
      end else begin
        if S.Attr and faDirectory = faDirectory then begin
          if not DeleteTree(Directory+S.Name) then exit;
        end else begin
          if not SysUtils.DeleteFile(Directory+S.Name) then exit;
        end;
      end;
    end;
    R:= FindNext(S);
  end;
  SysUtils.FindClose(S);
  DeleteTree:=RemoveDir(Directory);
end;

function DeleteTree(Directory: UnicodeString): boolean;
begin
  Result:=DeleteTree(RawByteString(Directory));
end;

function MkTempDir(Parent: RawByteString): RawByteString;
var
  R : RawByteString;
begin
  MkTempDir:='';
  if Parent = '' then
    Parent:=GetTempDir;
  Parent:=IncludeTrailingPathDelimiter(Parent);
  if Not DirectoryExists(Parent) then
    if Not CreateDir(Parent) then Exit;
  repeat
    R:=Parent+'tmp-'+RandomStr(16);
  until Not DirectoryExists(R);
  if not CreateDir(R) then Exit;
  R:=IncludeTrailingPathDelimiter(R);
  if not Assigned(TempItems) then
    TempItems:=TStringlist.Create;
  TempItems.Add(R);
  MkTempDir:=R;
end;

function MkTempDir(Parent: UnicodeString): UnicodeString;
begin
  Result:=UnicodeString(MkTempDir(RawByteString(Parent)));
end;

procedure RmTempDir(TempDir: RawByteString);
var
  I : integer;
begin
  if DeleteTree(TempDir) then begin
    if Assigned(TempItems) then begin
      I:=TempItems.IndexOf(TempDir);
      if I <> -1 then TempItems.Delete(I);
    end;
  end;
end;

procedure RmTempDir(TempDir: UnicodeString);
begin
  RmTempDir(RawByteString(TempDir));
end;

{$POP}

function BitSize(Bits : word) : word;
begin
  Result:=Bits shr 3;
  if Bits and 7 <> 0 then Inc(Result);
end;

function BitCount(V : Byte; MaxBits : integer) : integer;
var
  I : integer;
begin
  Result:=0;
  if MaxBits < 0 then MaxBits:=SizeOf(V);
  for I := 0 to SizeOf(V) - 1 do begin
    if MaxBits < 1 then Break;
    if V and (1 shl I) <> 0 then
      Inc(Result);
    Dec(MaxBits);
  end;
end;

function BitCount(V : Word; MaxBits : integer) : integer;
var
  I : integer;
begin
  Result:=0;
  if MaxBits < 0 then MaxBits:=SizeOf(V);
  for I := 0 to SizeOf(V) - 1 do begin
    if MaxBits < 1 then Break;
    if V and (1 shl I) <> 0 then
      Inc(Result);
    Dec(MaxBits);
  end;
end;

function BitCount(V : DWord; MaxBits : integer) : integer;
var
  I : integer;
begin
  Result:=0;
  if MaxBits < 0 then MaxBits:=SizeOf(V);
  for I := 0 to SizeOf(V) - 1 do begin
    if MaxBits < 1 then Break;
    if V and (1 shl I) <> 0 then
      Inc(Result);
    Dec(MaxBits);
  end;
end;

function BitCount(A : TArrayOfByte; MaxBits : integer) : integer;
const
  ItemSize=8;
var
  I, B : integer;
begin
  Result:=0;
  I := 0;
  if MaxBits < 0 then MaxBits:=Length(A) * ItemSize;
  while MaxBits > 0 do begin
    B:=0;
    While (MaxBits > 0) and (B < ItemSize) do begin
      if A[I] and (1 shl B) <> 0 then
        Inc(Result);
      Inc(B);
      Dec(MaxBits);
    end;
    Inc(I);
  end;
end;

function BitPack(Data : TArrayOfByte; BitWidth : integer) : TArrayOfByte;
var
  I, O, B, BI, BO : integer;
begin
  if BitWidth and 7 = 0 then begin
    Result:=Data;
    Exit;
  end;
  Result:=[];
  if Length(Data) mod BitSize(BitWidth) <> 0 then begin
    raise Exception.Create('Bitpack error, invalid bitwidth for specified array');
    Exit;
  end;
  SetLength(Result, BitSize(Length(Data) div BitSize(BitWidth) * BitWidth));
  I := 0;
  O := 0;
  B := BitWidth;
  BI := 0;
  BO := 0;
  Result[O] := 0;
  repeat
    if Data[I] and (1 shl BI) <> 0 then
      Result[O] := Result[O] or (1 shl BO);
    Dec(B);
    Inc(BI);
    if (BI = 8) or (B = 0) then begin
      if B = 0 then B := BitWidth;
      BI:=0;
      Inc(I);
      if I = Length(Data) then Break;
    end;
    Inc(BO);
    if BO = 8 then begin
      BO:=0;
      Inc(O);
      Result[O]:=0;
    end;
  until False;
end;

function BitUnPack(Data : TArrayOfByte; BitWidth : integer; MaxBytes : integer) : TArrayOfByte;
var
  I, O, B, BI, BO : integer;
begin
  if BitWidth and 7 = 0 then begin
    Result:=Data;
    Exit;
  end;
  Result:=[];
  if (MaxBytes > 0) then
    SetLength(Result, MaxBytes)
  else
    SetLength(Result, (Length(Data) * 8) div BitWidth);
  I := 0;
  O := 0;
  B := BitWidth;
  BI := 0;
  BO := 0;
  Result[O] := 0;
  repeat
    if Data[I] and (1 shl BI) <> 0 then
      Result[O] := Result[O] or (1 shl BO);
    Inc(BI);
    if BI = 8 then begin
      BI:=0;
      Inc(I);
      if I = Length(Data) then Break;
    end;
    Dec(B);
    Inc(BO);
    if (BO = 8) or (B = 0)then begin
      BO:=0;
      if B = 0 then B := BitWidth;
      Inc(O);
      if O = MaxBytes then Break;
      Result[O]:=0;
    end;
  until False;
  for I := O + 1 to Length(Result) - 1 do
    Result[O]:=0;
end;

initialization

  LogWriter := @LogToConsole;
  Initialize;

finalization

  Finalize;

end.
