(* FIX_UTF8 is required for correct handling of UTF-8 characters.

   1) Enables support for UTF-8 characters embedded in the binary.
   2) Enables support for UTF-8 characters output to the console.
   3) Enables support for UTF-8 characters in XML files.
   4) Enables the conversion of UTF-8 to UTF-16 characters which is performed
      behind the scenes when then type-casting Unicode Strings.
*)

{$DEFINE FIX_UTF8}

{$IFDEF FIX_UTF8}
  {$CODEPAGE UTF-8}

  {$if defined(darwin)}
    {$define USES_CWString}
  {$elseif defined(linux)}
    {$define USES_CWString}
  {$endif}

{$ENDIF}

