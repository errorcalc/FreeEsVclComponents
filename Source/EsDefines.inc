// Compiler verisons
// 2010
{$IFDEF VER210}
  ERROR, DELPHI2010 VERSION HERE: https://github.com/errorcalc/FreeEsVclComponents/tree/Version2
{$ENDIF}
// XE1
{$IFDEF VER220}
  ERROR, DELPHIXE VERSION HERE: https://github.com/errorcalc/FreeEsVclComponents/tree/Version2
{$ENDIF}
// XE2
{$IFDEF VER230}
  {$DEFINE VER230UP}
{$ENDIF}
// XE3
{$IFDEF VER240}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
{$ENDIF}
// XE4
{$IFDEF VER250}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
{$ENDIF}
// XE5
{$IFDEF VER260}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
{$ENDIF}
// XE6
{$IFDEF VER270}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
{$ENDIF}
// XE7
{$IFDEF VER280}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
{$ENDIF}
// XE8
{$IFDEF VER290}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
{$ENDIF}
// XE10.0 Seattle
{$IFDEF VER300}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
{$ENDIF}
// XE10.1 Berlin
{$IFDEF VER310}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
{$ENDIF}
// XE10.2 Tokyo
{$IFDEF VER320}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
  {$DEFINE VER320UP}
{$ENDIF}
// XE10.3 Rio
{$IFDEF VER330}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
  {$DEFINE VER320UP}
  {$DEFINE VER330UP}
{$ENDIF}
// XE10.4 Sydney
{$IFDEF VER340}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
  {$DEFINE VER320UP}
  {$DEFINE VER330UP}
  {$DEFINE VER340UP}
{$ENDIF}
// XE11 Alexandria
{$IFDEF VER350}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
  {$DEFINE VER320UP}
  {$DEFINE VER330UP}
  {$DEFINE VER340UP}
  {$DEFINE VER350UP}
{$ENDIF}
// XE12 Athens
{$IFDEF VER360}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
  {$DEFINE VER320UP}
  {$DEFINE VER330UP}
  {$DEFINE VER340UP}
  {$DEFINE VER350UP}
  {$DEFINE VER360UP}
{$ENDIF}
// Next versions
{$IFNDEF VER230UP}
  {$DEFINE VER230UP}
  {$DEFINE VER240UP}
  {$DEFINE VER250UP}
  {$DEFINE VER260UP}
  {$DEFINE VER270UP}
  {$DEFINE VER280UP}
  {$DEFINE VER290UP}
  {$DEFINE VER300UP}
  {$DEFINE VER310UP}
  {$DEFINE VER320UP}
  {$DEFINE VER330UP}
  {$DEFINE VER340UP}
  {$DEFINE VER350UP}
  {$DEFINE VER360UP}
{$ENDIF}

// Compiler options
{.$DEFINE ESDEBUG}
{$IFNDEF ESDEBUG}
  {$RANGECHECKS OFF}
  {$IOCHECKS ON}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}

// Vcl
(*
{$IFDEF VER240UP}
  // XE3 has support StyleElements property
  {$DEFINE STYLE_ELEMENTS}
{$ENDIF}
{$IFDEF VER330UP}
{$ENDIF}
{$IFDEF VER340UP}
  // XE10.4 Sydney has support StyleName property
  {$DEFINE STYLE_NAME}
  // XE10.3 Rio has support virtual image
  {$DEFINE VIRTUAL_IMAGE}
  // XE3 has support StyleElements property
  {$DEFINE STYLE_ELEMENTS}
{$ENDIF}
*)
