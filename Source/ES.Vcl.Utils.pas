{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2015-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.Utils;

{$IF CompilerVersion >= 24}
{$DEFINE VER240UP}
{$IFEND}
{$IF CompilerVersion >= 23}
{$DEFINE VER230UP}
{$IFEND}

interface

uses
  Windows, SysUtils, Classes, Controls, Messages, Graphics, Themes;

function IsShowFocusRect(Control: TWinControl): Boolean;
function IsStyledClientControl(Control: TControl): Boolean;
function IsStyledFontControl(Control: TControl): Boolean;
function IsStyledBorderControl(Control: TControl): Boolean;

function ClientColorToRgb(Color: TColor; Control: TControl = nil): TColor;
function BorderColorToRgb(Color: TColor; Control: TControl = nil): TColor;
function FontColorToRgb(Color: TColor; Control: TControl = nil): TColor;

// Auto assigning for TPersistent
function AssignPersistent(Dest, Src: TPersistent): Boolean;

procedure SerializeToStream(Obj: TPersistent; Stream: TStream; Name: string = '');
procedure SerializeToResource(Obj: TPersistent; Instance: HINST; const ResourceName: string;
  ResourceType: PChar; Name: string = '');
procedure SerializeToFile(Obj: TPersistent; FileName: string; Name: string = '');
procedure DeserializeFromStream(Obj: TPersistent; Stream: TStream; Name: string = '');
procedure DeserializeFromResource(Obj: TPersistent; Instance: HINST; const ResourceName: string;
  ResourceType: PChar; Name: string = '');
procedure DeserializeFromFile(Obj: TPersistent; FileName: string; Name: string = '');

implementation

function IsShowFocusRect(Control: TWinControl): Boolean;
begin
  Result := ((Control.Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0) and Control.Focused;
end;

function IsStyledClientControl(Control: TControl): Boolean;
begin
  Result := False;

  {$ifdef VER230UP}
  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seClient in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
  {$endif}
end;

function IsStyledFontControl(Control: TControl): Boolean;
begin
  Result := False;

  {$ifdef VER230UP}
  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seFont in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
  {$endif}
end;

function IsStyledBorderControl(Control: TControl): Boolean;
begin
  Result := False;

  {$ifdef VER230UP}
  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seBorder in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
  {$endif}
end;

function ClientColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  {$ifdef VER230UP}
  if IsStyledClientControl(Control) then
    Result := StyleServices.GetSystemColor(Color)
  else
  {$endif}
    Result := ColorToRGB(Color);
end;

function BorderColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  {$ifdef VER230UP}
  if IsStyledBorderControl(Control) then
    Result := StyleServices.GetSystemColor(Color)
  else
  {$endif}
    Result := ColorToRGB(Color);
end;

function FontColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  {$ifdef VER230UP}
  if IsStyledFontControl(Control) then
    Result := StyleServices.GetSystemColor(Color)
  else
  {$endif}
    Result := ColorToRGB(Color);
end;

type
  TReaderHack = class(TReader) end;
  TWriterHack = class(TWriter) end;

function AssignPersistent(Dest, Src: TPersistent): Boolean;
var
  Writer: TWriterHack;
  Reader: TReaderHack;
  MemStream: TMemoryStream;
begin
  Result := False;

  if Dest.ClassType = Src.ClassType then
  begin
    MemStream := nil;
    Writer := nil;
    Reader := nil;
    try
      MemStream := TMemoryStream.Create;

      Writer := TWriterHack.Create(MemStream, 512);
      try
        Writer.WriteProperties(Src);
        Writer.WriteListEnd;
      finally
        FreeAndNil(Writer);
      end;

      MemStream.Position := 0;
      Reader := TReaderHack.Create(MemStream, 512);
      while not Reader.EndOfList do
        Reader.ReadProperty(Dest);
      Reader.ReadListEnd;
    finally
      MemStream.Free;
      Reader.Free;
    end;
    Result := True;// ok
  end;
end;

type
  TEsSerializer = class(TComponent)
  private
    FObj: TPersistent;
  published
    property Obj: TPersistent read FObj write FObj;
  end;

procedure SerializeToStream(Obj: TPersistent; Stream: TStream; Name: string = '');
var
  Component: TEsSerializer;
begin
  Component := TEsSerializer.Create(nil);
  try
    Component.Obj := Obj;
    if Name <> '' then
      Component.Name := Name
    else
      Component.Name := Obj.ClassName;
    Stream.WriteComponent(Component);
  finally
    Component.Free;
  end;
end;

procedure SerializeToResource(Obj: TPersistent; Instance: HINST; const ResourceName: string;
  ResourceType: PChar; Name: string = '');
var
  ResStream: TResourceStream;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SerializeToStream(Obj, Stream, Name);
    ResStream := TResourceStream.Create(Instance, ResourceName, ResourceType);
    try
      Stream.Seek(0, soFromBeginning);
      ObjectBinaryToText(Stream, ResStream);
    finally
      ResStream.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure SerializeToFile(Obj: TPersistent; FileName: string; Name: string = '');
var
  FileStream: TFileStream;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SerializeToStream(Obj, Stream, Name);
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.Seek(0, soFromBeginning);
      ObjectBinaryToText(Stream, FileStream);
    finally
      FileStream.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure DeserializeFromStream(Obj: TPersistent; Stream: TStream; Name: string = '');
var
  Component: TEsSerializer;
begin
  Component := TEsSerializer.Create(nil);
  try
    Component.Obj := Obj;
    if Name <> '' then
      Component.Name := Name
    else
      Component.Name := Obj.ClassName;
    Stream.ReadComponent(Component);
  finally
    Component.Free;
  end;
end;

procedure DeserializeFromResource(Obj: TPersistent; Instance: HINST; const ResourceName: string;
  ResourceType: PChar; Name: string = '');
var
  ResStream: TResourceStream;
  Stream: TMemoryStream;
begin
  ResStream := TResourceStream.Create(Instance, ResourceName, ResourceType);
  try
    Stream := TMemoryStream.Create;
    try
      ObjectTextToBinary(ResStream, Stream);
      Stream.Seek(0, soFromBeginning);
      DeserializeFromStream(Obj, Stream, Name);
    finally
      Stream.Free;
    end;
  finally
    ResStream.Free;
  end;
end;

procedure DeserializeFromFile(Obj: TPersistent; FileName: string; Name: string = '');
var
  FileStream: TFileStream;
  Stream: TMemoryStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream := TMemoryStream.Create;
    try
      ObjectTextToBinary(FileStream, Stream);
      Stream.Seek(0, soFromBeginning);
      DeserializeFromStream(Obj, Stream, Name);
    finally
      Stream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

end.
