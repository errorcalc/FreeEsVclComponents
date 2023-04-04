{******************************************************************************}
{                                                                              }
{                       EsVclComponents/EsVclCore v4.4                         }
{                           errorsoft(c) 2009-2023                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{    errorsoft@mail.ru | github.com/errorcalc | habrahabr.ru/user/error1024    }
{          You can write to me in the Telegram messenger: @errorsoft           }
{                                                                              }
{           Star me github: github.com/errorcalc/FreeEsVclComponents           }
{                                                                              }
{                 You can order developing vcl/fmx components,                 }
{               please submit your requests to mail or telegram.               }
{                                                                              }
{******************************************************************************}
unit ES.Utils;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes, System.UITypes, Vcl.Controls,
  WinApi.Messages, Vcl.Graphics, Vcl.Themes, Vcl.ImgList, WinApi.DwmApi;

function IsShowFocusRect(Control: TWinControl): Boolean;
function IsStyledClientControl(Control: TControl): Boolean;
function IsStyledFontControl(Control: TControl): Boolean;
function IsStyledBorderControl(Control: TControl): Boolean;
function IsStyledControl(Control: TControl): Boolean;

function GetControlStyle(Control: TControl): TCustomStyleServices;

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

function ImageListGetBitmap(ImageList: TCustomImageList; Index: Integer; Bitmap: TBitmap; IsPremultiplied: Boolean = True): Boolean;

//function GetMainColor: Cardinal; overload;
function GetMainColor(var Color: TAlphaColor): Boolean; overload;
procedure InitMainColor;

implementation

uses
  WinApi.CommCtrl;

function IsShowFocusRect(Control: TWinControl): Boolean;
begin
  if Control.Focused then
    Result := (Control.Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0
  else
    Result := False;
end;

function IsStyledControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  {$IFDEF VER340UP}
  if StyleServices(Control).Enabled then
  begin
    Result := not StyleServices(Control).IsSystemStyle;
  end;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function IsStyledClientControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  {$IFDEF VER340UP}
  if StyleServices(Control).Enabled then
  begin
    Result := (seClient in Control.StyleElements) and
              (not StyleServices(Control).IsSystemStyle);
  end;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := {$IFDEF VER240UP}(seClient in Control.StyleElements) and{$ENDIF}
      TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function IsStyledFontControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  {$IFDEF VER340UP}
  if StyleServices(Control).Enabled then
  begin
    Result := (seFont in Control.StyleElements) and
              (not StyleServices(Control).IsSystemStyle);
  end;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := {$IFDEF VER240UP}(seFont in Control.StyleElements) and{$ENDIF}
      TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function IsStyledBorderControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  {$IFDEF VER340UP}
  if StyleServices(Control).Enabled then
  begin
    Result := (seBorder in Control.StyleElements) and
              (not StyleServices(Control).IsSystemStyle);
  end;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := {$IFDEF VER240UP}(seBorder in Control.StyleElements) and{$ENDIF}
      TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function GetControlStyle(Control: TControl): TCustomStyleServices;
begin
  Result := TStyleManager.SystemStyle;
  if Control = nil then
    Exit;

  {$IFDEF VER340UP}
  if StyleServices(Control).Enabled then
    Result := StyleServices(Control)
  else
    TStyleManager.SystemStyle;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := TStyleManager.ActiveStyle;
  end;
  {$ENDIF}
end;

function ClientColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  {$IFDEF VER340UP}
  if IsStyledClientControl(Control) then
  begin
    Result := StyleServices(Control).GetSystemColor(Color);
    if Result and $FF000000 <> 0 then
      Result := ColorToRGB(Color);
  end
  else
  {$ELSE}
  if IsStyledClientControl(Control) then
  begin
    Result := StyleServices.GetSystemColor(Color);
    if Result and $FF000000 <> 0 then
      Result := ColorToRGB(Color);
  end
  else
  {$ENDIF}
    Result := ColorToRGB(Color);
end;

function BorderColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  {$IFDEF VER340UP}
  if IsStyledBorderControl(Control) then
  begin
    Result := StyleServices(Control).GetSystemColor(Color);
    if Result and $FF000000 <> 0 then
      Result := ColorToRGB(Color);
  end
  else
  {$ELSE}
  if IsStyledBorderControl(Control) then
  begin
    Result := StyleServices.GetSystemColor(Color);
    if Result and $FF000000 <> 0 then
      Result := ColorToRGB(Color);
  end
  else
  {$ENDIF}
    Result := ColorToRGB(Color);
end;

function FontColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  {$IFDEF VER340UP}
  if IsStyledFontControl(Control) then
  begin
    Result := StyleServices(Control).GetSystemColor(Color);
    if Result and $FF000000 <> 0 then
      Result := ColorToRGB(Color);
  end
  else
  {$ELSE}
  if IsStyledFontControl(Control) then
  begin
    Result := StyleServices.GetSystemColor(Color);
    if Result and $FF000000 <> 0 then
      Result := ColorToRGB(Color);
  end
  else
  {$ENDIF}
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

function ImageListGetBitmap(ImageList: TCustomImageList; Index: Integer; Bitmap: TBitmap; IsPremultiplied: Boolean = True): Boolean;
var
  Info: TIconInfo;
  DC: HDC;
  Icon: TIcon;
begin
  Result := (Bitmap <> nil) and ImageList.HandleAllocated and (Index > -1) and (Index < ImageList.Count);

  if Result then
    if (ImageList.ColorDepth <> TColorDepth.cd32Bit) then
      Result := ImageList.GetBitmap(Index, Bitmap)
    else
    begin
      Bitmap.SetSize(0, 0);
      // set alpha bitmap
      Bitmap.PixelFormat := TPixelFormat.pf32bit;
      // new size
      Bitmap.SetSize(ImageList.Width, ImageList.Height);

      // create icon
      Icon := TIcon.Create;
      DC := 0;
      Info.hbmMask := 0;
      Info.hbmColor := 0;
      try
        // get icon
        ImageList.GetIcon(Index, Icon);
        GetIconInfo(Icon.Handle, Info);

        // select icon bitmap to dc
        DC := CreateCompatibleDC(Bitmap.Canvas.Handle);
        SelectObject(DC, Info.hbmColor);

        // copy icon to bitmap
        BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
          DC, 0, 0, SRCCOPY);

        if IsPremultiplied then
          Bitmap.AlphaFormat := TAlphaFormat.afPremultiplied
        else
          Bitmap.AlphaFormat := TAlphaFormat.afDefined;
      finally
        DeleteDC(DC);
        DeleteObject(Info.hbmMask);
        DeleteObject(Info.hbmColor);
        Icon.Free;
      end;
    end;
end;

var
  MainColor: Cardinal = $FF000000;

function GetDwmMainColor: TAlphaColor;
begin
  if MainColor = $FF000000 then
    InitMainColor;
  Result := MainColor;
end;

function GetMainColor(var Color: TAlphaColor): Boolean;
begin
  Color := GetDwmMainColor;
  Result := Color <> 0;
end;

procedure InitMainColor;
var
  DwnOpaqueBlend: BOOL;
begin
  // windows 8
  if (not CheckWin32Version(6, 2)) or (DwmGetColorizationColor(MainColor, DwnOpaqueBlend) <> S_OK) then
    MainColor := 0;
end;

end.
