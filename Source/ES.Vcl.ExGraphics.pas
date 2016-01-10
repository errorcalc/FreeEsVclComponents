{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2009-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.ExGraphics;

interface

{$if CompilerVersion >= 21}
{$define VER210UP}
{$ifend}
{$IF CompilerVersion >= 23}
{$DEFINE VER230UP}
{$IFEND}

uses
  Windows, Graphics, Themes;

type
//  {$ifdef VER210UP}
//  {$scopedenums on}
//  {$endif}
  TStretchMode = (smNormal, smTile, smHorzFit, smVertFit, smHorzTile, smVertTile, smHorzTileFit, smVertTileFit);
//  {$ifdef VER210UP}
//  {$scopedenums off}
//  {$endif}

  {$ifdef VER210UP}
  TEsCanvasHelper = class helper for TCanvas
  {$else}
  TEsCanvas = class(TCanvas)
  {$endif}
  public
    procedure StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap); overload;
    procedure StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap; Alpha: byte); overload;
//    procedure StretchDraw(DestRect, ClipRect, SrcRect: TRect; Bitmap: TBitmap; Alpha: byte); overload;
    procedure DrawNinePath(Dest: TRect; Bounds: TRect; Bitmap: TBitmap); overload;
    procedure DrawNinePath(Dest: TRect; Bounds: TRect; Bitmap: TBitmap; Alpha: byte); overload;
    procedure DrawNinePath(Dest: TRect; Bounds: TRect; Bitmap: TBitmap; Mode: TStretchMode; Alpha: Byte = 255); overload;
    {$ifdef VER230UP}
    procedure DrawThemeText(Details: TThemedElementDetails; Rect: TRect; Text: string; Format: TTextFormat);
    {$endif}
    procedure DrawChessFrame(R: TRect; Color1, Color2: TColor);
  end;

  TEsBitMap = class(TBitmap)
  private
  protected
    property Palette;
    {$ifdef VER210UP}
    property AlphaFormat;
    {$endif}
    property PixelFormat;
  public
    Constructor Create; override;
    {$ifndef VER210UP}
    procedure PreMultiplyAlpha;
    procedure UnPreMultiplyAlpha;
    {$endif}
    procedure LoadFromResourceName(Instance: THandle; const ResName: String; ResType: PChar); overload;
  end;

  function ColorToAlphaColor(Color: TColor; Alpha: byte = 255): DWORD; Inline;
  function RgbToArgb(Color: TColor; Alpha: byte = 255): DWORD; Inline;

implementation

uses
  Classes, Types;

type
  TRGBAArray = array[Word] of TRGBQuad;
  PRGBAArray = ^TRGBAArray;

function RgbToArgb(Color: TColor; Alpha: byte = 255): DWORD;
begin
  Result := ColorToAlphaColor(Color, Alpha);
end;

function ColorToAlphaColor(Color: TColor; Alpha: byte = 255): DWORD;
var
  BRG: DWORD;
begin
  BRG := ColorToRGB(Color);

  Result := ((BRG shl 16) and $00FF0000) or ((BRG shr 16) and $000000FF) or (BRG and $0000FF00) or (Alpha shl 24);
end;

{TEsBitMap}

{$ifdef VER210UP} {$REGION 'Old delphi support'} {$endif}
{$ifndef VER210UP}
procedure TEsBitMap.PreMultiplyAlpha;
var
  x, y: integer;
  TripleAlpha: double;
  pBmp: pRGBAArray;
  Alpha: word;
begin
  if PixelFormat <> pf32bit then exit;
  for y := 0 to Height-1 do
    begin
    pBmp := ScanLine[y];
    for x := 0 to Width-1 do
      begin
        Alpha := pBmp[x].rgbReserved;
        pBmp[x].rgbRed := MulDiv(pBmp[x].rgbRed, Alpha, 255);
        pBmp[x].rgbGreen := MulDiv(pBmp[x].rgbGreen, Alpha, 255);
        pBmp[x].rgbBlue := MulDiv(pBmp[x].rgbBlue, Alpha, 255);
      end;
    end;
end;

procedure TEsBitMap.UnPreMultiplyAlpha;
var
  x, y: integer;
  TripleAlpha: double;
  pBmp: pRGBAArray;
  Alpha: word;
begin
  if PixelFormat <> pf32bit then exit;
  for y := 0 to Height-1 do
    begin
    pBmp := ScanLine[y];
    for x := 0 to Width-1 do
      begin
        Alpha := pBmp[x].rgbReserved;
        pBmp[x].rgbRed := MulDiv(pBmp[x].rgbRed, 255, Alpha);
        pBmp[x].rgbGreen := MulDiv(pBmp[x].rgbGreen, 255, Alpha);
        pBmp[x].rgbBlue := MulDiv(pBmp[x].rgbBlue, 255, Alpha);
      end;
    end;
end;
{$endif}
{$ifdef VER210UP} {$ENDREGION} {$endif}

constructor TEsBitMap.Create;
begin
  inherited;
  {$ifdef VER210UP}
  self.AlphaFormat := afDefined;
  {$endif}
  self.PixelFormat := pf32bit;
end;

procedure TEsBitMap.LoadFromResourceName(Instance: THandle;
  const ResName: String; ResType: PChar);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, ResType);
  try
    self.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TEsCanvas && TEsCanvasHelper }

procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}.
  StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap);
begin
  StretchDraw(DestRect, SrcRect, BitMap, 255);
end;

procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}.
  StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap; Alpha: byte);
var
  BF: TBlendFunction;
begin
  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Alpha;
  BF.AlphaFormat := AC_SRC_ALPHA;

  AlphaBlend(Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    Bitmap.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, BF);
end;

procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}.
  DrawNinePath(Dest: TRect; Bounds: TRect; Bitmap: TBitmap);
begin
  DrawNinePath(Dest, Bounds, Bitmap, 255);
end;

procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}.
  DrawChessFrame(R: TRect; Color1, Color2: TColor);
var
  Brush: HBRUSH;
  Bitmap: TBitmap;
begin
  Brush := 0;
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    Bitmap.SetSize(2, 2);
    Bitmap.Canvas.Pixels[0, 0] := ColorToRGB(Color1);
    Bitmap.Canvas.Pixels[1, 1] := ColorToRGB(Color1);
    Bitmap.Canvas.Pixels[1, 0] := ColorToRGB(Color2);
    Bitmap.Canvas.Pixels[0, 1] := ColorToRGB(Color2);

    Brush := CreatePatternBrush(Bitmap.Handle);

    Windows.FrameRect(Handle, R, Brush);
  finally
    DeleteObject(Brush);
    Bitmap.Free;
  end;
end;

function ValidRect(Rect: TRect): Boolean;
begin
  Result := (RectWidth(Rect) > 0)and(RectHeight(Rect) > 0);
end;

procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}
  .DrawNinePath(Dest: TRect; Bounds: TRect; Bitmap: TBitmap; Alpha: byte);
var
  dx, dy: Integer;
  D, S: TRect;
  IntD, IntS: TRect;
begin
  if (Dest.Left >= Dest.Right)or(Dest.Top >= Dest.Bottom) then
    exit;

  IntD := Rect(Dest.Left + Bounds.Left, Dest.Top + Bounds.Top,
    Dest.Right - Bounds.Right, Dest.Bottom - Bounds.Bottom);
  IntS := Rect(Bounds.Left, Bounds.Top, Bitmap.Width - Bounds.Right, Bitmap.Height - Bounds.Bottom);

  // needs to adjust to get rid of overdraw and painting was correct
  // cut left
  if Dest.Right - Dest.Left < Bounds.Left then
  begin
    dx := Bounds.Left - (Dest.Right - Dest.Left);
    IntD.Left := IntD.Left - dx;
    IntS.Left := IntS.Left - dx;
    //
    IntD.Right := Dest.Right;
  end else
  // cut right
  if Dest.Right - Dest.Left < Bounds.Left + Bounds.Right then
  begin
    dx := (Bounds.Left + Bounds.Right) - (Dest.Right - Dest.Left);
    IntD.Right := IntD.Right + dx;
    IntS.Right := IntS.Right + dx;
  end;
  // cut top
  if Dest.Bottom - Dest.Top < Bounds.Top then
  begin
    dy := Bounds.Top - (Dest.Bottom - Dest.Top);
    IntD.Top := IntD.Top - dy;
    IntS.Top := IntS.Top - dy;
    //
    IntD.Bottom := Dest.Bottom;
  end else
  // cut bottom
  if Dest.Bottom - Dest.Top < Bounds.Top + Bounds.Bottom then
  begin
    dy := (Bounds.Top + Bounds.Bottom) - (Dest.Bottom - Dest.Top);
    IntD.Bottom := IntD.Bottom + dy;
    IntS.Bottom := IntS.Bottom + dy;
  end;

//  // correct!
//  if IntD.Left > Dest.Right then
//    IntD.Left := Dest.Right;
//  if IntD.Top > Dest.Bottom then
//    IntD.Top := Dest.Bottom;
//  if IntD.Right < Dest.Left then
//    IntD.Right := Dest.Left;
//  if IntD.Bottom < Dest.Top then
//    IntD.Bottom := Dest.Top;


  //   ---
  //  |*  |
  //  |   |
  //  |   |
  //   ---
  D := Rect(Dest.Left, Dest.Top, IntD.Left, IntD.Top);
  S := Rect(0, 0, IntS.Left, IntS.Top);
  StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |   |
  //  |*  |
  //  |   |
  //   ---
  D := Rect(Dest.Left, IntD.Top, IntD.Left, IntD.Bottom);
  S := Rect(0, IntS.Top, IntS.Left, IntS.Bottom);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |   |
  //  |   |
  //  |*  |
  //   ---
  D := Rect(Dest.Left, IntD.Bottom, IntD.Left, Dest.Bottom);
  S := Rect(0, IntS.Bottom, IntS.Left, Bitmap.Height);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |   |
  //  |   |
  //  | * |
  //   ---
  D := Rect(IntD.Left, IntD.Bottom, IntD.Right, Dest.Bottom);
  S := Rect(IntS.Left, IntS.Bottom, IntS.Right, Bitmap.Height);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |   |
  //  |   |
  //  |  *|
  //   ---
  D := Rect(IntD.Right, IntD.Bottom, Dest.Right, Dest.Bottom);
  S := Rect(IntS.Right, IntS.Bottom, Bitmap.Width, Bitmap.Height);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |   |
  //  |  *|
  //  |   |
  //   ---
  D := Rect(IntD.Right, IntD.Top, Dest.Right, IntD.Bottom);
  S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Bottom);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |  *|
  //  |   |
  //  |   |
  //   ---
  D := Rect(IntD.Right, Dest.Top, Dest.Right, IntD.Top);
  S := Rect(IntS.Right, 0, Bitmap.Width, IntS.Top);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  | * |
  //  |   |
  //  |   |
  //   ---
  D := Rect(IntD.Left, Dest.Top, IntD.Right, IntD.Top);
  S := Rect(IntS.Left, 0, IntS.Right, IntS.Top);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
  //   ---
  //  |   |
  //  | * |
  //  |   |
  //   ---
  D := Rect(IntD.Left, IntD.Top, IntD.Right, IntD.Bottom);
  S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Bottom);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Alpha);
end;

{$ifdef VER230UP}
procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}
  .DrawThemeText(Details: TThemedElementDetails; Rect: TRect; Text: string;
  Format: TTextFormat);
var
  Opt: TStyleTextOptions;
begin
  if StyleServices.Enabled then
  begin
    Opt.TextColor := Self.Font.Color;
    StyleServices.DrawText(Handle, Details, Text, Rect, Format, Opt);
  end;
end;
{$endif}

//procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}
//  .StretchDraw(DestRect, ClipRect, SrcRect: TRect; Bitmap: TBitmap; Alpha: byte);
//var
//  BF: TBlendFunction;
//begin
//  if not IntersectRect(DestRect, ClipRect) then
//    exit;
//
//  BF.BlendOp := AC_SRC_OVER;
//  BF.BlendFlags := 0;
//  BF.SourceConstantAlpha := Alpha;
//  BF.AlphaFormat := AC_SRC_ALPHA;
//
//  // Cutting
//  //---
//  // Left:
//  if DestRect.Left < ClipRect.Left then
//  begin
//    // |----*-------|
//    // (Numerator * Number) / den
//    SrcRect.Left := SrcRect.Left + Trunc((SrcRect.Right - SrcRect.Left) * ((ClipRect.Left - DestRect.Left) / (DestRect.Right - DestRect.Left)));
//    DestRect.Left := ClipRect.Left;
//  end;
//  // Right
//  if DestRect.Right > ClipRect.Right then
//  begin
//    // |----*-------|
//    // (Numerator * Number) / den
//    SrcRect.Right := SrcRect.Right - Trunc((SrcRect.Right - SrcRect.Left) * ((DestRect.Right - ClipRect.Right) / (DestRect.Right - DestRect.Left)));
//    DestRect.Right := ClipRect.Right;
//  end;
//
//
//  AlphaBlend(Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
//    Bitmap.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, BF);
//end;

// REFACTOR ME PLEASE !!!
// TOOOOOOOOOO LONG PROCEDURE
// FFFFUUUUU!!!!1111
procedure {$ifdef VER210UP}TEsCanvasHelper{$else}TEsCanvas{$endif}
  .DrawNinePath(Dest, Bounds: TRect; Bitmap: TBitmap; Mode: TStretchMode;
  Alpha: Byte);
var
  dx, dy: Integer;
  D, S: TRect;
  IntD, IntS: TRect;
  W, H, X, Y: Integer;
begin
  if (Dest.Left >= Dest.Right)or(Dest.Top >= Dest.Bottom) then
    exit;

  if (Mode = TStretchMode.smHorzTileFit) or (Mode = TStretchMode.smHorzFit) then
  begin
    H := Bitmap.Height;
    Y := (Dest.Top + Dest.Bottom) div 2;
    Dest := Rect(Dest.Left, Y - H div 2, Dest.Right, Y + H - (H div 2));
  end else
  if (Mode = TStretchMode.smVertTileFit) or (Mode = TStretchMode.smVertFit) then
  begin
    W := Bitmap.Width;
    X := (Dest.Left + Dest.Right) div 2;
    Dest := Rect(X - W div 2, Dest.Top, X + W - (W div 2), Dest.Bottom);
  end;

  if (Mode = TStretchMode.smNormal) or (Mode = TStretchMode.smHorzFit) or (Mode = TStretchMode.smVertFit) then
  begin
    DrawNinePath(Dest, Bounds, Bitmap, Alpha);
    Exit;
  end;

  IntD := Rect(Dest.Left + Bounds.Left, Dest.Top + Bounds.Top,
    Dest.Right - Bounds.Right, Dest.Bottom - Bounds.Bottom);
  IntS := Rect(Bounds.Left, Bounds.Top, Bitmap.Width - Bounds.Right, Bitmap.Height - Bounds.Bottom);

  // needs to adjust to get rid of overdraw and painting was correct
  // cut left
  if Dest.Right - Dest.Left < Bounds.Left then
  begin
    dx := Bounds.Left - (Dest.Right - Dest.Left);
    IntD.Left := IntD.Left - dx;
    IntS.Left := IntS.Left - dx;
    //
    IntD.Right := Dest.Right;
  end else
  // cut right
  if Dest.Right - Dest.Left < Bounds.Left + Bounds.Right then
  begin
    dx := (Bounds.Left + Bounds.Right) - (Dest.Right - Dest.Left);
    IntD.Right := IntD.Right + dx;
    IntS.Right := IntS.Right + dx;
  end;
  // cut top
  if Dest.Bottom - Dest.Top < Bounds.Top then
  begin
    dy := Bounds.Top - (Dest.Bottom - Dest.Top);
    IntD.Top := IntD.Top - dy;
    IntS.Top := IntS.Top - dy;
    //
    IntD.Bottom := Dest.Bottom;
  end else
  // cut bottom
  if Dest.Bottom - Dest.Top < Bounds.Top + Bounds.Bottom then
  begin
    dy := (Bounds.Top + Bounds.Bottom) - (Dest.Bottom - Dest.Top);
    IntD.Bottom := IntD.Bottom + dy;
    IntS.Bottom := IntS.Bottom + dy;
  end;

//  // correct!
//  if IntD.Left > Dest.Right then
//    IntD.Left := Dest.Right;
//  if IntD.Top > Dest.Bottom then
//    IntD.Top := Dest.Bottom;
//  if IntD.Right < Dest.Left then
//    IntD.Right := Dest.Left;
//  if IntD.Bottom < Dest.Top then
//    IntD.Bottom := Dest.Top;

  if (Mode = TStretchMode.smHorzTile) or (Mode = TStretchMode.smHorzTileFit) then
  begin
    // Left Top
    D := Rect(Dest.Left, Dest.Top, IntD.Left, IntD.Top);
    S := Rect(0, 0, IntS.Left, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // Left Center
    D := Rect(Dest.Left, IntD.Top, IntD.Left, IntD.Bottom);
    S := Rect(0, IntS.Top, IntS.Left, IntS.Bottom);
    StretchDraw(D, S, Bitmap, Alpha);
    // Left Bottom
    D := Rect(Dest.Left, IntD.Bottom, IntD.Left, Dest.Bottom);
    S := Rect(0, IntS.Bottom, IntS.Left, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // Right Bottom
    D := Rect(IntD.Right, IntD.Bottom, Dest.Right, Dest.Bottom);
    S := Rect(IntS.Right, IntS.Bottom, Bitmap.Width, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // Right Center
    D := Rect(IntD.Right, IntD.Top, Dest.Right, IntD.Bottom);
    S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Bottom);
    StretchDraw(D, S, Bitmap, Alpha);
    // Right Top
    D := Rect(IntD.Right, Dest.Top, Dest.Right, IntD.Top);
    S := Rect(IntS.Right, 0, Bitmap.Width, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // [ I I I ]
    X := IntD.Left;
    W := RectWidth(IntS);
    if W > 0 then
      while X + W <= IntD.Right do
      begin
        // up
        D := Rect(X, Dest.Top, X + W, IntD.Top);
        S := Rect(IntS.Left, 0, IntS.Right, IntS.Top);
        if ValidRect(D) then
          StretchDraw(D, S, Bitmap, Alpha);
        // center
        D := Rect(X, IntD.Top, X + W, IntD.Bottom);
        S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Bottom);
        StretchDraw(D, S, Bitmap, Alpha);
        // down
        D := Rect(X, IntD.Bottom, X + W, Dest.Bottom);
        S := Rect(IntS.Left, IntS.Bottom, IntS.Right, Bitmap.Height);
        if ValidRect(D) then
          StretchDraw(D, S, Bitmap, Alpha);
        X := X + W;
      end;
    // cut up
    D := Rect(X, Dest.Top, IntD.Right, IntD.Top);
    S := Rect(IntS.Left, 0, IntS.Left + (IntD.Right - X), IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // cut center
    D := Rect(X, IntD.Top, IntD.Right, IntD.Bottom);
    S := Rect(IntS.Left, IntS.Top, IntS.Left + (IntD.Right - X), IntS.Bottom);
    StretchDraw(D, S, Bitmap, Alpha);
    // cut down
    D := Rect(X, IntD.Bottom, IntD.Right, Dest.Bottom);
    S := Rect(IntS.Left, IntS.Bottom, IntS.Left + (IntD.Right - X), Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
  end else
  if (Mode = TStretchMode.smVertTile) or (Mode = TStretchMode.smVertTileFit) then
  begin
    // Top Left
    D := Rect(Dest.Left, Dest.Top, IntD.Left, IntD.Top);
    S := Rect(0, 0, IntS.Left, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // Top Right
    D := Rect(IntD.Right, Dest.Top, Dest.Right, IntD.Top);
    S := Rect(IntS.Right, 0, Bitmap.Width, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // Top Center
    D := Rect(IntD.Left, Dest.Top, IntD.Right, IntD.Top);
    S := Rect(IntS.Left, 0, IntS.Right, IntS.Top);
    StretchDraw(D, S, Bitmap, Alpha);
    // Bottom Left
    D := Rect(Dest.Left, IntD.Bottom, IntD.Left, Dest.Bottom);
    S := Rect(0, IntS.Bottom, IntS.Left, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // Bottom Center
    D := Rect(IntD.Left, IntD.Bottom, IntD.Right, Dest.Bottom);
    S := Rect(IntS.Left, IntS.Bottom, IntS.Right, Bitmap.Height);
    StretchDraw(D, S, Bitmap, Alpha);
    // Bottom Right
    D := Rect(IntD.Right, IntD.Bottom, Dest.Right, Dest.Bottom);
    S := Rect(IntS.Right, IntS.Bottom, Bitmap.Width, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // [ I I I ]
    Y := IntD.Top;
    H := RectHeight(IntS);
    if H > 0 then
      while Y + H <= IntD.Bottom do
      begin
        // left
        D := Rect(Dest.Left, Y, IntD.Left, Y + H);
        S := Rect(0, IntS.Top, IntS.Left, IntS.Bottom);
        if ValidRect(D) then
          StretchDraw(D, S, Bitmap, Alpha);
        // center
        D := Rect(IntD.Left, Y, IntD.Right, Y + H);
        S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Bottom);
        StretchDraw(D, S, Bitmap, Alpha);
        // right
        D := Rect(IntD.Right, Y, Dest.Right, Y + H);
        S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Bottom);
        if ValidRect(D) then
          StretchDraw(D, S, Bitmap, Alpha);
        Y := Y + H;
      end;
    // cut left
    D := Rect(Dest.Left, Y, IntD.Left, IntD.Bottom);
    S := Rect(0, IntS.Top, IntS.Left, IntS.Top + (IntD.Bottom - Y));
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
    // cut center
    D := Rect(IntD.Left, Y, IntD.Right, IntD.Bottom);
    S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Top + (IntD.Bottom - Y));
    StretchDraw(D, S, Bitmap, Alpha);
    // cut right
    D := Rect(IntD.Right, Y, Dest.Right, IntD.Bottom);
    S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Top + (IntD.Bottom - Y));
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Alpha);
  end else
  if (Mode = TStretchMode.smTile) then
  begin
    if (Bitmap.Width <> 0)and(Bitmap.Height <> 0) then
    begin
      Y := Dest.Top;
      repeat
        if Y + Bitmap.Height <= Dest.Bottom then
          H := Bitmap.Height
        else
          H := Dest.Bottom - Y;
        X := Dest.Left;
        repeat
          if X + Bitmap.Width <= Dest.Right  then
            W := Bitmap.Width
          else
            W := Dest.Right - X;

          StretchDraw(Rect(X, Y, X + W, Y + H), Rect(0, 0, W, H), Bitmap, Alpha);
          X := X + Bitmap.Width;
        until X >= Dest.Right;
        Y := Y + Bitmap.Height;
      until Y >= Dest.Bottom;
    end;
  end;
end;

end.
