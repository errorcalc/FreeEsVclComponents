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
{                                                                              }
{ LICENSE:                                                                     }
{ This library is Open Source software, can be used this in commercial         }
{ projects, modify, and distribute as source code or binary files.             }
{ ===                                                                          }
{ This library licensed at two license: GNU GPLv2 & Modified MIT License(MIT)  }
{ You can choose one of two license.                                           }
{ 1. GNU GPL v2: https://www.gnu.org/licenses/gpl2.html                        }
{ 2. Modified MIT License (MIT):                                               }
{ ===                                                                          }
{ Modified MIT License (MIT)                                                   }
{ Copyright (c) 2009-2021 Peter Sokolov, errorsoft.org, errorsoft(c)           }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"),        }
{ to deal in the Software without restriction, including without limitation    }
{ the rights to use, copy, modify, merge, publish, distribute, sublicense,     }
{ and/or sell copies of the Software, and to permit persons to whom the        }
{ Software is furnished to do so, subject to the following conditions:         }
{ 1. The above copyright notice and this permission notice shall be included   }
{    in all copies or substantial portions of the Software.                    }
{ 2. Do not have to sell this library as a standalone components library or    }
{    as part of another components library.                                    }
{    (you can agree with me on licensing).                                     }
{ 3. Desirable specify the use of this library in your software                }
{    (for example: about window).                                              }
{ ===                                                                          }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      }
{ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{******************************************************************************}

// -----------------------------------------------------------------------------
//                 Module for direct access to pixels at TBitmap
//
// This module/file does not depend on other units/files ErrorsoftVclComponents,
// you can use it separately from other units in your project.
// -----------------------------------------------------------------------------

{$REGION 'Examples'}
(*
  Examples of use:

  --- 1 ---
  procedure InvertColors(const Bitmap: TBitmap);
  var
    Data: TBitmapData;
    X, Y: Integer;
    Pixel: TPixelRec;
  begin
    Data.Map(Bitmap, TAccessMode.ReadWrite, False);// RGB access
    try
      for Y := 0 to Data.Height - 1 do
      begin
        for X := 0 to Data.Width - 1 do
        begin
          Pixel := Data.GetPixel(X, Y);
          Pixel.R := 255 - Pixel.R;
          Pixel.G := 255 - Pixel.G;
          Pixel.B := 255 - Pixel.B;
          Data.SetPixel(X, Y, Pixel);
        end;
      end;
    finally
      Data.Unmap();
    end;
  end;

  --- 2 ---
  procedure HalfAlpha(const Bitmap: TBitmap);
  var
    Data: TBitmapData;
    X, Y: Integer;
    Pixel: TPixelRec;
  begin
    Data.Map(Bitmap, TAccessMode.ReadWrite, True);// ARGB access
    try
      for Y := 0 to Data.Height - 1 do
      begin
        for X := 0 to Data.Width - 1 do
        begin
          Pixel := Data.GetPixel(X, Y);
          Pixel.A := Pixel.A div 2;
          Data.SetPixel(X, Y, Pixel);
        end;
      end;
    finally
      Data.Unmap();
    end;
  end;

  --- 3 ---
  function MakePlasm(): TBitmap;
  var
    Data: TBitmapData;
    X, Y: Integer;
    Pixel: TPixelRec;
  begin
    Result := TBitmap.Create();
    Result.SetSize(300, 300);

    Data.Map(Result, TAccessMode.Write, False);
    try
      for Y := 0 to Data.Height - 1 do
      begin
        for X := 0 to Data.Width - 1 do
        begin
          Pixel.R := Byte(Trunc(
            100 + 100 * (Sin(X * Cos(Y * 0.049) * 0.01) + Cos(X * 0.0123 - Y * 0.09))));
          Pixel.G := 0;
          Pixel.B := Byte(Trunc(
            Pixel.R + 100 * (Sin(X * Cos(X * 0.039) * 0.022) + Sin(X * 0.01 - Y * 0.029))));
          Data.SetPixel(X, Y, Pixel);
        end;
      end;
    finally
      Data.Unmap();
    end;
  end;

  --- 4 ---
  function Mix(const A, B: TBitmap): TBitmap;
    function Min(A, B: Integer): Integer;
    begin
      if A < B then Exit(A) else Exit(B);
    end;
  var
    DataA, DataB, DataResult: TBitmapData;
    X, Y: Integer;
    PixelA, PixelB, PixelResult: TPixelRec;
  begin
    Result := TBitmap.Create();
    Result.SetSize(Min(A.Width, B.Width), Min(A.Height, B.Height));
    // this needed for correct Unmap() on exception
    DataA.Init();
    DataB.Init();
    DataResult.Init();
    try
      DataA.Map(A, TAccessMode.Read, False);
      DataB.Map(B, TAccessMode.Read, False);
      DataResult.Map(Result, TAccessMode.Write, False);
      for Y := 0 to DataResult.Height - 1 do
      begin
        for X := 0 to DataResult.Width - 1 do
        begin
          PixelA := DataA.Pixels[X, Y];
          PixelB := DataB.Pixels[X, Y];
          PixelResult.R := (PixelA.R + PixelB.R) div 2;
          PixelResult.G := (PixelA.G + PixelB.G) div 2;
          PixelResult.B := (PixelA.B + PixelB.B) div 2;
          DataResult[X, Y] := PixelResult;
        end;
      end;
    finally
      DataA.Unmap();
      DataB.Unmap();
      DataResult.Unmap();
    end;
  end;
*)
{$ENDREGION}

unit ES.BitmapPixels;

{$SCOPEDENUMS ON}
{$POINTERMATH ON}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

interface

uses
  Windows, Classes, SysUtils, Graphics;

type
  // $AARRGGBB
  TPixel = Cardinal;
  PPixel = ^TPixel;

  TPixelArray = array [0..High(Integer) div SizeOf(TPixel) - 1] of TPixel;
  PPixelArray = ^TPixelArray;

  TAccessMode = (Read, Write, ReadWrite);

  { TBitmapData }

  TBitmapData = record
  private
    FData: PPixelArray;
    FBitmap: TBitmap;
    FWidth: Integer;
    FHeight: Integer;
    FAccessMode: TAccessMode;
    FHasAlpha: Boolean;
    FDataArray: array of TPixel;// do not use this
  public
    procedure Init();
    procedure Map(const Bitmap: TBitmap; const Mode: TAccessMode; const UseAlpha: Boolean;
      const Background: TColor = clNone);
    procedure Unmap();
    function GetPixel(X, Y: Integer): TPixel; inline;
    procedure SetPixel(X, Y: Integer; AValue: TPixel); inline;
    function GetPixelUnsafe(X, Y: Integer): TPixel; inline;
    procedure SetPixelUnsafe(X, Y: Integer; AValue: TPixel); inline;
    property Data: PPixelArray read FData;
    property HasAlpha: Boolean read FHasAlpha;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixels[X, Y: Integer]: TPixel read GetPixel write SetPixel; default;
  end;

  { TPixelRec }

  TPixelRec = packed record
    constructor Create(const R, G, B: Byte; const A: Byte = 255);
    class operator Implicit(Pixel: TPixel): TPixelRec; inline;
    class operator Implicit(Pixel: TPixelRec): TPixel; inline;
  case Byte of
    0: (B, G, R, A: Byte);
    1: (Color: TPixel);
  end;

  TPixelColors = record
  {$REGION 'HTML Colors'}
  const
    AliceBlue = $FFF0F8FF;
    AntiqueWhite = $FFFAEBD7;
    Aqua = $FF00FFFF;
    Aquamarine = $FF7FFFD4;
    Azure = $FFF0FFFF;
    Beige = $FFF5F5DC;
    Bisque = $FFFFE4C4;
    Black = $FF000000;
    BlanchedAlmond = $FFFFEBCD;
    Blue = $FF0000FF;
    BlueViolet = $FF8A2BE2;
    Brown = $FFA52A2A;
    BurlyWood = $FFDEB887;
    CadetBlue = $FF5F9EA0;
    Chartreuse = $FF7FFF00;
    Chocolate = $FFD2691E;
    Coral = $FFFF7F50;
    CornflowerBlue = $FF6495ED;
    Cornsilk = $FFFFF8DC;
    Crimson = $FFDC143C;
    Cyan = $FF00FFFF;
    DarkBlue = $FF00008B;
    DarkCyan = $FF008B8B;
    DarkGoldenrod = $FFB8860B;
    DarkGray = $FFA9A9A9;
    DarkGreen = $FF006400;
    DarkKhaki = $FFBDB76B;
    DarkMagenta = $FF8B008B;
    DarkOliveGreen = $FF556B2F;
    DarkOrange = $FFFF8C00;
    DarkOrchid = $FF9932CC;
    DarkRed = $FF8B0000;
    DarkSalmon = $FFE9967A;
    DarkSeaGreen = $FF8FBC8F;
    DarkSlateBlue = $FF483D8B;
    DarkSlateGray = $FF2F4F4F;
    DarkTurquoise = $FF00CED1;
    DarkViolet = $FF9400D3;
    DeepPink = $FFFF1493;
    DeepSkyBlue = $FF00BFFF;
    DimGray = $FF696969;
    DodgerBlue = $FF1E90FF;
    FireBrick = $FFB22222;
    FloralWhite = $FFFFFAF0;
    ForestGreen = $FF228B22;
    Fuchsia = $FFFF00FF;
    Gainsboro = $FFDCDCDC;
    GhostWhite = $FFF8F8FF;
    Gold = $FFFFD700;
    Goldenrod = $FFDAA520;
    Gray = $FF808080;
    Green = $FF008000;
    GreenYellow = $FFADFF2F;
    Honeydew = $FFF0FFF0;
    HotPink = $FFFF69B4;
    IndianRed = $FFCD5C5C;
    Indigo = $FF4B0082;
    Ivory = $FFFFFFF0;
    Khaki = $FFF0E68C;
    Lavender = $FFE6E6FA;
    LavenderBlush = $FFFFF0F5;
    LawnGreen = $FF7CFC00;
    LemonChiffon = $FFFFFACD;
    LightBlue = $FFADD8E6;
    LightCoral = $FFF08080;
    LightCyan = $FFE0FFFF;
    LightGoldenrodYellow = $FFFAFAD2;
    LightGreen = $FF90EE90;
    LightGrey = $FFD3D3D3;
    LightPink = $FFFFB6C1;
    LightSalmon = $FFFFA07A;
    LightSeaGreen = $FF20B2AA;
    LightSkyBlue = $FF87CEFA;
    LightSlateGray = $FF778899;
    LightSteelBlue = $FFB0C4DE;
    LightYellow = $FFFFFFE0;
    Lime = $FF00FF00;
    LimeGreen = $FF32CD32;
    Linen = $FFFAF0E6;
    Magenta = $FFFF00FF;
    Maroon = $FF800000;
    MediumAquamarine = $FF66CDAA;
    MediumBlue = $FF0000CD;
    MediumOrchid = $FFBA55D3;
    MediumPurple = $FF9370DB;
    MediumSeaGreen = $FF3CB371;
    MediumSlateBlue = $FF7B68EE;
    MediumSpringGreen = $FF00FA9A;
    MediumTurquoise = $FF48D1CC;
    MediumVioletRed = $FFC71585;
    MidnightBlue = $FF191970;
    MintCream = $FFF5FFFA;
    MistyRose = $FFFFE4E1;
    Moccasin = $FFFFE4B5;
    NavajoWhite = $FFFFDEAD;
    Navy = $FF000080;
    OldLace = $FFFDF5E6;
    Olive = $FF808000;
    OliveDrab = $FF6B8E23;
    Orange = $FFFFA500;
    OrangeRed = $FFFF4500;
    Orchid = $FFDA70D6;
    PaleGoldenrod = $FFEEE8AA;
    PaleGreen = $FF98FB98;
    PaleTurquoise = $FFAFEEEE;
    PaleVioletRed = $FFDB7093;
    PapayaWhip = $FFFFEFD5;
    PeachPuff = $FFFFDAB9;
    Peru = $FFCD853F;
    Pink = $FFFFC0CB;
    Plum = $FFDDA0DD;
    PowderBlue = $FFB0E0E6;
    Purple = $FF800080;
    Red = $FFFF0000;
    RosyBrown = $FFBC8F8F;
    RoyalBlue = $FF4169E1;
    SaddleBrown = $FF8B4513;
    Salmon = $FFFA8072;
    SandyBrown = $FFF4A460;
    SeaGreen = $FF2E8B57;
    Seashell = $FFFFF5EE;
    Sienna = $FFA0522D;
    Silver = $FFC0C0C0;
    SkyBlue = $FF87CEEB;
    SlateBlue = $FF6A5ACD;
    SlateGray = $FF708090;
    Snow = $FFFFFAFA;
    SpringGreen = $FF00FF7F;
    SteelBlue = $FF4682B4;
    Tan = $FFD2B48C;
    Teal = $FF008080;
    Thistle = $FFD8BFD8;
    Tomato = $FFFF6347;
    Turquoise = $FF40E0D0;
    Violet = $FFEE82EE;
    Wheat = $FFF5DEB3;
    White = $FFFFFFFF;
    WhiteSmoke = $FFF5F5F5;
    Yellow = $FFFFFF00;
    YellowGreen = $FF9ACD32;
    {$ENDREGION}
  end;

function MakePixel(const R, G, B: Byte; const A: Byte = 255): TPixel; inline;
function PixelGetA(const Pixel: TPixel): Byte; inline;
function PixelGetR(const Pixel: TPixel): Byte; inline;
function PixelGetG(const Pixel: TPixel): Byte; inline;
function PixelGetB(const Pixel: TPixel): Byte; inline;
function ColorToPixel(const Color: TColor): TPixel; inline;
function PixelToColor(const Pixel: TPixel): TColor; inline;
function ClipPixelValue(const Value: Integer): Byte;

implementation

function MakePixel(const R, G, B: Byte; const A: Byte): TPixel; inline;
begin
  Result := B or (G shl 8) or (R shl 16) or (A shl 24);
end;

function PixelGetA(const Pixel: TPixel): Byte; inline;
begin
  Result := (Pixel shr 24) and $FF;
end;

function PixelGetR(const Pixel: TPixel): Byte; inline;
begin
  Result := (Pixel shr 16) and $FF;
end;

function PixelGetG(const Pixel: TPixel): Byte; inline;
begin
  Result := (Pixel shr 8) and $FF;
end;

function PixelGetB(const Pixel: TPixel): Byte; inline;
begin
  Result := Pixel and $FF;
end;

function ColorToPixel(const Color: TColor): TPixel; inline;
begin
  Result:=
    (UInt32(Color) and $0000FF00) or
    ((UInt32(Color) shr 16) and $000000FF) or
    ((UInt32(Color) shl 16) and $00FF0000) or
    $FF000000;
end;

function PixelToColor(const Pixel: TPixel): TColor; inline;
begin
  Result:=
    (Pixel and $0000FF00) or
    ((Pixel shr 16) and $000000FF) or
    ((Pixel shl 16) and $00FF0000);
end;

function ClipPixelValue(const Value: Integer): Byte;
begin
  if Value < 0 then
    Result := 0
  else if Value > 255 then
    Result := 255
  else
    Result := Value;
end;


function SwapRedBlueChanel(const Pixel: TPixel): TPixel; inline;
begin
  Result:=
    (Pixel and $FF00FF00) or
    ((Pixel shr 16) and $000000FF) or
    ((Pixel shl 16) and $00FF0000);
end;

procedure BlendData(var BitmapData: TBitmapData; const Background: TColor);
var
  I: Integer;
  R, G, B, A: Byte;
  DestR, DestG, DestB: Byte;
  C: TPixel;
begin
  DestR := Background and $FF;
  DestG := (Background shr 8) and $FF;
  DestB := (Background shr 16) and $FF;

  for I := 0 to BitmapData.FWidth * BitmapData.FHeight - 1 do
  begin
    C := BitmapData.FData[I];
    // read
    B := C and $FF;
    G := (C shr 8) and $FF;
    R := (C shr 16) and $FF;
    A := (C shr 24) and $FF;
    // blend
    B := ((B * A) + (DestB * (255 - A))) div 255;
    G := ((G * A) + (DestG * (255 - A))) div 255;
    R := ((R * A) + (DestR * (255 - A))) div 255;
    // write
    BitmapData.FData[I] := B or ((G shl 8) and $0000FF00) or ((R shl 16) and $00FF0000);
  end;
end;

procedure ReadData(var BitmapData: TBitmapData);
var
  TempBitmap: TBitmap;
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PByte;
begin
  TempBitmap := TBitmap.Create();
  try
    TempBitmap.Assign(BitmapData.FBitmap);
    TempBitmap.PixelFormat := pf24bit;

    pDst := @(BitmapData.FData[0]);
    for Y := 0 to BitmapData.FHeight - 1 do
    begin
      pSrc := TempBitmap.ScanLine[Y];
      for X := 0 to BitmapData.FWidth - 1 do
      begin
        pDst^ := pSrc[0] or (pSrc[1] shl 8) or (pSrc[2] shl 16);
        Inc(pDst, 1);
        Inc(pSrc, 3);
      end;
    end;
  finally
    TempBitmap.Free;
  end;
end;

procedure ReadDataOpaque(var BitmapData: TBitmapData);
var
  TempBitmap: TBitmap;
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PByte;
begin
  TempBitmap := TBitmap.Create();
  try
    TempBitmap.Assign(BitmapData.FBitmap);
    TempBitmap.PixelFormat := pf24bit;

    pDst := @(BitmapData.FData[0]);
    for Y := 0 to BitmapData.FHeight - 1 do
    begin
      pSrc := TempBitmap.ScanLine[Y];
      for X := 0 to BitmapData.FWidth - 1 do
      begin
        pDst^ := pSrc[0] or (pSrc[1] shl 8) or (pSrc[2] shl 16) or $FF000000;
        Inc(pDst, 1);
        Inc(pSrc, 3);
      end;
    end;
  finally
    TempBitmap.Free;
  end;
end;

procedure ReadDataBGR(var BitmapData: TBitmapData);
var
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PByte;
begin
  pDst := @(BitmapData.FData[0]);
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pSrc := BitmapData.FBitmap.ScanLine[Y];
    for X := 0 to BitmapData.FWidth - 1 do
    begin
      pDst^ := pSrc[0] or (pSrc[1] shl 8) or (pSrc[2] shl 16);
      Inc(pDst, 1);
      Inc(pSrc, 3);
    end;
  end;
end;

procedure ReadDataBGROpaque(var BitmapData: TBitmapData);
var
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PByte;
begin
  pDst := @(BitmapData.FData[0]);
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pSrc := BitmapData.FBitmap.ScanLine[Y];
    for X := 0 to BitmapData.FWidth - 1 do
    begin
      pDst^ := pSrc[0] or (pSrc[1] shl 8) or (pSrc[2] shl 16) or $FF000000;
      Inc(pDst, 1);
      Inc(pSrc, 3);
    end;
  end;
end;

procedure ReadDataBGRA(var BitmapData: TBitmapData);// fast
var
  Y: Integer;
  pDst: PPixel;
  pSrc: PPixel;
begin
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pSrc := BitmapData.FBitmap.ScanLine[Y];
    pDst := @(BitmapData.FData[BitmapData.FWidth * Y]);
    Move(pSrc^, pDst^, BitmapData.FWidth * SizeOf(TPixel));
  end;
end;

procedure ReadDataPremultipliedBGRA(var BitmapData: TBitmapData);// slow
var
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PPixel;
  Pixel: TPixelRec;
begin
  pDst := @(BitmapData.FData[0]);
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pSrc := BitmapData.FBitmap.ScanLine[Y];
    for X := 0 to BitmapData.FWidth - 1 do
    begin
      Pixel := pSrc^;
      Pixel.R := MulDiv(Pixel.R, 255, Pixel.A);
      Pixel.G := MulDiv(Pixel.G, 255, Pixel.A);
      Pixel.B := MulDiv(Pixel.B, 255, Pixel.A);
      pDst^ := Pixel;
      Inc(pDst, 1);
      Inc(pSrc, 1);
    end;
  end;
end;

procedure ReadDataBGRN(var BitmapData: TBitmapData);
var
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PPixel;
begin
  pDst := @(BitmapData.FData[0]);
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pSrc := BitmapData.FBitmap.ScanLine[Y];
    for X := 0 to BitmapData.FWidth - 1 do
    begin
      pDst^ := pSrc^ and $00FFFFFF;
      Inc(pDst, 1);
      Inc(pSrc, 1);
    end;
  end;
end;

procedure WriteDataBGRA(var BitmapData: TBitmapData);// fast
var
  Y: Integer;
  pDst: PPixel;
  pSrc: PPixel;
begin
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pSrc := @(BitmapData.FData[BitmapData.FWidth * Y]);
    pDst := BitmapData.FBitmap.ScanLine[Y];
    Move(pSrc^, pDst^, BitmapData.FWidth * SizeOf(TPixel));
  end;
end;

procedure WriteDataPremultipliedBGRA(var BitmapData: TBitmapData);// slow
var
  X, Y: Integer;
  pDst: PPixel;
  pSrc: PPixel;
  Pixel: TPixelRec;
begin
  pSrc := @(BitmapData.FData[0]);
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pDst := BitmapData.FBitmap.ScanLine[Y];
    for X := 0 to BitmapData.FWidth - 1 do
    begin
      Pixel := pSrc^;
      Pixel.R := MulDiv(Pixel.R, Pixel.A, 255);
      Pixel.G := MulDiv(Pixel.G, Pixel.A, 255);
      Pixel.B := MulDiv(Pixel.B, Pixel.A, 255);
      pDst^ := Pixel;
      Inc(pDst, 1);
      Inc(pSrc, 1);
    end;
  end;
end;

procedure WriteDataBGR(var BitmapData: TBitmapData);
var
  X, Y: Integer;
  pDst: PByte;
  pSrc: PPixel;
begin
  pSrc := @(BitmapData.FData[0]);
  for Y := 0 to BitmapData.FHeight - 1 do
  begin
    pDst := BitmapData.FBitmap.ScanLine[Y];
    for X := 0 to BitmapData.FWidth - 1 do
    begin
      pDst[0] := pSrc^ and $FF;
      pDst[1] := (pSrc^ shr 8) and $FF;
      pDst[2] := (pSrc^ shr 16) and $FF;
      Inc(pDst, 3);
      Inc(pSrc, 1);
    end;
  end;
end;

procedure BitmapDataMap(out BitmapData: TBitmapData; const Bitmap: TBitmap; const Mode: TAccessMode;
  const UseAlpha: Boolean; const Background: TColor);
begin
  BitmapData.FBitmap := Bitmap;
  BitmapData.FAccessMode := Mode;
  BitmapData.FHasAlpha := UseAlpha;
  BitmapData.FWidth := Bitmap.Width;
  BitmapData.FHeight := Bitmap.Height;

  SetLength(BitmapData.FDataArray, BitmapData.FHeight * BitmapData.FWidth);
  if Length(BitmapData.FDataArray) > 0 then
    BitmapData.FData := @(BitmapData.FDataArray[0])
  else
    BitmapData.FData := nil;

  if (BitmapData.FData <> nil) and (BitmapData.FAccessMode in [TAccessMode.ReadWrite, TAccessMode.Read]) then
  begin
    case BitmapData.FBitmap.PixelFormat of
    pfDevice,
    pf1bit,
    pf4bit,
    pf8bit,
    pf15bit,
    pf16bit,
    pfCustom:
      if BitmapData.FHasAlpha then
        ReadDataOpaque(BitmapData)
      else
        ReadData(BitmapData);

    pf24bit:
      if BitmapData.FHasAlpha then
        ReadDataBGROpaque(BitmapData)
      else
        ReadDataBGR(BitmapData);

    pf32bit:
      if BitmapData.FHasAlpha then
      begin
        if BitmapData.FBitmap.AlphaFormat = afIgnored then
          ReadDataBGRA(BitmapData)
        else
          ReadDataPremultipliedBGRA(BitmapData);
      end
      else
        if Background <> clNone then
        begin
          if BitmapData.FBitmap.AlphaFormat = afIgnored then
            ReadDataBGRA(BitmapData)
          else
            ReadDataPremultipliedBGRA(BitmapData);
          BlendData(BitmapData, Background);
        end else
          ReadDataBGRN(BitmapData);
    end;
  end;
end;

type
  TOpenBitmap = class(TBitmap);

procedure BitmapDataUnmap(var BitmapData: TBitmapData);
begin
  try
    if (BitmapData.FData <> nil) and (BitmapData.FAccessMode in [TAccessMode.ReadWrite, TAccessMode.Write]) then
    begin
      if BitmapData.FHasAlpha then
      begin
        if (BitmapData.FBitmap.PixelFormat = pf32bit) and (BitmapData.FBitmap.AlphaFormat = afIgnored) then
        begin
          WriteDataBGRA(BitmapData);// fast way
        end else
        begin
          BitmapData.FBitmap.PixelFormat := pf32bit;
          TOpenBitmap(BitmapData.FBitmap).FAlphaFormat := afPremultiplied;
          WriteDataPremultipliedBGRA(BitmapData);
        end;
      end else
      begin
        BitmapData.FBitmap.PixelFormat := pf24bit;
        WriteDataBGR(BitmapData);
      end;

      BitmapData.FBitmap.Modified := True;
    end;
  finally
    Finalize(BitmapData.FDataArray);
  end;
end;

{ TPixelRec }

constructor TPixelRec.Create(const R, G, B: Byte; const A: Byte);
begin
  Self.Color := B or (G shl 8) or (R shl 16) or (A shl 24);
end;

class operator TPixelRec.Implicit(Pixel: TPixel): TPixelRec;
begin
  Result.Color := Pixel;
end;

class operator TPixelRec.Implicit(Pixel: TPixelRec): TPixel;
begin
  Result := Pixel.Color;
end;

{ TBitmapData }

function TBitmapData.GetPixel(X, Y: Integer): TPixel;
begin
  if (X < 0) or (X >= Self.Width) or (Y < 0) or (Y >= Self.Height) then Exit(0);
  Result := Self.Data[Y * Self.Width + X];
end;

procedure TBitmapData.SetPixel(X, Y: Integer; AValue: TPixel);
begin
  if (X < 0) or (X >= Self.Width) or (Y < 0) or (Y >= Self.Height) then Exit;
  Self.Data[Y * Self.Width + X] := AValue;
end;

function TBitmapData.GetPixelUnsafe(X, Y: Integer): TPixel;
begin
  Result := Self.Data[Y * Self.Width + X];
end;

procedure TBitmapData.SetPixelUnsafe(X, Y: Integer; AValue: TPixel);
begin
  Self.Data[Y * Self.Width + X] := AValue;
end;

procedure TBitmapData.Init();
begin
  Self := Default(TBitmapData);
end;

procedure TBitmapData.Map(const Bitmap: TBitmap; const Mode: TAccessMode; const UseAlpha: Boolean;
      const Background: TColor = clNone);
begin
  BitmapDataMap(Self, Bitmap, Mode, UseAlpha, Background);
end;

procedure TBitmapData.Unmap();
begin
  BitmapDataUnmap(Self);
end;

end.


