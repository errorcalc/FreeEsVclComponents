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
unit ES.ExGraphics;

interface

{$I 'EsDefines.inc'}
{$I 'EsVclCore.inc'}
{$SCOPEDENUMS ON}

uses
  WinApi.Windows, System.SysUtils, Vcl.Controls, Vcl.Graphics, Vcl.Themes, Vcl.Imaging.PngImage
  {$IFDEF USE_GDIPLUS}, WinApi.GdipObj, WinApi.GdipApi{$ENDIF};

type
  TStretchMode = (Normal, Tile, HorzFit, VertFit, HorzTile, VertTile, HorzTileFit, VertTileFit);

  {$REGION 'deprecated names'}
  {$IFDEF SUPPORT_ENUMS_ALIASES}
  TStretchModeHelper = record helper for TStretchMode
  const
    smNormal = TStretchMode.Normal deprecated 'Use TStretchMode.Normal';
    smTile = TStretchMode.Tile deprecated 'Use TStretchMode.Tile';
    smHorzFit = TStretchMode.HorzFit deprecated 'Use TStretchMode.HorzFit';
    smVertFit = TStretchMode.VertFit deprecated 'Use TStretchMode.VertFit';
    smHorzTile = TStretchMode.HorzTile deprecated 'Use TStretchMode.HorzTile';
    smVertTile = TStretchMode.VertTile deprecated 'Use TStretchMode.VertTile';
    smHorzTileFit = TStretchMode.HorzTileFit deprecated 'Use TStretchMode.HorzTileFit';
    smVertTileFit = TStretchMode.VertTileFit deprecated 'Use TStretchMode.VertTileFit';
  end;
  {$ENDIF}
  {$ENDREGION}

  /// <summary> Class for save canvas state </summary>
  TGraphicsClass = class of TGraphicsObject;
  ICanvasSaver = interface end;
  TCanvasSaver = class(TInterfacedObject, ICanvasSaver)
  private
    FPen: TPen;
    FBrush: TBrush;
    FFont: TFont;
    FCanvas: TCanvas;
    function GetBrush: TBrush;
    function GetFont: TFont;
    function GetPen: TPen;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
  public
    property Pen: TPen read GetPen write SetPen;
    property Brush: TBrush read GetBrush write SetBrush;
    property Font: TFont read GetFont write SetFont;

    constructor Create(Canvas: TCanvas);
    destructor Destroy; override;
    procedure Restore;
  end;

  TEsCanvasHelper = class helper for TCanvas
  public
    procedure DrawHighQuality(ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255; HighQuality: Boolean = False); overload;
    procedure DrawHighQuality(ARect: TRect; Graphic: TGraphic; Opacity: Byte = 255; HighQuality: Boolean = False); overload;
    procedure StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap); overload;
    /// <summary> Only for 32bit premultipled bitmaps!</summary>
    procedure StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap; Opacity: Byte); overload;
    procedure StretchDraw(Rect: TRect; Graphic: TGraphic; Opacity: Byte); overload;
    procedure Draw(X, Y: Integer; Graphic: TGraphic; Opacity: Byte); overload;
    procedure DrawNinePatch(Dest: TRect; Bounds: TRect; Bitmap: TBitmap); overload;
    procedure DrawNinePatch(Dest: TRect; Bounds: TRect; Bitmap: TBitmap; Opacity: Byte); overload;
    procedure DrawNinePatch(Dest: TRect; Bounds: TRect; Bitmap: TBitmap; Mode: TStretchMode; Opacity: Byte = 255); overload;
    procedure DrawThemeText(Details: TThemedElementDetails; Rect: TRect; Text: string; Format: TTextFormat);
    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure DrawChessFrame(R: TRect; Color1, Color2: TColor); overload;
    procedure DrawTransparentFrame(R: TRect; Color1, Color2: TColor; Opacity: Integer = -1; const Mask: ShortString = '12');
    procedure DrawInsideFrame(R: TRect; Width: Integer; Color: TColor = clNone);
    procedure DrawCorners(R: TRect; Width: Integer);
    procedure FillRect(R: TRect; Color: TColor); overload;
    // support save/restore state
    function SaveState(const Objects: array of TGraphicsClass): ICanvasSaver; overload;
    function SaveState: ICanvasSaver; overload;
    procedure RestoreState(var State: ICanvasSaver);
  end;

  TEsBitMap = class(TBitmap)
  private
  protected
    property Palette;
    property AlphaFormat;
    property PixelFormat;
  public
    Constructor Create; override;
    {$IFDEF OLD_BITMAP_PROC}
    procedure PreMultiplyAlpha;
    procedure UnPreMultiplyAlpha;
    {$ENDIF}
    procedure LoadFromResourceName(Instance: THandle; const ResName: String; ResType: PChar); overload;
  end;

  // Utils
  function ColorToAlphaColor(Color: TColor; Alpha: byte = 255): DWORD; Inline;
  function RgbToArgb(Color: TColor; Alpha: byte = 255): DWORD; Inline;
  procedure DrawBitmapHighQuality(Handle: THandle; ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255;
    HighQality: Boolean = False; EgdeFill: Boolean = False);
  procedure PngImageAssignToBitmap(Bitmap: TBitmap; PngImage: TPngImage; IsPremultipledBitmap: Boolean = True);
  procedure BitmapAssignToPngImage(PngImage: TPngImage; Bitmap: TBitmap; IsPremultipledBitmap: Boolean = True);
  procedure GraphicAssignToBitmap(Bitmap: TBitmap; Graphic: TGraphic); Inline;
  {$IFDEF USE_GDIPLUS}
  function BitmapToGPBitmap(Bitmap: TBitmap): TGPBitmap;
  {$ENDIF}

  procedure GaussianBlur(Bitmap: TBitmap; Radius: Real);
  /// <summary>A cool procedure for a really fast Blur!</summary>
  procedure FastBlur(Bitmap: TBitmap; Radius: Real; BlurScale: Integer; HighQuality: Boolean = True);
  /// <summary>Draw halftone bitmap to canvas</summary>
  procedure DrawHalftoneBitmap(Canvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
   Value: Byte; Color: TColor = clBlack);
  /// <summary>Halftone bitmap</summary>
  procedure HalftoneBitmap(Bitmap: TBitmap; Value: Byte; Color: TColor = clBlack);

  // Color spaces
  procedure ColorToHSL(Color: TColor; var H, S, L: Integer);
  function HSLToColor(Hue, Saturation, Lightness: Integer): TColor;
  //---
  function LuminanceColor(Color: TColor; Value: Integer): TColor;
  function HighlightColor(Color: TColor; Value: Integer): TColor;
  function GetLuminanceColor(Color: TColor): Byte;

implementation

uses
  System.Classes, System.Types, Vcl.GraphUtil, System.UITypes,
  System.TypInfo, System.Math;

//------------------------------------------------------------------------------
// Utils
//------------------------------------------------------------------------------

type
  TRGBAArray = array[Word] of TRGBQuad;
  PRGBAArray = ^TRGBAArray;
  TRGBArray = array[Word] of TRGBTriple;
  PRGBArray = ^TRGBArray;

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

procedure DrawBitmapHighQuality(Handle: THandle; ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255;
  HighQality: Boolean = False; EgdeFill: Boolean = False);
var
  {$IFDEF USE_GDIPLUS}
  Graphics: TGPGraphics;
  GdiPBitmap: TGPBitmap;
  Attr: TGPImageAttributes;
  M: TColorMatrix;
  {$ELSE}
  BF: TBlendFunction;
  {$ENDIF}
begin
  {$IFDEF USE_GDIPLUS}
  if Bitmap.Empty then
    Exit;

  GdiPBitmap := nil;
  Graphics := TGPGraphics.Create(Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeDefault);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);

    if not HighQality then
      Graphics.SetInterpolationMode(InterpolationModeHighQualityBilinear)
    else
      Graphics.SetInterpolationMode(InterpolationModeHighQuality);

    if Bitmap.PixelFormat = pf32bit then
    begin
      Assert(Bitmap.HandleType = bmDIB);
      GdiPBitmap := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -Bitmap.Width * 4,
        PixelFormat32bppPARGB, Bitmap.ScanLine[0]);
    end else
    if Bitmap.PixelFormat = pf24bit then
    begin
      Assert(Bitmap.HandleType = bmDIB);
      GdiPBitmap := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -BytesPerScanline(Bitmap.Width, 24, 32),
        PixelFormat24bppRGB, Bitmap.ScanLine[0]);
    end else
      GdiPBitmap := TGPBitmap.Create(Bitmap.Handle, Bitmap.Palette);

    if EgdeFill or (Opacity <> 255) then
    begin
      FillMemory(@M, SizeOf(TColorMatrix), 0);
      M[0, 0] := 1;
      M[1, 1] := 1;
      M[2, 2] := 1;
      M[3, 3] := Opacity / 255;
      M[4, 4] := 1;

      Attr := TGPImageAttributes.Create;
      try
        Attr.SetColorMatrix(M);
        if EgdeFill then Attr.SetWrapMode(WrapModeTileFlipXY);
        Graphics.DrawImage(GdiPBitmap, MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height),
          0, 0, Bitmap.Width, Bitmap.Height, UnitPixel, Attr);
      finally
        Attr.Free;
      end;
    end else
      Graphics.DrawImage(GdiPBitmap, MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
  finally
    Graphics.Free;
    GdiPBitmap.Free;
  end;
  {$ELSE}
  if Bitmap.Empty then
    Exit;

  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Opacity;
  BF.AlphaFormat := AC_SRC_ALPHA;

  AlphaBlend(Handle, ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top,
    Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, BF);
  {$ENDIF}
end;

procedure PngImageAssignToBitmap(Bitmap: TBitmap; PngImage: TPngImage; IsPremultipledBitmap: Boolean = True);
var
  X, Y: Integer;
  pBitmap: PRGBAArray;
  pPng: PRGBArray;
  pPngAlpha: PByteArray;
  pPngTable: PByteArray;
  C: TRGBQuad;
  A: Byte;
begin
  if (PngImage = nil) or (PngImage.Empty) then
  begin
    Bitmap.SetSize(0, 0);
    Exit;
  end;

  if (PngImage.TransparencyMode <> ptmPartial) or (PngImage.Header.BitDepth <> 8) then
  begin
    Bitmap.Assign(PngImage);
  end else
  begin
    Bitmap.SetSize(0, 0);
    if IsPremultipledBitmap then
      Bitmap.AlphaFormat := TAlphaFormat.afPremultiplied
    else
      Bitmap.AlphaFormat := TAlphaFormat.afDefined;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(PngImage.Width, PngImage.Height);

    for Y := 0 to Bitmap.Height - 1 do
    begin
      pBitmap := Bitmap.ScanLine[Y];
      pPng := PngImage.Scanline[Y];
      pPngTable := PngImage.Scanline[Y];
      pPngAlpha := PngImage.AlphaScanline[Y];

      if PngImage.Header.ColorType = COLOR_RGBALPHA then
      // RGBA
        if IsPremultipledBitmap then
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := (pPng[x].rgbtBlue * pPngAlpha[X]) div 255;
            pBitmap[X].rgbGreen := (pPng[x].rgbtGreen * pPngAlpha[X]) div 255;
            pBitmap[X].rgbRed := (pPng[x].rgbtRed * pPngAlpha[X]) div 255;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
        else
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := pPng[x].rgbtBlue;
            pBitmap[X].rgbGreen := pPng[x].rgbtGreen;
            pBitmap[X].rgbRed := pPng[x].rgbtRed;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
      else if PngImage.Header.ColorType = COLOR_PALETTE then
      // PALETTE
        if IsPremultipledBitmap then
          for X := 0 to Bitmap.Width - 1 do
          begin
            C := TChunkPLTE(PngImage.Chunks.ItemFromClass(TChunkPLTE)).Item[pPngTable[X]];
            A := TChunktRNS(PngImage.Chunks.ItemFromClass(TChunktRNS)).PaletteValues[pPngTable[X]];
            pBitmap[X].rgbBlue := (C.rgbBlue * A) div 255;
            pBitmap[X].rgbGreen := (C.rgbGreen * A) div 255;
            pBitmap[X].rgbRed := (C.rgbRed * A) div 255;
            pBitmap[X].rgbReserved := A;
          end
        else
          for X := 0 to Bitmap.Width - 1 do
          begin
            C := TChunkPLTE(PngImage.Chunks.ItemFromClass(TChunkPLTE)).Item[pPngTable[X]];
            A := TChunktRNS(PngImage.Chunks.ItemFromClass(TChunktRNS)).PaletteValues[pPngTable[X]];
            pBitmap[X].rgbBlue := C.rgbBlue;
            pBitmap[X].rgbGreen := C.rgbGreen;
            pBitmap[X].rgbRed := C.rgbRed;
            pBitmap[X].rgbReserved := A;
          end
      else
      // GRAYSCALE
        if IsPremultipledBitmap then
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := (pPngTable[X] * pPngAlpha[X]) div 255;
            pBitmap[X].rgbGreen := pBitmap[X].rgbBlue;
            pBitmap[X].rgbRed := pBitmap[X].rgbBlue;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
        else
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := pPngTable[X];;
            pBitmap[X].rgbGreen := pBitmap[X].rgbBlue;
            pBitmap[X].rgbRed := pBitmap[X].rgbBlue;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
    end;
  end;
end;

procedure BitmapAssignToPngImage(PngImage: TPngImage; Bitmap: TBitmap; IsPremultipledBitmap: Boolean = True);
var
  TempPng: TPngImage;
  X, Y: Integer;
  pBitmap: PRGBAArray;
  pPng: PRGBArray;
  pPngAlpha: PByteArray;
begin
  if Bitmap.Empty or (Bitmap.PixelFormat <> pf32bit) then
  begin
    PngImage.Assign(Bitmap);
  end else
  begin
    // set need settings
    TempPng := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, 1, 1);
    try
      PngImage.Assign(TempPng);
    finally
      TempPng.Free;
    end;
    PngImage.SetSize(Bitmap.Width, Bitmap.Height);

    for Y := 0 to PngImage.Height - 1 do
    begin
      pBitmap := Bitmap.ScanLine[Y];
      pPng := PngImage.Scanline[Y];
      pPngAlpha := PngImage.AlphaScanline[Y];
      for X := 0 to PngImage.Width - 1 do
      begin
        if pBitmap[X].rgbReserved <> 0 then
        begin
          if IsPremultipledBitmap then
          begin
            pPng[X].rgbtBlue := (pBitmap[x].rgbBlue * 255) div pBitmap[x].rgbReserved;
            pPng[X].rgbtGreen := (pBitmap[x].rgbGreen * 255) div pBitmap[x].rgbReserved;
            pPng[X].rgbtRed := (pBitmap[x].rgbRed * 255) div pBitmap[x].rgbReserved;
          end else
          begin
            pPng[X].rgbtBlue := pBitmap[x].rgbBlue;
            pPng[X].rgbtGreen := pBitmap[x].rgbGreen;
            pPng[X].rgbtRed := pBitmap[x].rgbRed;
          end;
        end else
        begin
          pPng[X].rgbtBlue := 0; pPng[X].rgbtGreen := 0; pPng[X].rgbtRed := 0;
        end;
        pPngAlpha[X] := pBitmap[X].rgbReserved;
      end;
    end;
  end;
end;

procedure GraphicAssignToBitmap(Bitmap: TBitmap; Graphic: TGraphic); Inline;
begin
  // standart TPngImage.AssignTo works is bad!
  if Graphic is TPngImage then
    PngImageAssignToBitmap(Bitmap, TPngImage(Graphic))
  else
    Bitmap.Assign(Graphic);
end;

{$IFDEF USE_GDIPLUS}
function BitmapToGPBitmap(Bitmap: TBitmap): TGPBitmap;
begin

  if Bitmap.PixelFormat = pf32bit then
  begin
    Assert(Bitmap.HandleType = bmDIB);
    Result := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -Bitmap.Width * 4,
      PixelFormat32bppPARGB, Bitmap.ScanLine[0]);
  end else
  if Bitmap.PixelFormat = pf24bit then
  begin
    Assert(Bitmap.HandleType = bmDIB);
    Result := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -BytesPerScanline(Bitmap.Width, 24, 32),
      PixelFormat24bppRGB, Bitmap.ScanLine[0]);
  end else
    Result := TGPBitmap.Create(Bitmap.Handle, Bitmap.Palette);
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------
// *** Begin changed GBlur2.pas ***
type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    b: Byte; {easier to type than rgbtBlue}
    g: Byte;
    r: Byte;
  end;
  PRow = ^TRow;
  TRow = array[Word] of TRGBTriple;
  PPRows = ^TPRows;
  TPRows = array[Word] of PRow;

const
  MaxKernelSize = 50;

type
  TKernelSize = 1..MaxKernelSize;
  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of Single;
  end;
  {the idea is that when using a TKernel you ignore the Weights except
  for Weights in the range -Size..Size.}

procedure MakeGaussianKernel(var K: TKernel; radius: Real; MaxData, DataGranularity: Real);
{makes K into a gaussian kernel with standard deviation = radius. For the current application
you set MaxData = 255 and DataGranularity = 1. Now the procedure sets the value of K.Size so
that when we use K we will ignore the Weights that are so small they can't possibly matter. (Small
Size is good because the execution time is going to be propertional to K.Size.)}
var
  j: Integer;
  temp, delta: Real;
  KernelSize: TKernelSize;
begin
  for j := Low(K.Weights) to High(K.Weights) do
  begin
    temp := j / radius;
    K.Weights[j] := exp(-temp * temp / 2);
  end;
  {now divide by constant so sum(Weights) = 1:}
  temp := 0;
  for j := Low(K.Weights) to High(K.Weights) do
    temp := temp + K.Weights[j];
  for j := Low(K.Weights) to High(K.Weights) do
    K.Weights[j] := K.Weights[j] / temp;
  {now discard (or rather mark as ignorable by setting Size) the entries that are too small to matter.
  This is important, otherwise a blur with a small radius will take as long as with a large radius...}
  KernelSize := MaxKernelSize;
  delta := DataGranularity / (2 * MaxData);
  temp := 0;
  while (temp < delta) and (KernelSize > 1) do
  begin
    temp := temp + 2 * K.Weights[KernelSize];
    dec(KernelSize);
  end;
  K.Size := KernelSize;
  {now just to be correct go back and jiggle again so the sum of the entries we'll be using is exactly 1}
  temp := 0;
  for j := -K.Size to K.Size do
    temp := temp + K.Weights[j];
  for j := -K.Size to K.Size do
    K.Weights[j] := K.Weights[j] / temp;
  // finally correct
  K.Weights[0] := K.Weights[0] + (0.000001);// HACK
end;

function TrimInt(Lower, Upper, theInteger: Integer): integer;
begin
  if (theInteger <= Upper) and (theInteger >= Lower) then
    result := theInteger
  else if theInteger > Upper then
    result := Upper
  else
    result := Lower;
end;

function TrimReal(Lower, Upper: Integer; x: Real): integer;
begin
  if (x < upper) and (x >= lower) then
    result := trunc(x)
  else if x > Upper then
    result := Upper
  else
    result := Lower;
end;

procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
var
  j, n: Integer;
  tr, tg, tb: Real; {tempRed, etc}
  w: Real;
begin
  for j := 0 to High(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;
    for n := -K.Size to K.Size do
    begin
      w := K.Weights[n];
      {the TrimInt keeps us from running off the edge of the row...}
      with theRow[TrimInt(0, High(theRow), j - n)] do
      begin
        tb := tb + w * b;
        tg := tg + w * g;
        tr := tr + w * r;
      end;
    end;
    with P[j] do
    begin
      b := TrimReal(0, 255, tb);
      g := TrimReal(0, 255, tg);
      r := TrimReal(0, 255, tr);
    end;
  end;
  Move(P[0], theRow[0], (High(theRow) + 1) * Sizeof(TRGBTriple));
end;

procedure GaussianBlur(Bitmap: TBitmap; Radius: Real);
var
  Row, Col: Integer;
  theRows: PPRows;
  K: TKernel;
  ACol: PRow;
  P: PRow;
begin
  if (Bitmap.HandleType <> bmDIB) or (Bitmap.PixelFormat <> pf24Bit) then
    raise Exception.Create('GaussianBlur only works for 24-bit bitmaps');
  MakeGaussianKernel(K, radius, 255, 1);
  GetMem(theRows, Bitmap.Height * SizeOf(PRow));
  GetMem(ACol, Bitmap.Height * SizeOf(TRGBTriple));
  {record the location of the bitmap data:}
  for Row := 0 to Bitmap.Height - 1 do
    theRows[Row] := Bitmap.Scanline[Row];
  {blur each row:}
  P := AllocMem(Bitmap.Width * SizeOf(TRGBTriple));
  for Row := 0 to Bitmap.Height - 1 do
    BlurRow(Slice(theRows[Row]^, Bitmap.Width), K, P);
  {now blur each column}
  ReAllocMem(P, Bitmap.Height * SizeOf(TRGBTriple));
  for Col := 0 to Bitmap.Width - 1 do
  begin
    {first read the column into a TRow:}
    for Row := 0 to Bitmap.Height - 1 do
      ACol[Row] := theRows[Row][Col];
    BlurRow(Slice(ACol^, Bitmap.Height), K, P);
    {now put that row, um, column back into the data:}
    for Row := 0 to Bitmap.Height - 1 do
      theRows[Row][Col] := ACol[Row];
  end;
  FreeMem(theRows);
  FreeMem(ACol);
  ReAllocMem(P, 0);
end;

// *** End changed GBlur2.pas ***

procedure FastBlur(Bitmap: TBitmap; Radius: Real; BlurScale: Integer; HighQuality: Boolean = True);
  function Max(A, B: Integer): Integer;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

var
  Mipmap: TBitmap;
begin
  BlurScale := Max(BlurScale, 1);

  Mipmap := TBitmap.Create;
  try
    Mipmap.PixelFormat := pf24bit;
    Mipmap.SetSize(Max(Bitmap.Width div BlurScale, 4), Max(Bitmap.Height div BlurScale, 4));

    // create mipmap
    if HighQuality then
      DrawBitmapHighQuality(Mipmap.Canvas.Handle, Rect(0, 0, Mipmap.Width, Mipmap.Height), Bitmap, 255, False, True)
    else
      Mipmap.Canvas.StretchDraw(Rect(0, 0, Mipmap.Width, Mipmap.Height), Bitmap);

    // gaussian blur
    GaussianBlur(Mipmap, Radius);

    // stretch to source bitmap
    DrawBitmapHighQuality(Bitmap.Canvas.Handle, Rect(0, 0, Bitmap.Width, Bitmap.Height), Mipmap, 255, False, True);
  finally
    Mipmap.Free;
  end;
end;

procedure DrawHalftoneBitmap(Canvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
   Value: Byte; Color: TColor = clBlack);
var
  Brush: TBrush;
begin
  Brush := nil;
  try
    Brush := TBrush.Create;
    Brush.Color := Color;

    FillRect(Canvas.Handle, Rect(X, Y, X + Bitmap.Width, Y + Bitmap.Height), Brush.Handle);
    Canvas.Draw(X, Y, Bitmap, 255 - Value);
  finally
    Brush.Free;
  end;
end;

procedure HalftoneBitmap(Bitmap: TBitmap; Value: Byte; Color: TColor = clBlack);
var
//  {$ifndef DISABLE_GDIPLUS}
//  GPBitmap: TGPBitmap;
//  Graphics: TGPGraphics;
//  Attr: TGPImageAttributes;
//  M: TColorMatrix;
//  {$endif}
  Temp: TBitmap;
  Brush: TBrush;

begin
  Color := ColorToRGB(Color);

//  {$ifndef DISABLE_GDIPLUS}
//  GPBitmap := nil;
//  Graphics := nil;
//  Attr := nil;
//  try
//    GPBitmap := BitmapToGPBitmap(Bitmap);
//    Graphics := TGPGraphics.Create(Bitmap.Canvas.Handle);
//    Attr := TGPImageAttributes.Create;
//
//    FillMemory(@M, SizeOf(TColorMatrix), 0);
//    M[0, 0] := 1 - Value / 255;// r
//    M[1, 1] := 1 - Value / 255;// g
//    M[2, 2] := 1 - Value / 255;// b
//
//    M[3, 0] := (Value / 255) * (GetRValue(Color) / 255);// r
//    M[3, 1] := (Value / 255) * (GetGValue(Color) / 255);// g
//    M[3, 2] := (Value / 255) * (GetBValue(Color) / 255);// b
//
//    M[3, 3] := 1;// a
//    M[4, 4] := 1;// a
//
//    Attr.SetColorMatrix(M);
//
//    Graphics.DrawImage(GPBitmap,
//      MakeRect(0, 0, Bitmap.Width, Bitmap.Height),
//      0, 0, Bitmap.Width, Bitmap.Height, UnitPixel, Attr);
//
//  finally
//    GPBitmap.Free;
//    Graphics.Free;
//    Attr.Free;
//  end;
//  {$endif}

  Brush := nil;
  Temp := nil;
  try
    Temp := TBitmap.Create;
    Temp.Assign(Bitmap);
    Brush := TBrush.Create;
    Brush.Color := Color;

    FillRect(Bitmap.Canvas.Handle, Rect(0, 0, Bitmap.Width, Bitmap.Height), Brush.Handle);
    Bitmap.Canvas.Draw(0, 0, Temp, 255 - Value);
  finally
    Temp.Free;
    Brush.Free;
  end;
end;

//------------------------------------------------------------------------------
// Color spaces
//------------------------------------------------------------------------------

const
  //HSV_MAX = 240;
  HLS_MAX = 240;
  HLS_MAX_HALF = HLS_MAX / 2.0;
  HLS_MAX_ONE_THIRD = HLS_MAX / 3.0;
  HLS_MAX_TWO_THIRDS = (HLS_MAX * 2.0) / 3.0;
  HLS_MAX_SIXTH = HLS_MAX / 6.0;
  HLS_MAX_TWELVETH = HLS_MAX / 12.0;
  RGB_MAX = 255;

// Original source this function: JEDI Code Library (jvFullColorSpaces)
procedure ColorToHSL(Color: TColor; var H, S, L: Integer);
var
  Hue, Lightness, Saturation: Double;
  Red, Green, Blue: Integer;
  ColorMax, ColorMin, ColorDiff, ColorSum: Double;
  RedDelta, GreenDelta, BlueDelta: Extended;
begin
  Red := GetRValue(Color);
  Green := GetGValue(Color);
  Blue := GetBValue(Color);

  if Red > Green then
    ColorMax := Red
  else
    ColorMax := Green;
  if Blue > ColorMax then
    ColorMax := Blue;
  if Red < Green then
    ColorMin := Red
  else
    ColorMin := Green;
  if Blue < ColorMin then
    ColorMin := Blue;
  ColorDiff := ColorMax - ColorMin;
  ColorSum := ColorMax + ColorMin;

  Lightness := (ColorSum * HLS_MAX + RGB_MAX) / (2.0 * RGB_MAX);
  if ColorMax = ColorMin then
  begin
    L := Round(Lightness);
    S := 0;
    H := (2 * HLS_MAX div 3);
    //Color := (Round(Lightness) shl 8) or (2 * HLS_MAX div 3)
  end
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Saturation := (ColorDiff * HLS_MAX + ColorSum / 2.0) / ColorSum
    else
      Saturation := (ColorDiff * HLS_MAX + ((2.0 * RGB_MAX - ColorMax - ColorMin) / 2.0)) /
        (2.0 * RGB_MAX - ColorMax - ColorMin);

    RedDelta := ((ColorMax - Red) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    GreenDelta := ((ColorMax - Green) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    BlueDelta := ((ColorMax - Blue) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;

    if Red = ColorMax then
      Hue := BlueDelta - GreenDelta
    else
    if Green = ColorMax then
      Hue := HLS_MAX_ONE_THIRD + RedDelta - BlueDelta
    else
      Hue := 2.0 * HLS_MAX_ONE_THIRD + GreenDelta - RedDelta;

    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    H := Cardinal(Round(Hue));
    L := Cardinal(Round(Lightness));
    S := Cardinal(Round(Saturation));
  end;
end;

// Original source this function: JEDI Code Library (jvFullColorSpaces)
function HSLToColor(Hue, Saturation, Lightness: Integer): TColor;
var
  Red, Green, Blue: Double;
  Magic1, Magic2: Double;

  function HueToRGB(Lightness, Saturation, Hue: Double): Integer;
  var
    ResultEx: Double;
  begin
    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    if Hue < HLS_MAX_SIXTH then
      ResultEx := Lightness + ((Saturation - Lightness) * Hue + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
    if Hue < HLS_MAX_HALF then
      ResultEx := Saturation
    else
    if Hue < HLS_MAX_TWO_THIRDS then
      ResultEx := Lightness + ((Saturation - Lightness) * (HLS_MAX_TWO_THIRDS - Hue) + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
      ResultEx := Lightness;
    Result := Round(ResultEx);
  end;

  function RoundColor(Value: Double): Integer;
  begin
    if Value > RGB_MAX then
      Result := RGB_MAX
    else
      Result := Round(Value);
  end;

begin
  if Saturation = 0 then
  begin
    Red := (Lightness * RGB_MAX) / HLS_MAX;
    Green := Red;
    Blue := Red;
  end
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Magic2 := (Lightness * (HLS_MAX + Saturation) + HLS_MAX_HALF) / HLS_MAX
    else
      Magic2 := Lightness + Saturation - ((Lightness * Saturation) + HLS_MAX_HALF) / HLS_MAX;

    Magic1 := 2 * Lightness - Magic2;

    Red := (HueToRGB(Magic1, Magic2, Hue + HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Green := (HueToRGB(Magic1, Magic2, Hue) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Blue := (HueToRGB(Magic1, Magic2, Hue - HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
  end;

  Result := RGB(RoundColor(Red), RoundColor(Green), RoundColor(Blue));
end;

function LuminanceColor(Color: TColor; Value: Integer): TColor;
var
  H, S, L: Integer;
begin
  ColorToHSL(ColorToRgb(Color), H, S, L);
  Result := HSLToColor(H, S, Value);
end;

function HighlightColor(Color: TColor; Value: Integer): TColor;
begin
  Result := LuminanceColor(Color, Min(255, Max(0, GetLuminanceColor(Color) + Value)));
end;

function GetLuminanceColor(Color: TColor): Byte;
begin
  Color := ColorToRgb(Color);
  Result := Trunc((GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) * (240/255) / 3);
end;

//------------------------------------------------------------------------------
// Classes
//------------------------------------------------------------------------------

{ TCanvasSaver }

constructor TCanvasSaver.Create(Canvas: TCanvas);
begin
  FCanvas := Canvas;
end;

destructor TCanvasSaver.Destroy;
begin
  if FCanvas <> nil then
  begin
    if FPen <> nil then
      FCanvas.Pen := FPen;
    if FBrush <> nil then
      FCanvas.Brush := FBrush;
    if FFont <> nil then
      FCanvas.Font := FFont;
  end;
  inherited;
end;

function TCanvasSaver.GetBrush: TBrush;
begin
  if FBrush = nil then
    FBrush := TBrush.Create;
  Result := FBrush;
end;

function TCanvasSaver.GetFont: TFont;
begin
  if FFont = nil then
    FFont := TFont.Create;
  Result := FFont;
end;

function TCanvasSaver.GetPen: TPen;
begin
  if FPen = nil then
    FPen := TPen.Create;
  Result := FPen;
end;

procedure TCanvasSaver.Restore;
begin
  Free;
end;

procedure TCanvasSaver.SetBrush(const Value: TBrush);
begin
  Brush.Assign(Value);
end;

procedure TCanvasSaver.SetFont(const Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TCanvasSaver.SetPen(const Value: TPen);
begin
  Pen.Assign(Value);
end;

{ TEsCanvasHelper }

procedure TEsCanvasHelper.StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap);
begin
  if Bitmap.PixelFormat = pf32bit then
    StretchDraw(DestRect, SrcRect, BitMap, 255)
  else
    WinApi.Windows.StretchBlt(Handle, DestRect.Left, DestRect.Top, RectWidth(DestRect), RectHeight(DestRect),
      Bitmap.Canvas.Handle, SrcRect.Left, SrcRect.Top, RectWidth(SrcRect), RectHeight(SrcRect), SRCCOPY);
end;

procedure TEsCanvasHelper.StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap; Opacity: byte);
var
  BF: TBlendFunction;
begin
  if Bitmap.Empty then
    Exit;

  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Opacity;
  if Bitmap.PixelFormat = pf32bit then
    BF.AlphaFormat := AC_SRC_ALPHA
  else
    BF.AlphaFormat := 0;

  AlphaBlend(Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    Bitmap.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, BF);
end;

procedure TEsCanvasHelper.DrawNinePatch(Dest: TRect; Bounds: TRect; Bitmap: TBitmap);
begin
  DrawNinePatch(Dest, Bounds, Bitmap, 255);
end;

procedure TEsCanvasHelper.DrawHighQuality(ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255; HighQuality: Boolean = False);
begin
  DrawBitmapHighQuality(Handle, ARect, Bitmap, Opacity, HighQuality);
end;

procedure TEsCanvasHelper.DrawHighQuality(ARect: TRect; Graphic: TGraphic; Opacity: Byte = 255; HighQuality: Boolean = False);
{$IFDEF USE_GDIPLUS}
var
  Bitmap: TBitmap;
{$ENDIF}
begin
  {$IFDEF USE_GDIPLUS}
  if Graphic is TBitmap then
    DrawHighQuality(ARect, TBitmap(Graphic), Opacity, HighQuality)
  else
  begin
    Bitmap := TBitmap.Create;
    try
      GraphicAssignToBitmap(Bitmap, Graphic);
      DrawHighQuality(ARect, Bitmap, Opacity, HighQuality);
    finally
      Bitmap.Free;
    end;
  end;
  {$ELSE}
  StretchDraw(ARect, Graphic, Opacity);
  {$ENDIF}
end;

procedure TEsCanvasHelper.Draw(X, Y: Integer; Graphic: TGraphic; Opacity: Byte);
var
  Bitmap: TBitmap;
begin
  if Graphic is TBitmap then
  begin
//    if (TBitmap(Graphic).PixelFormat = pf32bit) and (TBitmap(Graphic).AlphaFormat = afIgnored) then
//      TBitmap(Graphic).AlphaFormat := afDefined;
    Inherited Draw(X, Y, Graphic, Opacity)
  end
  else
  begin
    Bitmap := TBitmap.Create;
    try
      GraphicAssignToBitmap(Bitmap, Graphic);
      Inherited Draw(X, Y, Bitmap, Opacity);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TEsCanvasHelper.DrawChessFrame(R: TRect; Color1, Color2: TColor);
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

    WinApi.Windows.FrameRect(Handle, R, Brush);
  finally
    DeleteObject(Brush);
    Bitmap.Free;
  end;
end;

procedure TEsCanvasHelper.DrawCorners(R: TRect; Width: Integer);
begin
  MoveTo(R.Left, R.Top + Width - 1);
  LineTo(R.Left, R.Top);
  Self.LineTo(R.Left + Width, R.Top);

  MoveTo(R.Right - Width, R.Top);
  LineTo(R.Right - 1, R.Top);
  LineTo(R.Right - 1, R.Top + Width);

  MoveTo(R.Right - 1, R.Bottom - Width);
  LineTo(R.Right - 1, R.Bottom - 1);
  LineTo(R.Right - Width - 1, R.Bottom - 1);

  MoveTo(R.Left + Width - 1, R.Bottom - 1);
  LineTo(R.Left, R.Bottom - 1);
  LineTo(R.Left, R.Bottom - Width - 1);
end;

function ValidRect(Rect: TRect): Boolean;
begin
  Result := (RectWidth(Rect) > 0) and (RectHeight(Rect) > 0);
end;

procedure TEsCanvasHelper.DrawNinePatch(Dest: TRect; Bounds: TRect; Bitmap: TBitmap; Opacity: byte);
var
  dx, dy: Integer;
  D, S: TRect;
  IntD, IntS: TRect;
begin
  if (Dest.Left >= Dest.Right) or (Dest.Top >= Dest.Bottom) then
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
  StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |   |
  //  |*  |
  //  |   |
  //   ---
  D := Rect(Dest.Left, IntD.Top, IntD.Left, IntD.Bottom);
  S := Rect(0, IntS.Top, IntS.Left, IntS.Bottom);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |   |
  //  |   |
  //  |*  |
  //   ---
  D := Rect(Dest.Left, IntD.Bottom, IntD.Left, Dest.Bottom);
  S := Rect(0, IntS.Bottom, IntS.Left, Bitmap.Height);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |   |
  //  |   |
  //  | * |
  //   ---
  D := Rect(IntD.Left, IntD.Bottom, IntD.Right, Dest.Bottom);
  S := Rect(IntS.Left, IntS.Bottom, IntS.Right, Bitmap.Height);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |   |
  //  |   |
  //  |  *|
  //   ---
  D := Rect(IntD.Right, IntD.Bottom, Dest.Right, Dest.Bottom);
  S := Rect(IntS.Right, IntS.Bottom, Bitmap.Width, Bitmap.Height);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |   |
  //  |  *|
  //  |   |
  //   ---
  D := Rect(IntD.Right, IntD.Top, Dest.Right, IntD.Bottom);
  S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Bottom);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |  *|
  //  |   |
  //  |   |
  //   ---
  D := Rect(IntD.Right, Dest.Top, Dest.Right, IntD.Top);
  S := Rect(IntS.Right, 0, Bitmap.Width, IntS.Top);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  | * |
  //  |   |
  //  |   |
  //   ---
  D := Rect(IntD.Left, Dest.Top, IntD.Right, IntD.Top);
  S := Rect(IntS.Left, 0, IntS.Right, IntS.Top);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
  //   ---
  //  |   |
  //  | * |
  //  |   |
  //   ---
  D := Rect(IntD.Left, IntD.Top, IntD.Right, IntD.Bottom);
  S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Bottom);
  if ValidRect(D) then
    StretchDraw(D, S, Bitmap, Opacity);
end;

procedure TEsCanvasHelper.DrawThemeText(Details: TThemedElementDetails; Rect: TRect; Text: string;
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

procedure TEsCanvasHelper.DrawInsideFrame(R: TRect; Width: Integer; Color: TColor = clNone);
var
  ColorPen: HPen;
begin
  if Color = clNone then
    Color := Pen.Color;

  ColorPen := CreatePen(PS_INSIDEFRAME or PS_SOLID, Width, Color);
  SelectObject(Handle, ColorPen);
  SelectObject(Handle, GetStockObject(NULL_BRUSH));

  WinApi.Windows.Rectangle(Handle, R.Left, R.Top, R.Right, R.Bottom);

  SelectObject(Handle, GetStockObject(NULL_PEN));
  DeleteObject(ColorPen);
end;

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
procedure TEsCanvasHelper.DrawNinePatch(Dest, Bounds: TRect; Bitmap: TBitmap; Mode: TStretchMode;
  Opacity: Byte);
var
  dx, dy: Integer;
  D, S: TRect;
  IntD, IntS: TRect;
  W, H, X, Y: Integer;
begin
  if (Dest.Left >= Dest.Right)or(Dest.Top >= Dest.Bottom) then
    exit;

  if (Mode = TStretchMode.HorzTileFit) or (Mode = TStretchMode.HorzFit) then
  begin
    H := Bitmap.Height;
    Y := (Dest.Top + Dest.Bottom) div 2;
    Dest := Rect(Dest.Left, Y - H div 2, Dest.Right, Y + H - (H div 2));
  end else
  if (Mode = TStretchMode.VertTileFit) or (Mode = TStretchMode.VertFit) then
  begin
    W := Bitmap.Width;
    X := (Dest.Left + Dest.Right) div 2;
    Dest := Rect(X - W div 2, Dest.Top, X + W - (W div 2), Dest.Bottom);
  end;

  if (Mode = TStretchMode.Normal) or (Mode = TStretchMode.HorzFit) or (Mode = TStretchMode.VertFit) then
  begin
    DrawNinePatch(Dest, Bounds, Bitmap, Opacity);
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

  if (Mode = TStretchMode.HorzTile) or (Mode = TStretchMode.HorzTileFit) then
  begin
    // Left Top
    D := Rect(Dest.Left, Dest.Top, IntD.Left, IntD.Top);
    S := Rect(0, 0, IntS.Left, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // Left Center
    D := Rect(Dest.Left, IntD.Top, IntD.Left, IntD.Bottom);
    S := Rect(0, IntS.Top, IntS.Left, IntS.Bottom);
    StretchDraw(D, S, Bitmap, Opacity);
    // Left Bottom
    D := Rect(Dest.Left, IntD.Bottom, IntD.Left, Dest.Bottom);
    S := Rect(0, IntS.Bottom, IntS.Left, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // Right Bottom
    D := Rect(IntD.Right, IntD.Bottom, Dest.Right, Dest.Bottom);
    S := Rect(IntS.Right, IntS.Bottom, Bitmap.Width, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // Right Center
    D := Rect(IntD.Right, IntD.Top, Dest.Right, IntD.Bottom);
    S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Bottom);
    StretchDraw(D, S, Bitmap, Opacity);
    // Right Top
    D := Rect(IntD.Right, Dest.Top, Dest.Right, IntD.Top);
    S := Rect(IntS.Right, 0, Bitmap.Width, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
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
          StretchDraw(D, S, Bitmap, Opacity);
        // center
        D := Rect(X, IntD.Top, X + W, IntD.Bottom);
        S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Bottom);
        StretchDraw(D, S, Bitmap, Opacity);
        // down
        D := Rect(X, IntD.Bottom, X + W, Dest.Bottom);
        S := Rect(IntS.Left, IntS.Bottom, IntS.Right, Bitmap.Height);
        if ValidRect(D) then
          StretchDraw(D, S, Bitmap, Opacity);
        X := X + W;
      end;
    // cut up
    D := Rect(X, Dest.Top, IntD.Right, IntD.Top);
    S := Rect(IntS.Left, 0, IntS.Left + (IntD.Right - X), IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // cut center
    D := Rect(X, IntD.Top, IntD.Right, IntD.Bottom);
    S := Rect(IntS.Left, IntS.Top, IntS.Left + (IntD.Right - X), IntS.Bottom);
    StretchDraw(D, S, Bitmap, Opacity);
    // cut down
    D := Rect(X, IntD.Bottom, IntD.Right, Dest.Bottom);
    S := Rect(IntS.Left, IntS.Bottom, IntS.Left + (IntD.Right - X), Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
  end else
  if (Mode = TStretchMode.VertTile) or (Mode = TStretchMode.VertTileFit) then
  begin
    // Top Left
    D := Rect(Dest.Left, Dest.Top, IntD.Left, IntD.Top);
    S := Rect(0, 0, IntS.Left, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // Top Right
    D := Rect(IntD.Right, Dest.Top, Dest.Right, IntD.Top);
    S := Rect(IntS.Right, 0, Bitmap.Width, IntS.Top);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // Top Center
    D := Rect(IntD.Left, Dest.Top, IntD.Right, IntD.Top);
    S := Rect(IntS.Left, 0, IntS.Right, IntS.Top);
    StretchDraw(D, S, Bitmap, Opacity);
    // Bottom Left
    D := Rect(Dest.Left, IntD.Bottom, IntD.Left, Dest.Bottom);
    S := Rect(0, IntS.Bottom, IntS.Left, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // Bottom Center
    D := Rect(IntD.Left, IntD.Bottom, IntD.Right, Dest.Bottom);
    S := Rect(IntS.Left, IntS.Bottom, IntS.Right, Bitmap.Height);
    StretchDraw(D, S, Bitmap, Opacity);
    // Bottom Right
    D := Rect(IntD.Right, IntD.Bottom, Dest.Right, Dest.Bottom);
    S := Rect(IntS.Right, IntS.Bottom, Bitmap.Width, Bitmap.Height);
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
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
          StretchDraw(D, S, Bitmap, Opacity);
        // center
        D := Rect(IntD.Left, Y, IntD.Right, Y + H);
        S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Bottom);
        StretchDraw(D, S, Bitmap, Opacity);
        // right
        D := Rect(IntD.Right, Y, Dest.Right, Y + H);
        S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Bottom);
        if ValidRect(D) then
          StretchDraw(D, S, Bitmap, Opacity);
        Y := Y + H;
      end;
    // cut left
    D := Rect(Dest.Left, Y, IntD.Left, IntD.Bottom);
    S := Rect(0, IntS.Top, IntS.Left, IntS.Top + (IntD.Bottom - Y));
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
    // cut center
    D := Rect(IntD.Left, Y, IntD.Right, IntD.Bottom);
    S := Rect(IntS.Left, IntS.Top, IntS.Right, IntS.Top + (IntD.Bottom - Y));
    StretchDraw(D, S, Bitmap, Opacity);
    // cut right
    D := Rect(IntD.Right, Y, Dest.Right, IntD.Bottom);
    S := Rect(IntS.Right, IntS.Top, Bitmap.Width, IntS.Top + (IntD.Bottom - Y));
    if ValidRect(D) then
      StretchDraw(D, S, Bitmap, Opacity);
  end else
  if (Mode = TStretchMode.Tile) then
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

          StretchDraw(Rect(X, Y, X + W, Y + H), Rect(0, 0, W, H), Bitmap, Opacity);
          X := X + Bitmap.Width;
        until X >= Dest.Right;
        Y := Y + Bitmap.Height;
      until Y >= Dest.Bottom;
    end;
  end;
end;

procedure TEsCanvasHelper.DrawTransparentFrame(R: TRect; Color1, Color2: TColor; Opacity: Integer = -1; const Mask: ShortString = '12');
var
  C1, C2, DrawColor: TColor;
  Temp, X, Y: Integer;
  Index, Count: Integer;

  function MakeColor24(Dest: TColor; Src: TColor): TColor; inline;
  begin
    TRGBQuad(Result).rgbRed := (TRGBQuad(Dest).rgbRed *
      (255 - TRGBQuad(Src).rgbReserved) + TRGBQuad(Src).rgbRed * TRGBQuad(Src).rgbReserved) div 255;
    TRGBQuad(Result).rgbBlue := (TRGBQuad(Dest).rgbBlue *
      (255 - TRGBQuad(Src).rgbReserved) + TRGBQuad(Src).rgbBlue * TRGBQuad(Src).rgbReserved) div 255;
    TRGBQuad(Result).rgbGreen := (TRGBQuad(Dest).rgbGreen *
      (255 - TRGBQuad(Src).rgbReserved) + TRGBQuad(Src).rgbGreen * TRGBQuad(Src).rgbReserved) div 255;
    TRGBQuad(Result).rgbReserved := 0;
  end;

  function GetColor: TColor;
  begin
    if Mask[Index] = '1' then
      Result := C1
    else
    if Mask[Index] = '2' then
      Result := C2
    else
      Result := 0;

    Inc(Index);
    if Index > Count then
      Index := 1;
  end;
begin
  Count := Length(Mask);
  if Count = 0 then
    Exit;
  Index := 1;

  if Opacity <> -1 then
  begin
    C1 := ColorToRgb(Color1) or (Opacity shl 24);
    C2 := ColorToRgb(Color2) or (Opacity shl 24);
  end else
  begin
    C1 := Color1;
    C2 := Color2;
  end;

//  Bitmap := TBitmap.Create;
//  try
//    Bitmap.PixelFormat := pf32bit;
//    Bitmap.AlphaFormat := afDefined;
//    Bitmap.SetSize(4, 4);
//    // frame
//    SetPixel(0, 0, C1);
//    SetPixel(1, 0, C2);
//    SetPixel(2, 0, C1);
//    SetPixel(3, 0, C2);
//    SetPixel(3, 1, C1);
//    SetPixel(3, 2, C2);
//    SetPixel(3, 3, C1);
//    SetPixel(2, 3, C2);
//    SetPixel(1, 3, C1);
//    SetPixel(0, 3, C2);
//    SetPixel(0, 2, C1);
//    SetPixel(0, 1, C2);
//    // center
//    SetPixel(1, 1, 0);
//    SetPixel(1, 2, 0);
//    SetPixel(2, 1, 0);
//    SetPixel(2, 2, 0);
//
//    if (Alpha >= 0) and (Alpha <= 255) then
//      DrawNinePatch(R, Rect(2, 2, 2, 2), Bitmap, TStretchMode.smTile, Alpha)
//    else
//      DrawNinePatch(R, Rect(1, 1, 1, 1), Bitmap, TStretchMode.smTile, 255);
//  finally
//    Bitmap.Free;
//  end;

  if R.Left > R.Right then
  begin
    Temp := R.Right;
    R.Right := R.Left;
    R.Left := Temp;
  end;
  Dec(R.Right);
  if R.Top > R.Bottom then
  begin
    Temp := R.Bottom;
    R.Bottom := R.Top;
    R.Top := Temp;
  end;
  Dec(R.Bottom);

  for X := R.Left to R.Right - 1 do
  begin
    DrawColor := GetColor;
    if DrawColor <> 0 then
      Pixels[X, R.Top] := MakeColor24(Pixels[X, R.Top], DrawColor);
  end;

  for Y := R.Top to R.Bottom - 1do
  begin
    DrawColor := GetColor;
    if DrawColor <> 0 then
      Pixels[R.Right, Y] := MakeColor24(Pixels[R.Right, Y], DrawColor);
  end;

  if R.Bottom <> R.Top then
    for X := R.Right downto R.Left + 1 do
    begin
      DrawColor := GetColor;
      if DrawColor <> 0 then
        Pixels[X, R.Bottom] := MakeColor24(Pixels[X, R.Bottom], DrawColor);
    end
  else
    Pixels[R.Right, R.Top] := MakeColor24(Pixels[R.Right, R.Top], GetColor);

  if R.Right <> R.Left then
    for Y := R.Bottom downto R.Top + 1 do
    begin
      DrawColor := GetColor;
      if DrawColor <> 0 then
        Pixels[R.Left, Y] := MakeColor24(Pixels[R.Left, Y], DrawColor);
    end
  else
    Pixels[R.Right, R.Bottom] := MakeColor24(Pixels[R.Right, R.Bottom], GetColor);
end;

procedure TEsCanvasHelper.FillRect(R: TRect; Color: TColor);
var
  Brush: TBrush;
begin
  Brush := TBrush.Create;
  try
    Brush.Color := Color;
    Winapi.Windows.FillRect(Self.Handle, R, Brush.Handle);
  finally
    Brush.Free;
  end;
end;

procedure TEsCanvasHelper.Line(X1, Y1, X2, Y2: Integer);
begin
  MoveTo(X1, Y1);
  LineTo(X2, Y2);
end;

procedure TEsCanvasHelper.RestoreState(var State: ICanvasSaver);
begin
  State := nil;
end;

function TEsCanvasHelper.SaveState(const Objects: array of TGraphicsClass): ICanvasSaver;
var
  Saver: TCanvasSaver;
  I: Integer;
begin
  Saver := TCanvasSaver.Create(Self);

  for I := 0 to High(Objects) do
    if Objects[I] <> nil then
      if Objects[I].InheritsFrom(TPen) then
        Saver.Pen := Pen
      else
      if Objects[I].InheritsFrom(TBrush) then
        Saver.Brush := Brush
      else
      if Objects[I].InheritsFrom(TFont) then
        Saver.Font := Font;

  Result := Saver;
end;

function TEsCanvasHelper.SaveState: ICanvasSaver;
begin
  SaveState([TPen, TBrush, TFont]);
end;

type
  THackGraphic = class(TGraphic);

procedure TEsCanvasHelper.StretchDraw(Rect: TRect; Graphic: TGraphic; Opacity: Byte);
var
  Bitmap: TBitmap;
begin
  if Graphic <> nil then
  begin
    Changing;
    RequiredState([csHandleValid]);
    if Opacity = 255 then
      THackGraphic(Graphic).Draw(Self, Rect)
    else
      // for Opacity <> 255
      if Graphic is TBitmap then
      begin
        // god scenary
        THackGraphic(Graphic).DrawTransparent(Self, Rect, Opacity);
      end
      else
      begin
        // bed, we create temp buffer, it is slowly :(
        Bitmap := TBitmap.Create;
        try
          GraphicAssignToBitmap(Bitmap, Graphic);
          StretchDraw(Rect, Bitmap, Opacity);
        finally
          Bitmap.Free;
        end;
      end;

    Changed;
  end;
end;

{TEsBitMap}

{$IFDEF OLD_BITMAP_PROC}
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
{$ENDIF}

constructor TEsBitMap.Create;
begin
  inherited;
  self.AlphaFormat := afDefined;
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

initialization
  {$IFDEF VER270UP}
  AddEnumElementAliases(TypeInfo(TStretchMode), ['smNormal', 'smTile', 'smHorzFit', 'smVertFit', 'smHorzTile', 'smVertTile',
    'smHorzTileFit', 'smVertTileFit']);
  {$ENDIF}

finalization
  {$IFDEF VER270UP}
  RemoveEnumElementAliases(TypeInfo(TStretchMode));
  {$ENDIF}

end.

