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
unit ES.ExGdiPlus;

interface

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

uses
  WinApi.Windows, WinApi.GDIPApi, WinApi.GDIPObj, Vcl.Graphics, System.Types;

function RectToGPRect(Rect: TRect): TGPRectF;
procedure InflateGPRect(var Rect: TGPRectF; DX, DY: Single);
function ColorToGPColor(Color: TColor; Alpha: byte = 255): DWORD;
procedure DrawHighQuality(Handle: THandle; ARect: TRect; Bitmap: TBitmap);

implementation

function ColorToGPColor(Color: TColor; Alpha: byte = 255): DWORD;
var
  BRG: DWORD;
begin
  if Color = clNone then
    Alpha := 0;
  BRG := ColorToRGB(Color);

  Result := ((BRG shl 16) and $00FF0000) or
    ((BRG shr 16) and $000000FF) or
    (BRG and $0000FF00) or
    (Alpha shl 24);
end;

function RectToGPRect(Rect: TRect): TGPRectF;
begin
  Result.X := Rect.Left;
  Result.Y := Rect.Top;
  Result.Width := RectWidth(Rect);
  Result.Height := RectHeight(Rect);
end;

procedure InflateGPRect(var Rect: TGPRectF; DX, DY: Single);
begin
  Rect.X := Rect.X - DX;
  Rect.Y := Rect.Y - DY;
  Rect.Width := Rect.Width + DX * 2;
  Rect.Height := Rect.Height + DY * 2;
end;

procedure DrawHighQuality(Handle: THandle; ARect: TRect; Bitmap: TBitmap);
var
  Graphics: TGPGraphics;
  GdiPBitmap: TGPBitmap;
begin
  GdiPBitmap := nil;
  Graphics := TGPGraphics.Create(Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeDefault);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    Graphics.SetInterpolationMode(InterpolationModeLowQuality);

    if Bitmap.PixelFormat = pf32bit then
    begin
      Assert(Bitmap.HandleType = bmDIB);
      GdiPBitmap := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -Bitmap.Width * 4,
        PixelFormat32bppARGB, Bitmap.ScanLine[0]);
    end else
      GdiPBitmap := TGPBitmap.Create(Bitmap.Handle, Bitmap.Palette);

    Graphics.DrawImage(GdiPBitmap, RectToGPRect(ARect));
  finally
    Graphics.Free;
    GdiPBitmap.Free;
  end;
end;

end.
