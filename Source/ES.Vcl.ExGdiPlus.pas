{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2015-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.ExGdiPlus;

interface

uses
  Windows, GDIPApi, GDIPObj, Graphics, Types;

function RectToGPRect(Rect: TRect): TGPRectF;
procedure InflateGPRect(var Rect: TGPRectF; DX, DY: Single);
function ColorToGPColor(Color: TColor; Alpha: byte = 255): DWORD;

implementation

function ColorToGPColor(Color: TColor; Alpha: byte = 255): DWORD;
var
  BRG: DWORD;
begin
  BRG := ColorToRGB(Color);

  Result := ((BRG shl 16) and $00FF0000) or ((BRG shr 16) and $000000FF) or (BRG and $0000FF00) or (Alpha shl 24);
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

end.
