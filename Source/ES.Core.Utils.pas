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
// WARNING!!!
// This unit should not contain references to the VCL/FMX or API's.

unit ES.Core.Utils;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.Types, ES.Core.Classes;

function Range(Value, Min, Max: Integer): Integer;

function ProportionalContraction(Rect, Bounds: TRect; HW: Double; IsMainLine: Boolean): TRect;
function ProportionalRange(Rect: TRect; HW: Double; Min, Max: Integer; IsWidth: Boolean; DirectWidth, DirectHeight: Boolean): TRect;

implementation

function Range(Value, Min, Max: Integer): Integer;
begin
  if Value < Min then
    Result := Min
  else if Value > Max then
    Result := Max
  else
    Result := Value;
end;

function ProportionalContraction(Rect, Bounds: TRect; HW: Double; IsMainLine: Boolean): TRect;
var
  t: Integer;
begin
  if Rect.Left < Bounds.Left then
  begin
    t := Bounds.Left - Rect.Left;
    Rect.Offset(t, 0);
    Rect.Right := Rect.Right - t;
    if IsMainLine then
    begin
      Rect.Top := Rect.Bottom - Trunc(Rect.Width * HW);
    end else
      Rect.Height := Trunc(Rect.Width * HW);
  end;
  if Rect.Top < Bounds.Top then
  begin
    t := Bounds.Top - Rect.Top;
    Rect.Offset(0, t);
    Rect.Bottom := Rect.Bottom - t;
    if IsMainLine then
    begin
      Rect.Left := Rect.Right - Trunc(Rect.Height / HW);
    end else
      Rect.Width := Trunc(Rect.Height / HW);
  end;
  if Rect.Right > Bounds.Right then
  begin
    t := Rect.Right - Bounds.Right;
    Rect.Offset(-t, 0);
    Rect.Left := Rect.Left + t;
    if not IsMainLine then
    begin
      Rect.Top := Rect.Bottom - Trunc(Rect.Width * HW);
    end else
      Rect.Height := Trunc(Rect.Width * HW);
  end;
  if Rect.Bottom > Bounds.Bottom then
  begin
    t := Rect.Bottom - Bounds.Bottom;
    Rect.Offset(0, -t);
    Rect.Top := Rect.Top + t;
    if not IsMainLine then
    begin
      Rect.Left := Rect.Right - Trunc(Rect.Height / HW);
    end else
      Rect.Width := Trunc(Rect.Height / HW);
  end;
  Result := Rect;
end;

function ProportionalRange(Rect: TRect; HW: Double; Min, Max: Integer; IsWidth: Boolean; DirectWidth, DirectHeight: Boolean): TRect;
begin
  if IsWidth then
  begin
    if DirectWidth then
    begin
      Rect.Width := Range(Rect.Width, Min, Max);
      if DirectHeight then
        Rect.Height := Trunc(Rect.Width * HW)
      else
        Rect.Top := Rect.Bottom - Trunc(Rect.Width * HW);
    end else
    begin
      Rect.Left := Rect.Left + Rect.Width - Range(Rect.Width, Min, Max);
      if DirectHeight then
        Rect.Height := Trunc(Rect.Width * HW)
      else
        Rect.Top := Rect.Bottom - Trunc(Rect.Width * HW);
    end;
  end else
  begin
    if DirectHeight then
    begin
      Rect.Height := Range(Rect.Height, Min, Max);
      if DirectWidth then
        Rect.Width := Trunc(Rect.Height / HW)
      else
        Rect.Left := Rect.Right - Trunc(Rect.Height / HW);
    end else
    begin
      Rect.Top := Rect.Top + Rect.Height - Range(Rect.Height, Min, Max);
      if DirectWidth then
        Rect.Width := Trunc(Rect.Height / HW)
      else
        Rect.Left := Rect.Right - Trunc(Rect.Height / HW);
    end;
  end;
end;

end.
