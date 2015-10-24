{******************************************************************************}
{                             ESComponents for VCL                             }
{                              ErrorSoft(c) 2015                               }
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

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  WinApi.Messages, Vcl.Graphics, Vcl.Themes;

function IsShowFocusRect(Control: TWinControl): Boolean;
function IsStyledClientControl(Control: TControl): Boolean;
function IsStyledFontControl(Control: TControl): Boolean;
function IsStyledBorderControl(Control: TControl): Boolean;

function ClientColorToRgb(Color: TColor; Control: TControl = nil): TColor;
function BorderColorToRgb(Color: TColor; Control: TControl = nil): TColor;
function FontColorToRgb(Color: TColor; Control: TControl = nil): TColor;

implementation

function IsShowFocusRect(Control: TWinControl): Boolean;
begin
  Result := ((Control.Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0) and Control.Focused;
end;

function IsStyledClientControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seClient in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
end;

function IsStyledFontControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seFont in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
end;

function IsStyledBorderControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seBorder in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
end;

function ClientColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  if IsStyledClientControl(Control) then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := ColorToRGB(Color);
end;

function BorderColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  if IsStyledBorderControl(Control) then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := ColorToRGB(Color);
end;

function FontColorToRgb(Color: TColor; Control: TControl): TColor;
begin
  if IsStyledFontControl(Control) then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := ColorToRGB(Color);
end;

end.
