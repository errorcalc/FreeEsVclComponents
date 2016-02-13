{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2015-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.FreeReg;

interface

{$I 'FreeEsVclComponents.inc'}

procedure Register;

implementation

{$R 'Icons/Icons.res'}

uses
  System.Classes, ES.Vcl.Layouts, ES.Vcl.NinePatch, ES.Vcl.Indicators, ES.Vcl.Switch,
  Es.Vcl.FreeEditors, Designintf, Vcl.Imaging.PngImage;

procedure Register;
begin
  RegisterComponents('ErrorSoft', [
    TEsLayout,// Layouts
    TEsNinePatchImage, TEsImageLabel, TEsImageLayout, TEsLabelLayout,// NinePatch
    TEsActivityBar,// Indicators
    TEsSwitch// Switch
  ]);

  {$ifdef FixLoadPng}
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsNinePatchImage, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageLabel, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageLayout, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsLabelLayout, '', TEsPngPropertyFix);
  {$endif}
end;

end.
