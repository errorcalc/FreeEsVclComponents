{******************************************************************************}
{                            EsVclComponents v3.0                              }
{                           errorsoft(c) 2009-2018                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{         Open this on github: github.com/errorcalc/FreeEsVclComponents        }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
unit ES.FreeReg;

interface

{$I 'FreeEsVclComponents.inc'}

procedure Register;

implementation

{$R 'Icons/Icons.res'}

uses
  System.Classes, ES.Layouts, ES.NinePatch, ES.Indicators, ES.Switch, ES.Images, ES.RegExControls,
  ES.FreeEditors, Designintf, Vcl.Imaging.PngImage, Vcl.ImgList, System.UITypes, Vcl.Graphics,
  ES.RegexEditor, ES.PaintBox, ES.Labels, ToolsApi, DIalogs;

procedure Register;
begin

  RegisterComponents('errorsoft', [
    // Layouts
    TEsLayout, TEsPanel,
    // NinePatch
    TEsNinePatchImage, TEsImageLabel, TEsImageLayout, TEsImageStaticText,
    // Indicators
    TEsActivityBar,
    // Switch
    TEsSwitch,
    // Images
    TEsImage, TEsImageControl,
    // Regex Controls
    TEsRegexEdit, TEsRegexButtonedEdit, TEsRegexLabeledEdit,
    // PaintBox
    TEsPaintBox,
    // Labels
    TEsLinkLabel,
    TEsVersionLabel
  ]);

  // ImageList support
  RegisterPropertyEditor(TypeInfo(TImageIndex), TEsImage, '', TEsCustomImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TEsImageControl, '', TEsCustomImageIndexProperty);

  // Regex controls
  RegisterComponentEditor(TEsRegexEdit, TEsRegexEditorEditor);
  RegisterComponentEditor(TEsRegexButtonedEdit, TEsRegexEditorEditor);
  RegisterComponentEditor(TEsRegexLabeledEdit, TEsRegexEditorEditor);

  // AlphaControls/alphaskins is bad.
  // I has too much head pain, because of them!
  // Alpha controls COMPLETLY BREAK DOWN STANDART PNG LOADER.
  // I think in hell there is a personal cauldron...
  {$ifdef FixLoadPng}
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsNinePatchImage, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageLabel, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageLayout, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageStaticText, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPicture), TEsImage, '', TEsPicturePropertyFix);
  RegisterPropertyEditor(TypeInfo(TPicture), TEsImageControl, '', TEsPicturePropertyFix);
  {$endif}
end;

end.
