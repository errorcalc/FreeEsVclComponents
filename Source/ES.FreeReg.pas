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
unit ES.FreeReg;

{$I EsDefines.inc}

interface

{$I 'FreeEsVclComponents.inc'}

procedure Register;

implementation

{$R 'Icons/Icons.res'}

uses
  System.Classes, ES.Layouts, ES.NinePatch, ES.Indicators, ES.Switch, ES.Images, ES.RegExControls,
  ES.FreeEditors, Designintf, Vcl.Imaging.PngImage, Vcl.ImgList, System.UITypes, Vcl.Graphics,
  ES.RegexEditor, ES.PaintBox, ES.Labels, ES.Shapes, ES.ControlListControls, ToolsApi, Dialogs;

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
    TEsImage, TEsImageControl, {$IFDEF VER340UP}TEsVirtualImage, TEsVirtualImageControl,{$ENDIF}
    // Regex Controls
    TEsRegexEdit, TEsRegexButtonedEdit, TEsRegexLabeledEdit,
    // PaintBox
    TEsPaintBox,
    // Labels
    TEsLinkLabel,
    TEsVersionLabel,
    // ControlList controls
    {$IFDEF VER350UP}TEsControlListCheckBox,{$ENDIF}
    // Shapes
    TEsShape
  ]);

  // ImageList support
  RegisterPropertyEditor(TypeInfo(TImageIndex), TEsImage, '', TEsCustomImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TEsImageControl, '', TEsCustomImageIndexProperty);
  {$IFDEF VER340UP}
  RegisterPropertyEditor(TypeInfo(TImageName), TEsImage, '', TEsCustomImageNameProperty);
  RegisterPropertyEditor(TypeInfo(TImageName), TEsImageControl, '', TEsCustomImageNameProperty);
  // TEsVirtualImage
  RegisterPropertyEditor(TypeInfo(TImageName), TEsVirtualImage, 'ImageName', TEsCustomCollectionImageNameProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TEsVirtualImage, 'ImageIndex', TEsCustomCollectionImageIndexProperty);
  // TEsVirtualImageControl
  RegisterPropertyEditor(TypeInfo(TImageName), TEsVirtualImageControl, 'ImageName', TEsCustomCollectionImageNameProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TEsVirtualImageControl, 'ImageIndex', TEsCustomCollectionImageIndexProperty);
  {$ENDIF}

  // Regex controls
  RegisterComponentEditor(TEsRegexEdit, TEsRegexEditorEditor);
  RegisterComponentEditor(TEsRegexButtonedEdit, TEsRegexEditorEditor);
  RegisterComponentEditor(TEsRegexLabeledEdit, TEsRegexEditorEditor);

  // Guidelines
  RegisterComponentGuidelines(TEsPanel, TEsPanelGuidelines);
  RegisterComponentGuidelines(TEsSwitch, TEsSwitchGuidelines);
  RegisterComponentGuidelines(TEsImageLabel, TEsImageLabelGuidelines);
  RegisterComponentGuidelines(TEsImageStaticText, TEsImageStaticTextGuidelines);
  RegisterComponentGuidelines(TEsShape, TEsShapeTextGuidelines);

  // Alpha controls COMPLETLY BREAK DOWN STANDART PNG LOADER.
  {$IFDEF FixLoadPng}
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsNinePatchImage, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageLabel, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageLayout, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPngImage), TEsImageStaticText, '', TEsPngPropertyFix);
  RegisterPropertyEditor(TypeInfo(TPicture), TEsImage, '', TEsPicturePropertyFix);
  RegisterPropertyEditor(TypeInfo(TPicture), TEsImageControl, '', TEsPicturePropertyFix);
  {$ENDIF}
end;

end.
