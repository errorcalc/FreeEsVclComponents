unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ES.BaseControls, ES.Pickers, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes, ES.Layouts, ES.ExGraphics,
  ES.Switch, Vcl.Themes;

type
  TFormMain = class(TForm)
    EsRgbLinePickerRed: TEsRgbLinePicker;
    EsRgbLinePickerGreen: TEsRgbLinePicker;
    EsRgbLinePickerBlue: TEsRgbLinePicker;
    EsAlphaLinePicker: TEsAlphaLinePicker;
    CheckBoxColorModulation: TCheckBox;
    CheckBoxReverse: TCheckBox;
    CheckBoxAlphaModulation: TCheckBox;
    CheckBoxPositionLineVisible: TCheckBox;
    ComboBoxLayout: TComboBox;
    ComboBoxThumbLayout: TComboBox;
    LabelLayout: TLabel;
    LabelThumbLayout: TLabel;
    ShapeTest: TShape;
    LabelTest: TLabel;
    EsLayoutPreview: TEsLayout;
    ShapePreview: TShape;
    EsLayoutTest: TEsLayout;
    EsSwitchTheme: TEsSwitch;
    LabelTheme: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxLayoutChange(Sender: TObject);
    procedure ComboBoxThumbLayoutChange(Sender: TObject);
    procedure CheckBoxReverseClick(Sender: TObject);
    procedure CheckBoxColorModulationClick(Sender: TObject);
    procedure CheckBoxAlphaModulationClick(Sender: TObject);
    procedure CheckBoxPositionLineVisibleClick(Sender: TObject);
    procedure EsRgbLinePickerRedChange(Sender: TObject);
    procedure EsRgbLinePickerGreenChange(Sender: TObject);
    procedure EsRgbLinePickerBlueChange(Sender: TObject);
    procedure EsAlphaLinePickerChange(Sender: TObject);
    procedure EsSwitchThemeClick(Sender: TObject);
  private
    IsUpdate: Boolean;
    procedure SetLayout(Layout: TLinePickerLayout);
    procedure SetThumbLayout(Layout: TLinePickerThumbLayout);
    procedure SetReverse(Value: Boolean);
    procedure SetColorModulation(Value: Boolean);
    procedure SetAlphaModulation(Value: Boolean);
    procedure SetPositionLineVisible(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetAlpha(Value: Byte);
    procedure UpdatePreview(Color: TColor; Alpha: Byte);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  SetLayout(TLinePickerLayout.ExternalArrows);
  SetThumbLayout(TLinePickerThumbLayout.Both);
  SetPositionLineVisible(False);
  SetReverse(False);
  SetColorModulation(True);
  SetAlphaModulation(False);

  SetColor(clWebTomato);
  SetAlpha(200);
end;

procedure TFormMain.CheckBoxAlphaModulationClick(Sender: TObject);
begin
  SetAlphaModulation(CheckBoxAlphaModulation.Checked);
end;

procedure TFormMain.CheckBoxColorModulationClick(Sender: TObject);
begin
  SetColorModulation(CheckBoxColorModulation.Checked);
end;

procedure TFormMain.CheckBoxPositionLineVisibleClick(Sender: TObject);
begin
  SetPositionLineVisible(CheckBoxPositionLineVisible.Checked);
end;

procedure TFormMain.CheckBoxReverseClick(Sender: TObject);
begin
  SetReverse(CheckBoxReverse.Checked);
end;

procedure TFormMain.ComboBoxLayoutChange(Sender: TObject);
begin
  SetLayout(TLinePickerLayout(ComboBoxLayout.ItemIndex));
end;

procedure TFormMain.ComboBoxThumbLayoutChange(Sender: TObject);
begin
  SetThumbLayout(TLinePickerThumbLayout(ComboBoxThumbLayout.ItemIndex));
end;

procedure TFormMain.EsAlphaLinePickerChange(Sender: TObject);
begin
  SetAlpha(EsAlphaLinePicker.Alpha);
end;

procedure TFormMain.EsRgbLinePickerBlueChange(Sender: TObject);
begin
  SetColor(EsRgbLinePickerBlue.CurrentColor);
end;

procedure TFormMain.EsRgbLinePickerGreenChange(Sender: TObject);
begin
  SetColor(EsRgbLinePickerGreen.CurrentColor);
end;

procedure TFormMain.EsRgbLinePickerRedChange(Sender: TObject);
begin
  SetColor(EsRgbLinePickerRed.CurrentColor);
end;

procedure TFormMain.EsSwitchThemeClick(Sender: TObject);
begin
  if TEsSwitch(Sender).Checked then
    TStyleManager.TrySetStyle('Glow')
  else
    TStyleManager.TrySetStyle('Windows');
end;

procedure TFormMain.SetAlpha(Value: Byte);
begin
  UpdatePreview(EsAlphaLinePicker.ColorPreview, Value);
  EsRgbLinePickerRed.AlphaPreview := Value;
  EsRgbLinePickerGreen.AlphaPreview := Value;
  EsRgbLinePickerBlue.AlphaPreview := Value;
  EsAlphaLinePicker.Alpha := Value;
end;

procedure TFormMain.SetAlphaModulation(Value: Boolean);
begin
  EsRgbLinePickerRed.AlphaModulation := Value;
  EsRgbLinePickerGreen.AlphaModulation := Value;
  EsRgbLinePickerBlue.AlphaModulation := Value;
  CheckBoxAlphaModulation.Checked := Value;
end;

procedure TFormMain.SetColor(Value: TColor);
begin
  if IsUpdate then
  begin
    exit;
  end;

  IsUpdate := True;
  try
    EsRgbLinePickerRed.CurrentColor := Value;
    EsRgbLinePickerGreen.CurrentColor := Value;
    EsRgbLinePickerBlue.CurrentColor := Value;
    EsAlphaLinePicker.ColorPreview := Value;

    UpdatePreview(Value, EsAlphaLinePicker.Alpha);
  finally
    IsUpdate := False;
  end;
end;

procedure TFormMain.SetColorModulation(Value: Boolean);
begin
  EsRgbLinePickerRed.ColorModulation := Value;
  EsRgbLinePickerGreen.ColorModulation := Value;
  EsRgbLinePickerBlue.ColorModulation := Value;
  //EsAlphaLinePicker.ColorModulation := Value;
  CheckBoxColorModulation.Checked := Value;
end;

procedure TFormMain.SetLayout(Layout: TLinePickerLayout);
begin
  EsRgbLinePickerRed.Layout := Layout;
  EsRgbLinePickerGreen.Layout := Layout;
  EsRgbLinePickerBlue.Layout := Layout;
  EsAlphaLinePicker.Layout := Layout;
  ComboBoxLayout.ItemIndex := Integer(Layout);
end;

procedure TFormMain.SetPositionLineVisible(Value: Boolean);
begin
  EsRgbLinePickerRed.PositionLineVisible := Value;
  EsRgbLinePickerGreen.PositionLineVisible := Value;
  EsRgbLinePickerBlue.PositionLineVisible := Value;
  EsAlphaLinePicker.PositionLineVisible := Value;
  CheckBoxPositionLineVisible.Checked := Value;
end;

procedure TFormMain.SetReverse(Value: Boolean);
begin
  EsRgbLinePickerRed.Reverse := Value;
  EsRgbLinePickerGreen.Reverse := Value;
  EsRgbLinePickerBlue.Reverse := Value;
  EsAlphaLinePicker.Reverse := Value;
  CheckBoxReverse.Checked := Value;
end;

procedure TFormMain.SetThumbLayout(Layout: TLinePickerThumbLayout);
begin
  EsRgbLinePickerRed.ThumbLayout := Layout;
  EsRgbLinePickerGreen.ThumbLayout := Layout;
  EsRgbLinePickerBlue.ThumbLayout := Layout;
  EsAlphaLinePicker.ThumbLayout := Layout;
  ComboBoxThumbLayout.ItemIndex := Integer(Layout);
end;

procedure TFormMain.UpdatePreview(Color: TColor; Alpha: Byte);
begin
  ShapePreview.Brush.Color := Color;
  ShapeTest.Brush.Color := clWhite;
  LabelTest.Font.Color := AlphaColorToColor(BlendAlphaColor(ColorToAlphaColor(clWhite), ColorToAlphaColor(Color), Alpha));
end;

end.
