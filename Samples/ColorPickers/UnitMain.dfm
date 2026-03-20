object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 8
  Caption = 'ColorPickers'
  ClientHeight = 311
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object LabelLayout: TLabel
    Left = 0
    Top = 144
    Width = 39
    Height = 15
    Caption = 'Layout:'
  end
  object LabelThumbLayout: TLabel
    Left = 0
    Top = 173
    Width = 77
    Height = 15
    Caption = 'ThumbLayout:'
  end
  object LabelTheme: TLabel
    Left = 0
    Top = 293
    Width = 36
    Height = 15
    Caption = 'Theme'
  end
  object EsRgbLinePickerRed: TEsRgbLinePicker
    Left = 0
    Top = 0
    Width = 200
    Height = 25
    OnChange = EsRgbLinePickerRedChange
    TabOrder = 0
    Kind = Red
  end
  object EsRgbLinePickerGreen: TEsRgbLinePicker
    Left = 0
    Top = 31
    Width = 200
    Height = 25
    OnChange = EsRgbLinePickerGreenChange
    TabOrder = 1
    Kind = Green
  end
  object EsRgbLinePickerBlue: TEsRgbLinePicker
    Left = 0
    Top = 62
    Width = 200
    Height = 25
    OnChange = EsRgbLinePickerBlueChange
    TabOrder = 2
    Kind = Blue
  end
  object EsAlphaLinePicker: TEsAlphaLinePicker
    Left = 0
    Top = 93
    Width = 200
    Height = 25
    OnChange = EsAlphaLinePickerChange
    TabOrder = 3
  end
  object CheckBoxColorModulation: TCheckBox
    Left = 0
    Top = 245
    Width = 200
    Height = 17
    Caption = 'ColorModulation'
    TabOrder = 8
    OnClick = CheckBoxColorModulationClick
  end
  object CheckBoxReverse: TCheckBox
    Left = 0
    Top = 199
    Width = 200
    Height = 17
    Caption = 'Reverse'
    TabOrder = 6
    OnClick = CheckBoxReverseClick
  end
  object CheckBoxAlphaModulation: TCheckBox
    Left = 0
    Top = 268
    Width = 200
    Height = 17
    Caption = 'AlphaModulation'
    TabOrder = 9
    OnClick = CheckBoxAlphaModulationClick
  end
  object CheckBoxPositionLineVisible: TCheckBox
    Left = 0
    Top = 222
    Width = 200
    Height = 17
    Caption = 'PositionLineVisible'
    TabOrder = 7
    OnClick = CheckBoxPositionLineVisibleClick
  end
  object ComboBoxLayout: TComboBox
    Left = 83
    Top = 141
    Width = 117
    Height = 23
    Style = csDropDownList
    TabOrder = 4
    OnChange = ComboBoxLayoutChange
    Items.Strings = (
      'ExternalArrows'
      'InternalArrows'
      'CenterArrows')
  end
  object ComboBoxThumbLayout: TComboBox
    Left = 83
    Top = 170
    Width = 117
    Height = 23
    Style = csDropDownList
    TabOrder = 5
    OnChange = ComboBoxThumbLayoutChange
    Items.Strings = (
      'First'
      'Second'
      'Both')
  end
  object EsLayoutPreview: TEsLayout
    Left = 206
    Top = 0
    Width = 51
    Height = 87
    TabOrder = 11
    object ShapePreview: TShape
      Left = 0
      Top = 0
      Width = 51
      Height = 87
      Align = alClient
    end
  end
  object EsLayoutTest: TEsLayout
    Left = 206
    Top = 93
    Width = 51
    Height = 25
    TabOrder = 12
    object ShapeTest: TShape
      Left = 0
      Top = 0
      Width = 51
      Height = 25
      Align = alClient
    end
    object LabelTest: TLabel
      Left = 0
      Top = 0
      Width = 51
      Height = 25
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 'TEST'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      Layout = tlCenter
      StyleElements = [seClient, seBorder]
    end
  end
  object EsSwitchTheme: TEsSwitch
    Left = 42
    Top = 291
    Width = 64
    Height = 20
    ShowCaption = True
    TabOrder = 10
    OnClick = EsSwitchThemeClick
  end
end
