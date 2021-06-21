object RegExForm: TRegExForm
  Left = 0
  Top = 0
  Caption = 'RegEx controls Demo'
  ClientHeight = 273
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    473
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 166
    Height = 13
    Caption = 'TEsRegexEdit (float value RegEx):'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 186
    Height = 13
    Caption = 'TEsRegexLabeledEdit (integer RegEx):'
  end
  object Label3: TLabel
    Left = 8
    Top = 94
    Width = 183
    Height = 13
    Caption = 'TEsRegexButtonedEdit (Email RegEx):'
  end
  object Label4: TLabel
    Left = 327
    Top = 94
    Width = 57
    Height = 13
    Caption = '<- Click me!'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 120
    Width = 457
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 619
  end
  object Label5: TLabel
    Left = 8
    Top = 138
    Width = 209
    Height = 13
    Caption = 'It TEsRegexEdit with customizable settings:'
  end
  object Label6: TLabel
    Left = 16
    Top = 220
    Width = 40
    Height = 13
    Caption = 'Pattern:'
  end
  object Label7: TLabel
    Left = 261
    Top = 193
    Width = 72
    Height = 13
    Caption = 'ColorIntensity:'
  end
  object Label8: TLabel
    Left = 261
    Top = 220
    Width = 51
    Height = 13
    Caption = 'ColorValid:'
  end
  object Label9: TLabel
    Left = 261
    Top = 248
    Width = 61
    Height = 13
    Caption = 'ColorInvalid:'
  end
  object Label10: TLabel
    Left = 16
    Top = 248
    Width = 69
    Height = 13
    Caption = 'IndicateState:'
  end
  object FloatRegexEdit: TEsRegexEdit
    Left = 200
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '1.2e+10'
    Pattern = '^[+-]?[\d]+($|[\.][\d]+|([\.][\d]+[Ee]|[Ee])[+-]?\d+)$'
    AllowNeutral = True
    IndicateState = All
  end
  object IntegerRegexLabeledEdit: TEsRegexLabeledEdit
    Left = 200
    Top = 51
    Width = 121
    Height = 21
    EditLabel.Width = 108
    EditLabel.Height = 13
    EditLabel.Caption = 'Enter Integer number:'
    TabOrder = 1
    Text = '101'
    Pattern = '^[+-]?[0-9]+$'
    ColorValid = clLime
    ColorInvalid = clRed
    AllowNeutral = False
    IndicateState = All
  end
  object EmailRegexButtonedEdit: TEsRegexButtonedEdit
    Left = 200
    Top = 91
    Width = 121
    Height = 21
    Images = ImageList1
    RightButton.HotImageIndex = 1
    RightButton.ImageIndex = 0
    RightButton.PressedImageIndex = 1
    RightButton.Visible = True
    TabOrder = 2
    Text = 'errorsoft@mail.ru'
    OnRightButtonClick = EmailRegexButtonedEditRightButtonClick
    Pattern = 
      '^(?!\.)(""([^""\r\\]|\\[""\r\\])*""|([-a-z0-9!#$%&'#39'*+/=?^_`{|}~]' +
      '|(?<!\.)\.)*)(?<!\.)@[a-z0-9][\w\.-]*[a-z0-9]\.[a-z][a-z\.]*[a-z' +
      ']$'
    AllowNeutral = True
    IndicateState = All
  end
  object TestRegexEdit: TEsRegexEdit
    Left = 8
    Top = 157
    Width = 400
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'Test me!'
    Pattern = '^[0-9]+$'
    AllowNeutral = True
    IndicateState = All
  end
  object PatternEdit: TEdit
    Left = 92
    Top = 218
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '^[0-9]+$'
    OnChange = PatternEditChange
  end
  object AllowNeutralCheckBox: TCheckBox
    Left = 91
    Top = 192
    Width = 97
    Height = 17
    Caption = 'AllowNeutral'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = AllowNeutralCheckBoxClick
  end
  object ColorIntensityEdit: TEsRegexEdit
    Left = 339
    Top = 190
    Width = 121
    Height = 21
    TabOrder = 6
    Text = '50'
    OnChange = ColorIntensityEditChange
    Pattern = '^[+-]?[0-9]+$'
    AllowNeutral = True
    IndicateState = All
  end
  object ColorValidColorBox: TColorBox
    Left = 339
    Top = 217
    Width = 121
    Height = 22
    DefaultColorColor = clDefault
    Selected = clDefault
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    TabOrder = 7
    OnChange = ColorValidColorBoxChange
  end
  object ColorInvalidColorBox: TColorBox
    Left = 339
    Top = 245
    Width = 121
    Height = 22
    DefaultColorColor = clDefault
    Selected = clDefault
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    TabOrder = 8
    OnChange = ColorInvalidColorBoxChange
  end
  object IndicateStateComboBox: TComboBox
    Left = 91
    Top = 245
    Width = 122
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 9
    Text = 'All'
    OnChange = IndicateStateComboBoxChange
    Items.Strings = (
      'All'
      'Valid'
      'Invalid'
      'None')
  end
  object Button1: TButton
    Left = 414
    Top = 155
    Width = 51
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test'
    TabOrder = 10
    OnClick = Button1Click
  end
  object EnableThemesCheckBox: TCheckBox
    Left = 368
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Enable themes'
    TabOrder = 11
    OnClick = EnableThemesCheckBoxClick
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    Left = 320
    Top = 16
    Bitmap = {
      494C010102000800040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008080800000000000808080008080800000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00808080008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008080800080808000FFFFFF000000000080808000FFFFFF00808080008080
      800000000000FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00808080008080800000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF008080800080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008080
      8000FFFFFF00FFFFFF00FFFFFF000000000080808000FFFFFF00000000000000
      0000808080008080800000000000FFFFFF00FFFFFF00FFFFFF00808080008080
      8000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FFFFFF00000000000000
      000000000000FFFFFF008080800080808000808080008080800000000000FFFF
      FF00FFFFFF000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF008080800000000000FFFF0000FFFF0000FFFF0000FFFF0000000000008080
      8000FFFFFF00FFFFFF00FFFFFF000000000080808000FFFFFF0000000000FFFF
      FF00808080008080800000000000000000000000000000000000808080008080
      800000000000FFFFFF00FFFFFF00808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00808080000000
      000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      80000000000080808000FFFFFF000000000080808000FFFFFF00808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF00008080800000000000000000008080800080808000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF00008080800000000000000000000000000080808000808080000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080
      8000000000008080800000000000000000000000000000000000000000008080
      80008080800000000000FFFFFF00FFFFFF000000000000000000FFFFFF008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000080808000FFFF0000FFFF000080808000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      000000000000808080008080800000000000FFFFFF0080808000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFF800000000000
      0000000000000000000027F800000000000009E2000000000000320E00000000
      0000382600000000000023C80000000000000FF2000000000000000000000000
      0000000000000000800193F100000000C003E4C700000000F00FF91F00000000
      FC3FFE7F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
