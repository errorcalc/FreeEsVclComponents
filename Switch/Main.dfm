object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TEsSwitch - the best Delphi Switch control!'
  ClientHeight = 537
  ClientWidth = 643
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 18
  object EsImageLayout1: TEsImageLayout
    Left = 0
    Top = 0
    Width = 643
    Height = 537
    ImageMargins.Left = 6
    ImageMargins.Top = 6
    ImageMargins.Right = 6
    ImageMargins.Bottom = 6
    Image.Data = {
      89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
      87000000584944415478DAEDCFC10900200C00315DC6FDE7E934BE7D8928944A
      6E824B6FC5EBD9032F01237BE6B0F81610D9679B964F00000000000000000000
      000000000000000000000000000000000000806B4095FE0394AC3C6002D55854
      312E963B130000000049454E44AE426082}
    PaddingWithImage = True
    Align = alClient
    IsDrawHelper = True
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 9
      Width = 383
      Height = 24
      Caption = 'TEsSwitch - the best Delphi Switch control!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 152
      Top = 56
      Width = 75
      Height = 18
      Caption = '- TEsSwitch'
    end
    object Label3: TLabel
      Left = 152
      Top = 89
      Width = 229
      Height = 18
      Caption = '- TEsSwitch && ShowCaption = True'
    end
    object Label4: TLabel
      Left = 152
      Top = 126
      Width = 418
      Height = 18
      Caption = '- TEsSwitch && ShowCaption = True && SwitchLayout = AutoSize'
    end
    object Label5: TLabel
      Left = 152
      Top = 167
      Width = 397
      Height = 18
      Caption = '- TEsSwitch && ShowCaption = True && SwitchLayout = Client'
    end
    object Label6: TLabel
      Left = 152
      Top = 241
      Width = 364
      Height = 18
      Caption = '- TEsSwitch && ShowCaption = True && Animated = False'
    end
    object Label7: TLabel
      Left = 152
      Top = 289
      Width = 338
      Height = 18
      Caption = '- TEsSwitch && ShowCaption = True && CustomColors'
    end
    object Label8: TLabel
      Left = 152
      Top = 342
      Width = 195
      Height = 18
      Caption = '- TEsSwitch && Actions support'
    end
    object Label9: TLabel
      Left = 31
      Top = 465
      Width = 36
      Height = 18
      Caption = 'Style:'
    end
    object Label10: TLabel
      Left = 31
      Top = 494
      Width = 59
      Height = 18
      Caption = 'FontSize:'
    end
    object EsSwitch1: TEsSwitch
      Left = 31
      Top = 55
      Width = 44
      Height = 20
      TabOrder = 0
    end
    object EsLayout1: TEsLayout
      Left = 22
      Top = 55
      Width = 3
      Height = 457
      Color = 15987699
      ParentBackground = False
      ParentColor = False
      TabOrder = 1
    end
    object EsSwitch2: TEsSwitch
      Left = 31
      Top = 88
      Width = 68
      Height = 20
      ShowCaption = True
      TabOrder = 2
    end
    object EsSwitch3: TEsSwitch
      Left = 31
      Top = 123
      Width = 77
      Height = 24
      ShowCaption = True
      SwitchLayout = AutoSize
      TabOrder = 3
    end
    object EsSwitch4: TEsSwitch
      Left = 31
      Top = 160
      Width = 77
      Height = 32
      ShowCaption = True
      SwitchLayout = Client
      TabOrder = 4
    end
    object EsSwitch5: TEsSwitch
      Left = 31
      Top = 207
      Width = 133
      Height = 20
      TextOn = ':)'
      TextOff = 'Custom Text'
      ShowCaption = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object EsSwitch6: TEsSwitch
      Left = 31
      Top = 240
      Width = 68
      Height = 20
      ShowCaption = True
      Animated = False
      TabOrder = 6
    end
    object EsLayout2: TEsLayout
      Left = 25
      Top = 273
      Width = 608
      Height = 3
      Color = 15987699
      ParentBackground = False
      ParentColor = False
      TabOrder = 7
    end
    object EsSwitch7: TEsSwitch
      Left = 31
      Top = 283
      Width = 68
      Height = 20
      Checked = True
      ShowCaption = True
      FrameColor = clGray
      ThumbColor = clWhite
      MainColor = clLime
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object EsLayout3: TEsLayout
      Left = 25
      Top = 319
      Width = 608
      Height = 3
      Color = 15987699
      ParentBackground = False
      ParentColor = False
      TabOrder = 9
    end
    object EsSwitch8: TEsSwitch
      Left = 31
      Top = 329
      Width = 68
      Height = 20
      ShowCaption = True
      Action = Action1
      Enabled = False
      TabOrder = 10
    end
    object EsSwitch9: TEsSwitch
      Left = 31
      Top = 363
      Width = 68
      Height = 20
      ShowCaption = True
      Action = Action1
      TabOrder = 11
    end
    object CheckBox1: TCheckBox
      Left = 31
      Top = 429
      Width = 97
      Height = 17
      Action = Action1
      TabOrder = 12
    end
    object EsSwitch10: TEsSwitch
      Left = 31
      Top = 395
      Width = 68
      Height = 20
      ShowCaption = True
      Action = Action1
      TabOrder = 13
    end
    object EsLayout4: TEsLayout
      Left = 25
      Top = 452
      Width = 608
      Height = 3
      Color = 15987699
      ParentBackground = False
      ParentColor = False
      TabOrder = 14
    end
    object cbStyle: TComboBox
      Left = 152
      Top = 462
      Width = 153
      Height = 26
      Style = csDropDownList
      TabOrder = 15
      OnChange = cbStyleChange
    end
    object StaticText1: TStaticText
      Left = 152
      Top = 494
      Width = 137
      Height = 22
      AutoSize = False
      Caption = '5'
      TabOrder = 16
    end
    object udFontSize: TUpDown
      Left = 289
      Top = 494
      Width = 16
      Height = 22
      Associate = StaticText1
      Min = 5
      Max = 40
      Position = 5
      TabOrder = 17
      OnClick = udFontSizeClick
    end
  end
  object ActionList1: TActionList
    Left = 544
    Top = 368
    object Action1: TAction
      AutoCheck = True
      Caption = 'Action1'
      OnExecute = Action1Execute
    end
  end
end
