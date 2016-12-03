object FormConverter: TFormConverter
  Left = 0
  Top = 0
  Caption = 'DFM Converter'
  ClientHeight = 363
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LogMemo: TMemo
    Left = 0
    Top = 41
    Width = 419
    Height = 322
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 419
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object EsLabel: TLabel
      AlignWithMargins = True
      Left = 354
      Top = 0
      Width = 57
      Height = 41
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alRight
      Caption = 'ErrorSoft(c)'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object OpenButton: TButton
      Left = 8
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Open And Convert'
      TabOrder = 0
      OnClick = OpenButtonClick
    end
    object ClearLogButton: TButton
      Left = 127
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 1
      OnClick = ClearLogButtonClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = '*.dfm|*.dfm'
    Left = 192
    Top = 168
  end
end
