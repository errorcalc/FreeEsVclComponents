object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Labels Sample'
  ClientHeight = 241
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 57
    Caption = 'TEsVersionLabel'
    TabOrder = 0
    object EsVersionLabel: TEsVersionLabel
      Left = 11
      Top = 24
      Width = 78
      Height = 13
    end
    object Label1: TLabel
      Left = 237
      Top = 24
      Width = 68
      Height = 13
      Caption = '<--- Click this!'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 71
    Width = 321
    Height = 162
    Caption = 'TEsLinkLabel'
    TabOrder = 1
    object EsLinkLabel: TEsLinkLabel
      Left = 11
      Top = 136
      Width = 54
      Height = 13
      Caption = 'EsLinkLabel'
    end
    object EditCaption: TLabeledEdit
      Left = 11
      Top = 48
      Width = 294
      Height = 21
      EditLabel.Width = 37
      EditLabel.Height = 13
      EditLabel.Caption = 'Caption'
      TabOrder = 0
      Text = 'errorsoft.org'
      OnChange = EditCaptionChange
    end
    object EditAddress: TLabeledEdit
      Left = 11
      Top = 96
      Width = 294
      Height = 21
      EditLabel.Width = 39
      EditLabel.Height = 13
      EditLabel.Caption = 'Address'
      TabOrder = 1
      Text = 'http://errorsoft.org'
      OnChange = EditAddressChange
    end
  end
end
