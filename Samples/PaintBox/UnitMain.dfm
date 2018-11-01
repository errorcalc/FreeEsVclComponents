object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'FormMain'
  ClientHeight = 435
  ClientWidth = 822
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 27
    Width = 400
    Height = 400
    OnPaint = PaintBoxPaint
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 93
    Height = 13
    Caption = 'Standart TPaintBox'
  end
  object Label2: TLabel
    Left = 414
    Top = 8
    Width = 105
    Height = 13
    Caption = 'errorsoft TEsPaintBox'
  end
  object EsPaintBox: TEsPaintBox
    Left = 414
    Top = 27
    Width = 400
    Height = 400
    OnPaint = EsPaintBoxPaint
  end
  object Timer: TTimer
    Interval = 20
    OnTimer = TimerTimer
    Left = 160
    Top = 8
  end
end
