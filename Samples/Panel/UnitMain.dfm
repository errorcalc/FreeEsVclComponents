object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Panel Sample'
  ClientHeight = 185
  ClientWidth = 415
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
  object Label10: TLabel
    Left = 8
    Top = 125
    Width = 62
    Height = 13
    Caption = 'FrameWidth:'
  end
  object Label11: TLabel
    Left = 168
    Top = 125
    Width = 59
    Height = 13
    Caption = 'FrameColor:'
  end
  object EsPanel1: TEsPanel
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    TabOrder = 0
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 100
      Height = 13
      Caption = 'FrameStyle = Raised'
    end
  end
  object EsPanel2: TEsPanel
    Left = 143
    Top = 8
    Width = 129
    Height = 25
    TabOrder = 1
    FrameStyle = Lowered
    object Label2: TLabel
      Left = 3
      Top = 3
      Width = 109
      Height = 13
      Caption = 'FrameStyle = Lowered'
    end
  end
  object EsPanel3: TEsPanel
    Left = 278
    Top = 8
    Width = 129
    Height = 25
    TabOrder = 2
    FrameStyle = None
    object Label3: TLabel
      Left = 3
      Top = 3
      Width = 93
      Height = 13
      Caption = 'FrameStyle = None'
    end
  end
  object EsPanel4: TEsPanel
    Left = 278
    Top = 39
    Width = 129
    Height = 25
    TabOrder = 3
    FrameStyle = Etched
    object Label4: TLabel
      Left = 3
      Top = 3
      Width = 101
      Height = 13
      Caption = 'FrameStyle = Etched'
    end
  end
  object EsPanel5: TEsPanel
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    TabOrder = 4
    FrameStyle = Bump
    object Label5: TLabel
      Left = 3
      Top = 3
      Width = 94
      Height = 13
      Caption = 'FrameStyle = Bump'
    end
  end
  object EsPanel6: TEsPanel
    Left = 143
    Top = 39
    Width = 129
    Height = 25
    TabOrder = 5
    FrameStyle = Chess
    object Label6: TLabel
      Left = 3
      Top = 3
      Width = 97
      Height = 13
      Caption = 'FrameStyle = Chess'
    end
  end
  object EsPanelDown: TEsPanel
    Left = 8
    Top = 86
    Width = 129
    Height = 25
    BorderWidth = 1
    TabOrder = 6
    FrameStyle = Down
    object Label7: TLabel
      Left = 2
      Top = 2
      Width = 125
      Height = 21
      Align = alClient
      Caption = 'FrameStyle = Down'
      ExplicitWidth = 95
      ExplicitHeight = 13
    end
  end
  object EsPanelUp: TEsPanel
    Left = 143
    Top = 86
    Width = 129
    Height = 25
    BorderWidth = 1
    TabOrder = 7
    FrameStyle = Up
    object Label8: TLabel
      Left = 2
      Top = 2
      Width = 125
      Height = 21
      Align = alClient
      Caption = 'FrameStyle = Up'
      ExplicitWidth = 81
      ExplicitHeight = 13
    end
  end
  object EsPanelFlat: TEsPanel
    Left = 278
    Top = 86
    Width = 129
    Height = 25
    BorderWidth = 1
    TabOrder = 8
    FrameStyle = Flat
    object Label9: TLabel
      Left = 2
      Top = 2
      Width = 125
      Height = 21
      Align = alClient
      Caption = 'FrameStyle = Flat'
      ExplicitWidth = 86
      ExplicitHeight = 13
    end
  end
  object TrackBarFrameWidth: TTrackBar
    Left = 8
    Top = 144
    Width = 150
    Height = 33
    Min = 1
    Position = 1
    TabOrder = 9
    OnChange = TrackBarFrameWidthChange
  end
  object ColorBoxFrameColor: TColorBox
    Left = 168
    Top = 144
    Width = 104
    Height = 22
    TabOrder = 10
    OnChange = ColorBoxFrameColorChange
  end
  object EsSwitch1: TEsSwitch
    Left = 306
    Top = 146
    Width = 90
    Height = 20
    TextOn = 'Style On'
    TextOff = 'Style Off'
    ShowCaption = True
    TabOrder = 11
    OnClick = EsSwitch1Click
  end
end
