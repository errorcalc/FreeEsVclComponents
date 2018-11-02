object StyleImageEditor: TStyleImageEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Style Image Editor'
  ClientHeight = 353
  ClientWidth = 485
  Color = clBtnFace
  Constraints.MinHeight = 212
  Constraints.MinWidth = 270
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object DispScrollBox: TScrollBox
    Left = 0
    Top = 33
    Width = 485
    Height = 281
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    Color = clAppWorkSpace
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentColor = False
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 108
      Height = 108
      AutoSize = True
      BevelOuter = bvNone
      DoubleBuffered = False
      ParentDoubleBuffered = False
      TabOrder = 0
      object Disp: TPaintBox
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 100
        Height = 100
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        OnMouseDown = DispMouseDown
        OnMouseMove = DispMouseMove
        OnMouseUp = DispMouseUp
        OnPaint = DispPaint
      end
    end
  end
  object FooterPanel: TPanel
    Left = 0
    Top = 314
    Width = 485
    Height = 39
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 32
    ExplicitTop = 306
    DesignSize = (
      485
      37)
    object OkButton: TButton
      Left = 321
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 402
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
    end
    object LoadButton: TButton
      Left = 8
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 2
      Visible = False
    end
  end
  object HeaderPanel: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 33
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = -16
    ExplicitTop = 69
    object PlusSpeedButton: TSpeedButton
      Left = 125
      Top = 5
      Width = 23
      Height = 23
      Caption = '+'
      OnClick = PlusSpeedButtonClick
    end
    object MinusSpeedButton: TSpeedButton
      Left = 43
      Top = 5
      Width = 23
      Height = 23
      Caption = '-'
      OnClick = MinusSpeedButtonClick
    end
    object ZoomLabel: TLabel
      Left = 8
      Top = 9
      Width = 30
      Height = 13
      Caption = 'Zoom:'
    end
    object ZoomComboBox: TComboBox
      Left = 67
      Top = 6
      Width = 57
      Height = 21
      Style = csDropDownList
      DropDownCount = 10
      ItemIndex = 0
      TabOrder = 0
      Text = '100'
      OnChange = ZoomComboBoxChange
      Items.Strings = (
        '100'
        '200'
        '300'
        '400'
        '500'
        '600'
        '700'
        '800'
        '900'
        '1000')
    end
  end
end
