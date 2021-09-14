object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Shapes'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  StyleName = 'Windows'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 8
    Top = 32
    Width = 608
    Height = 401
    Margins.Left = 8
    Margins.Top = 4
    Margins.Right = 8
    Margins.Bottom = 8
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Shapes'
      object EsLayoutShapes: TEsLayout
        Left = 0
        Top = 47
        Width = 600
        Height = 324
        Align = alClient
        ParentBufferedChildren = False
        TabOrder = 0
        object EsShapeAni13: TEsShape
          Left = 411
          Top = 209
          Width = 145
          Height = 41
          Brush.Color = clRed
          Caption = 'Sides!'
          Pen.Color = clMaroon
          Pen.Width = 3
          Sides = [Bottom, Right]
        end
        object EsShapeAni11: TEsShape
          Left = 506
          Top = 209
          Width = 65
          Height = 113
          Brush.Color = 12436544
          Brush.Opacity = 140
          Pen.Color = clTeal
          Shape = Rhomb
        end
        object EsShapeAni10: TEsShape
          Left = 303
          Top = 256
          Width = 102
          Height = 65
          Brush.StartColor = clBlue
          Brush.EndColor = clSkyBlue
          Brush.Style = HorizontalGradient
          Pen.Color = clNavy
          Pen.Style = DashDot
          Pen.Width = 3
          Shape = RoundRectangle
        end
        object EsShapeAni9: TEsShape
          Left = 351
          Top = 55
          Width = 86
          Height = 86
          Brush.Color = 33023
          Pen.Style = Dot
          Pen.Width = 4
          Shape = RoundSquare
          CornerRadius = 20
        end
        object EsShapeAni8: TEsShape
          Left = 11
          Top = 106
          Width = 109
          Height = 110
          Brush.Color = clRed
          Brush.Style = HalfHatch
          Pen.Color = clMaroon
          Shape = Star
          IsDrawHelper = True
        end
        object EsShapeAni1: TEsShape
          Left = 72
          Top = 18
          Width = 81
          Height = 82
          Brush.Style = VerticalGradient
          Shape = Triangle
        end
        object EsShapeAni7: TEsShape
          Left = 459
          Top = 18
          Width = 65
          Height = 41
          Brush.StartColor = 8388863
          Brush.EndColor = clRed
          Brush.Style = HorizontalGradient
          Pen.Color = clNavy
          Pen.Width = 2
          Shape = Arrow
          Direction = Right
        end
        object EsShapeAni6: TEsShape
          Left = 468
          Top = 83
          Width = 88
          Height = 88
          Brush.Color = clPurple
          Brush.Style = DiagonalCross
          Pen.Color = clMedGray
          Shape = Square
        end
        object EsShapeAni5: TEsShape
          Left = 226
          Top = 198
          Width = 64
          Height = 119
          Brush.Color = clYellow
          Brush.Opacity = 200
          Pen.Color = clRed
          Shape = Ellipse
        end
        object EsShapeAni4: TEsShape
          Left = 24
          Top = 269
          Width = 109
          Height = 52
          Brush.StartColor = 10944256
          Brush.EndColor = clGreen
          Brush.Style = VerticalGradient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'Gradient!'
          ParentFont = False
        end
        object EsShapeAni2: TEsShape
          Left = 185
          Top = 12
          Width = 104
          Height = 53
          Brush.Color = clBlue
          Brush.Opacity = 100
          Caption = 'Caption!'
          Pen.Color = clSkyBlue
          Pen.Width = 10
          Shape = Bubble
        end
        object EsShapeAni12: TEsShape
          Left = 296
          Top = 168
          Width = 109
          Height = 52
          Brush.StartColor = clGray
          Brush.EndColor = clBlack
          Brush.Style = VerticalGradient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'Corners!'
          Corners = [TopRight, BottomLeft]
          ParentFont = False
          Shape = RoundRectangle
          CornerRadius = 22
        end
        object EsShapeAni14: TEsShape
          Left = 118
          Top = 222
          Width = 81
          Height = 41
          Brush.Style = Clear
          Pen.Color = 15145051
          Pen.Style = Dash
          Pen.Width = 3
          Shape = Triangle
          Direction = Left
        end
        object EsShapeAni15: TEsShape
          Left = 225
          Top = 83
          Width = 88
          Height = 30
          Brush.Color = clMoneyGreen
          Pen.Color = clGreen
          Pen.Width = 2
          Shape = RoundRectangle
          Sides = [Left, Right]
        end
        object EsShapeAni3: TEsShape
          Left = 159
          Top = 94
          Width = 110
          Height = 110
          Brush.Color = clFuchsia
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'Opacity!'
          ParentFont = False
          Pen.Width = 6
          Shape = Circle
          Opacity = 100
        end
      end
      object EsLayout1: TEsLayout
        Left = 0
        Top = 0
        Width = 600
        Height = 47
        Align = alTop
        IsDrawHelper = False
        TabOrder = 1
        object EsShape1: TEsShape
          Left = 0
          Top = 0
          Width = 600
          Height = 47
          Align = alClient
          Brush.Color = clSkyBlue
          Brush.Opacity = 100
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'TEsShape graphic control'
          Padding.Left = 100
          Padding.Top = 4
          Padding.Right = 100
          Padding.Bottom = 4
          ParentFont = False
          Pen.Color = clSkyBlue
          Pen.Width = 2
          Pen.Opacity = 100
          Shape = Bubble
          IsDrawHelper = True
          ExplicitHeight = 25
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'RoundRectangle'
      ImageIndex = 1
      object Label1: TLabel
        Left = 40
        Top = 211
        Width = 74
        Height = 15
        Caption = 'CornerRadius:'
      end
      object CheckBoxTopLeft: TCheckBox
        Left = 33
        Top = 33
        Width = 81
        Height = 17
        BiDiMode = bdRightToLeft
        Caption = 'TopLeft'
        ParentBiDiMode = False
        TabOrder = 0
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxBottomLeft: TCheckBox
        Left = 33
        Top = 159
        Width = 81
        Height = 17
        BiDiMode = bdRightToLeft
        Caption = 'BottomLeft'
        ParentBiDiMode = False
        TabOrder = 1
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxBottomRight: TCheckBox
        Left = 295
        Top = 159
        Width = 98
        Height = 17
        Caption = 'BottomRight'
        TabOrder = 2
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxTopRight: TCheckBox
        Left = 295
        Top = 33
        Width = 98
        Height = 17
        Caption = 'TopRight'
        TabOrder = 3
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxRight: TCheckBox
        Left = 295
        Top = 97
        Width = 81
        Height = 17
        Caption = 'Right'
        TabOrder = 4
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxLeft: TCheckBox
        Left = 33
        Top = 97
        Width = 81
        Height = 17
        BiDiMode = bdRightToLeft
        Caption = 'Left'
        ParentBiDiMode = False
        TabOrder = 5
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxBottom: TCheckBox
        Left = 198
        Top = 159
        Width = 81
        Height = 17
        Caption = 'Bottom'
        TabOrder = 6
        OnClick = CheckBoxRoundRectClick
      end
      object CheckBoxTop: TCheckBox
        Left = 198
        Top = 33
        Width = 81
        Height = 17
        Caption = 'Top'
        TabOrder = 7
        OnClick = CheckBoxRoundRectClick
      end
      object TrackBarRadius: TTrackBar
        Left = 120
        Top = 211
        Width = 145
        Height = 38
        Max = 50
        Frequency = 3
        ShowSelRange = False
        TabOrder = 8
        OnChange = TrackBarRadiusChange
      end
      object EsLayout2: TEsLayout
        Left = 120
        Top = 56
        Width = 169
        Height = 97
        TabOrder = 9
        object ShapeRoundRect: TEsShape
          Left = 0
          Top = 0
          Width = 169
          Height = 97
          Brush.StartColor = clCream
          Brush.Style = VerticalGradient
          Corners = [TopLeft, TopRight]
          Shape = RoundRectangle
          CornerRadius = 24
          Sides = [Top, Left, Right]
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Styles'
      ImageIndex = 2
      object Label2: TLabel
        Left = 3
        Top = 3
        Width = 52
        Height = 15
        Caption = 'Windows:'
      end
      object Label3: TLabel
        Left = 3
        Top = 132
        Width = 30
        Height = 15
        Caption = 'Glow:'
      end
      object Label4: TLabel
        Left = 210
        Top = 132
        Width = 116
        Height = 15
        Caption = 'Windows10 SlateGray:'
      end
      object Label5: TLabel
        Left = 210
        Top = 3
        Width = 74
        Height = 15
        Caption = 'Slate Classico:'
      end
      object EsPanel1: TEsPanel
        Left = 3
        Top = 24
        Width = 201
        Height = 102
        ParentBackground = False
        TabOrder = 0
        StyleName = 'Windows'
        object EsShape2: TEsShape
          Left = 8
          Top = 8
          Width = 89
          Height = 41
          Brush.Color = clBtnFace
          Caption = 'clBtnFace'
          Shape = Bubble
        end
        object EsShape3: TEsShape
          Left = 8
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clHighlight
          Caption = 'clHighlight'
          Shape = Bubble
        end
        object EsShape5: TEsShape
          Left = 103
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clBtnHighlight
          Caption = 'clBtnHighlight'
          Shape = Bubble
        end
        object EsShape4: TEsShape
          Left = 103
          Top = 8
          Width = 89
          Height = 41
          Brush.StartColor = clGrayText
          Brush.EndColor = clBtnText
          Brush.Style = HorizontalGradient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'clBtnText..'
          ParentFont = False
          Shape = Bubble
        end
      end
      object EsPanel2: TEsPanel
        Left = 3
        Top = 153
        Width = 201
        Height = 102
        ParentBackground = False
        TabOrder = 1
        StyleName = 'Glow'
        object EsShape6: TEsShape
          Left = 8
          Top = 8
          Width = 89
          Height = 41
          Brush.Color = clBtnFace
          Caption = 'clBtnFace'
          Shape = Bubble
        end
        object EsShape7: TEsShape
          Left = 8
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clHighlight
          Caption = 'clHighlight'
          Shape = Bubble
        end
        object EsShape9: TEsShape
          Left = 103
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clBtnHighlight
          Caption = 'clBtnHighlight'
          Shape = Bubble
        end
        object EsShape8: TEsShape
          Left = 103
          Top = 8
          Width = 89
          Height = 41
          Brush.StartColor = clGrayText
          Brush.EndColor = clBtnText
          Brush.Style = HorizontalGradient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'clBtnText..'
          ParentFont = False
          Shape = Bubble
        end
      end
      object EsPanel3: TEsPanel
        Left = 210
        Top = 153
        Width = 201
        Height = 102
        ParentBackground = False
        TabOrder = 2
        StyleName = 'Windows10 SlateGray'
        object EsShape10: TEsShape
          Left = 8
          Top = 8
          Width = 89
          Height = 41
          Brush.Color = clBtnFace
          Caption = 'clBtnFace'
          Shape = Bubble
        end
        object EsShape11: TEsShape
          Left = 8
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clHighlight
          Caption = 'clHighlight'
          Shape = Bubble
        end
        object EsShape13: TEsShape
          Left = 103
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clBtnHighlight
          Caption = 'clBtnHighlight'
          Shape = Bubble
        end
        object EsShape16: TEsShape
          Left = 103
          Top = 8
          Width = 89
          Height = 41
          Brush.StartColor = clGrayText
          Brush.EndColor = clBtnText
          Brush.Style = HorizontalGradient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'clBtnText..'
          ParentFont = False
          Shape = Bubble
        end
      end
      object EsPanel4: TEsPanel
        Left = 210
        Top = 24
        Width = 201
        Height = 102
        ParentBackground = False
        TabOrder = 3
        StyleName = 'Slate Classico'
        object EsShape14: TEsShape
          Left = 8
          Top = 8
          Width = 89
          Height = 41
          Brush.Color = clBtnFace
          Caption = 'clBtnFace'
          Shape = Bubble
        end
        object EsShape15: TEsShape
          Left = 8
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clHighlight
          Caption = 'clHighlight'
          Shape = Bubble
        end
        object EsShape17: TEsShape
          Left = 103
          Top = 55
          Width = 89
          Height = 41
          Brush.Color = clBtnHighlight
          Caption = 'clBtnHighlight'
          Shape = Bubble
        end
        object EsShape12: TEsShape
          Left = 103
          Top = 8
          Width = 89
          Height = 41
          Brush.StartColor = clGrayText
          Brush.EndColor = clBtnText
          Brush.Style = HorizontalGradient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Caption = 'clBtnText..'
          ParentFont = False
          Shape = Bubble
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'ControlList sample'
      ImageIndex = 3
      object EsShape18: TEsShape
        AlignWithMargins = True
        Left = 369
        Top = 13
        Width = 9
        Height = 27
        Margins.Left = 8
        Brush.Color = clBlue
        Brush.EndColor = 14079702
        Pen.Style = Clear
        Shape = Circle
        IsDrawHelper = True
      end
      object Label6: TLabel
        Left = 384
        Top = 19
        Width = 24
        Height = 15
        Caption = 'New'
      end
      object EsShape19: TEsShape
        AlignWithMargins = True
        Left = 369
        Top = 40
        Width = 9
        Height = 27
        Margins.Left = 8
        Brush.Color = clSkyBlue
        Brush.EndColor = 14079702
        Pen.Style = Clear
        Shape = Circle
        IsDrawHelper = True
      end
      object Label7: TLabel
        Left = 384
        Top = 46
        Width = 72
        Height = 15
        Caption = 'Coming soon'
      end
      object ControlList: TControlList
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 345
        Height = 355
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alLeft
        ItemCount = 500
        ItemHeight = 50
        ItemMargins.Left = 0
        ItemMargins.Top = 0
        ItemMargins.Right = 0
        ItemMargins.Bottom = 0
        ParentColor = False
        TabOrder = 0
        OnBeforeDrawItem = ControlListBeforeDrawItem
        object LabelProduct: TLabel
          AlignWithMargins = True
          Left = 23
          Top = 3
          Width = 122
          Height = 41
          Margins.Bottom = 6
          Align = alClient
          Caption = 'LabelProduct'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -18
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
          ExplicitWidth = 103
          ExplicitHeight = 25
        end
        object EsShapeAviable: TEsShape
          AlignWithMargins = True
          Left = 8
          Top = 3
          Width = 9
          Height = 44
          Margins.Left = 8
          Align = alLeft
          Brush.Color = clSkyBlue
          Brush.EndColor = 14079702
          Pen.Style = Clear
          Shape = Circle
          IsDrawHelper = True
          ExplicitHeight = 54
        end
        object EsShapeStar1: TEsShape
          AlignWithMargins = True
          Left = 156
          Top = 0
          Width = 32
          Height = 50
          Margins.Left = 8
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alRight
          Brush.EndColor = 14079702
          Padding.Left = 2
          Padding.Right = 2
          Shape = Star
          IsDrawHelper = True
          ExplicitLeft = 149
          ExplicitHeight = 70
        end
        object EsShapeStar2: TEsShape
          Left = 188
          Top = 0
          Width = 32
          Height = 50
          Align = alRight
          Brush.EndColor = 14079702
          Padding.Left = 2
          Padding.Right = 2
          Shape = Star
          IsDrawHelper = True
          ExplicitLeft = 168
          ExplicitHeight = 70
        end
        object EsShapeStar3: TEsShape
          Left = 220
          Top = 0
          Width = 32
          Height = 50
          Align = alRight
          Brush.EndColor = 14079702
          Padding.Left = 2
          Padding.Right = 2
          Shape = Star
          IsDrawHelper = True
          ExplicitLeft = 213
          ExplicitTop = -2
          ExplicitHeight = 70
        end
        object EsShapeStar4: TEsShape
          Left = 252
          Top = 0
          Width = 32
          Height = 50
          Align = alRight
          Brush.EndColor = 14079702
          Padding.Left = 2
          Padding.Right = 2
          Shape = Star
          IsDrawHelper = True
          ExplicitLeft = 226
          ExplicitHeight = 70
        end
        object EsShapeStar5: TEsShape
          AlignWithMargins = True
          Left = 284
          Top = 0
          Width = 32
          Height = 50
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alRight
          Brush.EndColor = 14079702
          Padding.Left = 2
          Padding.Right = 2
          Shape = Star
          IsDrawHelper = True
          ExplicitLeft = 258
          ExplicitHeight = 70
        end
      end
    end
  end
  object EsSwitchScale2X: TEsSwitch
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 608
    Height = 20
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 0
    TextOn = 'Scale 2X'
    TextOff = 'Scale 1X'
    ShowCaption = True
    Align = alTop
    TabOrder = 0
    OnClick = EsSwitchScale2XClick
    ExplicitWidth = 90
  end
  object TimerAnimate: TTimer
    Interval = 40
    OnTimer = TimerAnimateTimer
    Left = 504
    Top = 192
  end
end
