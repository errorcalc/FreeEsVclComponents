object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'TEsTransparentSplitter'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  TextHeight = 15
  object GridPanel: TGridPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 441
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.00000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = PanelTransparentSplitter
        Row = 0
      end
      item
        Column = 0
        Control = PanelSplitter
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.00000000000000000
      end
      item
        Value = 50.00000000000000000
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 0
    object PanelTransparentSplitter: TPanel
      Left = 0
      Top = 0
      Width = 464
      Height = 220
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 0
      object PanelTransparentSplitterTitle: TEsPanel
        Left = 4
        Top = 4
        Width = 456
        Height = 23
        Align = alTop
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        Caption = 'TEsTransparentSplitter'
      end
      object PanelTransparentSplitterBody: TPanel
        Left = 4
        Top = 27
        Width = 456
        Height = 189
        Margins.Left = 0
        Margins.Top = 4
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 1
        object TransparentSplitterLeft: TEsTransparentSplitter
          Left = 189
          Top = 4
          Width = 10
          Height = 177
          AutoSnap = False
          OnCanResize = TransparentSplitterLeftCanResize
          OnMoved = TransparentSplitterLeftMoved
        end
        object TransparentSplitterRight: TEsTransparentSplitter
          Left = 385
          Top = 4
          Width = 10
          Height = 177
          Align = alRight
          AutoSnap = False
          OnMoved = TransparentSplitterRightMoved
          ExplicitLeft = 565
          ExplicitTop = -12
        end
        object PanelTransparentSplitterLeft: TPanel
          Left = 4
          Top = 4
          Width = 185
          Height = 177
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object TransparentSplitterBottom: TEsTransparentSplitter
            Left = 0
            Top = 146
            Width = 185
            Height = 10
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            MinSize = 10
            OnMoved = TransparentSplitterBottomMoved
            ExplicitTop = 145
          end
          object TransparentSplitterTop: TEsTransparentSplitter
            Left = 0
            Top = 21
            Width = 185
            Height = 10
            Cursor = crVSplit
            Align = alTop
            AutoSnap = False
            MinSize = 10
            OnMoved = TransparentSplitterTopMoved
          end
          object TreeViewTransparentSplitter: TTreeView
            Left = 0
            Top = 31
            Width = 185
            Height = 115
            Align = alClient
            Indent = 17
            ReadOnly = True
            TabOrder = 0
            TabStop = False
            OnCollapsing = TreeViewTransparentSplitterCollapsing
            Items.NodeData = {
              080300000009540054007200650065004E006F00640065003500000000000000
              00000000FFFFFFFFFFFFFFFF0000000000000000000000000005000000010950
              006C006100740066006F0072006D0073000000310000000000000000000000FF
              FFFFFFFFFFFFFF00000000000000000000000000000000000107570069006E00
              64006F007700730000002D0000000000000000000000FFFFFFFFFFFFFFFF0000
              00000000000000000000000000000001054C0069006E007500780000002D0000
              000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000000000
              000001054D00610063004F0053000000290000000000000000000000FFFFFFFF
              FFFFFFFF0000000000000000000000000000000000010369004F005300000031
              0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000
              00000000010741006E00640072006F0069006400000037000000000000000000
              0000FFFFFFFFFFFFFFFF0000000000000000000000000002000000010A460072
              0061006D00650077006F0072006B0073000000290000000000000000000000FF
              FFFFFFFFFFFFFF00000000000000000000000000000000000103560043004C00
              0000290000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000
              00000000000000010346004D0058000000290000000000000000000000FFFFFF
              FFFFFFFFFF000000000000000000000000000300000001034300500055000000
              290000000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000
              000000000001035800380036000000290000000000000000000000FFFFFFFFFF
              FFFFFF0000000000000000000000000000000000010358003600340000002900
              00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000000
              0000000103410052004D00}
          end
          object PanelTransparentSplitterBottom: TPanel
            Left = 0
            Top = 156
            Width = 185
            Height = 21
            Align = alBottom
            BevelKind = bkTile
            BevelOuter = bvNone
            Caption = #55358#56433' Try move'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 1
            VerticalAlignment = taAlignTop
          end
          object PanelTransparentSplitterTop: TPanel
            Left = 0
            Top = 0
            Width = 185
            Height = 21
            Align = alTop
            BevelKind = bkTile
            BevelOuter = bvNone
            Caption = #55358#56435' Try move'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 2
            VerticalAlignment = taAlignTop
          end
        end
        object PanelTransparentSplitterClient: TPanel
          Left = 199
          Top = 4
          Width = 186
          Height = 177
          Align = alClient
          BevelKind = bkTile
          BevelOuter = bvNone
          BorderWidth = 4
          Color = clWindow
          ParentBackground = False
          TabOrder = 1
          object LabelTransparentSplitter: TLabel
            Left = 4
            Top = 132
            Width = 174
            Height = 15
            Align = alTop
            Caption = '0000000'
            ExplicitWidth = 42
          end
          object ProgressBarTransparentSplitter: TProgressBar
            Left = 4
            Top = 29
            Width = 174
            Height = 27
            Align = alTop
            Style = pbstMarquee
            TabOrder = 0
          end
          object GridPanelTransparentSplitter: TGridPanel
            Left = 4
            Top = 56
            Width = 174
            Height = 59
            Align = alTop
            BevelOuter = bvNone
            ColumnCollection = <
              item
                Value = 50.00000000000000000
              end
              item
                Value = 50.00000000000000000
              end>
            ControlCollection = <
              item
                Column = 0
                Control = ActivityIndicatorTransparentSplitter
                Row = 0
                RowSpan = 2
              end>
            ParentColor = True
            RowCollection = <
              item
                Value = 50.00000000000000000
              end
              item
                Value = 50.00000000000000000
              end>
            TabOrder = 1
            DesignSize = (
              174
              59)
            object ActivityIndicatorTransparentSplitter: TActivityIndicator
              Left = 19
              Top = 5
              Anchors = []
              Animate = True
              IndicatorSize = aisLarge
            end
          end
          object EditTransparentSplitter: TEdit
            Left = 4
            Top = 147
            Width = 174
            Height = 23
            TabStop = False
            Align = alTop
            TabOrder = 2
          end
          object ScrollBarTransparentSplitter: TScrollBar
            Left = 4
            Top = 115
            Width = 174
            Height = 17
            Align = alTop
            PageSize = 0
            TabOrder = 3
            TabStop = False
          end
          object PanelTransparentSplitterHint: TPanel
            Left = 4
            Top = 4
            Width = 174
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 4
            object LabelTransparentSplitterHintLeft: TLabel
              AlignWithMargins = True
              Left = 0
              Top = 4
              Width = 63
              Height = 17
              Margins.Left = 0
              Margins.Top = 4
              Margins.Right = 0
              Margins.Bottom = 4
              Align = alLeft
              Caption = #55358#56432' Try move'
              ExplicitHeight = 15
            end
            object LabelTransparentSplitterHintRight: TLabel
              AlignWithMargins = True
              Left = 111
              Top = 4
              Width = 63
              Height = 17
              Margins.Left = 0
              Margins.Top = 4
              Margins.Right = 0
              Margins.Bottom = 4
              Align = alRight
              Caption = 'Try move '#55358#56434
              ExplicitHeight = 15
            end
          end
        end
        object PanelTransparentSplitterRight: TPanel
          Left = 395
          Top = 4
          Width = 53
          Height = 177
          Align = alRight
          BevelKind = bkTile
          BevelOuter = bvNone
          Caption = '0000000'
          Color = clWindow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = [fsItalic]
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object PanelSplitter: TPanel
      Left = 0
      Top = 220
      Width = 464
      Height = 221
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 1
      object PanelSplitterTitle: TEsPanel
        Left = 4
        Top = 4
        Width = 456
        Height = 23
        Align = alTop
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        Caption = 'TSplitter'
      end
      object PanelSplitterBody: TPanel
        Left = 4
        Top = 27
        Width = 456
        Height = 190
        Margins.Left = 0
        Margins.Top = 4
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 1
        object SplitterLeft: TSplitter
          Left = 189
          Top = 4
          Width = 10
          Height = 178
          AutoSnap = False
          OnCanResize = SplitterLeftCanResize
          OnMoved = SplitterLeftMoved
          ExplicitHeight = 177
        end
        object SplitterRight: TSplitter
          Left = 385
          Top = 4
          Width = 10
          Height = 178
          Align = alRight
          AutoSnap = False
          OnMoved = SplitterRightMoved
          ExplicitLeft = 565
          ExplicitTop = -12
          ExplicitHeight = 177
        end
        object PanelSplitterLeft: TPanel
          Left = 4
          Top = 4
          Width = 185
          Height = 178
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object SplitterBottom: TSplitter
            Left = 0
            Top = 147
            Width = 185
            Height = 10
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            MinSize = 10
            OnMoved = SplitterBottomMoved
            ExplicitTop = 145
          end
          object SplitterTop: TSplitter
            Left = 0
            Top = 21
            Width = 185
            Height = 10
            Cursor = crVSplit
            Align = alTop
            AutoSnap = False
            MinSize = 10
            OnMoved = SplitterTopMoved
          end
          object TreeViewSplitter: TTreeView
            Left = 0
            Top = 31
            Width = 185
            Height = 116
            Align = alClient
            Indent = 17
            ReadOnly = True
            TabOrder = 0
            TabStop = False
            OnCollapsing = TreeViewTransparentSplitterCollapsing
            Items.NodeData = {
              080300000009540054007200650065004E006F00640065003500000000000000
              00000000FFFFFFFFFFFFFFFF0000000000000000000000000005000000010950
              006C006100740066006F0072006D0073000000310000000000000000000000FF
              FFFFFFFFFFFFFF00000000000000000000000000000000000107570069006E00
              64006F007700730000002D0000000000000000000000FFFFFFFFFFFFFFFF0000
              00000000000000000000000000000001054C0069006E007500780000002D0000
              000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000000000
              000001054D00610063004F0053000000290000000000000000000000FFFFFFFF
              FFFFFFFF0000000000000000000000000000000000010369004F005300000031
              0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000
              00000000010741006E00640072006F0069006400000037000000000000000000
              0000FFFFFFFFFFFFFFFF0000000000000000000000000002000000010A460072
              0061006D00650077006F0072006B0073000000290000000000000000000000FF
              FFFFFFFFFFFFFF00000000000000000000000000000000000103560043004C00
              0000290000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000
              00000000000000010346004D0058000000290000000000000000000000FFFFFF
              FFFFFFFFFF000000000000000000000000000300000001034300500055000000
              290000000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000
              000000000001035800380036000000290000000000000000000000FFFFFFFFFF
              FFFFFF0000000000000000000000000000000000010358003600340000002900
              00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000000
              0000000103410052004D00}
          end
          object PanelSplitterBottom: TPanel
            Left = 0
            Top = 157
            Width = 185
            Height = 21
            Align = alBottom
            BevelKind = bkTile
            BevelOuter = bvNone
            Caption = #55358#56433' Try move'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 1
            VerticalAlignment = taAlignTop
          end
          object PanelSplitterTop: TPanel
            Left = 0
            Top = 0
            Width = 185
            Height = 21
            Align = alTop
            BevelKind = bkTile
            BevelOuter = bvNone
            Caption = #55358#56435' Try move'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 2
            VerticalAlignment = taAlignTop
          end
        end
        object PanelSplitterClient: TPanel
          Left = 199
          Top = 4
          Width = 186
          Height = 178
          Align = alClient
          BevelKind = bkTile
          BevelOuter = bvNone
          BorderWidth = 4
          Color = clWindow
          ParentBackground = False
          TabOrder = 1
          object LabelSplitter: TLabel
            Left = 4
            Top = 132
            Width = 174
            Height = 15
            Align = alTop
            Caption = '0000000'
            ExplicitWidth = 42
          end
          object ProgressBarSplitter: TProgressBar
            Left = 4
            Top = 29
            Width = 174
            Height = 27
            Align = alTop
            Style = pbstMarquee
            TabOrder = 0
          end
          object GridPanelSplitter: TGridPanel
            Left = 4
            Top = 56
            Width = 174
            Height = 59
            Align = alTop
            BevelOuter = bvNone
            ColumnCollection = <
              item
                Value = 50.00000000000000000
              end
              item
                Value = 50.00000000000000000
              end>
            ControlCollection = <
              item
                Column = 0
                Control = ActivityIndicatorSplitter
                Row = 0
                RowSpan = 2
              end>
            ParentColor = True
            RowCollection = <
              item
                Value = 50.00000000000000000
              end
              item
                Value = 50.00000000000000000
              end>
            TabOrder = 1
            DesignSize = (
              174
              59)
            object ActivityIndicatorSplitter: TActivityIndicator
              Left = 19
              Top = 5
              Anchors = []
              Animate = True
              IndicatorSize = aisLarge
            end
          end
          object EditSplitter: TEdit
            Left = 4
            Top = 147
            Width = 174
            Height = 23
            TabStop = False
            Align = alTop
            TabOrder = 2
          end
          object ScrollBarSplitter: TScrollBar
            Left = 4
            Top = 115
            Width = 174
            Height = 17
            Align = alTop
            PageSize = 0
            TabOrder = 3
            TabStop = False
          end
          object PanelSplitterHint: TPanel
            Left = 4
            Top = 4
            Width = 174
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 4
            object LabelSplitterHintLeft: TLabel
              AlignWithMargins = True
              Left = 0
              Top = 4
              Width = 63
              Height = 17
              Margins.Left = 0
              Margins.Top = 4
              Margins.Right = 0
              Margins.Bottom = 4
              Align = alLeft
              Caption = #55358#56432' Try move'
              ExplicitHeight = 15
            end
            object LabelSplitterHintRight: TLabel
              AlignWithMargins = True
              Left = 111
              Top = 4
              Width = 63
              Height = 17
              Margins.Left = 0
              Margins.Top = 4
              Margins.Right = 0
              Margins.Bottom = 4
              Align = alRight
              Caption = 'Try move '#55358#56434
              ExplicitHeight = 15
            end
          end
        end
        object PanelSplitterRight: TPanel
          Left = 395
          Top = 4
          Width = 53
          Height = 178
          Align = alRight
          BevelKind = bkTile
          BevelOuter = bvNone
          Caption = '0000000'
          Color = clWindow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = [fsItalic]
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
        end
      end
    end
  end
  object PanelProps: TPanel
    Left = 464
    Top = 0
    Width = 160
    Height = 441
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object LabelSplitterOpacity: TLabel
      Left = 6
      Top = 8
      Width = 81
      Height = 15
      Caption = 'SplitterOpacity:'
    end
    object LabelScplitterColor: TLabel
      Left = 6
      Top = 173
      Width = 69
      Height = 15
      Caption = 'SplitterColor:'
    end
    object ButtonSetColor1: TSpeedButton
      Left = 16
      Top = 222
      Width = 137
      Height = 22
      Caption = 'Set clDefault (Auto)'
      Flat = True
      OnClick = ButtonSetColor1Click
    end
    object ButtonSetColor2: TSpeedButton
      Left = 16
      Top = 250
      Width = 137
      Height = 22
      Caption = 'Set clHighlight'
      Flat = True
      OnClick = ButtonSetColor2Click
    end
    object ButtonSetColor4: TSpeedButton
      Left = 16
      Top = 306
      Width = 137
      Height = 22
      Caption = 'Set clGray'
      Flat = True
      OnClick = ButtonSetColor4Click
    end
    object ButtonSetColor3: TSpeedButton
      Left = 16
      Top = 278
      Width = 137
      Height = 22
      Caption = 'Set clHotPink'
      Flat = True
      OnClick = ButtonSetColor3Click
    end
    object LabelTheme: TLabel
      Left = 6
      Top = 334
      Width = 39
      Height = 15
      Caption = 'Theme:'
    end
    object TrackBarSplitterOpacity: TTrackBar
      Left = 6
      Top = 29
      Width = 147
      Height = 33
      Max = 255
      PageSize = 4
      Frequency = 16
      ShowSelRange = False
      TabOrder = 0
      OnChange = TrackBarSplitterOpacityChange
    end
    object RadioGroupResizeStyle: TRadioGroup
      Left = 6
      Top = 68
      Width = 147
      Height = 99
      Caption = 'ResizeStyle'
      Items.Strings = (
        'rsNone'
        'rsLine'
        'rsUpdate'
        'rsPattern')
      TabOrder = 1
      OnClick = RadioGroupResizeStyleClick
    end
    object ColorBoxSplitterColor: TColorBox
      Left = 6
      Top = 194
      Width = 147
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor]
      TabOrder = 2
      OnChange = ColorBoxSplitterColorChange
    end
    object SwitchTheme: TEsSwitch
      Left = 6
      Top = 355
      Width = 44
      Height = 20
      TabOrder = 3
      OnClick = SwitchThemeClick
    end
  end
  object Timer1: TTimer
    Interval = 950
    OnTimer = Timer1Timer
    Left = 464
    Top = 384
  end
  object Timer2: TTimer
    Interval = 800
    OnTimer = Timer2Timer
    Left = 512
    Top = 384
  end
  object Timer3: TTimer
    Interval = 200
    OnTimer = Timer3Timer
    Left = 560
    Top = 384
  end
end
