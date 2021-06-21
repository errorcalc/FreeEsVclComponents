object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'ActivityBar Sample'
  ClientHeight = 376
  ClientWidth = 647
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    647
    376)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 376
    Align = alClient
    BevelWidth = 3
    ShowCaption = False
    TabOrder = 1
    OnResize = Panel1Resize
    object EsLayoutMain: TEsLayout
      Left = 0
      Top = 11
      Width = 621
      Height = 342
      ParentBackground = False
      ParentBufferedChildren = False
      ParentDoubleBuffered = True
      TabOrder = 0
      object EsImageStaticText1: TEsImageStaticText
        Left = 3
        Top = 3
        Width = 201
        Height = 336
        ImageMargins.Left = 4
        ImageMargins.Top = 4
        ImageMargins.Right = 4
        ImageMargins.Bottom = 4
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000040000000400806000000AA6971
          DE000002604944415478DAE5DBCD4B54511806F0B9E9647EE4C768F73A4EA291
          A2A5A8244986284951349B76AE5ACC407BFD4B742FCC2C5CB9733392148A2216
          8A926296A468944EF7A6638EA995DAF55E398F827B7D619E17DEC53D77F39CDF
          399BB37835CF59653AED73FA8AEA54ACFFAA134EEFBB0B9AFA911D89446A83C1
          60BAAEEB07D2292FB22CCBF2C662B1C370383CEF7CEEBA0099CEE6EB42A1902D
          1DEE322B1A8D6A0EC29C0B10304DB33CD54FFE7CB937C1308C5517A0D4B66DBF
          742089D2342DEE0294D9F6912E1D460620CD520087A400E900382005F002E01F
          29C05500FC2505C800C01F52806B00D8BB211D460620EBA702D82505C806C06F
          52801C00EC90025C07409214201700DBA4007900D82A920E230350B0A10012A4
          003E006C92021402608314A008001629800E009314C000C08F42E9303200C59B
          0A204E0AE007C01A29400000DF49016E02E01B29402900BE9202940160D5271D
          4606A03CA1005648016E01609914E036009648012A00F08514A012008BA40055
          00F85C201D4606A07A4B012C9002DC05C04752801A00CC9302D402608E14A00E
          00B3A400F500F8902F1D4606A0E19702982105B8078069528046004C9102DC07
          C02429401300DE93023C00C0BB3CE9303200CDDB0A608214E02100C649015A00
          30460AD00A8051528036008C90023C02C070AE74181980F6A402784B0AF01800
          6F48019E00608814E029005E93023C03C02029C0F3A41A9B8B913E8682278FA1
          C0FA7A9FE1F7FBA80627E3F184B7A4E4A579323ADBDDFDEA4E67E70B2A809E9E
          016F5757EFA7D3E16907A1AAA3A3D593EA37C13DF9FEFE318FB3F9458F1A9E46
          518ECF1F035453285C0C8F91CB0000000049454E44AE426082}
        PaddingWithImage = True
        TabOrder = 0
        TextLayout = Top
        Caption = 'Main'
        DesignSize = (
          201
          336)
        object Label3: TLabel
          Left = 7
          Top = 251
          Width = 68
          Height = 13
          Caption = 'VerticalSpace:'
        end
        object Label4: TLabel
          Left = 7
          Top = 205
          Width = 35
          Height = 13
          Caption = 'Height:'
        end
        object Label7: TLabel
          Left = 7
          Top = 300
          Width = 28
          Height = 13
          Caption = 'Style:'
        end
        object rgDisplayMode: TRadioGroup
          Left = 7
          Top = 47
          Width = 187
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'DisplayMode'
          ItemIndex = 0
          Items.Strings = (
            'Overlay'
            'Docked')
          TabOrder = 0
          OnClick = rgDisplayModeClick
        end
        object rgPlacement: TRadioGroup
          Left = 7
          Top = 126
          Width = 187
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Placement'
          ItemIndex = 0
          Items.Strings = (
            'Top'
            'Bottom')
          TabOrder = 1
          OnClick = rgPlacementClick
        end
        object edVerticalSpace: TEdit
          Left = 7
          Top = 270
          Width = 170
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 2
          Text = '0'
        end
        object upVerticalSpace: TUpDown
          Left = 177
          Top = 270
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = edVerticalSpace
          Max = 10
          TabOrder = 3
          OnClick = upVerticalSpaceClick
        end
        object edHeight: TEdit
          Left = 7
          Top = 224
          Width = 170
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 4
          Text = '0'
        end
        object udHeight: TUpDown
          Left = 177
          Top = 224
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = edHeight
          TabOrder = 5
          OnClick = udHeightClick
        end
        object ckbActive: TCheckBox
          Left = 7
          Top = 24
          Width = 187
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Active'
          Checked = True
          State = cbChecked
          TabOrder = 6
          OnClick = ckbActiveClick
        end
        object cbStyle: TComboBox
          Left = 41
          Top = 297
          Width = 153
          Height = 21
          Style = csDropDownList
          TabOrder = 7
          OnChange = cbStyleChange
        end
      end
      object EsImageStaticText2: TEsImageStaticText
        Left = 210
        Top = 3
        Width = 201
        Height = 336
        ImageMargins.Left = 4
        ImageMargins.Top = 4
        ImageMargins.Right = 4
        ImageMargins.Bottom = 4
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000040000000400806000000AA6971
          DE000002604944415478DAE5DBCD4B54511806F0B9E9647EE4C768F73A4EA291
          A2A5A8244986284951349B76AE5ACC407BFD4B742FCC2C5CB9733392148A2216
          8A926296A468944EF7A6638EA995DAF55E398F827B7D619E17DEC53D77F39CDF
          399BB37835CF59653AED73FA8AEA54ACFFAA134EEFBB0B9AFA911D89446A83C1
          60BAAEEB07D2292FB22CCBF2C662B1C370383CEF7CEEBA0099CEE6EB42A1902D
          1DEE322B1A8D6A0EC29C0B10304DB33CD54FFE7CB937C1308C5517A0D4B66DBF
          742089D2342DEE0294D9F6912E1D460620CD520087A400E900382005F002E01F
          29C05500FC2505C800C01F52806B00D8BB211D460620EBA702D82505C806C06F
          52801C00EC90025C07409214201700DBA4007900D82A920E230350B0A10012A4
          003E006C92021402608314A008001629800E009314C000C08F42E9303200C59B
          0A204E0AE007C01A29400000DF49016E02E01B29402900BE9202940160D5271D
          4606A03CA1005648016E01609914E036009648012A00F08514A012008BA40055
          00F85C201D4606A07A4B012C9002DC05C04752801A00CC9302D402608E14A00E
          00B3A400F500F8902F1D4606A0E19702982105B8078069528046004C9102DC07
          C02429401300DE93023C00C0BB3CE9303200CDDB0A608214E02100C649015A00
          30460AD00A8051528036008C90023C02C070AE74181980F6A402784B0AF01800
          6F48019E00608814E029005E93023C03C02029C0F3A41A9B8B913E8682278FA1
          C0FA7A9FE1F7FBA80627E3F184B7A4E4A579323ADBDDFDEA4E67E70B2A809E9E
          016F5757EFA7D3E16907A1AAA3A3D593EA37C13DF9FEFE318FB3F9458F1A9E46
          518ECF1F035453285C0C8F91CB0000000049454E44AE426082}
        PaddingWithImage = True
        TabOrder = 1
        TextLayout = Top
        Caption = 'Visual'
        DesignSize = (
          201
          336)
        object Label1: TLabel
          Left = 7
          Top = 47
          Width = 29
          Height = 13
          Caption = 'Color:'
        end
        object Label2: TLabel
          Left = 7
          Top = 94
          Width = 53
          Height = 13
          Caption = 'PointColor:'
        end
        object Label5: TLabel
          Left = 7
          Top = 220
          Width = 57
          Height = 13
          Caption = 'PointSpace:'
        end
        object Label6: TLabel
          Left = 7
          Top = 270
          Width = 57
          Height = 13
          Caption = 'PointCount:'
        end
        object ckbParentBackground: TCheckBox
          Left = 7
          Top = 24
          Width = 187
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'ParentBackground'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = ckbParentBackgroundClick
        end
        object crbColor: TColorBox
          Left = 7
          Top = 66
          Width = 187
          Height = 22
          Selected = clWhite
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = crbColorChange
        end
        object crbPointColor: TColorBox
          Left = 7
          Top = 113
          Width = 187
          Height = 22
          Selected = clHighlight
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = crbPointColorChange
        end
        object rgPointType: TRadioGroup
          Left = 7
          Top = 141
          Width = 187
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'PointType'
          ItemIndex = 0
          Items.Strings = (
            'Box'
            'Circle')
          TabOrder = 3
          OnClick = rgPointTypeClick
        end
        object TrackBar1: TTrackBar
          Left = 7
          Top = 239
          Width = 187
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 50
          Min = 1
          Frequency = 5
          Position = 12
          ShowSelRange = False
          TabOrder = 4
          ThumbLength = 15
          OnChange = TrackBar1Change
        end
        object edPointCount: TEdit
          Left = 7
          Top = 289
          Width = 170
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 5
          Text = '5'
        end
        object udPointCount: TUpDown
          Left = 177
          Top = 289
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = edPointCount
          Min = 1
          Max = 30
          Position = 5
          TabOrder = 6
          OnClick = udPointCountClick
        end
      end
      object EsImageStaticText3: TEsImageStaticText
        Left = 417
        Top = 3
        Width = 201
        Height = 336
        ImageMargins.Left = 4
        ImageMargins.Top = 4
        ImageMargins.Right = 4
        ImageMargins.Bottom = 4
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000040000000400806000000AA6971
          DE000002604944415478DAE5DBCD4B54511806F0B9E9647EE4C768F73A4EA291
          A2A5A8244986284951349B76AE5ACC407BFD4B742FCC2C5CB9733392148A2216
          8A926296A468944EF7A6638EA995DAF55E398F827B7D619E17DEC53D77F39CDF
          399BB37835CF59653AED73FA8AEA54ACFFAA134EEFBB0B9AFA911D89446A83C1
          60BAAEEB07D2292FB22CCBF2C662B1C370383CEF7CEEBA0099CEE6EB42A1902D
          1DEE322B1A8D6A0EC29C0B10304DB33CD54FFE7CB937C1308C5517A0D4B66DBF
          742089D2342DEE0294D9F6912E1D460620CD520087A400E900382005F002E01F
          29C05500FC2505C800C01F52806B00D8BB211D460620EBA702D82505C806C06F
          52801C00EC90025C07409214201700DBA4007900D82A920E230350B0A10012A4
          003E006C92021402608314A008001629800E009314C000C08F42E9303200C59B
          0A204E0AE007C01A29400000DF49016E02E01B29402900BE9202940160D5271D
          4606A03CA1005648016E01609914E036009648012A00F08514A012008BA40055
          00F85C201D4606A07A4B012C9002DC05C04752801A00CC9302D402608E14A00E
          00B3A400F500F8902F1D4606A0E19702982105B8078069528046004C9102DC07
          C02429401300DE93023C00C0BB3CE9303200CDDB0A608214E02100C649015A00
          30460AD00A8051528036008C90023C02C070AE74181980F6A402784B0AF01800
          6F48019E00608814E029005E93023C03C02029C0F3A41A9B8B913E8682278FA1
          C0FA7A9FE1F7FBA80627E3F184B7A4E4A579323ADBDDFDEA4E67E70B2A809E9E
          016F5757EFA7D3E16907A1AAA3A3D593EA37C13DF9FEFE318FB3F9458F1A9E46
          518ECF1F035453285C0C8F91CB0000000049454E44AE426082}
        PaddingWithImage = True
        TabOrder = 2
        TextLayout = Top
        Caption = 'Animation'
        DesignSize = (
          201
          336)
        object lblAnimationTime: TLabel
          Left = 7
          Top = 129
          Width = 69
          Height = 13
          Caption = 'AnimationTime'
        end
        object lblAnimationDelay: TLabel
          Left = 7
          Top = 179
          Width = 74
          Height = 13
          Caption = 'AnimationDelay'
        end
        object lblAnimationEnergy: TLabel
          Left = 7
          Top = 229
          Width = 81
          Height = 13
          Caption = 'AnimationEnergy'
        end
        object lblTimerInterval: TLabel
          Left = 7
          Top = 279
          Width = 64
          Height = 13
          Caption = 'TimerInterval'
        end
        object rgAnimationType: TRadioGroup
          Left = 7
          Top = 24
          Width = 187
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'AnimationType'
          ItemIndex = 0
          Items.Strings = (
            'WindowsX'
            'Sin'
            'Bar'
            'Progress')
          TabOrder = 0
          OnClick = rgAnimationTypeClick
        end
        object tbProgress: TTrackBar
          Left = 85
          Top = 94
          Width = 106
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 100
          Frequency = 10
          ShowSelRange = False
          TabOrder = 1
          ThumbLength = 15
          Visible = False
          OnChange = tbProgressChange
        end
        object tbAnimationTime: TTrackBar
          Left = 7
          Top = 148
          Width = 187
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 10000
          Min = 500
          Frequency = 500
          Position = 500
          ShowSelRange = False
          TabOrder = 2
          ThumbLength = 15
          OnChange = tbAnimationTimeChange
        end
        object tbAnimationDelay: TTrackBar
          Left = 7
          Top = 198
          Width = 187
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 2000
          Frequency = 100
          ShowSelRange = False
          TabOrder = 3
          ThumbLength = 15
          OnChange = tbAnimationDelayChange
        end
        object tbAnimationEnergy: TTrackBar
          Left = 7
          Top = 248
          Width = 187
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 200
          Min = 1
          Frequency = 10
          Position = 1
          ShowSelRange = False
          TabOrder = 4
          ThumbLength = 15
          OnChange = tbAnimationEnergyChange
        end
        object tbTimerInterval: TTrackBar
          Left = 6
          Top = 298
          Width = 187
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 100
          Min = 1
          Frequency = 10
          Position = 1
          ShowSelRange = False
          TabOrder = 5
          ThumbLength = 15
          OnChange = tbTimerIntervalChange
        end
      end
    end
  end
  object EsActivityBar: TEsActivityBar
    Left = 0
    Top = 0
    Width = 647
    Height = 11
    Anchors = [akLeft, akTop, akRight]
    AutoHide = True
    Active = True
    Placement = Top
    Color = clWhite
    ParentColor = False
    TabOrder = 0
  end
end
