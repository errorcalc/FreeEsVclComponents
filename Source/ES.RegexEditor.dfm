object EsRegexEditorForm: TEsRegexEditorForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'RegEx Editor'
  ClientHeight = 141
  ClientWidth = 409
  Color = clBtnFace
  Constraints.MinHeight = 180
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    409
    141)
  TextHeight = 13
  object PatternEdit: TLabeledEdit
    Left = 8
    Top = 24
    Width = 322
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 40
    EditLabel.Height = 13
    EditLabel.Caption = 'Pattern:'
    TabOrder = 0
    Text = ''
    OnChange = PatternEditChange
    ExplicitWidth = 316
  end
  object TestEdit: TEsRegexLabeledEdit
    Left = 8
    Top = 68
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = 'Test:'
    TabOrder = 2
    Text = ''
    AllowNeutral = False
    IndicateState = All
    ExplicitWidth = 387
  end
  object PatternsButton: TButton
    Left = 336
    Top = 22
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Patterns...'
    TabOrder = 1
    OnClick = PatternsButtonClick
    ExplicitLeft = 330
  end
  object FotterLayout: TEsLayout
    AlignWithMargins = True
    Left = 8
    Top = 108
    Width = 393
    Height = 25
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    ParentBackground = False
    TabOrder = 3
    ExplicitTop = 89
    ExplicitWidth = 387
    object CancelButton: TButton
      Left = 318
      Top = 0
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelButtonClick
      ExplicitLeft = 312
    end
    object OkButton: TButton
      Left = 243
      Top = 0
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      TabOrder = 0
      OnClick = OkButtonClick
      ExplicitLeft = 237
    end
    object LinkLabelEs: TLinkLabel
      Left = 0
      Top = 6
      Width = 110
      Height = 29
      Caption = '<a href="http://errorsoft.org">errorsoft.org</a>'
      TabOrder = 2
      UseVisualStyle = True
      OnLinkClick = LinkLabelEsLinkClick
    end
  end
  object PatternsPopup: TPopupMenu
    Left = 112
    Top = 8
  end
end
