unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, ES.BaseControls,
  ES.Layouts, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.WinXCtrls, Vcl.NumberBox,
  ES.Switch, Vcl.Themes, Vcl.Buttons, Math;

type
  TFormMain = class(TForm)
    GridPanel: TGridPanel;
    PanelTransparentSplitter: TPanel;
    PanelSplitter: TPanel;
    PanelTransparentSplitterTitle: TEsPanel;
    PanelSplitterTitle: TEsPanel;
    PanelTransparentSplitterBody: TPanel;
    PanelSplitterBody: TPanel;
    PanelTransparentSplitterLeft: TPanel;
    PanelSplitterLeft: TPanel;
    TransparentSplitterLeft: TEsTransparentSplitter;
    SplitterLeft: TSplitter;
    TreeViewTransparentSplitter: TTreeView;
    TreeViewSplitter: TTreeView;
    PanelTransparentSplitterClient: TPanel;
    PanelSplitterClient: TPanel;
    ProgressBarTransparentSplitter: TProgressBar;
    ProgressBarSplitter: TProgressBar;
    LabelTransparentSplitterHintLeft: TLabel;
    LabelSplitterHintLeft: TLabel;
    GridPanelTransparentSplitter: TGridPanel;
    GridPanelSplitter: TGridPanel;
    ActivityIndicatorTransparentSplitter: TActivityIndicator;
    ActivityIndicatorSplitter: TActivityIndicator;
    EditTransparentSplitter: TEdit;
    EditSplitter: TEdit;
    Timer1: TTimer;
    LabelTransparentSplitter: TLabel;
    LabelSplitter: TLabel;
    Timer2: TTimer;
    ScrollBarTransparentSplitter: TScrollBar;
    ScrollBarSplitter: TScrollBar;
    Timer3: TTimer;
    TransparentSplitterBottom: TEsTransparentSplitter;
    SplitterBottom: TSplitter;
    PanelTransparentSplitterBottom: TPanel;
    PanelSplitterBottom: TPanel;
    TransparentSplitterTop: TEsTransparentSplitter;
    SplitterTop: TSplitter;
    PanelTransparentSplitterTop: TPanel;
    PanelSplitterTop: TPanel;
    PanelTransparentSplitterRight: TPanel;
    PanelSplitterRight: TPanel;
    TransparentSplitterRight: TEsTransparentSplitter;
    SplitterRight: TSplitter;
    PanelTransparentSplitterHint: TPanel;
    PanelSplitterHint: TPanel;
    LabelTransparentSplitterHintRight: TLabel;
    LabelSplitterHintRight: TLabel;
    PanelProps: TPanel;
    TrackBarSplitterOpacity: TTrackBar;
    LabelSplitterOpacity: TLabel;
    RadioGroupResizeStyle: TRadioGroup;
    ColorBoxSplitterColor: TColorBox;
    LabelScplitterColor: TLabel;
    SwitchTheme: TEsSwitch;
    ButtonSetColor1: TSpeedButton;
    ButtonSetColor2: TSpeedButton;
    ButtonSetColor4: TSpeedButton;
    ButtonSetColor3: TSpeedButton;
    LabelTheme: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewTransparentSplitterCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure TransparentSplitterLeftMoved(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure ColorBoxSplitterColorChange(Sender: TObject);
    procedure SwitchThemeClick(Sender: TObject);
    procedure ButtonSetColor1Click(Sender: TObject);
    procedure ButtonSetColor2Click(Sender: TObject);
    procedure ButtonSetColor4Click(Sender: TObject);
    procedure ButtonSetColor3Click(Sender: TObject);
    procedure RadioGroupResizeStyleClick(Sender: TObject);
    procedure TrackBarSplitterOpacityChange(Sender: TObject);
    procedure SplitterLeftMoved(Sender: TObject);
    procedure TransparentSplitterRightMoved(Sender: TObject);
    procedure SplitterRightMoved(Sender: TObject);
    procedure TransparentSplitterTopMoved(Sender: TObject);
    procedure SplitterTopMoved(Sender: TObject);
    procedure TransparentSplitterBottomMoved(Sender: TObject);
    procedure SplitterBottomMoved(Sender: TObject);
    procedure SplitterLeftCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure TransparentSplitterLeftCanResize(Sender: TObject;
      var NewSize: Integer; var Accept: Boolean);
  private
    StateLoad: Integer;
    procedure UpdateSplitters;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TreeViewTransparentSplitter.FullExpand();
  TreeViewTransparentSplitter.Items.GetFirstNode.Selected := True;
  TreeViewSplitter.FullExpand();
  TreeViewSplitter.Items.GetFirstNode.Selected := True;

  // defaults
  ColorBoxSplitterColor.Selected := clDefault;
  RadioGroupResizeStyle.ItemIndex := 3;
  TrackBarSplitterOpacity.Position := 180;
  UpdateSplitters();
end;

procedure TFormMain.UpdateSplitters();
begin
  // opacity
  TransparentSplitterLeft.SplitterOpacity := TrackBarSplitterOpacity.Position;
  TransparentSplitterTop.SplitterOpacity := TrackBarSplitterOpacity.Position;
  TransparentSplitterRight.SplitterOpacity := TrackBarSplitterOpacity.Position;
  TransparentSplitterBottom.SplitterOpacity := TrackBarSplitterOpacity.Position;
  // resize style
  TransparentSplitterLeft.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  TransparentSplitterTop.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  TransparentSplitterRight.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  TransparentSplitterBottom.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  SplitterLeft.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  SplitterTop.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  SplitterRight.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  SplitterBottom.ResizeStyle := TResizeStyle(RadioGroupResizeStyle.ItemIndex);
  // color
  TransparentSplitterLeft.SplitterColor := ColorBoxSplitterColor.Selected;
  TransparentSplitterTop.SplitterColor := ColorBoxSplitterColor.Selected;
  TransparentSplitterRight.SplitterColor := ColorBoxSplitterColor.Selected;
  TransparentSplitterBottom.SplitterColor := ColorBoxSplitterColor.Selected;
end;

procedure TFormMain.RadioGroupResizeStyleClick(Sender: TObject);
begin
  UpdateSplitters();
end;

procedure TFormMain.ColorBoxSplitterColorChange(Sender: TObject);
begin
  UpdateSplitters();
end;

procedure TFormMain.SwitchThemeClick(Sender: TObject);
begin
  if TEsSwitch(Sender).Checked then
    TStyleManager.TrySetStyle('Glow')
  else
    TStyleManager.TrySetStyle('Windows');
end;

procedure TFormMain.TrackBarSplitterOpacityChange(Sender: TObject);
begin
  UpdateSplitters();
end;

procedure TFormMain.TransparentSplitterBottomMoved(Sender: TObject);
begin
  PanelSplitterBottom.Height := PanelTransparentSplitterBottom.Height;
end;

procedure TFormMain.TransparentSplitterLeftCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  NewSize := Max(NewSize, 100);
end;

procedure TFormMain.TransparentSplitterLeftMoved(Sender: TObject);
begin
  PanelSplitterLeft.Width := PanelTransparentSplitterLeft.Width;
end;

procedure TFormMain.TransparentSplitterRightMoved(Sender: TObject);
begin
  PanelSplitterRight.Width := PanelTransparentSplitterRight.Width;
end;

procedure TFormMain.TransparentSplitterTopMoved(Sender: TObject);
begin
  PanelSplitterTop.Height := PanelTransparentSplitterTop.Height;
end;

procedure TFormMain.ButtonSetColor1Click(Sender: TObject);
begin
  ColorBoxSplitterColor.Selected := clDefault;
  UpdateSplitters();
end;

procedure TFormMain.ButtonSetColor2Click(Sender: TObject);
begin
  ColorBoxSplitterColor.Selected := clHighlight;
  UpdateSplitters();
end;

procedure TFormMain.ButtonSetColor4Click(Sender: TObject);
begin
  ColorBoxSplitterColor.Selected := clGray;
  UpdateSplitters();
end;

procedure TFormMain.ButtonSetColor3Click(Sender: TObject);
begin
  ColorBoxSplitterColor.Selected := clWebHotPink;
  UpdateSplitters();
end;

procedure TFormMain.SplitterBottomMoved(Sender: TObject);
begin
  PanelTransparentSplitterBottom.Height := PanelSplitterBottom.Height;
end;

procedure TFormMain.SplitterLeftCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  NewSize := Max(NewSize, 100);
end;

procedure TFormMain.SplitterLeftMoved(Sender: TObject);
begin
  PanelTransparentSplitterLeft.Width := PanelSplitterLeft.Width;
end;

procedure TFormMain.SplitterRightMoved(Sender: TObject);
begin
  PanelTransparentSplitterRight.Width := PanelSplitterRight.Width;
end;

procedure TFormMain.SplitterTopMoved(Sender: TObject);
begin
  PanelTransparentSplitterTop.Height := PanelSplitterTop.Height;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  EditTransparentSplitter.Text := 'Random: ' + Random.ToString(TFormatSettings.Invariant);
  EditSplitter.Text := EditTransparentSplitter.Text;
end;

procedure TFormMain.Timer2Timer(Sender: TObject);
const
  ModeStr: array [0..3] of string = ('Loading', 'Loading.', 'Loading..', 'Loading...');
begin
  LabelTransparentSplitter.Caption := 'Date && Time: ' + DateTimeToStr(Now, TFormatSettings.Invariant);
  LabelSplitter.Caption := LabelTransparentSplitter.Caption;

  PanelTransparentSplitterRight.Caption := ModeStr[StateLoad];
  PanelSplitterRight.Caption := ModeStr[StateLoad];
  StateLoad := (StateLoad + 1) mod 4;

end;

procedure TFormMain.Timer3Timer(Sender: TObject);
const
  ModeStr: array [0..1] of string = (' (Release)', ' (Debug)');
begin
  TreeViewTransparentSplitter.Items[0][0].Text := TreeViewTransparentSplitter.Items[0][0].Text.Split([' '])[0] + ModeStr[Random(2)];
  TreeViewSplitter.Items[0][0].Text := TreeViewTransparentSplitter.Items[0][0].Text;
  TreeViewTransparentSplitter.Items[0][1].Text := TreeViewTransparentSplitter.Items[0][1].Text.Split([' '])[0] + ModeStr[Random(2)];
  TreeViewSplitter.Items[0][1].Text := TreeViewTransparentSplitter.Items[0][1].Text;
  TreeViewTransparentSplitter.Items[0][2].Text := TreeViewTransparentSplitter.Items[0][2].Text.Split([' '])[0] + ModeStr[Random(2)];
  TreeViewSplitter.Items[0][2].Text := TreeViewTransparentSplitter.Items[0][2].Text;
  TreeViewTransparentSplitter.Items[0][3].Text := TreeViewTransparentSplitter.Items[0][3].Text.Split([' '])[0] + ModeStr[Random(2)];
  TreeViewSplitter.Items[0][3].Text := TreeViewTransparentSplitter.Items[0][3].Text;
  TreeViewTransparentSplitter.Items[0][4].Text := TreeViewTransparentSplitter.Items[0][4].Text.Split([' '])[0] + ModeStr[Random(2)];
  TreeViewSplitter.Items[0][4].Text := TreeViewTransparentSplitter.Items[0][4].Text;

  ScrollBarTransparentSplitter.Position := Random(100);
  ScrollBarSplitter.Position := ScrollBarTransparentSplitter.Position;
end;

procedure TFormMain.TreeViewTransparentSplitterCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := False;
end;

end.
