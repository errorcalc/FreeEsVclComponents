unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, ES.Indicators,
  ES.BaseControls, ES.Layouts, ES.NinePatch;

type
  TMainForm = class(TForm)
    EsActivityBar: TEsActivityBar;
    Panel1: TPanel;
    EsLayoutMain: TEsLayout;
    EsImageStaticText1: TEsImageStaticText;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    rgDisplayMode: TRadioGroup;
    rgPlacement: TRadioGroup;
    edVerticalSpace: TEdit;
    upVerticalSpace: TUpDown;
    edHeight: TEdit;
    udHeight: TUpDown;
    ckbActive: TCheckBox;
    cbStyle: TComboBox;
    EsImageStaticText2: TEsImageStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ckbParentBackground: TCheckBox;
    crbColor: TColorBox;
    crbPointColor: TColorBox;
    rgPointType: TRadioGroup;
    TrackBar1: TTrackBar;
    edPointCount: TEdit;
    udPointCount: TUpDown;
    EsImageStaticText3: TEsImageStaticText;
    lblAnimationTime: TLabel;
    lblAnimationDelay: TLabel;
    lblAnimationEnergy: TLabel;
    lblTimerInterval: TLabel;
    rgAnimationType: TRadioGroup;
    tbProgress: TTrackBar;
    tbAnimationTime: TTrackBar;
    tbAnimationDelay: TTrackBar;
    tbAnimationEnergy: TTrackBar;
    tbTimerInterval: TTrackBar;
    procedure rgDisplayModeClick(Sender: TObject);
    procedure rgPlacementClick(Sender: TObject);
    procedure rgAnimationTypeClick(Sender: TObject);
    procedure tbProgressChange(Sender: TObject);
    procedure ckbParentBackgroundClick(Sender: TObject);
    procedure crbColorChange(Sender: TObject);
    procedure crbPointColorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure udHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure upVerticalSpaceClick(Sender: TObject; Button: TUDBtnType);
    procedure rgPointTypeClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure udPointCountClick(Sender: TObject; Button: TUDBtnType);
    procedure ckbActiveClick(Sender: TObject);
    procedure tbAnimationTimeChange(Sender: TObject);
    procedure tbAnimationDelayChange(Sender: TObject);
    procedure tbAnimationEnergyChange(Sender: TObject);
    procedure tbTimerIntervalChange(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure cbStyleChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Themes;

procedure TMainForm.cbStyleChange(Sender: TObject);
begin
  TStyleManager.SetStyle(TComboBox(Sender).Text);
end;

procedure TMainForm.ckbActiveClick(Sender: TObject);
begin
  EsActivityBar.Active := TCheckBox(Sender).Checked;
end;

procedure TMainForm.ckbParentBackgroundClick(Sender: TObject);
begin
  EsActivityBar.ParentBackground := TCheckBox(Sender).Checked;
end;

procedure TMainForm.crbColorChange(Sender: TObject);
begin
  EsActivityBar.Color := TColorBox(Sender).Selected;
end;

procedure TMainForm.crbPointColorChange(Sender: TObject);
begin
  EsActivityBar.PointColor := TColorBox(Sender).Selected;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Style: string;
begin
  for Style in TStyleManager.StyleNames do
    cbStyle.Items.Add(Style);

  cbStyle.ItemIndex := cbStyle.Items.IndexOf(TStyleManager.ActiveStyle.Name);

  udHeight.Position := EsActivityBar.Height;
  tbAnimationTime.Position := EsActivityBar.AnimationTime;
  tbAnimationDelay.Position := EsActivityBar.AnimationDelay;
  tbAnimationEnergy.Position := EsActivityBar.AnimationEnergy;
  tbTimerInterval.Position := EsActivityBar.TimerInterval;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  EsLayoutMain.Left := TPanel(Sender).ClientWidth div 2 - EsLayoutMain.Width div 2;
  EsLayoutMain.Top := TPanel(Sender).ClientHeight div 2 - EsLayoutMain.Height div 2;
end;

procedure TMainForm.rgAnimationTypeClick(Sender: TObject);
begin
  tbProgress.Visible := False;
  case TRadioGroup(Sender).ItemIndex of
    0: EsActivityBar.AnimationType := TActivityAnimationType.WindowsX;
    1: EsActivityBar.AnimationType := TActivityAnimationType.Sin;
    2: EsActivityBar.AnimationType := TActivityAnimationType.Bar;
    3:
      begin
        EsActivityBar.AnimationType := TActivityAnimationType.Progress;
        tbProgress.Visible := True;
      end;
  end;
end;

procedure TMainForm.rgDisplayModeClick(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: EsActivityBar.DisplayMode := TActivityDisplayMode.Overlay;
    1: EsActivityBar.DisplayMode := TActivityDisplayMode.Docked;
  end;
end;

procedure TMainForm.rgPlacementClick(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: EsActivityBar.Placement := TActivityPlacement.Top;
    1: EsActivityBar.Placement := TActivityPlacement.Bottom;
  end;
end;

procedure TMainForm.rgPointTypeClick(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: EsActivityBar.PointType := TActivityPointType.Box;
    1: EsActivityBar.PointType := TActivityPointType.Circle;
  end;
end;

procedure TMainForm.tbAnimationDelayChange(Sender: TObject);
begin
  lblAnimationDelay.Caption := 'AnimationDelay: ' + IntToStr(TTrackBar(Sender).Position);
  EsActivityBar.AnimationDelay := TTrackBar(Sender).Position;
end;

procedure TMainForm.tbAnimationEnergyChange(Sender: TObject);
begin
  lblAnimationEnergy.Caption := 'AnimationEnergy: ' + IntToStr(TTrackBar(Sender).Position);
  EsActivityBar.AnimationEnergy := TTrackBar(Sender).Position;
end;

procedure TMainForm.tbAnimationTimeChange(Sender: TObject);
begin
  lblAnimationTime.Caption := 'AnimationTime: ' + IntToStr(TTrackBar(Sender).Position);
  EsActivityBar.AnimationTime := TTrackBar(Sender).Position;
end;

procedure TMainForm.tbProgressChange(Sender: TObject);
begin
  EsActivityBar.Position := TTrackBar(Sender).Position;
end;

procedure TMainForm.tbTimerIntervalChange(Sender: TObject);
begin
  lblTimerInterval.Caption := 'TimerInterval: ' + IntToStr(TTrackBar(Sender).Position);
  EsActivityBar.TimerInterval := TTrackBar(Sender).Position;
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  EsActivityBar.PointSpace := TTrackBar(Sender).Position;
end;

procedure TMainForm.udHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  EsActivityBar.Height := TUpDown(Sender).Position;
end;

procedure TMainForm.udPointCountClick(Sender: TObject; Button: TUDBtnType);
begin
  EsActivityBar.PointCount := TUpDown(Sender).Position;
end;

procedure TMainForm.upVerticalSpaceClick(Sender: TObject; Button: TUDBtnType);
begin
  EsActivityBar.VerticalSpace := TUpDown(Sender).Position;
end;

end.
