unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ES.BaseControls,
  ES.Layouts, Vcl.ExtCtrls, Vcl.ComCtrls, ES.Switch, Vcl.Themes;

type
  TFormMain = class(TForm)
    EsPanel1: TEsPanel;
    Label1: TLabel;
    EsPanel2: TEsPanel;
    Label2: TLabel;
    EsPanel3: TEsPanel;
    Label3: TLabel;
    EsPanel4: TEsPanel;
    Label4: TLabel;
    EsPanel5: TEsPanel;
    Label5: TLabel;
    EsPanel6: TEsPanel;
    Label6: TLabel;
    EsPanelDown: TEsPanel;
    Label7: TLabel;
    EsPanelUp: TEsPanel;
    Label8: TLabel;
    EsPanelFlat: TEsPanel;
    Label9: TLabel;
    TrackBarFrameWidth: TTrackBar;
    Label10: TLabel;
    Label11: TLabel;
    ColorBoxFrameColor: TColorBox;
    EsSwitch1: TEsSwitch;
    procedure TrackBarFrameWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColorBoxFrameColorChange(Sender: TObject);
    procedure EsSwitch1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ColorBoxFrameColorChange(Sender: TObject);
begin
  EsPanelDown.FrameColor := TColorBox(Sender).Selected;
  EsPanelUp.FrameColor := TColorBox(Sender).Selected;
  EsPanelFlat.FrameColor := TColorBox(Sender).Selected;
end;

procedure TFormMain.EsSwitch1Click(Sender: TObject);
begin
  if TEsSwitch(Sender).Checked then
    TStyleManager.SetStyle('Windows10 SlateGray')
  else
    TStyleManager.SetStyle('Windows');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ColorBoxFrameColor.Selected := clBtnShadow;
  TrackBarFrameWidth.Position := 2;
end;

procedure TFormMain.TrackBarFrameWidthChange(Sender: TObject);
begin
  EsPanelDown.FrameWidth := TTrackBar(Sender).Position;
  EsPanelUp.FrameWidth := TTrackBar(Sender).Position;
  EsPanelFlat.FrameWidth := TTrackBar(Sender).Position;
end;

end.
