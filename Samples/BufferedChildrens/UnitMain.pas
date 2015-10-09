unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ES.Vcl.BaseControls, ES.Vcl.Layouts, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.Imaging.pngimage;

type
  TForm2 = class(TForm)
    EsLayout: TEsLayout;
    CheckBoxBufferedChildrens: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TimerRepaint: TTimer;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    CheckBoxUseTheme: TCheckBox;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    Image1: TImage;
    SpeedButton15: TSpeedButton;
    Bevel1: TBevel;
    procedure EsLayoutPaint(Sender: TObject; Canvas: TCanvas; Rect: TRect);
    procedure CheckBoxBufferedChildrensClick(Sender: TObject);
    procedure TimerRepaintTimer(Sender: TObject);
    procedure CheckBoxUseThemeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  Styles, Themes;

procedure TForm2.CheckBoxBufferedChildrensClick(Sender: TObject);
begin
  EsLayout.Invalidate;
  EsLayout.BufferedChildrens := TCheckBox(Sender).Checked;
end;

procedure TForm2.CheckBoxUseThemeClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    TStyleManager.TrySetStyle('Windows10')
  else
    TStyleManager.TrySetStyle('Windows');
end;

procedure TForm2.EsLayoutPaint(Sender: TObject; Canvas: TCanvas; Rect: TRect);
begin
  Canvas.Pen.Width := 1;
  if TEsLayout(EsLayout).BufferedChildrens then
  begin
    Canvas.Pen.Color := RGB(0, 230, 0);
    Canvas.Brush.Color := RGB(240, 255, 240);
  end
  else
  begin
    Canvas.Pen.Color := RGB(230, 0, 0);
    Canvas.Brush.Color := RGB(255, 240, 240);
  end;
  Canvas.Rectangle(Rect);
  Canvas.Font.Color := clBlue;
  Canvas.TextOut(500, 2, 'TEsCustomControl <- TEsLayot');
end;

procedure TForm2.TimerRepaintTimer(Sender: TObject);
begin
  Label1.Caption := 'Label' + Random(100000).toString;
  Label2.Caption := 'Label' + Random(100000).toString;
  Label3.Caption := 'Label' + Random(100000).toString;
  Label4.Caption := 'Label' + Random(100000).toString;
  Label5.Caption := 'Label' + Random(100000).toString;
  Label6.Caption := 'Label' + Random(100000).toString;
  Label7.Caption := 'Label' + Random(100000).toString;
  Label8.Caption := 'Label' + Random(100000).toString;
  Label9.Caption := 'Label' + Random(100000).toString;
  Label10.Caption := 'Label' + Random(100000).toString;
  //
  SpeedButton1.Caption := 'SpeedButton' + Random(100000).toString;
  SpeedButton2.Caption := 'SpeedButton' + Random(100000).toString;
  SpeedButton3.Caption := 'SpeedButton' + Random(100000).toString;
  SpeedButton4.Caption := 'SpeedButton' + Random(100000).toString;
  SpeedButton5.Caption := 'SpeedButton' + Random(100000).toString;
  //
  Shape1.Brush.Color := RGB(Random(256), Random(256), Random(256));
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
