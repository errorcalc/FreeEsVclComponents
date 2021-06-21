unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, ES.BaseControls,
  ES.PaintBox, Vcl.StdCtrls;

type
  TCircle = record
    x, y: Integer;
    radius: Integer;
    color: TColor;
  end;

  TFormMain = class(TForm)
    PaintBox: TPaintBox;
    EsPaintBox: TEsPaintBox;
    Timer: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure EsPaintBoxPaint(Sender: TObject; Canvas: TCanvas; Rect: TRect);
  private
    { Private declarations }
    Circles: array [0..400 - 1] of TCircle;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.EsPaintBoxPaint(Sender: TObject; Canvas: TCanvas;
  Rect: TRect);
var
  I, r: Integer;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect);
  for I := 0 to High(Circles) do
  begin
    r := Circles[I].radius;
    Canvas.Brush.Color :=  Circles[I].color;
    Canvas.Ellipse(Circles[I].x - r, Circles[I].y - r,
      Circles[I].x + r, Circles[I].y + r);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(Circles) do
  begin
    Circles[I].x := Random(PaintBox.Width);
    Circles[I].y := Random(PaintBox.Height);
    Circles[I].color := RGB(Random(256), Random(20), Random(256));
    Circles[I].radius := Random(50);
  end;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var
  I, r: Integer;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(TPaintBox(Sender).BoundsRect);
  for I := 0 to High(Circles) do
  begin
    r := Circles[I].radius;
    TPaintBox(Sender).Canvas.Brush.Color :=  Circles[I].color;
    TPaintBox(Sender).Canvas.Ellipse(Circles[I].x - r, Circles[I].y - r,
      Circles[I].x + r, Circles[I].y + r);
  end;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(Circles) do
  begin
    Circles[I].radius := Circles[I].radius + 1;
    if Circles[I].radius > 50 then
      Circles[I].radius := 0;
  end;
  PaintBox.Invalidate;
  EsPaintBox.Invalidate;
end;

end.
