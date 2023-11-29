unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ES.BaseControls, ES.Layouts, ES.NinePatch,
  Vcl.ExtCtrls, Math;

type
  TFormMain = class(TForm)
    EsImageLayout1: TEsImageLayout;
    EsImageLabel1: TEsImageLabel;
    EsImageLabel2: TEsImageLabel;
    EsImageLabel3: TEsImageLabel;
    EsImageLabel4: TEsImageLabel;
    EsImageLabel5: TEsImageLabel;
    EsImageStaticText1: TEsImageStaticText;
    TimerAnimate: TTimer;
    procedure TimerAnimateTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.TimerAnimateTimer(Sender: TObject);
begin
  if EsImageLayout1.OverlayOpacity - 10 < 0 then
    EsImageLayout1.OverlayOpacity := 255;

  EsImageLayout1.OverlayOpacity := EsImageLayout1.OverlayOpacity - 10;
end;

end.
