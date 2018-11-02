program BufferedChildrens;



uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  UnitMain in 'UnitMain.pas' {FormDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
