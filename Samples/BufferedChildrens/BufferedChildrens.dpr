program BufferedChildrens;



uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  UnitMain in 'UnitMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
