program Project9PatchLayout;

uses
  Vcl.Forms,
  U9PathLayout in 'U9PathLayout.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
