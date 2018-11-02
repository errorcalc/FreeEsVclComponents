program DfmConverter;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormConverter};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormConverter, FormConverter);
  Application.Run;
end.
