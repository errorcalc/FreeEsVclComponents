program RegExTest;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {RegExForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRegExForm, RegExForm);
  Application.Run;
end.
