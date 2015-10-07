unit ES.Vcl.ControlsReg;

interface

procedure Register;

implementation

{$R 'Icons/Icons.res'}

uses
  System.Classes, ES.Vcl.Layouts, ES.Vcl.NinePath, ES.Vcl.Indicators;

procedure Register;
begin
  RegisterComponents('ErrorSoft', [
    TEsLayout,// Layouts
    TEsNinePathImage, TEsImageLabel, TEsImageLayout, TEsLabelLayout,// NinePath
    TEsActivityBar// Indicators
  ]);
end;

end.
