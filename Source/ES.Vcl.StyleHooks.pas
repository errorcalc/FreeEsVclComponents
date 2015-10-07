unit ES.Vcl.StyleHooks;

interface

uses
  Forms, Messages, Styles, Themes, Controls;

type
  TFixScrollBoxStyleHook = class(TScrollBoxStyleHook)
    // Fix S3:
    // Is not clear why WM_ERASEBKGND message suppressed in TScrollingStyleHook,
    // perhaps it is improves performance, but create graphical artifacts!
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
  end;

implementation

uses
  Graphics, Windows;

{ TFixScrollBoxStyleHook }

procedure TFixScrollBoxStyleHook.WMEraseBkgnd(var Msg: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  SaveIndex: Integer;
begin
  if OverrideEraseBkgnd and not DoubleBuffered then
  begin
    DC := HDC(Msg.WParam);
    SaveIndex := SaveDC(DC);
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      Canvas.Brush.Color := Brush.Color;
      Canvas.FillRect(Control.ClientRect);
      PaintBackground(Canvas);// incorrectly work
      if PaintOnEraseBkgnd then
        Paint(Canvas);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
      RestoreDC(DC, SaveIndex);
    end;
  end;
  Handled := True;
  Msg.Result := 1;
  //if (Self.Control <> nil) and (seClient in Self.Control.StyleElements) then
  //  Self.DefaultHandler(Msg);
end;

initialization
  TCustomStyleEngine.RegisterStyleHook(TScrollBox, TFixScrollBoxStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TScrollBox, TFixScrollBoxStyleHook);

end.
