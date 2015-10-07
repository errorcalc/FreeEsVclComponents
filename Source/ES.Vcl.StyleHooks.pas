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

{ TFixScrollBoxStyleHook }

procedure TFixScrollBoxStyleHook.WMEraseBkgnd(var Msg: TMessage);
begin
  if (Self.Control <> nil) and (seClient in Self.Control.StyleElements) then
    Self.DefaultHandler(Msg);
end;

initialization
  TCustomStyleEngine.RegisterStyleHook(TScrollBox, TFixScrollBoxStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TScrollBox, TFixScrollBoxStyleHook);

end.
