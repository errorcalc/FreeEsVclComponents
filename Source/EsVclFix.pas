{******************************************************************************}
{                             ESComponents for VCL                             }
{                              ErrorSoft(c) 2015                               }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit EsVclFix;

interface

{$IF CompilerVersion >= 21}
{$DEFINE VER210UP}
{$IFEND}
{$IF CompilerVersion >= 24}
{$DEFINE VER240UP}
{$IFEND}

uses
  ComCtrls, Messages, CommCtrl;

type
  TCustomListView = class(ComCtrls.TCustomListView)
    // Fix D1: Selection blinking if used LVM_SETEXTENDEDLISTVIEWSTYLE (blue transparent selection rect)
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    // Fix U2: Used style selection rectangle in Win3.1: inverted pixels, expected: blue transparent selection rect
    procedure LVMSetExtendedListViewStyle(var Message: TMessage); message LVM_SETEXTENDEDLISTVIEWSTYLE;
  end;

  TListView = class(ComCtrls.TListView)
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure LVMSetExtendedListViewStyle(var Message: TMessage); message LVM_SETEXTENDEDLISTVIEWSTYLE;
  end;

implementation

uses
  Themes, Controls {$IFDEF VER210UP}, ES.Vcl.StyleHooks{$ENDIF};

{ TCustomListView }

procedure TCustomListView.LVMSetExtendedListViewStyle(var Message: TMessage);
begin
  if StyleServices.Enabled and (GetComCtlVersion >= ComCtlVersionIE6) {$ifdef VER210UP}and
     (StyleServices.IsSystemStyle{$ifdef VER240UP} or not(seClient in Self.StyleElements){$endif}){$endif} then
  begin
    Message.LParam := Message.LParam or LVS_EX_DOUBLEBUFFER;
    DefaultHandler(Message);
  end else
    Inherited;
end;

procedure TCustomListView.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  DefaultHandler(Message);
end;

{ TListView }

procedure TListView.LVMSetExtendedListViewStyle(var Message: TMessage);
begin
  if StyleServices.Enabled and (GetComCtlVersion >= ComCtlVersionIE6) {$ifdef VER210UP}and
     (StyleServices.IsSystemStyle{$ifdef VER240UP} or not(seClient in Self.StyleElements){$endif}){$endif} then
  begin
    Message.LParam := Message.LParam or LVS_EX_DOUBLEBUFFER;
    DefaultHandler(Message);
  end else
    Inherited;
end;

procedure TListView.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  DefaultHandler(Message);
end;

end.
