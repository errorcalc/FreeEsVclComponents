{******************************************************************************}
{                               EsVclFix unit                                  }
{                             ErrorSoft(c) 2015                                }
{                                                                              }
{            errorsoft@mail.ch | errorsoft-demoscene.narod.ru                  }
{ errorsoft@protonmail.ch | github.com/errorcalc | habrahabr.ru/user/error1024 }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit EsVclFix;

interface

uses
  ComCtrls, Messages, CommCtrl;

type
  TCustomListView = class(ComCtrls.TCustomListView)
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure LVMSetExtendedListViewStyle(var Message: TMessage); message LVM_SETEXTENDEDLISTVIEWSTYLE;
  end;

  TListView = class(ComCtrls.TListView)
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure LVMSetExtendedListViewStyle(var Message: TMessage); message LVM_SETEXTENDEDLISTVIEWSTYLE;
  end;

implementation

uses
  Themes, Controls;

{ TCustomListView }

procedure TCustomListView.LVMSetExtendedListViewStyle(var Message: TMessage);
begin
  if StyleServices.Enabled and (GetComCtlVersion >= ComCtlVersionIE6) and
     (StyleServices.IsSystemStyle or not(seClient in Self.StyleElements)) then
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
  if StyleServices.Enabled and (GetComCtlVersion >= ComCtlVersionIE6) and
     (StyleServices.IsSystemStyle or not(seClient in Self.StyleElements)) then
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
