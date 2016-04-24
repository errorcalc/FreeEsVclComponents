{******************************************************************************}
{                          FreeEsVclComponents/Core                            }
{                           ErrorSoft(c) 2015-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{     errorsoft@protonmail.ch | habrahabr.ru/user/error1024 | errorsoft.org    }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{                                                                              }
{ Вы можете заказать разработку VCL/FMX компонента на заказ                    }
{ You can order the development of VCL/FMX components to order                 }
{******************************************************************************}
unit Es.VclFix;

interface

{$IF CompilerVersion >= 23}
{$DEFINE VER230UP}
{$IFEND}
{$IF CompilerVersion >= 24}
{$DEFINE VER240UP}
{$IFEND}

uses
  ComCtrls, {$ifdef VER230UP}WinApi.Messages, WinApi.CommCtrl{$else}Messages, CommCtrl{$endif};

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
  Themes, Controls {$IFDEF VER230UP}, ES.StyleHooks{$ENDIF};

{ TCustomListView }

procedure TCustomListView.LVMSetExtendedListViewStyle(var Message: TMessage);
begin
  if (GetComCtlVersion >= ComCtlVersionIE6) and ThemeControl(Self) {$ifdef VER230UP}
    and (StyleServices.IsSystemStyle {$ifdef VER240UP} or not(seClient in Self.StyleElements){$endif}){$endif} then
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
  if (GetComCtlVersion >= ComCtlVersionIE6) and ThemeControl(Self) {$ifdef VER230UP}
    and (StyleServices.IsSystemStyle {$ifdef VER240UP} or not(seClient in Self.StyleElements){$endif}){$endif} then
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
