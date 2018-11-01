{******************************************************************************}
{                            EsVclComponents v3.0                              }
{                           errorsoft(c) 2009-2018                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{         Open this on github: github.com/errorcalc/FreeEsVclComponents        }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
unit ES.Labels;

interface

uses
  Vcl.ExtCtrls, System.Classes, Vcl.Controls, Vcl.StdCtrls, WinApi.Messages,
  Vcl.Graphics, Vcl.Themes;

type

  TEsLinkLabel = class(TCustomLabel)
  private
    LinkState: (lsNormal, lsHot, lsDown);
    FAddress: string;
    function IsAddressStored: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Address: string read FAddress write FAddress stored IsAddressStored;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Cursor default crHandPoint;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Touch;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property StyleElements default [seClient, seBorder];
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

  TEsVersionLabel = class(TCustomLabel)
  private
  const
    DefaultVersionText = 'Version: ';
  var
    FVersionText: string;
    procedure SetVersionText(const Value: string);
    procedure UpdateCaption;
    function IsVersionTextStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property VersionText: string read FVersionText write SetVersionText stored IsVersionTextStored;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Touch;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property StyleElements;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  WinApi.Windows, WinApi.ShellAPI, ES.Core.Classes, ES.ExGraphics,
  ES.Utils, System.SysUtils;

{ TEsLinkLabel }

procedure TEsLinkLabel.Click;
var
  s: string;
  Res: Integer;
begin
  inherited;

  if Address <> '' then
    s := Address
  else
    s := Caption;

  // try loocal or user defined
  Res := Integer(ShellExecute(0, 'Open', PChar(s), PChar(''), nil, SW_SHOWNORMAL));

  // try http://
  if (Res <= HINSTANCE_ERROR) and ((Res or SE_ERR_SHARE) <> 0) then
  begin
    s := 'http://' + s;
    ShellExecute(0, 'Open', PChar(s), PChar(''), nil, SW_SHOWNORMAL);
  end;
end;

procedure TEsLinkLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  LinkState := lsHot;
  Invalidate;
end;

procedure TEsLinkLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  LinkState := lsNormal;
  Invalidate;
end;

constructor TEsLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  StyleElements := [seClient, seBorder];
  Cursor := crHandPoint;
end;

function TEsLinkLabel.IsAddressStored: Boolean;
begin
  result := FAddress <> '';
end;

procedure TEsLinkLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  LinkState := lsDown;
  Invalidate;
end;

procedure TEsLinkLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  LinkState := lsNormal;
  Invalidate;
end;

procedure TEsLinkLabel.Paint;
var
  OnChange: TNotifyEvent;
begin

  OnChange := Font.OnChange;
  Font.OnChange := nil;
  try
    Font.Style := [fsUnderline];
    if LinkState = lsNormal then
      Font.Color := clHotLight
    else if LinkState = lsHot then
      Font.Color := HighlightColor(FontColorToRgb(clHotLight, Self), 30)
    else
      Font.Color := HighlightColor(FontColorToRgb(clHotLight, Self), -30);

    inherited;
  finally
    Font.OnChange := OnChange;
  end;
end;

{ TEsVersionLabel }

constructor TEsVersionLabel.Create(AOwner: TComponent);
begin
  inherited;
  FVersionText := DefaultVersionText;
  UpdateCaption;
end;

function TEsVersionLabel.IsVersionTextStored: Boolean;
begin
  result := FVersionText <> DefaultVersionText;
end;

procedure TEsVersionLabel.SetVersionText(const Value: string);
begin
  if FVersionText <> Value then
  begin
    FVersionText := Value;
    UpdateCaption;
  end;
end;

procedure TEsVersionLabel.UpdateCaption;
begin
  Caption := VersionText + TFileVersion.CreateForFile(ParamStr(0)).ToString;
end;

end.
