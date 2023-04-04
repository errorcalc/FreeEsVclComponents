{******************************************************************************}
{                                                                              }
{                       EsVclComponents/EsVclCore v4.4                         }
{                           errorsoft(c) 2009-2023                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{    errorsoft@mail.ru | github.com/errorcalc | habrahabr.ru/user/error1024    }
{          You can write to me in the Telegram messenger: @errorsoft           }
{                                                                              }
{           Star me github: github.com/errorcalc/FreeEsVclComponents           }
{                                                                              }
{                 You can order developing vcl/fmx components,                 }
{               please submit your requests to mail or telegram.               }
{                                                                              }
{******************************************************************************}
unit ES.Labels;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Vcl.ExtCtrls, System.Classes, Vcl.Controls, Vcl.StdCtrls, WinApi.Messages,
  Vcl.Graphics, Vcl.Themes;

type
  TLinkLabelStyle = (Underline, Mixed, Normal);

  TEsLinkLabel = class(TCustomLabel)
  private
    LinkState: (lsNormal, lsHot, lsDown);
    FUrl: string;
    FLinkColor: TColor;
    FLinkStyle: TLinkLabelStyle;
    function IsUrlStored: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure SetLinkColor(const Value: TColor);

    procedure SetLinkStyle(const Value: TLinkLabelStyle);
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
    property Url: string read FUrl write FUrl stored IsUrlStored;
    property Layout;
    property LinkColor: TColor read FLinkColor write SetLinkColor default clHotLight;
    property LinkStyle: TLinkLabelStyle read FLinkStyle write SetLinkStyle default TLinkLabelStyle.Underline;
    property Visible;
    property WordWrap;
    {$IFDEF VER240UP}
    property StyleElements default [seClient, seBorder, seFont];
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
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
  protected
    procedure Click; override;
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
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
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
  System.UITypes, WinApi.Windows, WinApi.ShellAPI, ES.Core.Classes, ES.ExGraphics,
  ES.Utils, System.SysUtils, Vcl.ClipBrd, ES.Hints;

{ TEsLinkLabel }

procedure TEsLinkLabel.Click;
var
  s: string;
  Res: Integer;
begin
  inherited;

  if Url <> '' then
    s := Url
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
  {$IFDEF VER240UP}
  StyleElements := [seClient, seBorder, seFont];
  {$ENDIF}
  Cursor := crHandPoint;
  FLinkColor := clHotLight;
end;

function TEsLinkLabel.IsUrlStored: Boolean;
begin
  result := FUrl <> '';
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
  StoredFont: TFont;
begin
  OnChange := Font.OnChange;
  StoredFont := TFont.Create;
  try
    StoredFont.Assign(Font);
    Font.OnChange := nil;

    if (LinkStyle = TLinkLabelStyle.Underline) or
       ((LinkStyle = TLinkLabelStyle.Mixed) and (LinkState <> lsNormal)) then
      Font.Style := Font.Style + [fsUnderline]
    else
      Font.Style := Font.Style - [fsUnderline];

    if LinkState = lsNormal then
      Font.Color := FontColorToRgb(LinkColor, Self)
    else if LinkState = lsHot then
      Font.Color := HighlightColor(FontColorToRgb(LinkColor, Self), 60)
    else
      Font.Color := HighlightColor(FontColorToRgb(LinkColor, Self), -60);

    inherited;
  finally
    Font.Assign(StoredFont);
    Font.OnChange := OnChange;
    StoredFont.Free;
  end;
end;

procedure TEsLinkLabel.SetLinkColor(const Value: TColor);
begin
  if Value <> FLinkColor then
  begin
    FLinkColor := Value;
    Invalidate;
  end;
end;

procedure TEsLinkLabel.SetLinkStyle(const Value: TLinkLabelStyle);
begin
  if Value <> FLinkStyle then
  begin
    FLinkStyle := Value;
    Invalidate;
  end;
end;

{ TEsVersionLabel }

procedure TEsVersionLabel.Click;
begin
  inherited;
  Clipboard.AsText := Caption;
  ShowNormalHint(self, 'Version has been copied to clipboard!');
end;

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
