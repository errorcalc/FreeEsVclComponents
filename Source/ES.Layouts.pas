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
unit ES.Layouts;

interface

uses
  Winapi.Messages, Vcl.Controls, System.Classes, System.Types, Vcl.Graphics, ES.BaseControls,
  ES.CfxClasses;

type
  TEsCustomLayout = class(TEsBaseLayout)
  private
    FLocked: Boolean;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property UseDockManager default True;
  public
    constructor Create(AOwner: TComponent); override;
    property Color default clBtnFace;
    property DockManager;
    property Locked: Boolean read FLocked write FLocked default False;
  end;

  TEsLayout = class(TEsCustomLayout)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BufferedChildren;// TEsCustomControl
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property IsCachedBuffer;// TEsCustomControl
    property IsCachedBackground;// TEsCustomControl
    property IsDrawHelper;// TEsCustomControl
    property IsOpaque;// TEsCustomControl
    property IsFullSizeBuffer;// TEsCustomControl
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentBufferedChildren;// TEsCustomControl
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    {$if CompilerVersion > 23}
    property StyleElements;
    {$ifend}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnPaint;// TEsCustomControl
    property OnPainting;// TEsCustomControl
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TEsPanel = class(TEsLayout)
  private
    FFrameWidth: TFrameWidth;
    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
  protected
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.Raised;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
  end;

implementation

uses
  ES.ExGraphics, ES.Utils, Vcl.Themes;

procedure TEsCustomLayout.CMIsToolControl(var Message: TMessage);
begin
  if not FLocked then Message.Result := 1;
end;

constructor TEsCustomLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csParentBackground, csDoubleClicks, csReplicatable, csPannable, csGestures];
  Width := 185;
  Height := 41;
  UseDockManager := True;
end;

procedure TEsCustomLayout.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // nope now
end;

{ TEsPanel }

procedure TEsPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  if FrameStyle <> TExFrameStyle.None then
  begin
    Rect.Inflate(-GetFrameWidth(FrameStyle, FrameWidth), -GetFrameWidth(FrameStyle, FrameWidth));
  end;
end;

constructor TEsPanel.Create(AOwner: TComponent);
begin
  inherited;

  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
  FFrameStyle := TExFrameStyle.Raised;
end;

procedure TEsPanel.Paint;
begin
  if (csDesigning in ComponentState) and IsDrawHelper then
    DrawControlHelper(Self, [hoPadding, hoClientRect], GetFrameWidth(FrameStyle, FrameWidth));

  if FrameStyle <> TExFrameStyle.None then
    if IsStyledBorderControl(Self) then
      DrawFrame(Canvas, ClientRect, FrameStyle, FrameWidth, StyleServices.GetSystemColor(FrameColor),
        StyleServices.GetSystemColor(clBtnHighlight), StyleServices.GetSystemColor(clBtnShadow))
    else
      DrawFrame(Canvas, ClientRect, FrameStyle, FrameWidth, FrameColor, clBtnHighlight, clBtnShadow);

end;

procedure TEsPanel.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TEsPanel.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TEsPanel.SetFrameWidth(const Value: TFrameWidth);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Realign;
    Invalidate;
  end;
end;

end.
