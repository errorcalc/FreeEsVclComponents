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
unit ES.Layouts;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

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
    property IsDrawHelper default True;
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
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
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
    FCaptionVisible: Boolean;
    FCaptionVertLayout: TVertLayout;
    FCaptionHorzLayout: THorzLayout;
    FCaptionDistance: Integer;
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
    procedure SetCaptionVisible(const Value: Boolean);
    procedure SetCaptionHorzLayout(const Value: THorzLayout);
    procedure SetCaptionVertLayout(const Value: TVertLayout);
    procedure SetCaptionDistance(const Value: Integer);
  protected
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AdjustContentRect(var Rect: TRect); override;
    procedure UpdateText; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property CaptionVertLayout: TVertLayout read FCaptionVertLayout write
      SetCaptionVertLayout default TVertLayout.Center;
    property CaptionHorzLayout: THorzLayout read FCaptionHorzLayout write
      SetCaptionHorzLayout default THorzLayout.Center;
    property CaptionVisible: Boolean read FCaptionVisible write SetCaptionVisible default True;
    property CaptionDistance: Integer read FCaptionDistance write SetCaptionDistance default 2;
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.Raised;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
    property IsDrawHelper default False;
  end;

implementation

uses
  Winapi.Windows, ES.ExGraphics, ES.Utils, Vcl.Themes;

procedure TEsCustomLayout.CMIsToolControl(var Message: TMessage);
begin
  if not FLocked then
    Message.Result := 1;
end;

constructor TEsCustomLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csParentBackground, csDoubleClicks, csReplicatable, csPannable, csGestures];
  Width := 185;
  Height := 41;
  UseDockManager := True;
  IsDrawHelper := True;
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

procedure TEsPanel.AdjustContentRect(var Rect: TRect);
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

  ControlStyle := ControlStyle - [csSetCaption];

  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
  FFrameStyle := TExFrameStyle.Raised;

  FCaptionVertLayout := TVertLayout.Center;
  FCaptionHorzLayout := THorzLayout.Center;
  FCaptionDistance := 2;
  FCaptionVisible := True;

  IsDrawHelper := False;
end;

procedure TEsPanel.Paint;
const
  HorzLayout: array[THorzLayout] of Integer = (DT_LEFT, DT_CENTER, DT_RIGHT);
  VertLayout: array[TVertLayout] of Integer = (DT_TOP, DT_VCENTER, DT_BOTTOM);
var
  TextFlags: Integer;
  R: TRect;
begin
  (* {$ifdef VER240UP}
  if (seClient in Control.StyleElements) then
  begin
    FillBackground(Canvas.Handle);
  end;
  {$endif}*)

  if FCaptionVisible then
  begin
    TextFlags := DT_SINGLELINE or HorzLayout[CaptionHorzLayout] or VertLayout[CaptionVertLayout];
    TextFlags := DrawTextBiDiModeFlags(TextFlags);

    R := ContentRect;
    // fix text bounds
    if CaptionVertLayout <> TVertLayout.Center then
      if CaptionVertLayout = TVertLayout.Top then
        R.Top := R.Top + CaptionDistance
      else
        R.Bottom := R.Bottom - CaptionDistance;
    if CaptionHorzLayout <> THorzLayout.Center then
      if CaptionHorzLayout = THorzLayout.Left then
        R.Left := R.Left + CaptionDistance
      else
        R.Right := R.Right - CaptionDistance;

    Canvas.Font := Font;
    Canvas.Font.Color := FontColorToRgb(Font.Color, Self);
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, Caption, -1, R, TextFlags);
  end;

  if (csDesigning in ComponentState) and IsDrawHelper then
    DrawControlHelper(Self, [hoPadding, hoClientRect], GetFrameWidth(FrameStyle, FrameWidth));

  if FrameStyle <> TExFrameStyle.None then
    DrawFrame(Canvas, Self, ClientRect, FrameStyle, FrameWidth,
      FrameColor, clBtnHighlight, clBtnShadow, True);
end;

procedure TEsPanel.SetCaptionDistance(const Value: Integer);
begin
  if (FCaptionDistance <> Value) and (Value >= 0) then
  begin
    FCaptionDistance := Value;
    if FCaptionVisible then
      Invalidate;
  end;
end;

procedure TEsPanel.SetCaptionHorzLayout(const Value: THorzLayout);
begin
  if FCaptionHorzLayout <> Value then
  begin
    FCaptionHorzLayout := Value;
    if FCaptionVisible then
      Invalidate;
  end;
end;

procedure TEsPanel.SetCaptionVertLayout(const Value: TVertLayout);
begin
  if FCaptionVertLayout <> Value then
  begin
    FCaptionVertLayout := Value;
    if FCaptionVisible then
      Invalidate;
  end;
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

procedure TEsPanel.SetCaptionVisible(const Value: Boolean);
begin
  if FCaptionVisible <> Value then
  begin
    FCaptionVisible := Value;
    Invalidate;
  end;
end;

procedure TEsPanel.UpdateText;
begin
  if FCaptionVisible then
    Invalidate;
end;

end.
