{******************************************************************************}
{                             ESComponents for VCL                             }
{                            ErrorSoft(c) 2012-2015                            }
{                                                                              }
{            errorsoft@mail.ch | errorsoft-demoscene.narod.ru                  }
{ errorsoft@protonmail.ch | github.com/errorcalc | habrahabr.ru/user/error1024 }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.Layouts;

interface

uses
  WinApi.Messages, Vcl.Controls, System.Classes, System.Types, Vcl.Graphics, ES.Vcl.BaseControls;

type
  TEsCustomLayout = class(TEsCustomControl)
  private
    FLocked: Boolean;
    FBorderWidth: TBorderWidth;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
    procedure SetBorderWidth(const Value: TBorderWidth);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    property UseDockManager default True;
    procedure Paint; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color default clBtnFace;
    property DockManager;
    property Locked: Boolean read FLocked write FLocked default False;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
  end;

  TEsLayout = class(TEsCustomLayout)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
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
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
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
    property StyleElements;
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
    property OnPaint;
    property OnPainting;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

//procedure Register;

implementation

uses
  ES.Vcl.ExGraphics;

//procedure Register;
//begin
//  RegisterComponents('ErrorSoft', [TEsLayout]);
//end;

{ TESCustomLayout }

procedure TEsCustomLayout.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if BorderWidth <> 0 then
  begin
    InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));
    if Rect.Width < 0 then Rect.Width := 0;
    if Rect.Height < 0 then Rect.Height := 0;
  end;
end;

procedure TEsCustomLayout.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if (csDesigning in ComponentState) and IsDrawHelper then
    Invalidate;
end;

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

procedure TEsCustomLayout.Paint;
  procedure Line(Canvas: TCanvas; x1, y1, x2, y2: Integer);
  begin
    Canvas.MoveTo(x1, y1);
    Canvas.LineTo(x2, y2);
  end;
begin
  inherited;
  if (csDesigning in ComponentState) and IsDrawHelper then
  begin
    if (FBorderWidth * 2 > Height)or(FBorderWidth * 2 > Width) then
      exit;
    Canvas.Pen.Mode := pmNot;
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    if FBorderWidth <> 0 then
      Canvas.Rectangle(FBorderWidth, FBorderWidth, Width - FBorderWidth, Height - FBorderWidth);

    if (FBorderWidth * 2 + Padding.Top + Padding.Bottom > Height)or
       (FBorderWidth * 2 + Padding.Left + Padding.Right > Width) then
      exit;
    //Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Style := psDot;
    //Canvas.Pen.Color := clBlue;
    if Padding.Left <> 0 then
      Line(Canvas, Padding.Left + FBorderWidth, Padding.Top + FBorderWidth,
        Padding.Left + FBorderWidth, Height - Padding.Bottom - FBorderWidth - 1);
    if Padding.Top <> 0 then
      Line(Canvas, Padding.Left + FBorderWidth, Padding.Top + FBorderWidth,
        Width - Padding.Right - FBorderWidth - 1, Padding.Top + FBorderWidth);
    if Padding.Right <> 0 then
      Line(Canvas, Width - Padding.Right - FBorderWidth - 1, Padding.Top + FBorderWidth,
        Width - Padding.Right - FBorderWidth - 1, Height - Padding.Bottom - FBorderWidth - 1);
    if Padding.Bottom <> 0 then
      Line(Canvas, Padding.Left + FBorderWidth, Height - Padding.Bottom - FBorderWidth - 1,
        Width - Padding.Right - FBorderWidth - 1, Height - Padding.Bottom - FBorderWidth - 1);
  end;
end;

procedure TEsCustomLayout.SetBorderWidth(const Value: TBorderWidth);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Realign;
    Invalidate;
  end;
end;

end.
