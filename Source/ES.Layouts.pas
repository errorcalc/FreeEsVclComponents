{******************************************************************************}
{                                                                              }
{                       EsVclComponents/EsVclCore v4.7                         }
{                           errorsoft(c) 2009-2025                             }
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
  Winapi.Windows, Winapi.Messages, Vcl.Controls, System.Classes, System.Types, Vcl.Graphics,
  Vcl.ExtCtrls, Vcl.Forms, ES.BaseControls, ES.CfxClasses;

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
    {$IFDEF VER360UP}
    property DoubleBufferedMode;
    {$ENDIF}
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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

  TEsTransparentSplitter = class(TSplitter)
  private type
    TSplitterWindow = class(TCustomControl)
    private
      FAlpha: Byte;
      FChess: Boolean;
      FColor: TColor;
      FBrushBitmap: TBitmap;
      procedure MakeBrush();
      procedure SetAlpha(const Value: Byte);
      procedure SetChess(const Value: Boolean);
      procedure SetColor(const Value: TColor);
    protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure CreateWnd; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy(); override;
      property Alpha: Byte read FAlpha write SetAlpha;
      property Chess: Boolean read FChess write SetChess;
      property Color: TColor read FColor write SetColor;
    end;
  private
    FSplitterWindow: TSplitterWindow;
    FInitialSize: Integer;
    FNewSize: Integer;
    FSetInitialSize: Boolean;
    FResizeStyle: TResizeStyle;
    FSplitterOpacity: Byte;
    FSplitterColor: TColor;
    procedure MakeSplitterWindow();
    procedure KillSplitterWindow();
    procedure UpdateSplitterWindow();
    procedure SetResizeStyle(const Value: TResizeStyle);
    procedure SetSplitterOpacity(const Value: Byte);
    procedure SetSplitterColor(const Value: TColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoCanResize(var NewSize: Integer): Boolean; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure StopSizing(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property ResizeStyle: TResizeStyle read FResizeStyle write SetResizeStyle default rsPattern;
    property SplitterOpacity: Byte read FSplitterOpacity write SetSplitterOpacity default 180;
    property SplitterColor: TColor read FSplitterColor write SetSplitterColor default clDefault;
  end;

implementation

uses
  ES.ExGraphics, ES.Utils, Vcl.Themes;

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

{ TEsTransparentSplitter.TSplitterWindow }

constructor TEsTransparentSplitter.TSplitterWindow.Create(AOwner: TComponent);
begin
  inherited;
  FBrushBitmap := TBitmap.Create();
  FBrushBitmap.SetSize(8, 8);
  FAlpha := 255;
  FChess := True;
end;

destructor TEsTransparentSplitter.TSplitterWindow.Destroy();
begin
  FBrushBitmap.Free();
  inherited;
end;

procedure TEsTransparentSplitter.TSplitterWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_POPUP and (not WS_CHILD);
  Params.ExStyle := Params.ExStyle or WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE;
end;

procedure TEsTransparentSplitter.TSplitterWindow.CreateWnd();
begin
  inherited;
  SetLayeredWindowAttributes(Handle, 0, FAlpha, LWA_ALPHA);
  MakeBrush();
end;

procedure TEsTransparentSplitter.TSplitterWindow.MakeBrush();
var
  X, Y: Integer;
  Color1, Color2: TColor;
begin
  if FChess then
  begin
    Color1 := HighlightColor(FColor, -50);
    Color2 := HighlightColor(FColor, +50);
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (X xor Y) and 2 = 0 then
          FBrushBitmap.Canvas.Pixels[X, Y] := Color1
        else
          FBrushBitmap.Canvas.Pixels[X, Y] := Color2;

    Brush.Bitmap := FBrushBitmap;
  end else
  begin
    Brush.Bitmap := nil;
    Brush.Color := FColor;
  end;
end;

procedure TEsTransparentSplitter.TSplitterWindow.SetAlpha(const Value: Byte);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    SetLayeredWindowAttributes(Handle, 0, FAlpha, LWA_ALPHA);
  end;
end;

procedure TEsTransparentSplitter.TSplitterWindow.SetChess(const Value: Boolean);
begin
  if FChess <> Value then
  begin
    FChess := Value;
    MakeBrush();
    Invalidate();
  end;
end;

procedure TEsTransparentSplitter.TSplitterWindow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    MakeBrush();
    Invalidate();
  end;
end;

{ TEsTransparentSplitter }

constructor TEsTransparentSplitter.Create(AOwner: TComponent);
begin
  inherited;
  TSplitter(Self).ResizeStyle := rsNone;
  FResizeStyle := rsPattern;
  FSplitterColor := clDefault;
  FSplitterOpacity := 180;
end;

destructor TEsTransparentSplitter.Destroy();
begin
  KillSplitterWindow();
  inherited;
end;

procedure TEsTransparentSplitter.MakeSplitterWindow();
var
  ParentForm: TCustomForm;
  ParentHandle: HWND;
begin
  KillSplitterWindow();

  // get parent handle
  ParentForm := GetParentForm(Self, True);
  if ParentForm <> nil then
    ParentHandle := ParentForm.Handle
  else
    ParentHandle := Application.Handle;

  // make window
  FSplitterWindow := TSplitterWindow.Create(nil);
  FSplitterWindow.ParentWindow := ParentHandle;

  // show
  UpdateSplitterWindow();
  FSplitterWindow.Show();
end;

procedure TEsTransparentSplitter.KillSplitterWindow();
begin
  if FSplitterWindow = nil then
    exit;

  FSplitterWindow.Free();
  FSplitterWindow := nil;
end;

procedure TEsTransparentSplitter.UpdateSplitterWindow();
var
  ScreenPoint: TPoint;
begin
  if FSplitterWindow = nil then
    exit;

  // calc screen point
  case Align of
    alLeft: ScreenPoint := ClientToScreen(TPoint.Create(FNewSize - FInitialSize, 0));
    alRight: ScreenPoint := ClientToScreen(TPoint.Create(FInitialSize - FNewSize, 0));
    alTop: ScreenPoint := ClientToScreen(TPoint.Create(0, FNewSize - FInitialSize));
    alBottom: ScreenPoint := ClientToScreen(TPoint.Create(0, FInitialSize - FNewSize));
    else
      exit;
  end;

  // opacity
  FSplitterWindow.Alpha := FSplitterOpacity;

  // chess
  FSplitterWindow.Chess := ResizeStyle = rsPattern;

  // color
  if FSplitterColor <> clDefault then
    FSplitterWindow.Color := BorderColorToRgb(FSplitterColor, Self)
  else
  begin
    if GetLuminanceColor(BorderColorToRgb(clBtnFace, Self)) > 240 div 2 then
      FSplitterWindow.Color := HighlightColor(BorderColorToRgb(clBtnFace, Self), -100)
    else
      FSplitterWindow.Color := HighlightColor(BorderColorToRgb(clBtnFace, Self), 100);
  end;

  // bounds
  FSplitterWindow.SetBounds(ScreenPoint.X, ScreenPoint.Y, Width, Height);
end;

procedure TEsTransparentSplitter.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  FNewSize := MulDiv(FNewSize, M, D);
  FInitialSize := MulDiv(FInitialSize, M, D);
end;

function TEsTransparentSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  if FSetInitialSize then
  begin
    FInitialSize := NewSize;
  end;

  // update pos
  FNewSize := NewSize;
  UpdateSplitterWindow();

  Result := inherited;
end;

procedure TEsTransparentSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  // for update FNewSize
  FSetInitialSize := True;
  try
    MouseMove(Shift, X, Y);
  finally
    FSetInitialSize := False;
  end;

  // show splitter window
  if FResizeStyle in [rsLine, rsPattern] then
    MakeSplitterWindow();
end;

procedure TEsTransparentSplitter.SetResizeStyle(const Value: TResizeStyle);
begin
  if Value <> FResizeStyle then
  begin
    FResizeStyle := Value;
    if FResizeStyle = rsUpdate then
      TSplitter(Self).ResizeStyle := rsUpdate
    else
      TSplitter(Self).ResizeStyle := rsNone;
    UpdateSplitterWindow();
  end;
end;

procedure TEsTransparentSplitter.SetSplitterColor(const Value: TColor);
begin
  if FSplitterColor <> Value then
  begin
    FSplitterColor := Value;
    UpdateSplitterWindow();
  end;
end;

procedure TEsTransparentSplitter.SetSplitterOpacity(const Value: Byte);
begin
  if FSplitterOpacity <> Value then
  begin
    FSplitterOpacity := Value;
    UpdateSplitterWindow();
  end;
end;

procedure TEsTransparentSplitter.StopSizing();
begin
  KillSplitterWindow();
  inherited;
end;

end.
