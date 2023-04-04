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
unit ES.Switch;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Winapi.Messages, ES.ExGraphics, ES.BaseControls, System.UITypes, ES.CfxClasses, Vcl.Graphics;

type
  // only interal use
  TSwitchStyle = class(TPersistent)
  private
    FControl: TWinControl;
    FOnChange: TNotifyEvent;
    FMainColor: TColor;
    FFrameColor: TColor;
    FThumbColor: TColor;
    procedure SetMainColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetThumbColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Change; virtual;
    function GetColor(const Index: Integer): TAlphaColor; virtual;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Control: TWinControl read FControl write FControl;
  public
    property OffNormalFillColor: TAlphaColor Index 0 read GetColor;
    property OffNormalFrameColor: TAlphaColor Index 1 read GetColor;
    property OffNormalThumbColor: TAlphaColor Index 2 read GetColor;
    property OffHotFillColor: TAlphaColor Index 3 read GetColor;
    property OffHotFrameColor: TAlphaColor Index 4 read GetColor;
    property OffHotThumbColor: TAlphaColor Index 5 read GetColor;
    property OffDisableFillColor: TAlphaColor Index 6 read GetColor;
    property OffDisableFrameColor: TAlphaColor Index 7 read GetColor;
    property OffDisableThumbColor: TAlphaColor Index 8 read GetColor;
    property OnNormalFillColor: TAlphaColor Index 9 read GetColor;
    property OnNormalFrameColor: TAlphaColor Index 10 read GetColor;
    property OnNormalThumbColor: TAlphaColor Index 11 read GetColor;
    property OnHotFillColor: TAlphaColor Index 12 read GetColor;
    property OnHotFrameColor: TAlphaColor Index 13 read GetColor;
    property OnHotThumbColor: TAlphaColor Index 14 read GetColor;
    property OnDisableFillColor: TAlphaColor Index 15 read GetColor;
    property OnDisableFrameColor: TAlphaColor Index 16 read GetColor;
    property OnDisableThumbColor: TAlphaColor Index 17 read GetColor;
    property DragFillColor: TAlphaColor Index 18 read GetColor;
    property DragFrameColor: TAlphaColor Index 19 read GetColor;
    property DragThumbColor: TAlphaColor Index 20 read GetColor;
  published
    property FrameColor: TColor read FFrameColor write SetFrameColor default clDefault;
    property ThumbColor: TColor read FThumbColor write SetThumbColor default clDefault;
    property MainColor: TColor read FMainColor write SetMainColor default clDefault;
  end;

  TEsCustomSwitch = class;

  TEsCustomSwitchActionLinkClass = class(TWinControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TThumbBorder = 1..100;
  TSwitchBorder = 1..100;
  TSwitchAlignment = (Left, Right);
  TSwitchLayout = (Fixed, AutoSize, Client);
  {$REGION 'deprecated names'}
  {$IFDEF SUPPORT_ENUMS_ALIASES}
  TSwitchAlignmentHelper = record helper for TSwitchAlignment
  const
    saLeft = TSwitchAlignment.Left deprecated 'Use TSwitchAlignment.Left';
    saRight = TSwitchAlignment.Right deprecated 'Use TSwitchAlignment.Right';
  end;
  TSwitchLayoutHelper = record helper for TSwitchLayout
  const
    slFixed = TSwitchLayout.Fixed deprecated 'Use TSwitchLayout.Fixed';
    slAutoSize = TSwitchLayout.Autosize deprecated 'Use TSwitchLayout.Autosize';
    slClient = TSwitchLayout.Client deprecated 'Use TSwitchLayout.Client';
  end;
  {$ENDIF}
  {$ENDREGION}

  // now: only interal use, this class can be too refactored!
  TEsCustomSwitch = class(TEsCustomControl)
  private type
    {$SCOPEDENUMS OFF}
    TSwitchState = (ssOffNormal, ssOffHot, ssDrag, ssOnNormal, ssOnHot);
    {$SCOPEDENUMS ON}
  const
    sDefaultSwitchOn = 'On';
    sDefaultSwitchOff = 'Off';
    TextSpace = 3;
    TrackingSensitivity = 3;
    ThumbDurationTime = 200;
    DefaultWidth = 44;
    DefaultHeight = 20;
    SwitchSizeFactor = 2.2;
  private
    Style: TSwitchStyle;
    FChecked: Boolean;
    FThumbBorder: TThumbBorder;
    FTextOn: string;
    FTextOff: string;
    FShowCaption: Boolean;
    TextSize: TSize;
    StartPos: TPoint;
    DeltaPos: Integer;
    Pos: Integer;
    Animation: TIntegerAnimation;
    ClicksDisabled: Boolean;
    FAnimated: Boolean;
    FSwitchWidth: Integer;
    FSwitchHeight: Integer;
    FAutoSize: Boolean;
    FSwitchLayout: TSwitchLayout;
    FVerticalSpace: Cardinal;
    FAlignment: TSwitchAlignment;
    FSwitchBorder: TSwitchBorder;
    // Internal
    function GetSwitchRect: TRect;
    function GetThumbPos: Integer;
    procedure ChangeState;
    procedure AnimationProcess(Sender: TObject);
    procedure AnimationFinish(Sender: TObject);
    // Messages
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    // Setters and getters
    procedure ColorsChange(Sender: TObject);
    procedure SetChecked(const Value: Boolean);
    procedure SetThumbBorder(const Value: TThumbBorder);
    procedure SetTextOff(const Value: string);
    procedure SetTextOn(const Value: string);
    function IsStoredTextOff: Boolean;
    function IsStoredTextOn: Boolean;
    procedure SetShowCaption(const Value: Boolean);
    procedure SetSwitchHeight(const Value: Integer);
    procedure SetSwitchWidth(const Value: Integer);
    procedure SetSwitchLayout(const Value: TSwitchLayout);
    procedure SetVerticalSpace(const Value: Cardinal);
    procedure SetSwitchBorder(const Value: TSwitchBorder);
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetAlignment(const Value: TSwitchAlignment);
    function GetFrameColor: TColor;
    function GetMainColor: TColor;
    function GetThumbColor: TColor;
    procedure SetFrameColor(const Value: TColor);
    procedure SetMainColor(const Value: TColor);
    procedure SetThumbColor(const Value: TColor);
  protected
    State: TSwitchState;
    IsTracking: Boolean;
    IsAnimating: Boolean;
    CanAutoCheck: Boolean;
    {$IFDEF VER240UP}
    procedure UpdateStyleElements; override;
    {$ENDIF}
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure CreateWnd; override;
    {$IFDEF VER310UP}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    // for external styles
    function CreateStyle: TSwitchStyle; dynamic;
    function FrameColorForState(State: TSwitchState): TAlphaColor; virtual;
    function FillColorForState(State: TSwitchState): TAlphaColor; virtual;
    function ThumbColorForState(State: TSwitchState): TAlphaColor; virtual;
    function GetThumbSize: TSize; virtual;
    procedure CalcTextSize; dynamic;
    procedure AdjustBounds; dynamic;
    // Actions
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    // Thumb & Switch
    function ThumbPosForState(IsChacked: Boolean): Integer;
    procedure AnimateThumbTo(CurrentPos: Integer; IsChacked: Boolean);
    property SwitchRect: TRect read GetSwitchRect;
    property ThumbSize: TSize read GetThumbSize;
    property ThumbPos: Integer read GetThumbPos;
    // to published...
    property FrameColor: TColor read GetFrameColor write SetFrameColor default clDefault;
    property ThumbColor: TColor read GetThumbColor write SetThumbColor default clDefault;
    property MainColor: TColor read GetMainColor write SetMainColor default clDefault;
    //
    property Alignment: TSwitchAlignment read FAlignment write SetAlignment default TSwitchAlignment.Right;
    property Checked: Boolean read FChecked write SetChecked default False;
    property ThumbBorder: TThumbBorder read FThumbBorder write SetThumbBorder default 3;
    property SwitchBorder: TSwitchBorder read FSwitchBorder write SetSwitchBorder default 2;
    property TextOn: string read FTextOn write SetTextOn stored IsStoredTextOn;
    property TextOff: string read FTextOff write SetTextOff stored IsStoredTextOff;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
    property Animated: Boolean read FAnimated write FAnimated default True;
    property SwitchWidth: Integer read FSwitchWidth write SetSwitchWidth default DefaultWidth;
    property SwitchHeight: Integer read FSwitchHeight write SetSwitchHeight default DefaultHeight;
    property SwitchLayout: TSwitchLayout read FSwitchLayout write SetSwitchLayout default TSwitchLayout.Fixed;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property VerticalSpace: Cardinal read FVerticalSpace write SetVerticalSpace default 0;
    //
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // for public use
  TEsSwitch = class(TEsCustomSwitch)
  published
    property Alignment;
    property Checked;
    property ThumbBorder;
    property TextOn;
    property TextOff;
    property ShowCaption;
    property Animated;
    property SwitchWidth;
    property SwitchHeight;
    property SwitchLayout;
    property AutoSize;
    property VerticalSpace;
    //
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property FrameColor;
    property ThumbColor;
    property MainColor;
    property Constraints;
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
    property Height;
    property HelpContext;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property ParentBackground;
    property PopupMenu;
    property ShowHint;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnPaint;// TEsCustomControl
    property OnPainting;// TEsCustomControl
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  System.Math, WinApi.GdipObj, WinApi.GdipApi, Vcl.Consts, Vcl.Themes, ES.ExGdiPlus, WinApi.DwmApi,
  ES.Utils, Vcl.ActnList, System.TypInfo;

{ TEsCustomSwitch }

procedure TEsCustomSwitch.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  begin
    if not CheckDefaults or (not Self.Checked) then
    begin
      Self.Checked := TCustomAction(Sender).Checked;
    end;
  end;
end;

procedure TEsCustomSwitch.AdjustBounds;
var
  H: Cardinal;
begin
  if not(csLoading in ComponentState) and FAutoSize and (FSwitchLayout <> TSwitchLayout.Client) then
  begin
    CalcTextSize;
    if TextSize.Height > SwitchRect.Height then
      H := TextSize.Height
    else
      H := SwitchRect.Height;
    SetBounds(Left, Top, SwitchRect.Width + TextSize.Width * Integer(FShowCaption), H + FVerticalSpace * 2);
  end;
end;

procedure TEsCustomSwitch.AnimateThumbTo(CurrentPos: Integer; IsChacked: Boolean);
begin
  FreeAndNil(Animation);
  IsAnimating := True;
  Animation := TIntegerAnimation.Create;
  Animation.Mode := TAnimationMode.Cubic;
  Animation.ReverseMode := True;
  Animation.OnProcess := AnimationProcess;
  Animation.OnFinish := AnimationFinish;
  Animation.StartValue := CurrentPos;
  Animation.EndValue := ThumbPosForState(IsChacked);
  Animation.Duration := ThumbDurationTime;
  Animation.Start;
end;

procedure TEsCustomSwitch.AnimationFinish(Sender: TObject);
begin
  FreeAndNil(Animation);
  IsAnimating := False;
  Invalidate;
end;

procedure TEsCustomSwitch.AnimationProcess(Sender: TObject);
begin
  Pos := Animation.Value;
  Invalidate;
end;

procedure TEsCustomSwitch.CalcTextSize;
  procedure RealCalc;
  begin
    Canvas.Font := Font;
    TextSize.Width := Max(Canvas.TextWidth(FTextOn), Canvas.TextWidth(FTextOff)) + TextSpace;
    TextSize.Height := Canvas.TextHeight('Lb|y');// VCL use same hack, see: Shift+Ctrl+F "TextHeight('" in VCl source
  end;
var
  DC: HDC;
begin
  if WindowHandle = 0 then
  begin
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      RealCalc;
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
  end else
    RealCalc;
end;

{$IFDEF VER310UP}
procedure TEsCustomSwitch.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  FSwitchWidth := MulDiv(FSwitchWidth, M, D);
  FSwitchHeight := MulDiv(FSwitchHeight, M, D);
end;
{$ELSE}
procedure TEsCustomSwitch.ChangeScale(M, D: Integer);
begin
  inherited;
  FSwitchWidth := MulDiv(FSwitchWidth, M, D);
  FSwitchHeight := MulDiv(FSwitchHeight, M, D);
end;
{$ENDIF}

procedure TEsCustomSwitch.ChangeState;
begin
  if Action <> nil then
    Action.Execute
  else
    if Assigned(OnClick) then
      OnClick(Self);
end;

procedure TEsCustomSwitch.Click;
begin
end;

procedure TEsCustomSwitch.CMEnabledChanged(var Message: TMessage);
begin
  Inherited;
  Invalidate;
end;

procedure TEsCustomSwitch.CMFontChanged(var Message: TMessage);
begin
  Inherited;
  CalcTextSize;
  AdjustBounds;
end;

procedure TEsCustomSwitch.CMMouseEnter(var Message: TMessage);
begin
  Inherited;
  if FChecked then
    State := ssOnHot
  else
    State := ssOffHot;
  Invalidate;
end;

procedure TEsCustomSwitch.CMMouseLeave(var Message: TMessage);
begin
  Inherited;
  if FChecked then
    State := ssOnNormal
  else
    State := ssOffNormal;
  Invalidate;
end;

procedure TEsCustomSwitch.CMStyleChanged(var Message: TMessage);
begin
  Inherited;
  // FColors.UpdateColors;
end;

procedure TEsCustomSwitch.ColorsChange(Sender: TObject);
begin
  Invalidate;
end;

constructor TEsCustomSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse] - [csDoubleClicks];
  TabStop := True;
  Width := DefaultWidth;
  Height := DefaultHeight;

  Style := CreateStyle;
  Style.OnChange := ColorsChange;
  Style.Control := Self;
  State := ssOffNormal;

  IsFullSizeBuffer := True;
  FSwitchWidth := DefaultWidth;
  FSwitchHeight := DefaultHeight;
  FAutoSize := True;
  FVerticalSpace := 0;
  FSwitchLayout := TSwitchLayout.Fixed;
  FThumbBorder := 3;
  FSwitchBorder := 2;
  FTextOn := sDefaultSwitchOn;
  FTextOff := sDefaultSwitchOff;
  FAnimated := True;
  FAlignment := TSwitchAlignment.Right;
end;

function TEsCustomSwitch.CreateStyle: TSwitchStyle;
begin
  Result := TSwitchStyle.Create;
end;

procedure TEsCustomSwitch.CreateWnd;
begin
  inherited;
  Perform(WM_CHANGEUISTATE, MakeWParam(UIS_INITIALIZE,  UISF_HIDEFOCUS), 0 );
  AdjustBounds;
end;

destructor TEsCustomSwitch.Destroy;
begin
  Style.Free;
  Animation.Free;
  inherited;
end;

function TEsCustomSwitch.FillColorForState(State: TSwitchState): TAlphaColor;
begin
  if Enabled then
    case State of
      ssOffNormal: Result := Style.OffNormalFillColor;
      ssOffHot: Result := Style.OffHotFillColor;
      ssDrag: Result := Style.DragFillColor;
      ssOnNormal: Result := Style.OnNormalFillColor;
      ssOnHot: Result := Style.OnHotFillColor;
      else Result := 0;// for compiler paranoia
    end
  else
    if Checked then
      Result := Style.OnDisableFillColor
    else
      Result := Style.OffDisableFillColor;
end;

function TEsCustomSwitch.FrameColorForState(State: TSwitchState): TAlphaColor;
begin
  if Enabled then
    case State of
      ssOffNormal: Result := Style.OffNormalFrameColor;
      ssOffHot: Result := Style.OffHotFrameColor;
      ssDrag: Result := Style.DragFrameColor;
      ssOnNormal: Result := Style.OnNormalFrameColor;
      ssOnHot: Result := Style.OnHotFrameColor;
      else Result := 0;// for compiler paranoia
    end
  else
    if Checked then
      Result := Style.OnDisableFrameColor
    else
      Result := Style.OffDisableFrameColor;
end;

function TEsCustomSwitch.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TEsCustomSwitchActionLinkClass;
end;

function TEsCustomSwitch.GetFrameColor: TColor;
begin
  Result := Style.FrameColor;
end;

function TEsCustomSwitch.GetMainColor: TColor;
begin
  Result := Style.MainColor;
end;

function TEsCustomSwitch.GetSwitchRect: TRect;
var
  H, W: Integer;
begin
  case FSwitchLayout of
    TSwitchLayout.Fixed:
    begin
      if FAlignment = TSwitchAlignment.Right then
        Result.Left := 0
      else
        Result.Left := ClientWidth - FSwitchWidth;
      Result.Top := ClientHeight div 2 - FSwitchHeight div 2;

      Result.Width := FSwitchWidth;
      Result.Height := FSwitchHeight;
    end;
    TSwitchLayout.AutoSize:
    begin
      H := TextSize.Height + 6;
      W := Trunc(H * 2.2 + 0.5);

      if FAlignment = TSwitchAlignment.Right then
        Result.Left := 0
      else
        Result.Left := ClientWidth - W;
      Result.Top := ClientHeight div 2 - H div 2;

      Result.Width := W;
      Result.Height := H;
    end;
    TSwitchLayout.Client:
    begin
      if ShowCaption then
        W := ClientWidth - TextSize.Width - TextSpace
      else
        W := ClientWidth;

      if FAlignment = TSwitchAlignment.Right then
        Result.Left := 0
      else
        Result.Left := ClientWidth - W;
      Result.Top := 0;

      Result.Height := ClientHeight;
      Result.Width := W;
    end;
  end;
end;

function TEsCustomSwitch.GetThumbColor: TColor;
begin
  Result := Style.ThumbColor;
end;

function TEsCustomSwitch.GetThumbPos: Integer;
begin
  if IsTracking or IsAnimating then
    Result := Pos
  else
    Result := ThumbPosForState(Checked);
end;

function TEsCustomSwitch.GetThumbSize: TSize;
begin
  Result.Height := SwitchRect.Height;
  Result.Width := Result.Height;
end;

function TEsCustomSwitch.IsStoredTextOff: Boolean;
begin
  Result := FTextOff <> sDefaultSwitchOff;
end;

function TEsCustomSwitch.IsStoredTextOn: Boolean;
begin
  Result := FTextOn <> sDefaultSwitchOn;
end;

procedure TEsCustomSwitch.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    FChecked := not FChecked;

    if FChecked then
      State := ssOnNormal
    else
      State := ssOffNormal;

    if Animated then
      AnimateThumbTo(ThumbPosForState(not FChecked), FChecked);

    Invalidate;

    ChangeState;
  end;
end;

procedure TEsCustomSwitch.Loaded;
begin
  inherited;
  CalcTextSize;
  AdjustBounds;
end;

procedure TEsCustomSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    exit;

  SetFocus;

  Pos := ThumbPosForState(FChecked);
  DeltaPos := X - Pos;
  IsTracking := True;
  StartPos := TPoint.Create(X, Y);
  State := ssDrag;
  CanAutoCheck := True;

  if Animation <> nil then
    Animation.Stop;

  Invalidate;
end;

procedure TEsCustomSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if IsTracking then
  begin
    if (abs(StartPos.X - X) > TrackingSensitivity) or (abs(StartPos.Y - Y) > TrackingSensitivity) then
      CanAutoCheck := False;

    Pos := X - DeltaPos;
    if Pos < SwitchRect.Left then
      Pos := SwitchRect.Left;
    if Pos > SwitchRect.Right - ThumbSize.Width then
      Pos := SwitchRect.Right - ThumbSize.Width;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Old: Boolean;
begin
  inherited;

  if Button <> mbLeft then
    exit;

  IsTracking := False;

  Old := FChecked;
  if CanAutoCheck then
    FChecked := not FChecked
  else
    FChecked := Pos + ThumbSize.Width div 2 > SwitchRect.Left + SwitchRect.Width div 2;

  if FChecked then
    State := ssOnHot
  else
    State := ssOffHot;

  if FAnimated then
    AnimateThumbTo(Pos, FChecked);

  Invalidate;

  if Old <> FChecked then
    ChangeState;
end;

procedure TEsCustomSwitch.Paint;
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen: TGPPen;
  FrameColor: TAlphaColor;
  FillColor: TAlphaColor;
  LeftArc, RightArc: TGPRectF;
  FillPath, FramePath: TGPGraphicsPath;
  TextRect: TRect;
  Text: String;
begin
  Graphics := nil;
  Brush := nil;
  Pen := nil;
  FillPath := nil;
  FramePath := nil;
  try
    Graphics := TGPGraphics.Create(Canvas.Handle);
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    Brush := TGPSolidBrush.Create(0);
    Pen := TGPPen.Create(0, FSwitchBorder);

    // Left arc
    LeftArc.X := SwitchRect.Left;
    LeftArc.Y := SwitchRect.Top;
    LeftArc.Height := SwitchRect.Height;
    LeftArc.Width := LeftArc.Height;
    // Right arc
    RightArc.X := SwitchRect.Right - SwitchRect.Height;
    RightArc.Y := SwitchRect.Top;
    RightArc.Height := SwitchRect.Height;
    RightArc.Width := RightArc.Height;

    // Create path for Fill
    FillPath := TGPGraphicsPath.Create();
    FillPath.StartFigure;
    FillPath.AddArc(LeftArc, 90, 180);
    FillPath.AddArc(RightArc, -90, 180);
    FillPath.CloseFigure;

    // Create path for Frame
    FramePath := TGPGraphicsPath.Create();
    InflateGPRect(LeftArc, -FSwitchBorder / 2, -FSwitchBorder / 2);
    InflateGPRect(RightArc, -FSwitchBorder / 2, -FSwitchBorder / 2);
    FramePath.StartFigure;
    FramePath.AddArc(LeftArc, 90, 180);
    FramePath.AddArc(RightArc, -90, 180);
    FramePath.CloseFigure;

    FrameColor := FrameColorForState(State);
    FillColor := FillColorForState(State);
    if FillColor <> 0 then
    begin
      Brush.SetColor(FillColor);
      Graphics.FillPath(Brush, FillPath);
    end;
    if FrameColor <> 0 then
    begin
      Pen.SetColor(FrameColor);
      Graphics.DrawPath(Pen, FramePath);
    end;

    Brush.SetColor(ThumbColorForState(State));
    Graphics.FillEllipse(Brush,
        MakeRect(ThumbPos + FSwitchBorder + FThumbBorder, SwitchRect.Top + FSwitchBorder + FThumbBorder,
          ThumbSize.Width - FSwitchBorder * 2 - FThumbBorder * 2, ThumbSize.Height - FSwitchBorder * 2 - FThumbBorder * 2));

    if ShowCaption then
    begin
      if FChecked then
        Text := TextOn
      else
        Text := TextOff;

      Canvas.Brush.Style := bsClear;
      Canvas.Font := Font;
      if IsStyledFontControl(Self) then
        if Enabled then
          Canvas.Font.Color := FontColorToRgb(clBtnText, Self)
        else
          Canvas.Font.Color := FontColorToRgb(clGrayText, Self)
      else
        if not Enabled then
          Canvas.Font.Color := clGrayText;

      if Alignment = TSwitchAlignment.Right then
        TextRect := Rect(SwitchRect.Right + TextSpace, 0, ClientWidth, ClientHeight)
      else
        TextRect := Rect(SwitchRect.Left - TextSize.Width, 0, SwitchRect.Left, ClientHeight);
      Canvas.TextRect(TextRect, Text, [tfSingleLine, tfVerticalCenter]);
      Canvas.Brush.Style := bsSolid;
    end;

    if IsShowFocusRect(Self) {and IsDrawFocusRect} then
    begin
      Canvas.Brush.Color := TColor($FF000000);
      Canvas.DrawFocusRect(ClientRect);
    end;

  finally
    Graphics.Free;
    Brush.Free;
    Pen.Free;
    FillPath.Free;
    FramePath.Free;
  end;

  inherited;
end;

procedure TEsCustomSwitch.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TEsCustomSwitch.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;

    if FChecked then
      State := ssOnNormal
    else
      State := ssOffNormal;

    Invalidate;

    if not ClicksDisabled then
      ChangeState;
  end;
end;

procedure TEsCustomSwitch.SetFrameColor(const Value: TColor);
begin
  Style.FrameColor := Value;
end;

procedure TEsCustomSwitch.SetMainColor(const Value: TColor);
begin
  Style.MainColor := Value;
end;

procedure TEsCustomSwitch.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetAlignment(const Value: TSwitchAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetSwitchBorder(const Value: TSwitchBorder);
begin
  if FSwitchBorder <> Value then
  begin
    FSwitchBorder := Value;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetSwitchHeight(const Value: Integer);
begin
  if FSwitchHeight <> Value then
  begin
    FSwitchHeight := Value;
    if FAutoSize and (FSwitchLayout = TSwitchLayout.Fixed) then
      AdjustBounds;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetSwitchLayout(const Value: TSwitchLayout);
begin
  if FSwitchLayout <> Value then
  begin
    FSwitchLayout := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetSwitchWidth(const Value: Integer);
begin
  if FSwitchWidth <> Value then
  begin
    FSwitchWidth := Value;
    if FSwitchLayout = TSwitchLayout.Fixed then
      AdjustBounds;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetTextOff(const Value: string);
begin
  if FTextOff <> Value then
  begin
    FTextOff := Value;
    if not(csLoading in ComponentState) then
    begin
      CalcTextSize;
      AdjustBounds;
    end;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetTextOn(const Value: string);
begin
  if FTextOn <> Value then
  begin
    FTextOn := Value;
    if not(csLoading in ComponentState) then
    begin
      CalcTextSize;
      AdjustBounds;
    end;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetThumbBorder(const Value: TThumbBorder);
begin
  if FThumbBorder <> Value then
  begin
    FThumbBorder := Value;
    Invalidate;
  end;
end;

procedure TEsCustomSwitch.SetThumbColor(const Value: TColor);
begin
  Style.ThumbColor := Value;
end;

procedure TEsCustomSwitch.SetVerticalSpace(const Value: Cardinal);
begin
  FVerticalSpace := Value;
end;

function TEsCustomSwitch.ThumbColorForState(State: TSwitchState): TAlphaColor;
begin
  if Enabled then
    case State of
      ssOffNormal: Result := Style.OffNormalThumbColor;
      ssOffHot: Result := Style.OffHotThumbColor;
      ssDrag: Result := Style.DragThumbColor;
      ssOnNormal: Result := Style.OnNormalThumbColor;
      ssOnHot: Result := Style.OnHotThumbColor;
      else Result := 0;// for compiler paranoia
    end
  else
    if Checked then
      Result := Style.OnDisableThumbColor
    else
      Result := Style.OffDisableThumbColor;
end;

function TEsCustomSwitch.ThumbPosForState(IsChacked: Boolean): Integer;
begin
  if not IsChacked then
    Result := SwitchRect.Left
  else
    Result := SwitchRect.Right - ThumbSize.Width;
end;

{$IFDEF VER240UP}
procedure TEsCustomSwitch.UpdateStyleElements;
begin
  inherited;
  // Colors.UpdateColors;
  Invalidate;
end;
{$ENDIF}

procedure TEsCustomSwitch.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TEsCustomSwitch.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

{ TEsCustomSwitchActionLinkClass }

procedure TEsCustomSwitchActionLinkClass.AssignClient(AClient: TObject);
begin
  inherited;
  FClient := AClient as TEsCustomSwitch;
end;

function TEsCustomSwitchActionLinkClass.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (TEsCustomSwitch(FClient).Checked = TCustomAction(Action).Checked);
end;

procedure TEsCustomSwitchActionLinkClass.SetChecked(Value: Boolean);
begin
  inherited;
  if IsCheckedLinked then
  begin
    TEsCustomSwitch(FClient).ClicksDisabled := True;
    try
      TEsCustomSwitch(FClient).Checked := Value;
    finally
      TEsCustomSwitch(FClient).ClicksDisabled := False;
    end;
  end;
end;

{ TSwitchColorsProxy }

procedure TSwitchStyle.AssignTo(Dest: TPersistent);
begin
  if Dest is TSwitchStyle then
  begin
    TSwitchStyle(Dest).FMainColor := MainColor;
    TSwitchStyle(Dest).FFrameColor := FrameColor;
    TSwitchStyle(Dest).FThumbColor := ThumbColor;
    TSwitchStyle(Dest).Change;
  end
  else
    inherited;
end;

procedure TSwitchStyle.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSwitchStyle.Create;
begin
  FFrameColor := clDefault;
  FThumbColor := clDefault;
  FMainColor := clDefault;
end;

// TODO: refactor me PLEASE!!!
function TSwitchStyle.GetColor(const Index: Integer): TAlphaColor;
var
  LMainColor: TAlphaColor;
  LFrameColor: TAlphaColor;
  LThumbColor: TAlphaColor;
begin
  Result := 0; LMainColor := 0; LFrameColor := 0; LThumbColor := 0;// compiler paranoia

  case Index of
    1, 2, 4, 5, 7, 8, 15, 18:
    begin
      if FFrameColor = clDefault then
      begin
        if Assigned(FControl) and not IsStyledClientControl(FControl)  then
          LFrameColor := RgbToArgb(ColorToRGB(TEsSwitch(FControl).Font.Color), 0)
        else
          LFrameColor := RgbToArgb(ClientColorToRGB(clBtnText, FControl), 0);
      end else
        LFrameColor := RgbToArgb(ClientColorToRGB(FFrameColor, FControl), 0);
    end;

    9, 12:
    begin
      if FMainColor = clDefault then
      begin
        if IsStyledClientControl(FControl) then
          LMainColor := RgbToArgb(ClientColorToRGB(clHighlight, FControl))
        else
          if not GetMainColor(LMainColor) then
            LMainColor := RgbToArgb(ClientColorToRGB(clHighlight, FControl));
      end else
        LMainColor := RgbToArgb(ClientColorToRGB(FMainColor, FControl), 0);
      LMainColor := LMainColor and $00FFFFFF;
    end;

    else
    begin
      if FThumbColor = clDefault then
        LThumbColor := RgbToArgb(ClientColorToRGB(clWindow, FControl), 0)
      else
        LThumbColor := RgbToArgb(ClientColorToRGB(FThumbColor, FControl), 0);
    end;
  end;

  case Index of
    0: Result := 0;
    1: Result := LFrameColor or $CC000000;
    2: Result := LFrameColor or $CC000000;
    3: Result := 0;
    4: Result := LFrameColor or $FF000000;
    5: Result := LFrameColor or $FF000000;
    6: Result := 0;
    7: Result := LFrameColor or $33000000;
    8: Result := LFrameColor or $33000000;
    9: Result := LMainColor or $FF000000;
    10: Result := 0;
    11: Result := LThumbColor or $FF000000;
    12: Result := LMainColor or $CC000000;
    13: Result := 0;
    14: Result := LThumbColor or $FF000000;
    15: Result := LFrameColor or $99000000;
    16: Result := 0;
    17: Result := LThumbColor or $FF000000;
    18: Result := LFrameColor or $99000000;
    19: Result := 0;
    20: Result := LThumbColor or $FF000000;
  end;
end;

procedure TSwitchStyle.SetMainColor(const Value: TColor);
begin
  if FMainColor <> Value then
  begin
    FMainColor := Value;
    Change;
  end;
end;

procedure TSwitchStyle.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchStyle.SetThumbColor(const Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    Change;
  end;
end;

initialization
  {$IFDEF VER270UP}
  AddEnumElementAliases(TypeInfo(TSwitchAlignment), ['saLeft', 'saRight']);
  AddEnumElementAliases(TypeInfo(TSwitchLayout), ['slFixed', 'slAutoSize', 'slClient']);
  {$ENDIF}

finalization
  {$IFDEF VER270UP}
  RemoveEnumElementAliases(TypeInfo(TSwitchAlignment));
  RemoveEnumElementAliases(TypeInfo(TSwitchLayout));
  {$ENDIF}

end.
