{******************************************************************************}
{                           ESComponents for VCL                               }
{                            ErrorSoft(c) 2015                                 }
{                                                                              }
{            errorsoft@mail.ch | errorsoft-demoscene.narod.ru                  }
{ errorsoft@protonmail.ch | github.com/errorcalc | habrahabr.ru/user/error1024 }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.Switch;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  WinApi.Messages, ES.Vcl.ExGraphics, ES.Vcl.BaseControls, System.UITypes, ES.Vcl.CfxClasses;

type
  // Only internal use
  TSwitchColors = class(TPersistent)
  private
    FOffNormalThumbColor: TAlphaColor;
    FOffHotFrameColor: TAlphaColor;
    FOnHotFillColor: TAlphaColor;
    FDragThumbColor: TAlphaColor;
    FOnNormalThumbColor: TAlphaColor;
    FOffHotThumbColor: TAlphaColor;
    FIsSystemColors: Boolean;
    FOffNormalFrameColor: TAlphaColor;
    FDragFillColor: TAlphaColor;
    FOnNormalFillColor: TAlphaColor;
    FOnHotThumbColor: TAlphaColor;
    FOnChange: TNotifyEvent;
    FOffNormalFillColor: TAlphaColor;
    FOffHotFillColor: TAlphaColor;
    FOnNormalFrameColor: TAlphaColor;
    FOnHotFrameColor: TAlphaColor;
    FDragFrameColor: TAlphaColor;
    FOnDisableThumbColor: TAlphaColor;
    FOffDisableFillColor: TAlphaColor;
    FOffDisableFrameColor: TAlphaColor;
    FOnDisableFillColor: TAlphaColor;
    FOffDisableThumbColor: TAlphaColor;
    FOnDisableFrameColor: TAlphaColor;
    FControl: TWinControl;
    procedure SetDragFillColor(const Value: TAlphaColor);
    procedure SetDragThumbColor(const Value: TAlphaColor);
    procedure SetIsSystemColors(const Value: Boolean);
    procedure SetOffHotFrameColor(const Value: TAlphaColor);
    procedure SetOffHotThumbColor(const Value: TAlphaColor);
    procedure SetOffNormalFrameColor(const Value: TAlphaColor);
    procedure SetOffNormalThumbColor(const Value: TAlphaColor);
    procedure SetOnHotFillColor(const Value: TAlphaColor);
    procedure SetOnHotThumbColor(const Value: TAlphaColor);
    procedure SetOnNormalFillColor(const Value: TAlphaColor);
    procedure SetOnNormalThumbColor(const Value: TAlphaColor);
    function IsStroredColors: Boolean;
    procedure SetOffNormalFillColor(const Value: TAlphaColor);
    procedure SetOffHotFillColor(const Value: TAlphaColor);
    procedure SetOnNormalFrameColor(const Value: TAlphaColor);
    procedure SetOnHotFrameColor(const Value: TAlphaColor);
    procedure SetDragFrameColor(const Value: TAlphaColor);
    procedure SetOffDisableFillColor(const Value: TAlphaColor);
    procedure SetOffDisableFrameColor(const Value: TAlphaColor);
    procedure SetOffDisableThumbColor(const Value: TAlphaColor);
    procedure SetOnDisableFillColor(const Value: TAlphaColor);
    procedure SetOnDisableFrameColor(const Value: TAlphaColor);
    procedure SetOnDisableThumbColor(const Value: TAlphaColor);
    procedure SetControl(const Value: TWinControl);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Change; virtual;
  public
    constructor Create;
    procedure InitSysColors; dynamic;
    procedure InitDefaultColors; dynamic;
    procedure UpdateColors;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Control: TWinControl read FControl write SetControl;
  published
    property IsSystemColors: Boolean read FIsSystemColors write SetIsSystemColors default True;
    property OffNormalFillColor: TAlphaColor read FOffNormalFillColor write SetOffNormalFillColor stored IsStroredColors;
    property OffNormalFrameColor: TAlphaColor read FOffNormalFrameColor write SetOffNormalFrameColor stored IsStroredColors;
    property OffNormalThumbColor: TAlphaColor read FOffNormalThumbColor write SetOffNormalThumbColor stored IsStroredColors;
    property OffHotFillColor: TAlphaColor read FOffHotFillColor write SetOffHotFillColor stored IsStroredColors;
    property OffHotFrameColor: TAlphaColor read FOffHotFrameColor write SetOffHotFrameColor stored IsStroredColors;
    property OffHotThumbColor: TAlphaColor read FOffHotThumbColor write SetOffHotThumbColor stored IsStroredColors;
    property OffDisableFillColor: TAlphaColor read FOffDisableFillColor write SetOffDisableFillColor stored IsStroredColors;
    property OffDisableFrameColor: TAlphaColor read FOffDisableFrameColor write SetOffDisableFrameColor stored IsStroredColors;
    property OffDisableThumbColor: TAlphaColor read FOffDisableThumbColor write SetOffDisableThumbColor stored IsStroredColors;
    property OnNormalFillColor: TAlphaColor read FOnNormalFillColor write SetOnNormalFillColor stored IsStroredColors;
    property OnNormalFrameColor: TAlphaColor read FOnNormalFrameColor write SetOnNormalFrameColor stored IsStroredColors;
    property OnNormalThumbColor: TAlphaColor read FOnNormalThumbColor write SetOnNormalThumbColor stored IsStroredColors;
    property OnHotFillColor: TAlphaColor read FOnHotFillColor write SetOnHotFillColor stored IsStroredColors;
    property OnHotFrameColor: TAlphaColor read FOnHotFrameColor write SetOnHotFrameColor stored IsStroredColors;
    property OnHotThumbColor: TAlphaColor read FOnHotThumbColor write SetOnHotThumbColor stored IsStroredColors;
    property OnDisableFillColor: TAlphaColor read FOnDisableFillColor write SetOnDisableFillColor stored IsStroredColors;
    property OnDisableFrameColor: TAlphaColor read FOnDisableFrameColor write SetOnDisableFrameColor stored IsStroredColors;
    property OnDisableThumbColor: TAlphaColor read FOnDisableThumbColor write SetOnDisableThumbColor stored IsStroredColors;
    property DragFillColor: TAlphaColor read FDragFillColor write SetDragFillColor stored IsStroredColors;
    property DragFrameColor: TAlphaColor read FDragFrameColor write SetDragFrameColor stored IsStroredColors;
    property DragThumbColor: TAlphaColor read FDragThumbColor write SetDragThumbColor stored IsStroredColors;
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
  TSwitchLayout = (slFixed, slAutoSize, slClient);

  // now: only interal use, this class can be too refactored!
  TEsCustomSwitch = class(TEsCustomControl)
  type
    TSwitchState = (ssOffNormal, ssOffHot, ssDrag, ssOnNormal, ssOnHot);
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
    FColors: TSwitchColors;
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
    procedure SetColors(const Value: TSwitchColors);
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
  protected
    State: TSwitchState;
    IsTracking: Boolean;
    IsAnimating: Boolean;
    CanAutoCheck: Boolean;
    IsDrawFocusRect: Boolean;
    {$if CompilerVersion > 23}
    procedure UpdateStyleElements; override;
    {$ifend}
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    // for external styles
    function CreateColors: TSwitchColors; dynamic;
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
    property Colors: TSwitchColors read FColors write SetColors;
    property Checked: Boolean read FChecked write SetChecked default False;
    property ThumbBorder: TThumbBorder read FThumbBorder write SetThumbBorder default 3;
    property SwitchBorder: TSwitchBorder read FSwitchBorder write SetSwitchBorder default 2;
    property TextOn: string read FTextOn write SetTextOn stored IsStoredTextOn;
    property TextOff: string read FTextOff write SetTextOff stored IsStoredTextOff;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
    property Animated: Boolean read FAnimated write FAnimated default True;
    property SwitchWidth: Integer read FSwitchWidth write SetSwitchWidth default DefaultWidth;
    property SwitchHeight: Integer read FSwitchHeight write SetSwitchHeight default DefaultHeight;
    property SwitchLayot: TSwitchLayout read FSwitchLayout write SetSwitchLayout default slFixed;
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
    property Colors;
    property Checked;
    property ThumbBorder;
    property TextOn;
    property TextOff;
    property ShowCaption;
    property Animated;
    property SwitchWidth;
    property SwitchHeight;
    property SwitchLayot;
    property AutoSize;
    property VerticalSpace;
    //
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
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
    property IsTransparentMouse;// TEsCustomControl
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
    property StyleElements;
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
  System.Math, WinApi.GdipObj, WinApi.GdipApi, Vcl.Consts, Vcl.Themes, ES.Vcl.ExGdiPlus, WinApi.DwmApi,
  Vcl.Graphics, ES.Vcl.Utils, Vcl.ActnList, dialogs;

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
  if not(csLoading in ComponentState) and FAutoSize and (FSwitchLayout <> slCLient) then
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
  IsAnimating := True;
  Animation := TIntegerAnimation.Create;
  Animation.Mode := amCubic;
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
  Animation.Free;
  Animation := nil;
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
  if FChecked then
    State := ssOnHot
  else
    State := ssOffHot;
  Invalidate;
end;

procedure TEsCustomSwitch.CMMouseLeave(var Message: TMessage);
begin
  if FChecked then
    State := ssOnNormal
  else
    State := ssOffNormal;
  Invalidate;
end;

procedure TEsCustomSwitch.CMStyleChanged(var Message: TMessage);
begin
  FColors.UpdateColors;
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

  FColors := CreateColors;
  FColors.OnChange := ColorsChange;
  FColors.Control := Self;
  State := ssOffNormal;

  IsFullSizeBuffer := True;
  FSwitchWidth := DefaultWidth;
  FSwitchHeight := DefaultHeight;
  FAutoSize := True;
  FVerticalSpace := 0;
  FSwitchLayout := slFixed;
  FThumbBorder := 3;
  FSwitchBorder := 2;
  FTextOn := sDefaultSwitchOn;
  FTextOff := sDefaultSwitchOff;
  FAnimated := True;

  AdjustBounds;
end;

function TEsCustomSwitch.CreateColors: TSwitchColors;
begin
  Result := TSwitchColors.Create;
end;

destructor TEsCustomSwitch.Destroy;
begin
  FColors.Free;
  inherited;
end;

function TEsCustomSwitch.FillColorForState(State: TSwitchState): TAlphaColor;
begin
  if Enabled then
    case State of
      ssOffNormal: Result := Colors.OffNormalFillColor;
      ssOffHot: Result := Colors.OffHotFillColor;
      ssDrag: Result := Colors.DragFillColor;
      ssOnNormal: Result := Colors.OnNormalFillColor;
      ssOnHot: Result := Colors.OnHotFillColor;
      else Result := 0;// for compiler paranoia
    end
  else
    if Checked then
      Result := Colors.OnDisableFillColor
    else
      Result := Colors.OffDisableFillColor;
end;

function TEsCustomSwitch.FrameColorForState(State: TSwitchState): TAlphaColor;
begin
  if Enabled then
    case State of
      ssOffNormal: Result := Colors.OffNormalFrameColor;
      ssOffHot: Result := Colors.OffHotFrameColor;
      ssDrag: Result := Colors.DragFrameColor;
      ssOnNormal: Result := Colors.OnNormalFrameColor;
      ssOnHot: Result := Colors.OnHotFrameColor;
      else Result := 0;// for compiler paranoia
    end
  else
    if Checked then
      Result := Colors.OnDisableFrameColor
    else
      Result := Colors.OffDisableFrameColor;
end;

function TEsCustomSwitch.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TEsCustomSwitchActionLinkClass;
end;

function TEsCustomSwitch.GetSwitchRect: TRect;
var
  H: Integer;
begin
  case FSwitchLayout of
    slFixed:
    begin
      Result.Left := 0;
      Result.Top := Height div 2 - FSwitchHeight div 2;
      Result.Width := FSwitchWidth;
      Result.Height := FSwitchHeight;
    end;
    slAutoSize:
    begin
      H := TextSize.Height + 6;
      Result.Left := 0;
      Result.Top := Height div 2 - H div 2;
      Result.Width := Trunc(H * 2.2 + 0.5);
      Result.Height := H;
    end;
    slClient:
    begin
      Result.Top := 0;
      Result.Left := 0;
      Result.Height := Height;
      if ShowCaption then
        Result.Width := Width - TextSize.Width - TextSpace
      else
        Result.Width := Width;
    end;
  end;
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
  if Key = VK_SPACE then
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
        MakeRect(ThumbPos + SwitchRect.Left + FSwitchBorder + FThumbBorder, SwitchRect.Top + FSwitchBorder + FThumbBorder,
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

      TextRect := Rect(ClientWidth - TextSize.Width + TextSpace, 0, ClientWidth, ClientHeight);
      Canvas.TextRect(TextRect, Text, [tfSingleLine, tfVerticalCenter]);
      Canvas.Brush.Style := bsSolid;
    end;

    if IsShowFocusRect(Self) then
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

procedure TEsCustomSwitch.SetColors(const Value: TSwitchColors);
begin
  FColors.Assign(Value);
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
    if FAutoSize and (FSwitchLayout = slFixed) then
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
    if FSwitchLayout = slFixed then
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

procedure TEsCustomSwitch.SetVerticalSpace(const Value: Cardinal);
begin
  FVerticalSpace := Value;
end;

function TEsCustomSwitch.ThumbColorForState(State: TSwitchState): TAlphaColor;
begin
  if Enabled then
    case State of
      ssOffNormal: Result := Colors.OffNormalThumbColor;
      ssOffHot: Result := Colors.OffHotThumbColor;
      ssDrag: Result := Colors.DragThumbColor;
      ssOnNormal: Result := Colors.OnNormalThumbColor;
      ssOnHot: Result := Colors.OnHotThumbColor;
      else Result := 0;// for compiler paranoia
    end
  else
    if Checked then
      Result := Colors.OnDisableThumbColor
    else
      Result := Colors.OffDisableThumbColor;
end;

function TEsCustomSwitch.ThumbPosForState(IsChacked: Boolean): Integer;
begin
  if not IsChacked then
    Result := SwitchRect.Left
  else
    Result := SwitchRect.Right - ThumbSize.Width;
end;

{$if CompilerVersion > 23}
procedure TEsCustomSwitch.UpdateStyleElements;
begin
  inherited;
  Colors.UpdateColors;
  Invalidate;
end;
{$ifend}

procedure TEsCustomSwitch.WMKillFocus(var Message: TWMKillFocus);
begin
  Inherited;
  IsDrawFocusRect := False;
  Invalidate;
end;

procedure TEsCustomSwitch.WMSetFocus(var Message: TWMSetFocus);
begin
  Inherited;
  IsDrawFocusRect := True;
  Invalidate;
end;

{ TSwitchStyle }

procedure TSwitchColors.AssignTo(Dest: TPersistent);
var
  SwitchStyle: TSwitchColors;
begin
  if Dest is TSwitchColors then
  begin
    SwitchStyle := TSwitchColors(Dest);
    SwitchStyle.FOffNormalThumbColor := FOffNormalThumbColor;
    SwitchStyle.FOffHotFrameColor := FOffHotFrameColor;
    SwitchStyle.FOnHotFillColor := FOnHotFillColor;
    SwitchStyle.FDragThumbColor := FDragThumbColor;
    SwitchStyle.FOnNormalThumbColor := FOnNormalThumbColor;
    SwitchStyle.FOffHotThumbColor := FOffHotThumbColor;
    SwitchStyle.FIsSystemColors := FIsSystemColors;
    SwitchStyle.FOffNormalFrameColor := FOffNormalFrameColor;
    SwitchStyle.FDragFillColor := FDragFillColor;
    SwitchStyle.FOnNormalFillColor := FOnNormalFillColor;
    SwitchStyle.FOnHotThumbColor := FOnHotThumbColor;
    Change;
  end
  else
    inherited;
end;

procedure TSwitchColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSwitchColors.Create;
begin
  Inherited Create;
  FIsSystemColors := True;
  InitSysColors;
end;

procedure TSwitchColors.InitDefaultColors;
begin
  FOffNormalThumbColor := $CC000000;
  FOffNormalFillColor := 0;
  FOffNormalFrameColor := $CC000000;
  FOffHotThumbColor := $FF000000;
  FOffHotFillColor := 0;
  FOffHotFrameColor := $FF000000;
  FOffDisableThumbColor := $33000000;
  FOffDisableFillColor := 0;
  FOffDisableFrameColor := $33000000;
  // drag
  FDragThumbColor := $FFFFFFFF;
  FDragFillColor := $99000000;
  FDragFrameColor := 0;
  // on
  FOnNormalThumbColor := $FFFFFFFF;
  FOnNormalFillColor := $FF6362C7;
  FOnNormalFrameColor := 0;
  FOnHotThumbColor := $FFFFFFFF;
  FOnHotFillColor := $CC6362C7;
  FOnHotFrameColor := 0;
  FOnDisableThumbColor := $FFFFFFFF;
  FOnDisableFillColor := $99000000;
  FOnDisableFrameColor := 0;
end;

procedure TSwitchColors.InitSysColors;
var
  DwmColor: DWORD;
  DwnOpaqueBlend: BOOL;
begin
  if FIsSystemColors then
  begin
    if IsStyledClientControl(FControl) then
      DwmColor := RgbToArgb(StyleServices.GetSystemColor(clHighlight))
    else
      if (not CheckWin32Version(6, 0)) or (DwmGetColorizationColor(DwmColor, DwnOpaqueBlend) <> S_OK) then
        DwmColor := RgbToArgb(ColorToRGB(clHighlight));//$00D6696B;
    // off
    FOffNormalThumbColor := RgbToArgb(ClientColorToRGB(clBtnText, FControl), 0) or $CC000000;
    FOffNormalFillColor := 0;
    FOffNormalFrameColor := RgbToArgb(BorderColorToRGB(clBtnText, FControl), 0) or $CC000000;
    FOffHotThumbColor := RgbToArgb(ClientColorToRGB(clBtnText, FControl), 0) or $FF000000;
    FOffHotFillColor := 0;
    FOffHotFrameColor := RgbToArgb(BorderColorToRGB(clBtnText, FControl), 0) or $FF000000;
    FOffDisableThumbColor := RgbToArgb(ClientColorToRGB(clBtnText, FControl), 0) or $33000000;
    FOffDisableFillColor := 0;
    FOffDisableFrameColor := RgbToArgb(BorderColorToRGB(clBtnText, FControl), 0) or $33000000;
    // drag
    FDragThumbColor := RgbToArgb(ClientColorToRGB(clWindow, FControl), 0) or $FF000000;
    FDragFillColor := RgbToArgb(ClientColorToRGB(clBtnText, FControl), 0) or $99000000;
    FDragFrameColor := 0;
    // on
    FOnNormalThumbColor := RgbToArgb(ClientColorToRGB(clWindow, FControl), 0) or $FF000000;
    FOnNormalFillColor := (DwmColor and $00FFFFFF) or $FF000000;
    FOnNormalFrameColor := 0;
    FOnHotThumbColor := RgbToArgb(ClientColorToRGB(clWindow, FControl), 0) or $FF000000;
    FOnHotFillColor := (DwmColor and $00FFFFFF) or $CC000000;
    FOnHotFrameColor := 0;
    FOnDisableThumbColor := RgbToArgb(ClientColorToRGB(clWindow, FControl), 0) or $FF000000;
    FOnDisableFillColor := $99000000;
    FOnDisableFrameColor := 0;
  end;
end;

function TSwitchColors.IsStroredColors: Boolean;
begin
  Result := not IsSystemColors;
end;

procedure TSwitchColors.SetControl(const Value: TWinControl);
begin
  FControl := Value;
  if FIsSystemColors then
    InitSysColors;
end;

procedure TSwitchColors.SetDragFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FDragFillColor) then
  begin
    FDragFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetDragFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FDragFrameColor) then
  begin
    FDragFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetDragThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FDragThumbColor) then
  begin
    FDragThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetIsSystemColors(const Value: Boolean);
begin
  if Value <> FIsSystemColors then
  begin
    FIsSystemColors := Value;
    if Value then
      InitSysColors
    else
      InitDefaultColors;
  end;
end;

procedure TSwitchColors.SetOffDisableFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffDisableFillColor) then
  begin
    FOffDisableFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffDisableFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffDisableFrameColor) then
  begin
    FOffDisableFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffDisableThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffDisableThumbColor) then
  begin
    FOffDisableThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffHotFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffHotFillColor) then
  begin
    FOffHotFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffHotFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffHotFrameColor) then
  begin
    FOffHotFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffHotThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffHotThumbColor) then
  begin
    FOffHotThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffNormalFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffNormalFillColor) then
  begin
    FOffNormalFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffNormalFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffNormalFrameColor) then
  begin
    FOffNormalFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOffNormalThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOffNormalThumbColor) then
  begin
    FOffNormalThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnDisableFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnDisableFillColor) then
  begin
    FOnDisableFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnDisableFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnDisableFrameColor) then
  begin
    FOnDisableFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnDisableThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnDisableThumbColor) then
  begin
    FOnDisableThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnHotFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnHotFillColor) then
  begin
    FOnHotFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnHotFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnHotFrameColor) then
  begin
    FOnHotFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnHotThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnHotThumbColor) then
  begin
    FOnHotThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnNormalFillColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnNormalFillColor) then
  begin
    FOnNormalFillColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnNormalFrameColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnNormalFrameColor) then
  begin
    FOnNormalFrameColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.SetOnNormalThumbColor(const Value: TAlphaColor);
begin
  if (not FIsSystemColors) and (Value <> FOnNormalThumbColor) then
  begin
    FOnNormalThumbColor := Value;
    Change;
  end;
end;

procedure TSwitchColors.UpdateColors;
begin
  if FIsSystemColors then
    InitSysColors;
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

end.
