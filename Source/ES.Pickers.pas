{******************************************************************************}
{                                                                              }
{                       EsVclComponents/EsVclCore v4.9                         }
{                           errorsoft(c) 2009-2026                             }
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
unit ES.Pickers;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Controls, Vcl.Forms,
  Winapi.Messages, Vcl.Graphics, System.UITypes, ES.Utils, ES.ExGraphics, ES.CfxClasses, ES.BaseControls;

type
  { TLinePickerOrientation }

  TLinePickerOrientation = (Horizontal, Vertical);

  { TLinePickerLayout }

  TLinePickerLayout = (ExternalArrows, InternalArrows, CenterArrows);

  { TLinePickerThumbLayout }

  TLinePickerThumbLayout = (First, Second, Both);

  { TLinePickerRange }

  TLinePickerRange = record
    Min: Integer;
    Max: Integer;
  end;

  { TEsAbstractLinePicker }

  TEsAbstractLinePicker = class(TEsCustomControl)
  strict private const
    DefaultThumbExternalSize = 8;
    DefaultThumbInternalSize = 7;
    DefaultThumbCenterSize = 8;
  strict private
    FLayout: TLinePickerLayout;
    FAllowFocus: Boolean;
    FRangeBorderVisible: Boolean;
    FRangeBorderSpace: Integer;
    FThumbHeight: Integer;
    FRangeBorderFocusedColor: TColor;
    FThumbAutoSize: Boolean;
    FThumbLayout: TLinePickerThumbLayout;
    FReverse: Boolean;
    FRangePadding: TPadding;
    FThumbLineColor: TColor;
    FThumbWidth: Integer;
    FOnUserChange: TNotifyEvent;
    FThumbLineFocusedColor: TColor;
    FThumbColor: TColor;
    FRangeAutoPadding: Boolean;
    FOnChange: TNotifyEvent;
    FThumbSpace: Integer;
    FRangeBorderWidth: Integer;
    FOrientation: TLinePickerOrientation;
    FThumbFocusedColor: TColor;
    FRangeBorderColor: TColor;
    FRangeBitmap: TBitmap;
    FNeedUpdateRangeBitmap: Boolean;
    FPositionLineVisible: Boolean;
    procedure SetLayout(Value: TLinePickerLayout);
    procedure SetOrientation(Value: TLinePickerOrientation);
    procedure SetRangeAutoPadding(Value: Boolean);
    procedure SetRangePadding(Value: TPadding);
    procedure SetThumbAutoSize(Value: Boolean);
    procedure SetThumbHeight(Value: Integer);
    procedure SetThumbLayout(Value: TLinePickerThumbLayout);
    procedure SetThumbWidth(Value: Integer);
    procedure AdjustContent();
    procedure RangePaddingChange(Sender: TObject);
    procedure SetPositionLineVisible(Value: Boolean);
    procedure SetRangeBorderColor(Value: TColor);
    procedure SetRangeBorderFocusedColor(Value: TColor);
    procedure SetRangeBorderWidth(Value: Integer);
    procedure SetRangeBorderSpace(Value: Integer);
    procedure SetRangeBorderVisible(Value: Boolean);
    procedure SetReverse(Value: Boolean);
    procedure SetThumbColor(Value: TColor);
    procedure SetThumbFocusedColor(Value: TColor);
    procedure SetThumbLineColor(Value: TColor);
    procedure SetThumbLineFocusedColor(Value: TColor);
    procedure SetThumbSpace(Value: Integer);
    function GetPickerRangeRect(): TRect;
    function GetRangePaddingStored(): Boolean;
    function IsThumbHeightStored(): Boolean;
    function IsThumbWidthStored(): Boolean;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsAllowFocus(): Boolean; override;
    procedure DoEnter(); override;
    procedure DoExit(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(); override;
    procedure Resize(); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    function GetPickerAbsolutePosition(Width: Integer): Integer; virtual; abstract;
    procedure MovePicker(Delta: Integer; Alternative: Boolean); virtual; abstract;
    procedure PaintRangeBitmap(Bitmap: TBitmap); virtual; abstract;
    procedure PickerChanged();
    procedure PickerInvalidate(NeedUpdateRangeBitmap: Boolean);
    procedure PickerRepaint(NeedUpdateRangeBitmap: Boolean);
    procedure PickerUserChanged();
    procedure SetPickerAbsolutePosition(Width, Position: Integer); virtual; abstract;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
  published
    property Align;
    property AllowFocus: Boolean read FAllowFocus write FAllowFocus default True;
    property Anchors;
    property BidiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property DoubleBuffered;
    {$IFDEF VER360UP}
    property DoubleBufferedMode;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property IsDrawHelper;// TEsCustomControl
    property IsOpaque;// TEsCustomControl
    property Layout: TLinePickerLayout read FLayout write SetLayout default TLinePickerLayout.ExternalArrows;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnResize;
    property OnStartDrag;
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
    property Orientation: TLinePickerOrientation read FOrientation write SetOrientation default TLinePickerOrientation.Horizontal;
    property PositionLineVisible: Boolean read FPositionLineVisible write SetPositionLineVisible default False;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RangeBorderColor: TColor read FRangeBorderColor write SetRangeBorderColor default clDefault;
    property RangeBorderFocusedColor: TColor read FRangeBorderFocusedColor write SetRangeBorderFocusedColor default clDefault;
    property RangeBorderWidth: Integer read FRangeBorderWidth write SetRangeBorderWidth default 1;
    property RangeBorderSpace: Integer read FRangeBorderSpace write SetRangeBorderSpace default 0;
    property RangeBorderVisible: Boolean read FRangeBorderVisible write SetRangeBorderVisible default True;
    property RangePadding: TPadding read FRangePadding write SetRangePadding stored GetRangePaddingStored;
    property RangeAutoPadding: Boolean read FRangeAutoPadding write SetRangeAutoPadding default True;
    property Reverse: Boolean read FReverse write SetReverse default False;
    property ShowHint;
    property Touch;
    property TabOrder;
    property TabStop default True;
    property ThumbColor: TColor read FThumbColor write SetThumbColor default clDefault;
    property ThumbFocusedColor: TColor read FThumbFocusedColor write SetThumbFocusedColor default clDefault;
    property ThumbLayout: TLinePickerThumbLayout read FThumbLayout write SetThumbLayout default TLinePickerThumbLayout.Both;
    property ThumbLineColor: TColor read FThumbLineColor write SetThumbLineColor default clDefault;
    property ThumbLineFocusedColor: TColor read FThumbLineFocusedColor write SetThumbLineFocusedColor default clDefault;
    property ThumbWidth: Integer read FThumbWidth write SetThumbWidth stored IsThumbWidthStored;
    property ThumbHeight: Integer read FThumbHeight write SetThumbHeight stored IsThumbHeightStored;
    property ThumbAutoSize: Boolean read FThumbAutoSize write SetThumbAutoSize default True;
    property ThumbSpace: Integer read FThumbSpace write SetThumbSpace default 0;
    property Visible;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
  end;

  { TEsCustomLinePicker }

  TEsCustomLinePicker = class(TEsAbstractLinePicker)
  strict private
    FIncrement: Integer;
    FIncrementMultiplier: Integer;
    FMax: Integer;
    FMin: Integer;
    function IsIncrementMultiplierStored(): Boolean;
    function IsIncrementStored(): Boolean;
    function IsMaxStored(): Boolean;
    function IsMinStored(): Boolean;
    procedure SetIncrement(Value: Integer);
    procedure SetIncrementMultiplier(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure UserUpdatePickerValue(Value: Integer);
  protected
    function GetPickerAbsolutePosition(Width: Integer): Integer; override;
    procedure MovePicker(Delta: Integer; Alternative: Boolean); override;
    procedure SetPickerAbsolutePosition(Width, Position: Integer); override;
  protected
    function GetDefaultIncrement(): Integer; virtual;
    function GetDefaultIncrementMultiplier(): Integer; virtual;
    function GetDefaultRange(): TLinePickerRange; virtual;
    function GetPickerValue(): Integer; virtual; abstract;
    function GetRange(): TLinePickerRange; virtual;
    procedure PickerChange(NeedUpdateRangeBitmap: Boolean);
    procedure SetPickerValue(Value: Integer); virtual; abstract;
  public
    constructor Create(Owner: TComponent); override;
  published
    property Increment: Integer read FIncrement write SetIncrement stored IsIncrementStored;
    property IncrementMultiplier: Integer read FIncrementMultiplier write SetIncrementMultiplier stored IsIncrementMultiplierStored;
    property Max: Integer read FMax write SetMax stored IsMaxStored;
    property Min: Integer read FMin write SetMin stored IsMinStored;
  end;

  { TLinePickerShader1D }

  TLinePickerShader1D = function (Value: Integer): TAlphaColor of object;

  { TLinePickerShader2D }

  TLinePickerShader2D = function (ValueX: Integer; ValueY: Double): TAlphaColor of object;

  { TEsShaderLinePicker }

  TEsShaderLinePicker = class(TEsCustomLinePicker)
  strict private
    FAlphaModulation: Boolean;
    FAlphaPreview: Byte;
    FRangeBackground: TTransparentBackground;
    procedure DrawRangeBitmap1D(Bitmap: TBitmap; Shader: TLinePickerShader1D);
    procedure DrawRangeBitmap2D(Bitmap: TBitmap; Shader: TLinePickerShader2D);
    procedure RangeBackgroundChange(Sender: TObject);
    procedure SetAlphaModulation(Value: Boolean);
    procedure SetAlphaPreview(Value: Byte);
    procedure SetRangeBackground(Value: TTransparentBackground);
  protected
    function GetDefaultIncrement(): Integer; override;
    function GetDefaultIncrementMultiplier(): Integer; override;
    function GetRange(): TLinePickerRange; override;
    procedure PaintRangeBitmap(Bitmap: TBitmap); override;
  protected
    function GetPickerRangeOpacity(): Byte; virtual;
    function GetPickerRangeShader1D(): TLinePickerShader1D; virtual;
    function GetPickerRangeShader2D(): TLinePickerShader2D; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    property AlphaPreview: Byte read FAlphaPreview write SetAlphaPreview default 255;
    property AlphaModulation: Boolean read FAlphaModulation write SetAlphaModulation default True;
    property RangeBackground: TTransparentBackground read FRangeBackground write SetRangeBackground;
  end;

  { TRgbLinePickerKind }

  TRgbLinePickerKind = (Red, Green, Blue);

  { TEsRgbLinePicker }

  TEsRgbLinePicker = class(TEsShaderLinePicker)
  strict private
    FBlue: Byte;
    FColorModulation: Boolean;
    FGreen: Byte;
    FKind: TRgbLinePickerKind;
    FRed: Byte;
    function GetCurrentColor(): TColor;
    function ShaderBlue(Value: Integer): TAlphaColor;
    function ShaderGreen(Value: Integer): TAlphaColor;
    function ShaderModulatedBlue(Value: Integer): TAlphaColor;
    function ShaderModulatedGreen(Value: Integer): TAlphaColor;
    function ShaderModulatedRed(Value: Integer): TAlphaColor;
    function ShaderRed(Value: Integer): TAlphaColor;
    procedure SetBlue(Value: Byte);
    procedure SetColorModulation(Value: Boolean);
    procedure SetCurrentColor(Value: TColor);
    procedure SetGreen(Value: Byte);
    procedure SetKind(Value: TRgbLinePickerKind);
    procedure SetRed(Value: Byte);
    procedure UpdateRgb(Red, Green, Blue: Byte);
  protected
    function GetPickerRangeShader1D(): TLinePickerShader1D; override;
    function GetPickerValue(): Integer; override;
    procedure SetPickerValue(Value: Integer); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure GetHsl(out Hue, Saturation, Lightness: Double);
    procedure GetHsv(out Hue, Saturation, Value: Double);
    procedure GetRgb(out Red, Green, Blue: Byte);
    procedure SetHsl(Hue, Saturation, Lightness: Double);
    procedure SetHsv(Hue, Saturation, Value: Double);
    procedure SetRgb(Red, Green, Blue: Byte);
  published
    property AlphaPreview;
    property AlphaModulation;
    property Blue: Byte read FBlue write SetBlue default 127;
    property ColorModulation: Boolean read FColorModulation write SetColorModulation default True;
    property CurrentColor: TColor read GetCurrentColor write SetCurrentColor stored False nodefault;
    property Green: Byte read FGreen write SetGreen default 127;
    property Kind: TRgbLinePickerKind read FKind write SetKind;
    property RangeBackground;
    property Red: Byte read FRed write SetRed default 127;
  end;

  { TEsAlphaLinePicker }

  TEsAlphaLinePicker = class(TEsShaderLinePicker)
  strict private
    FAlpha: Byte;
    FColorModulation: Boolean;
    FColorPreview: TColor;
    function ShaderGreenBlueRedGradient(ValueX: Integer; ValueY: Double): TAlphaColor;
    function ShaderModulated(Value: Integer): TAlphaColor;
    procedure SetAlpha(Value: Byte);
    procedure SetColorModulation(Value: Boolean);
    procedure SetColorPreview(Value: TColor);
    procedure UpdateRgbPreview(Red, Green, Blue: Byte);
  protected
    function GetPickerRangeShader1D(): TLinePickerShader1D; override;
    function GetPickerRangeShader2D(): TLinePickerShader2D; override;
    function GetPickerValue(): Integer; override;
    procedure SetPickerValue(Value: Integer); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure GetHslPreview(out Hue, Saturation, Lightness: Double);
    procedure GetHsvPreview(out Hue, Saturation, Value: Double);
    procedure GetRgbPreview(out Red, Green, Blue: Byte);
    procedure SetHslPreview(Hue, Saturation, Lightness: Double);
    procedure SetHsvPreview(Hue, Saturation, Value: Double);
    procedure SetRgbPreview(Red, Green, Blue: Byte);
  published
    property RangeBackground;
    property Alpha: Byte read FAlpha write SetAlpha default 255;
    property ColorModulation: Boolean read FColorModulation write SetColorModulation default True;
    property ColorPreview: TColor read FColorPreview write SetColorPreview default $007F7F7F;
  end;

implementation

type
  TEsAlphaColorHelper = record helper for TAlphaColor
  public
    constructor Create(R, G, B, A: Byte); overload;
    constructor Create(R, G, B: Byte); overload;
  end;

{ TEsAlphaColorHelper }

constructor TEsAlphaColorHelper.Create(R, G, B, A: Byte);
begin
  TAlphaColorRec(Self).R := R;
  TAlphaColorRec(Self).G := G;
  TAlphaColorRec(Self).B := B;
  TAlphaColorRec(Self).A := A;
end;

constructor TEsAlphaColorHelper.Create(R, G, B: Byte);
begin
  TAlphaColorRec(Self).R := R;
  TAlphaColorRec(Self).G := G;
  TAlphaColorRec(Self).B := B;
  TAlphaColorRec(Self).A := 255;
end;

{ TEsAbstractLinePicker }

constructor TEsAbstractLinePicker.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  // make bitmaps
  FRangeBitmap := TBitmap.Create();
  FRangeBitmap.PixelFormat := pf32bit;

  // make padding size
  FRangePadding := TPadding.Create(nil);
  FRangePadding.OnChange := RangePaddingChange;

  // defaults
  FAllowFocus := True;
  FLayout := TLinePickerLayout.ExternalArrows;
  FOrientation := TLinePickerOrientation.Horizontal;
  FRangeBorderColor := clDefault;
  FRangeBorderFocusedColor := clDefault;
  FRangeBorderWidth := 1;
  FRangeBorderSpace := 0;
  FRangeBorderVisible := True;
  FReverse := False;
  FThumbColor := clDefault;
  FThumbFocusedColor := clDefault;
  FThumbLayout := TLinePickerThumbLayout.Both;
  FThumbLineColor := clDefault;
  FThumbLineFocusedColor := clDefault;
  FThumbSpace := 0;
  FPositionLineVisible := False;
  FRangeAutoPadding := True;
  FThumbAutoSize := True;

  // inherited defaults
  Width := 180;
  Height := 24;
  TabStop := True;
  ControlStyle := ControlStyle + [csCaptureMouse];

  // need update
  FNeedUpdateRangeBitmap := True;
end;

destructor TEsAbstractLinePicker.Destroy();
begin
  FRangeBitmap.Free();
  FRangePadding.Free();

  inherited Destroy();
end;


procedure TEsAbstractLinePicker.ChangeScale(M, D: Integer{$IFDEF VER310UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;

  FThumbSpace := MulDiv(FThumbSpace, M, D);

  FThumbWidth := MulDiv(FThumbWidth, M, D);
  FThumbHeight := MulDiv(FThumbHeight, M, D);

  FRangePadding.Left := MulDiv(FRangePadding.Left, M, D);
  FRangePadding.Right := MulDiv(FRangePadding.Right, M, D);
  FRangePadding.Top := MulDiv(FRangePadding.Top, M, D);
  FRangePadding.Bottom := MulDiv(FRangePadding.Bottom, M, D);
end;

procedure TEsAbstractLinePicker.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  if FOrientation = TLinePickerOrientation.Horizontal then
  begin
    Msg.Result := Integer((Msg.CharCode = VK_LEFT) or (Msg.CharCode = VK_RIGHT));
  end else
  begin
    Msg.Result := Integer((Msg.CharCode = VK_UP) or (Msg.CharCode = VK_DOWN));
  end;
end;

procedure TEsAbstractLinePicker.AdjustContent();
var
  FirstThumb, SecondThumb: Boolean;
begin
  if csLoading in ComponentState then
  begin
    exit;
  end;

  if FThumbAutoSize then
  begin
    case FLayout of
      TLinePickerLayout.ExternalArrows:
      begin
        FThumbWidth := MulDiv(DefaultThumbExternalSize, CurrentPPI, 96);
        FThumbHeight := MulDiv(DefaultThumbExternalSize, CurrentPPI, 96);
      end;
      TLinePickerLayout.InternalArrows:
      begin
        FThumbWidth := MulDiv(DefaultThumbInternalSize, CurrentPPI, 96);
        FThumbHeight := MulDiv(DefaultThumbInternalSize, CurrentPPI, 96);
      end;
      TLinePickerLayout.CenterArrows:
      begin
        FThumbWidth := MulDiv(DefaultThumbCenterSize, CurrentPPI, 96);
        FThumbHeight := MulDiv(DefaultThumbCenterSize, CurrentPPI, 96);
      end;
    end;
  end else
  begin
    FThumbWidth := EnsureRange(FThumbWidth, 4, 64);
    FThumbHeight := EnsureRange(FThumbHeight, 4, 64);
  end;

  FirstThumb := FThumbLayout in [TLinePickerThumbLayout.First, TLinePickerThumbLayout.Both];
  SecondThumb := FThumbLayout in [TLinePickerThumbLayout.Second, TLinePickerThumbLayout.Both];
  if Orientation = TLinePickerOrientation.Horizontal then
  begin
    case FLayout of
      TLinePickerLayout.ExternalArrows, TLinePickerLayout.CenterArrows:
      begin
        if FRangeAutoPadding then
        begin
          FRangePadding.Left := System.Math.Max(
            FThumbWidth div 2,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
          FRangePadding.Right := System.Math.Max(
            FThumbWidth div 2,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
          FRangePadding.Top := System.Math.Max(
            Integer(FirstThumb) * FThumbHeight div (Integer(FLayout = TLinePickerLayout.CenterArrows) + 1) + FThumbSpace,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
          FRangePadding.Bottom := System.Math.Max(
            Integer(SecondThumb) * FThumbHeight div (Integer(FLayout = TLinePickerLayout.CenterArrows) + 1) + FThumbSpace,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
        end;
      end;
      TLinePickerLayout.InternalArrows:
      begin
        if FRangeAutoPadding then
        begin
          FRangePadding.Left := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
          FRangePadding.Right := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
          FRangePadding.Top := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
          FRangePadding.Bottom := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
        end;
      end;
    end;
  end else
  begin
    case FLayout of
      TLinePickerLayout.ExternalArrows, TLinePickerLayout.CenterArrows:
      begin
        if FRangeAutoPadding then
        begin
          FRangePadding.Top := System.Math.Max(
            FThumbHeight div 2,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
          FRangePadding.Bottom := System.Math.Max(
            FThumbHeight div 2,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
          FRangePadding.Left := System.Math.Max(
            Integer(FirstThumb) * FThumbWidth div (Integer(FLayout = TLinePickerLayout.CenterArrows) + 1) + FThumbSpace,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
          FRangePadding.Right := System.Math.Max(
            Integer(SecondThumb) * FThumbWidth div (Integer(FLayout = TLinePickerLayout.CenterArrows) + 1) + FThumbSpace,
            Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace
          );
        end;
      end;
      TLinePickerLayout.InternalArrows:
      begin
        if FRangeAutoPadding then
        begin
          FRangePadding.Top := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
          FRangePadding.Bottom := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
          FRangePadding.Left := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
          FRangePadding.Right := Integer(FRangeBorderVisible) * FRangeBorderWidth + Integer(FRangeBorderVisible) * FRangeBorderSpace;
        end;
      end;
    end;
  end;
end;

procedure TEsAbstractLinePicker.DoEnter();
begin
  inherited DoEnter();

  Invalidate();
end;

procedure TEsAbstractLinePicker.DoExit();
begin
  inherited DoExit();

  Invalidate();
end;

function TEsAbstractLinePicker.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Delta: Integer;
begin
  inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  Result := True;

  if WheelDelta < 0 then
  begin
    Delta := System.Math.Min(-1, WheelDelta div 120);
  end else
  begin
    Delta := System.Math.Max(1, WheelDelta div 120);
  end;

  if FReverse then
  begin
    Delta := -Delta;
  end;

  MovePicker(Delta, ssCtrl in Shift);
end;

function TEsAbstractLinePicker.GetPickerRangeRect(): TRect;
begin
  Result := TRect.Create(
    ClientRect.Left + FRangePadding.Left,
    ClientRect.Top + FRangePadding.Top,
    ClientRect.Right - FRangePadding.Right,
    ClientRect.Bottom - FRangePadding.Bottom
  );
end;

function TEsAbstractLinePicker.GetRangePaddingStored(): Boolean;
begin
  Result := not FRangeAutoPadding;
end;

function TEsAbstractLinePicker.IsAllowFocus(): Boolean;
begin
  Result := FAllowFocus;
end;

function TEsAbstractLinePicker.IsThumbHeightStored(): Boolean;
begin
  Result := not FThumbAutoSize;
end;

function TEsAbstractLinePicker.IsThumbWidthStored(): Boolean;
begin
  Result := not FThumbAutoSize;
end;

procedure TEsAbstractLinePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  if FReverse then
  begin
  	Delta := -1;
  end else
  begin
    Delta := 1;
  end;

  if FOrientation = TLinePickerOrientation.Horizontal then
  begin
    case Key of
      VK_LEFT:
      begin
        MovePicker(-Delta, ssCtrl in Shift);
      end;
      VK_RIGHT:
      begin
        MovePicker(Delta, ssCtrl in Shift);
      end;
    end;
  end else
  begin
    case Key of
      VK_DOWN:
      begin
        MovePicker(Delta, ssCtrl in Shift);
      end;
      VK_UP:
      begin
        MovePicker(-Delta, ssCtrl in Shift);
      end;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TEsAbstractLinePicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    TrySetFocus();

    if FOrientation = TLinePickerOrientation.Horizontal then
    begin
      SetPickerAbsolutePosition(GetPickerRangeRect().Width, X - GetPickerRangeRect().Left);
    end else
    begin
      SetPickerAbsolutePosition(GetPickerRangeRect().Height, Y - GetPickerRangeRect().Top);
    end;
  end;
end;

procedure TEsAbstractLinePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if csLButtonDown in ControlState then
  begin
    if FOrientation = TLinePickerOrientation.Horizontal then
    begin
      SetPickerAbsolutePosition(GetPickerRangeRect().Width, X - GetPickerRangeRect().Left);
    end else
    begin
      SetPickerAbsolutePosition(GetPickerRangeRect().Height, Y - GetPickerRangeRect().Top);
    end;
  end;
end;

procedure TEsAbstractLinePicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    if FOrientation = TLinePickerOrientation.Horizontal then
    begin
      SetPickerAbsolutePosition(GetPickerRangeRect().Width, X - GetPickerRangeRect().Left);
    end else
    begin
      SetPickerAbsolutePosition(GetPickerRangeRect().Height, Y - GetPickerRangeRect().Top);
    end;
  end;
end;

procedure TEsAbstractLinePicker.Paint();
var
  FillColor, StrokeColor: TColor;
  FrameRect: TRect;
  RangeRect: TRect;
  Offset: Integer;
  Size: Integer;
  Index: Integer;
begin
  inherited Paint();

  RangeRect := GetPickerRangeRect();

  // update FRangeBitmap
  if FNeedUpdateRangeBitmap then
  begin
    FRangeBitmap.SetSize(RangeRect.Width, RangeRect.Height);
    if (FRangeBitmap.Width <> 0) and (FRangeBitmap.Height <> 0) then
    begin
    	PaintRangeBitmap(FRangeBitmap);
    end;
    FNeedUpdateRangeBitmap := False;
  end;

  // draw border
  if FRangeBorderVisible then
  begin
    FrameRect := RangeRect;
    InflateRect(FrameRect, FRangeBorderSpace + FRangeBorderWidth, FRangeBorderSpace + FRangeBorderWidth);

    if Focused then
    begin
      Canvas.DrawInsideFrame(FrameRect, FRangeBorderWidth, ColorToRgb(IfThen(FRangeBorderFocusedColor = clDefault, BorderColorToRgb(clHighlight, Self), FRangeBorderFocusedColor)));
    end else
    begin
      Canvas.DrawInsideFrame(FrameRect, FRangeBorderWidth, ColorToRgb(IfThen(FRangeBorderColor = clDefault, BorderColorToRgb(clGrayText, Self), FRangeBorderColor)));
    end;
  end;

  // draw range
  Canvas.Draw(RangeRect.Left, RangeRect.Top, FRangeBitmap);

  // choose thumb color
  if Focused then
  begin
    FillColor := ColorToRGB(IfThen(FThumbFocusedColor = clDefault, ClientColorToRgb(clHighlight, Self), FThumbFocusedColor));
    StrokeColor := ColorToRGB(IfThen(FThumbLineFocusedColor = clDefault, ClientColorToRgb(clHighlightText, Self), FThumbLineFocusedColor));
  end else
  begin
    FillColor := ColorToRGB(IfThen(FThumbColor = clDefault, ClientColorToRgb(clWindowText, Self), FThumbColor));
    StrokeColor := ColorToRGB(IfThen(FThumbLineColor = clDefault, ClientColorToRgb(clWindow, Self), FThumbLineColor));
  end;

  // draw thumb
  if FOrientation = TLinePickerOrientation.Horizontal then
  begin
    // calc disp offset
    Offset := GetPickerAbsolutePosition(RangeRect.Width);

    // draw position line
    if FPositionLineVisible then
    begin
      Index := SaveDC(Canvas.Handle);
      try
        IntersectClipRect(Canvas.Handle, RangeRect.Left, RangeRect.Top, RangeRect.Right, RangeRect.Bottom);

        DrawAxisLine(Canvas, RangeRect.Left + Offset, RangeRect.Top, RangeRect.Height, True, FillColor, StrokeColor, 160);
      finally
        RestoreDC(Canvas.Handle, Index);
      end;
    end;

    // draw thumb
    Size := FThumbWidth + Integer(not Odd(FThumbWidth));
    case FLayout of
      TLinePickerLayout.ExternalArrows, TLinePickerLayout.CenterArrows:
      begin
        // top
        if FThumbLayout in [TLinePickerThumbLayout.First, TLinePickerThumbLayout.Both] then
        begin
          DrawTriangleThumb(
            Canvas,
            RangeRect.Left + Offset - Size div 2,
            RangeRect.Top - FThumbHeight div (Integer(FLayout = TLinePickerLayout.CenterArrows) + 1) - FThumbSpace,
            Size,
            FThumbHeight,
            TTriangleThumbOrientation.Down,
            FillColor,
            StrokeColor
          );
        end;
        // bottom
        if FThumbLayout in [TLinePickerThumbLayout.Second, TLinePickerThumbLayout.Both] then
        begin
          DrawTriangleThumb(
            Canvas,
            RangeRect.Left + Offset - Size div 2,
            RangeRect.Bottom - (FThumbHeight div 2 * Integer(FLayout = TLinePickerLayout.CenterArrows)) + FThumbSpace,
            Size,
            FThumbHeight,
            TTriangleThumbOrientation.Up,
            FillColor,
            StrokeColor
          );
        end;
      end;
      TLinePickerLayout.InternalArrows:
      begin
        Index := SaveDC(Canvas.Handle);
        try
          IntersectClipRect(Canvas.Handle, RangeRect.Left, RangeRect.Top, RangeRect.Right, RangeRect.Bottom);
          // top
          if FThumbLayout in [TLinePickerThumbLayout.First, TLinePickerThumbLayout.Both] then
          begin
            DrawTriangleThumb(
              Canvas,
              RangeRect.Left + Offset - Size div 2,
              RangeRect.Top + FThumbSpace,
              Size,
              FThumbHeight,
              TTriangleThumbOrientation.Down,
              FillColor,
              StrokeColor
            );
          end;
          // bottom
          if FThumbLayout in [TLinePickerThumbLayout.Second, TLinePickerThumbLayout.Both] then
          begin
            DrawTriangleThumb(
              Canvas,
              RangeRect.Left + Offset - Size div 2,
              RangeRect.Bottom - FThumbHeight - FThumbSpace,
              Size,
              FThumbHeight,
              TTriangleThumbOrientation.Up,
              FillColor,
              StrokeColor
            );
          end;
        finally
          RestoreDC(Canvas.Handle, Index);
        end;
      end;
    end;
  end else
  begin
    // calc disp offset
    Offset := GetPickerAbsolutePosition(RangeRect.Height);

    // draw position line
    if FPositionLineVisible then
    begin
      Index := SaveDC(Canvas.Handle);
      try
        IntersectClipRect(Canvas.Handle, RangeRect.Left, RangeRect.Top, RangeRect.Right, RangeRect.Bottom);

        DrawAxisLine(Canvas, RangeRect.Left, RangeRect.Top + Offset, RangeRect.Width, False, FillColor, StrokeColor, 160);
      finally
        RestoreDC(Canvas.Handle, Index);
      end;
    end;

    // draw thumb
    Size := FThumbHeight + Integer(not Odd(FThumbHeight));
    case FLayout of
      TLinePickerLayout.ExternalArrows, TLinePickerLayout.CenterArrows:
      begin
        // left
        if FThumbLayout in [TLinePickerThumbLayout.First, TLinePickerThumbLayout.Both] then
        begin
          DrawTriangleThumb(
            Canvas,
            RangeRect.Left - FThumbWidth div (Integer(FLayout = TLinePickerLayout.CenterArrows) + 1) - FThumbSpace,
            RangeRect.Top + Offset - Size div 2,
            FThumbWidth,
            Size,
            TTriangleThumbOrientation.Right,
            FillColor,
            StrokeColor
          );
        end;
        // right
        if FThumbLayout in [TLinePickerThumbLayout.Second, TLinePickerThumbLayout.Both] then
        begin
          DrawTriangleThumb(
            Canvas,
            RangeRect.Right - (FThumbWidth div 2 * Integer(FLayout = TLinePickerLayout.CenterArrows)) + FThumbSpace,
            RangeRect.Top + Offset - Size div 2,
            FThumbWidth,
            Size,
            TTriangleThumbOrientation.Left,
            FillColor,
            StrokeColor
          );
        end;
      end;
      TLinePickerLayout.InternalArrows:
      begin
        Index := SaveDC(Canvas.Handle);
        try
          IntersectClipRect(Canvas.Handle, RangeRect.Left, RangeRect.Top, RangeRect.Right, RangeRect.Bottom);
          // top
          if FThumbLayout in [TLinePickerThumbLayout.First, TLinePickerThumbLayout.Both] then
          begin
            DrawTriangleThumb(
              Canvas,
              RangeRect.Left + FThumbSpace,
              RangeRect.Top + Offset - Size div 2,
              FThumbWidth,
              Size,
              TTriangleThumbOrientation.Right,
              FillColor,
              StrokeColor
            );
          end;
          // bottom
          if FThumbLayout in [TLinePickerThumbLayout.Second, TLinePickerThumbLayout.Both] then
          begin
            DrawTriangleThumb(
              Canvas,
              RangeRect.Right - FThumbWidth - FThumbSpace,
              RangeRect.Top + Offset - Size div 2,
              FThumbWidth,
              Size,
              TTriangleThumbOrientation.Left,
              FillColor,
              StrokeColor
            );
          end;
        finally
          RestoreDC(Canvas.Handle, Index);
        end;
      end;
    end;
  end;
end;

procedure TEsAbstractLinePicker.PickerChanged();
begin
  if Assigned(FOnChange) and not(csLoading in ComponentState) then
  begin
    FOnChange(Self);
  end;
end;

procedure TEsAbstractLinePicker.PickerInvalidate(NeedUpdateRangeBitmap: Boolean);
begin
  if NeedUpdateRangeBitmap then
  begin
    FNeedUpdateRangeBitmap := True;
  end;

  Invalidate();
end;

procedure TEsAbstractLinePicker.PickerRepaint(NeedUpdateRangeBitmap: Boolean);
begin
  if NeedUpdateRangeBitmap then
  begin
    FNeedUpdateRangeBitmap := True;
  end;

  if csLoading in ComponentState then
  begin
    exit;
  end;
  Refresh();
end;

procedure TEsAbstractLinePicker.PickerUserChanged();
begin
  if Assigned(FOnUserChange) and not(csLoading in ComponentState) then
  begin
    FOnUserChange(Self);
  end;
end;

procedure TEsAbstractLinePicker.RangePaddingChange(Sender: TObject);
begin
  AdjustContent();

  PickerInvalidate(True);
end;

procedure TEsAbstractLinePicker.Resize();
begin
  inherited Resize();

  AdjustContent();

  PickerInvalidate(True);
end;

procedure TEsAbstractLinePicker.SetLayout(Value: TLinePickerLayout);
begin
  if FLayout = Value then
  begin
    exit;
  end;

  FLayout := Value;

  AdjustContent();

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetOrientation(Value: TLinePickerOrientation);
var
  Temp: Integer;
begin
  if FOrientation = Value then
  begin
    exit;
  end;

  FOrientation := Value;

  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    // swap width and height
    Temp := Width;
    Width := Height;
    Height := Temp;
  end;

  PickerInvalidate(True);
end;

procedure TEsAbstractLinePicker.SetPositionLineVisible(Value: Boolean);
begin
  if FPositionLineVisible = Value then
  begin
    exit;
  end;

  FPositionLineVisible := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetRangeAutoPadding(Value: Boolean);
begin
  if Value = FRangeAutoPadding then
  begin
    exit;
  end;

  FRangeAutoPadding := Value;

  AdjustContent();

  PickerInvalidate(True);
end;

procedure TEsAbstractLinePicker.SetRangeBorderColor(Value: TColor);
begin
  if FRangeBorderColor = Value then
  begin
    exit;
  end;

  FRangeBorderColor := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetRangeBorderFocusedColor(Value: TColor);
begin
  if FRangeBorderFocusedColor = Value then
  begin
    exit;
  end;

  FRangeBorderFocusedColor := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetRangeBorderSpace(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 64);

  if FRangeBorderSpace = Value then
  begin
    exit;
  end;

  FRangeBorderSpace := Value;

  AdjustContent();

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetRangeBorderVisible(Value: Boolean);
begin
  if FRangeBorderVisible = Value then
  begin
    exit;
  end;

  FRangeBorderVisible := Value;

  AdjustContent();

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetRangeBorderWidth(Value: Integer);
begin
  Value := EnsureRange(Value, 1, 64);

  if FRangeBorderWidth = Value then
  begin
    exit;
  end;

  FRangeBorderWidth := Value;

  AdjustContent();

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetRangePadding(Value: TPadding);
begin
  FRangePadding.Assign(Value);
end;

procedure TEsAbstractLinePicker.SetReverse(Value: Boolean);
begin
  if FReverse = Value then
  begin
    exit;
  end;

  FReverse := Value;

  PickerInvalidate(True);
end;

procedure TEsAbstractLinePicker.SetThumbAutoSize(Value: Boolean);
begin
  if FThumbAutoSize = Value then
  begin
    exit;
  end;

  FThumbAutoSize := Value;

  AdjustContent();
end;

procedure TEsAbstractLinePicker.SetThumbColor(Value: TColor);
begin
  if FThumbColor = Value then
  begin
    exit;
  end;

  FThumbColor := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetThumbFocusedColor(Value: TColor);
begin
  if FThumbFocusedColor = Value then
  begin
    exit;
  end;

  FThumbFocusedColor := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetThumbHeight(Value: Integer);
begin
  if FThumbHeight = Value then
  begin
    exit;
  end;

  FThumbHeight := Value;

  AdjustContent();
end;

procedure TEsAbstractLinePicker.SetThumbLayout(Value: TLinePickerThumbLayout);
begin
  if FThumbLayout = Value then
  begin
    exit;
  end;

  FThumbLayout := Value;

  AdjustContent();
end;

procedure TEsAbstractLinePicker.SetThumbLineColor(Value: TColor);
begin
  if FThumbLineColor = Value then
  begin
    exit;
  end;

  FThumbLineColor := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetThumbLineFocusedColor(Value: TColor);
begin
  if FThumbLineFocusedColor = Value then
  begin
    exit;
  end;

  FThumbLineFocusedColor := Value;

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetThumbSpace(Value: Integer);
begin
  if FThumbSpace = Value then
  begin
    exit;
  end;

  FThumbSpace := Value;

  AdjustContent();

  Invalidate();
end;

procedure TEsAbstractLinePicker.SetThumbWidth(Value: Integer);
begin
  if FThumbWidth = Value then
  begin
    exit;
  end;

  FThumbWidth := Value;

  AdjustContent();
end;

{ TEsCustomLinePicker }

constructor TEsCustomLinePicker.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FMin := GetDefaultRange().Min;
  FMax := GetDefaultRange().Max;

  FIncrement := GetDefaultIncrement();
  FIncrementMultiplier := GetDefaultIncrementMultiplier();
end;

function TEsCustomLinePicker.GetDefaultIncrement(): Integer;
begin
  Result := 1;
end;

function TEsCustomLinePicker.GetDefaultIncrementMultiplier(): Integer;
begin
  Result := 10;
end;

function TEsCustomLinePicker.GetDefaultRange(): TLinePickerRange;
begin
  Result := GetRange();
end;

function TEsCustomLinePicker.GetPickerAbsolutePosition(Width: Integer): Integer;
var
  Offset: Integer;
begin
  if FMax > FMin then
  begin
    Offset := ((GetPickerValue() - FMin) * (Width - 1) + (FMax - FMin) div 2) div (FMax - FMin);
  end else
  begin
    Offset := 0;
  end;

  if Reverse then
  begin
    Result := (Width - 1) - Offset;
  end else
  begin
    Result := Offset;
  end;
end;

function TEsCustomLinePicker.GetRange(): TLinePickerRange;
begin
  Result.Min := 0;
  Result.Max := 255;
end;

function TEsCustomLinePicker.IsIncrementMultiplierStored(): Boolean;
begin
  Result := FIncrementMultiplier <> GetDefaultIncrementMultiplier();
end;

function TEsCustomLinePicker.IsIncrementStored(): Boolean;
begin
  Result := GetDefaultIncrement() <> FIncrement;
end;

function TEsCustomLinePicker.IsMaxStored(): Boolean;
begin
  Result := GetDefaultRange().Max <> FMax;
end;

function TEsCustomLinePicker.IsMinStored(): Boolean;
begin
  Result := GetDefaultRange().Min <> FMin;
end;

procedure TEsCustomLinePicker.MovePicker(Delta: Integer; Alternative: Boolean);
begin
  if Alternative then
  begin
    UserUpdatePickerValue(GetPickerValue() + Delta * FIncrement * FIncrementMultiplier);
  end else
  begin
    UserUpdatePickerValue(GetPickerValue() + Delta * FIncrement);
  end;
end;

procedure TEsCustomLinePicker.PickerChange(NeedUpdateRangeBitmap: Boolean);
begin
  // clip value
  SetPickerValue(System.Math.EnsureRange(GetPickerValue(), FMin, FMax));

  PickerRepaint(NeedUpdateRangeBitmap);

  PickerChanged();
end;

procedure TEsCustomLinePicker.SetIncrement(Value: Integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end;

  if FIncrement = Value then
  begin
    exit;
  end;

  FIncrement := Value;
end;

procedure TEsCustomLinePicker.SetIncrementMultiplier(Value: Integer);
begin
  if Value < 1 then
  begin
    Value := 1;
  end;

  if FIncrementMultiplier = Value then
  begin
    exit;
  end;

  FIncrementMultiplier := Value;
end;

procedure TEsCustomLinePicker.SetMax(Value: Integer);
begin
  Value := System.Math.EnsureRange(Value, GetRange().Min, GetRange().Max);

  if FMax = Value then
  begin
    exit;
  end;

  FMax := Value;
  FMin := System.Math.Min(FMin, FMax);

  if GetPickerValue() > FMax then
  begin
    PickerChange(True);
  end else
  begin
    PickerRepaint(True);
  end;
end;

procedure TEsCustomLinePicker.SetMin(Value: Integer);
begin
  Value := System.Math.EnsureRange(Value, GetRange().Min, GetRange().Max);

  if FMin = Value then
  begin
    exit;
  end;

  FMin := Value;
  FMin := System.Math.Min(FMin, FMax);

  if GetPickerValue() < FMin then
  begin
    PickerChange(True);
  end else
  begin
    PickerRepaint(True);
  end;
end;

procedure TEsCustomLinePicker.SetPickerAbsolutePosition(Width, Position: Integer);
var
  Offset: Integer;
begin
  if Width > 1 then
  begin
    Offset := (Position * (FMax - FMin) + (Width - 1) div 2) div (Width - 1);
  end else
  begin
    Offset := 0;
  end;

  if Reverse then
  begin
    UserUpdatePickerValue(FMax - Offset);
  end else
  begin
    UserUpdatePickerValue(FMin + Offset);
  end;
end;

procedure TEsCustomLinePicker.UserUpdatePickerValue(Value: Integer);
begin
  Value := EnsureRange(Value, FMin, FMax);

  if Value = GetPickerValue() then
  begin
    exit;
  end;

  SetPickerValue(Value);

  PickerRepaint(False);

  PickerChanged();

  PickerUserChanged();
end;

{ TEsShaderLinePicker }

constructor TEsShaderLinePicker.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FRangeBackground := TTransparentBackground.Create();
  FRangeBackground.OnChange := RangeBackgroundChange;

  FAlphaPreview := 255;
  FAlphaModulation := True;
end;

destructor TEsShaderLinePicker.Destroy();
begin
  FRangeBackground.Free();

  inherited Destroy();
end;

{$POINTERMATH ON}
procedure TEsShaderLinePicker.DrawRangeBitmap1D(Bitmap: TBitmap; Shader: TLinePickerShader1D);
var
  X, Y, I: Integer;
  Line: array of TAlphaColor;
  AlphaLine: array of array [Boolean] of TAlphaColor;
  Value, MaxValue, MinValue: Integer;
  Divider, Multipler: Integer;
  ShaderColor: TAlphaColor;
  Opacity, AlphaTest: Byte;
  // background
  BackgroundColor1, BackgroundColor2: TAlphaColor;
  BackgroundCellSize: Integer;
  BackgroundKind: TTransparentBackgroundKind;
  //
  Scanline: ^TAlphaColor;
begin
  Bitmap.PixelFormat := pf32bit;
  // we are caching this values for performance in hotload loops
  Opacity := GetPickerRangeOpacity();
  BackgroundKind := FRangeBackground.Kind;
  BackgroundCellSize := FRangeBackground.CellSize;
  BackgroundColor1 := ColorToAlphaColor(ClientColorToRgb(FRangeBackground.Color1, Self));
  BackgroundColor2 := ColorToAlphaColor(ClientColorToRgb(FRangeBackground.Color2, Self));
  MinValue := Min;
  MaxValue := Max;

  // make 1d gradient
  Line := nil;
  if Orientation = TLinePickerOrientation.Horizontal then
  begin
    SetLength(Line, Bitmap.Width);
  end else
  begin
    SetLength(Line, Bitmap.Height);
  end;
  Divider := System.Math.Max(1, Length(Line) - 1);
  Multipler := Max - Min;
  AlphaTest := $FF;
  if Reverse then
  begin
    for I := 0 to Length(Line) - 1 do
    begin
      Value := MaxValue - ((I * Multipler + Divider div 2) div Divider);
      ShaderColor := Shader(Value);
      Line[I] := ShaderColor;
      AlphaTest := AlphaTest and TAlphaColorRec(ShaderColor).A;
    end;
  end else
  begin
    for I := 0 to Length(Line) - 1 do
    begin
      Value := MinValue + ((I * Multipler + Divider div 2) div Divider);
      ShaderColor := Shader(Value);
      Line[I] := ShaderColor;
      AlphaTest := AlphaTest and TAlphaColorRec(ShaderColor).A;
    end;
  end;

  // draw
  if (Opacity = 255) and (AlphaTest = $FF) then
  begin
    // fast way, no alpha
    if Orientation = TLinePickerOrientation.Horizontal then
    begin
      for Y := 0 to Bitmap.Height - 1 do
      begin
        Scanline := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          Scanline[X] := Line[X];
        end;
      end;
    end else
    begin
      for Y := 0 to Bitmap.Height - 1 do
      begin
        Scanline := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          Scanline[X] := Line[Y];
        end;
      end;
    end;
  end else
  begin
    // create alpha lines
    AlphaLine := nil;
    SetLength(AlphaLine, Length(Line));
    for I := 0 to Length(AlphaLine) - 1 do
    begin
      AlphaLine[I, False] := BlendAlphaColor(BackgroundColor1, Line[I], Opacity);
      AlphaLine[I, True] := BlendAlphaColor(BackgroundColor2, Line[I], Opacity);
    end;
    // draw
    if Orientation = TLinePickerOrientation.Horizontal then
    begin
      for Y := 0 to Bitmap.Height - 1 do
      begin
        Scanline := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          Scanline[X] := AlphaLine[X, TransparentBackgroundCase(BackgroundKind, BackgroundCellSize, X, Y)];
        end;
      end;
    end else
    begin
      for Y := 0 to Bitmap.Height - 1 do
      begin
        Scanline := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          Scanline[X] := AlphaLine[Y, TransparentBackgroundCase(BackgroundKind, BackgroundCellSize, X, Y)];
        end;
      end;
    end;
  end;
end;
{$POINTERMATH OFF}

{$POINTERMATH ON}
procedure TEsShaderLinePicker.DrawRangeBitmap2D(Bitmap: TBitmap; Shader: TLinePickerShader2D);
var
  X, Y, I: Integer;
  ValueX: Integer;
  ValueY: Double;
  DividerX, DividerY, Multipler: Integer;
  MaxValue, MinValue: Integer;
  Values: array of Integer;
  ShaderColor: TAlphaColor;
  Opacity: Byte;
  BackgroundColor: TAlphaColor;
  // background
  BackgroundColor1, BackgroundColor2: TAlphaColor;
  BackgroundColors: array [Boolean] of TAlphaColor;
  BackgroundCellSize: Integer;
  BackgroundKind: TTransparentBackgroundKind;
  //
  Scanline: ^TAlphaColor;
begin
  Bitmap.PixelFormat := pf32bit;
  // we are caching this values for performance in hotload loops
  Opacity := GetPickerRangeOpacity();
  BackgroundKind := FRangeBackground.Kind;
  BackgroundCellSize := FRangeBackground.CellSize;
  BackgroundColor1 := ColorToAlphaColor(ClientColorToRgb(FRangeBackground.Color1, Self));
  BackgroundColor2 := ColorToAlphaColor(ClientColorToRgb(FRangeBackground.Color2, Self));
  BackgroundColors[False] := BackgroundColor1;
  BackgroundColors[True] := BackgroundColor2;
  MinValue := Min;
  MaxValue := Max;

  DividerX := System.Math.Max(1, Bitmap.Width - 1);
  DividerY := System.Math.Max(1, Bitmap.Height - 1);
  Multipler := MaxValue - MinValue;
  Values := nil;

  if Orientation = TLinePickerOrientation.Horizontal then
  begin
    SetLength(Values, Bitmap.Width);
    for I := 0 to Bitmap.Width - 1 do
    begin
      if Reverse then
      begin
        Values[I] := MaxValue - ((I * Multipler + DividerX div 2) div DividerX);
      end else
      begin
        Values[I] := MinValue + ((I * Multipler + DividerX div 2) div DividerX);
      end;
    end;

    for Y := 0 to Bitmap.Height - 1 do
    begin
      ValueY := Y / DividerY;
      Scanline := Bitmap.ScanLine[Y];
      for X := 0 to Bitmap.Width - 1 do
      begin
        ValueX := Values[X];
        ShaderColor := Shader(ValueX, ValueY);
        if TAlphaColorRec(ShaderColor).A and Opacity = 255 then
        begin
          Scanline[X] := ShaderColor;
        end else
        begin
          BackgroundColor := BackgroundColors[TransparentBackgroundCase(BackgroundKind, BackgroundCellSize, X, Y)];
          Scanline[X] := BlendAlphaColor(BackgroundColor, ShaderColor, Opacity);
        end;
      end;
    end;
  end else
  begin
    for Y := 0 to Bitmap.Height - 1 do
    begin
      if Reverse then
      begin
        ValueX := MaxValue - ((Y * Multipler + DividerY div 2) div DividerY);
      end else
      begin
        ValueX := MinValue + ((Y * Multipler + DividerY div 2) div DividerY);
      end;
      Scanline := Bitmap.ScanLine[Y];
      for X := 0 to Bitmap.Width - 1 do
      begin
        ValueY := X / DividerX;
        ShaderColor := Shader(ValueX, ValueY);
        if TAlphaColorRec(ShaderColor).A and Opacity = 255 then
        begin
          Scanline[X] := ShaderColor;
        end else
        begin
          BackgroundColor := BackgroundColors[TransparentBackgroundCase(BackgroundKind, BackgroundCellSize, X, Y)];
          Scanline[X] := BlendAlphaColor(BackgroundColor, ShaderColor, Opacity);
        end;
      end;
    end;
  end;
end;
{$POINTERMATH OFF}

function TEsShaderLinePicker.GetDefaultIncrement(): Integer;
begin
  Result := 1;
end;

function TEsShaderLinePicker.GetDefaultIncrementMultiplier(): Integer;
begin
  Result := 8;
end;

function TEsShaderLinePicker.GetPickerRangeOpacity(): Byte;
begin
  if FAlphaModulation then
  begin
    Result := FAlphaPreview;
  end else
  begin
    Result := 255;
  end;
end;

function TEsShaderLinePicker.GetPickerRangeShader1D(): TLinePickerShader1D;
begin
  Result := nil;
end;

function TEsShaderLinePicker.GetPickerRangeShader2D(): TLinePickerShader2D;
begin
  Result := nil;
end;

function TEsShaderLinePicker.GetRange(): TLinePickerRange;
begin
  Result.Min := 0;
  Result.Max := 255;
end;

procedure TEsShaderLinePicker.PaintRangeBitmap(Bitmap: TBitmap);
var
  Shader1D: TLinePickerShader1D;
  Shader2D: TLinePickerShader2D;
begin
  Shader1D := GetPickerRangeShader1D();
  if Assigned(Shader1D) then
  begin
    DrawRangeBitmap1D(Bitmap, Shader1D);
  end else
  begin
    Shader2D := GetPickerRangeShader2D();
    if Assigned(Shader2D) then
    begin
      DrawRangeBitmap2D(Bitmap, Shader2D);
    end;
  end;
end;

procedure TEsShaderLinePicker.RangeBackgroundChange(Sender: TObject);
begin
  PickerInvalidate(True);
end;

procedure TEsShaderLinePicker.SetAlphaModulation(Value: Boolean);
begin
  if FAlphaModulation = Value then
  begin
    exit;
  end;

  FAlphaModulation := Value;
  PickerInvalidate(True);
end;

procedure TEsShaderLinePicker.SetAlphaPreview(Value: Byte);
begin
  if FAlphaPreview = Value then
  begin
    exit;
  end;

  FAlphaPreview := Value;
  PickerRepaint(True);
end;

procedure TEsShaderLinePicker.SetRangeBackground(Value: TTransparentBackground);
begin
  FRangeBackground.Assign(Value);
end;

{ TEsRgbLinePicker }

constructor TEsRgbLinePicker.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FRed := 127;
  FGreen := 127;
  FBlue := 127;
  FColorModulation := True;
  FKind := TRgbLinePickerKind.Red;
end;

function TEsRgbLinePicker.GetCurrentColor(): TColor;
begin
  Result := RGB(FRed, FGreen, FBlue);
end;

procedure TEsRgbLinePicker.GetHsl(out Hue, Saturation, Lightness: Double);
begin
  RgbToHsl(FRed, FGreen, FBlue, Hue, Saturation, Lightness);
end;

procedure TEsRgbLinePicker.GetHsv(out Hue, Saturation, Value: Double);
begin
  RgbToHsv(FRed, FGreen, FBlue, Hue, Saturation, Value);
end;

function TEsRgbLinePicker.GetPickerRangeShader1D(): TLinePickerShader1D;
begin
  case FKind of
    TRgbLinePickerKind.Red:
    begin
      if FColorModulation then
      begin
        Result := ShaderModulatedRed;
      end else
      begin
        Result := ShaderRed;
      end;
    end;
    TRgbLinePickerKind.Green:
    begin
      if FColorModulation then
      begin
        Result := ShaderModulatedGreen;
      end else
      begin
        Result := ShaderGreen;
      end;
    end;
    TRgbLinePickerKind.Blue:
    begin
      if FColorModulation then
      begin
        Result := ShaderModulatedBlue;
      end else
      begin
        Result := ShaderBlue;
      end;
    end;
  end;
end;

function TEsRgbLinePicker.GetPickerValue(): Integer;
begin
  Result := 0;
  case FKind of
    TRgbLinePickerKind.Red:
    begin
      Result := FRed;
    end;
    TRgbLinePickerKind.Green:
    begin
      Result := FGreen;
    end;
    TRgbLinePickerKind.Blue:
    begin
      Result := FBlue;
    end;
  end;
end;

procedure TEsRgbLinePicker.GetRgb(out Red, Green, Blue: Byte);
begin
  Red := FRed;
  Green := FGreen;
  Blue := FBlue;
end;

procedure TEsRgbLinePicker.SetBlue(Value: Byte);
begin
  UpdateRgb(FRed, FGreen, Value);
end;

procedure TEsRgbLinePicker.SetColorModulation(Value: Boolean);
begin
  if FColorModulation = Value then
  begin
    exit;
  end;

  FColorModulation := Value;
  PickerInvalidate(True);
end;

procedure TEsRgbLinePicker.SetCurrentColor(Value: TColor);
begin
  Value := ColorToRGB(Value);

  UpdateRgb(GetRValue(Value), GetGValue(Value), GetBValue(Value));
end;

procedure TEsRgbLinePicker.SetGreen(Value: Byte);
begin
  UpdateRgb(FRed, Value, FBlue);
end;

procedure TEsRgbLinePicker.SetHsl(Hue, Saturation, Lightness: Double);
var
  R, G, B: Byte;
begin
  HslToRgb(Hue, Saturation, Lightness, R, G, B);

  UpdateRgb(R, G, B);
end;

procedure TEsRgbLinePicker.SetHsv(Hue, Saturation, Value: Double);
var
  R, G, B: Byte;
begin
  HsvToRgb(Hue, Saturation, Value, R, G, B);

  UpdateRgb(R, G, B);
end;

procedure TEsRgbLinePicker.SetKind(Value: TRgbLinePickerKind);
begin
  if FKind = Value then
  begin
    exit;
  end;

  FKind := Value;

  PickerChange(True);
end;

procedure TEsRgbLinePicker.SetPickerValue(Value: Integer);
begin
  case FKind of
    TRgbLinePickerKind.Red:
    begin
      FRed := Value;
    end;
    TRgbLinePickerKind.Green:
    begin
      FGreen := Value;
    end;
    TRgbLinePickerKind.Blue:
    begin
      FBlue := Value;
    end;
  end;
end;

procedure TEsRgbLinePicker.SetRed(Value: Byte);
begin
  UpdateRgb(Value, FGreen, FBlue);
end;

procedure TEsRgbLinePicker.SetRgb(Red, Green, Blue: Byte);
begin
  UpdateRgb(Red, Green, Blue);
end;

function TEsRgbLinePicker.ShaderBlue(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(0, 0, Value);
end;

function TEsRgbLinePicker.ShaderGreen(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(0, Value, 0);
end;

function TEsRgbLinePicker.ShaderModulatedBlue(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(FRed, FGreen, Value);
end;

function TEsRgbLinePicker.ShaderModulatedGreen(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(FRed, Value, FBlue);
end;

function TEsRgbLinePicker.ShaderModulatedRed(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(Value, FGreen, FBlue);
end;

function TEsRgbLinePicker.ShaderRed(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(Value, 0, 0);
end;

procedure TEsRgbLinePicker.UpdateRgb(Red, Green, Blue: Byte);
var
  NeedUpdate: Boolean;
begin
  if (Red = FRed) and (Green = FGreen) and (Blue = FBlue) then
  begin
    exit;
  end;

  NeedUpdate :=
    ((Red <> FRed) and ((FKind <> TRgbLinePickerKind.Red) and FColorModulation)) or
    ((Green <> FGreen) and ((FKind <> TRgbLinePickerKind.Green) and FColorModulation)) or
    ((Blue <> FBlue) and ((FKind <> TRgbLinePickerKind.Blue) and FColorModulation));

  FRed := Red;
  FGreen := Green;
  FBlue := Blue;

  PickerChange(NeedUpdate);
end;

{ TEsAlphaLinePicker }

constructor TEsAlphaLinePicker.Create(Owner: TComponent);
begin
  inherited;

  FAlpha := 255;
  FColorPreview := $007F7F7F;
  FColorModulation := True;
end;

procedure TEsAlphaLinePicker.GetHslPreview(out Hue, Saturation, Lightness: Double);
begin
  RgbToHsl(GetRValue(FColorPreview), GetGValue(FColorPreview), GetBValue(FColorPreview), Hue, Saturation, Lightness);
end;

procedure TEsAlphaLinePicker.GetHsvPreview(out Hue, Saturation, Value: Double);
begin
  RgbToHsv(GetRValue(FColorPreview), GetGValue(FColorPreview), GetBValue(FColorPreview), Hue, Saturation, Value);
end;

function TEsAlphaLinePicker.GetPickerRangeShader1D(): TLinePickerShader1D;
begin
  if FColorModulation then
  begin
    Result := ShaderModulated;
  end else
  begin
    Result := nil;
  end;
end;

function TEsAlphaLinePicker.GetPickerRangeShader2D(): TLinePickerShader2D;
begin
  Result := ShaderGreenBlueRedGradient;
end;

function TEsAlphaLinePicker.GetPickerValue(): Integer;
begin
  Result := FAlpha;
end;

procedure TEsAlphaLinePicker.GetRgbPreview(out Red, Green, Blue: Byte);
begin
  Red := GetRValue(FColorPreview);
  Green := GetGValue(FColorPreview);
  Blue := GetBValue(FColorPreview);
end;

procedure TEsAlphaLinePicker.SetAlpha(Value: Byte);
begin
  Value := EnsureRange(Value, 0, 255);

  if FAlpha = Value then
  begin
    exit;
  end;

  FAlpha := Value;
  PickerChange(FColorModulation);
end;

procedure TEsAlphaLinePicker.SetColorModulation(Value: Boolean);
begin
  if FColorModulation = Value then
  begin
    exit;
  end;

  FColorModulation := Value;
  PickerInvalidate(True);
end;

procedure TEsAlphaLinePicker.SetColorPreview(Value: TColor);
begin
  Value := ColorToRGB(Value);

  UpdateRgbPreview(GetRValue(Value), GetGValue(Value), GetBValue(Value));
end;

procedure TEsAlphaLinePicker.SetHslPreview(Hue, Saturation, Lightness: Double);
var
  R, G, B: Byte;
begin
  HslToRgb(Hue, Saturation, Lightness, R, G, B);

  UpdateRgbPreview(R, G, B);
end;

procedure TEsAlphaLinePicker.SetHsvPreview(Hue, Saturation, Value: Double);
var
  R, G, B: Byte;
begin
  HsvToRgb(Hue, Saturation, Value, R, G, B);

  UpdateRgbPreview(R, G, B);
end;

procedure TEsAlphaLinePicker.SetPickerValue(Value: Integer);
begin
  FAlpha := Value;
end;

procedure TEsAlphaLinePicker.SetRgbPreview(Red, Green, Blue: Byte);
begin
  UpdateRgbPreview(Red, Green, Blue);
end;

function TEsAlphaLinePicker.ShaderGreenBlueRedGradient(ValueX: Integer; ValueY: Double): TAlphaColor;
const
  Angle = 120.0;
  Range = 240.0;
  Scale = 1.0 - 1.0 / 9.0;
  Multiplier = (Range / 360.0) * (1.0 / Scale);
  Adder = (Angle / 360.0) + 0.5 * (Range / 360.0) - 0.5 * Multiplier;
var
  R, G, B: Byte;
begin
  if ValueY < (1.0 - Scale) / 2.0 then
  begin
    HslToRgb(120.0 / 360.0, 1.0, 0.5, R, G, B);
  end else
  if ValueY > (1.0 + Scale) / 2.0 then
  begin
    HslToRgb(360.0 / 360.0, 1.0, 0.5, R, G, B);
  end else
  begin
    HslToRgb(ValueY * Multiplier + Adder, 1.0, 0.5, R, G, B);
  end;

  Result := TAlphaColor.Create(R, G, B, ValueX);
end;

function TEsAlphaLinePicker.ShaderModulated(Value: Integer): TAlphaColor;
begin
  Result := TAlphaColor.Create(GetRValue(FColorPreview), GetGValue(FColorPreview), GetBValue(FColorPreview), Value);
end;

procedure TEsAlphaLinePicker.UpdateRgbPreview(Red, Green, Blue: Byte);
begin
  if (Red = GetRValue(FColorPreview)) and (Green = GetGValue(FColorPreview)) and (Blue = GetBValue(FColorPreview)) then
  begin
    exit;
  end;

  FColorPreview := RGB(Red, Green, Blue);

  if ColorModulation then
  begin
    PickerRepaint(True);
  end;
end;

end.
