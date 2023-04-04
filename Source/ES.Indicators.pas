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
unit ES.Indicators;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  Winapi.Messages, ES.ExGraphics, ES.BaseControls;

type
  TActivityPointType = (Box, Circle);
  TActivityPlacement = (None, Top, Bottom);
  TActivityAnimationType = (WindowsX, Bar, Sin, Progress);
  TActivityDisplayMode = (Overlay, Docked);
  {$REGION 'deprecated names'}
  {$IFDEF SUPPORT_ENUMS_ALIASES}
  TActivityPointTypeHelper = record helper for TActivityPointType
  const
    ptBox = TActivityPointType.Box deprecated 'Use TActivityPointType.Box';
    ptCircle = TActivityPointType.Circle deprecated 'Use TActivityPointType.Circle';
  end;
  TActivityPlacementHelper = record helper for TActivityPlacement
  const
    apNone = TActivityPlacement.None deprecated 'Use TActivityPlacement.None';
    apTop = TActivityPlacement.Top deprecated 'Use TActivityPlacement.Top';
    apBottom = TActivityPlacement.Bottom deprecated 'Use TActivityPlacement.Bottom';
  end;
  TActivityAnimationTypeHelper = record helper for TActivityAnimationType
  const
    atWindowsX = TActivityAnimationType.WindowsX deprecated 'Use TActivityPoint.WindowsX';
    atBar = TActivityAnimationType.Bar deprecated 'Use TActivityPoint.Bar';
    atSin = TActivityAnimationType.Sin deprecated 'Use TActivityPoint.Sin';
    atProgress = TActivityAnimationType.Progress deprecated 'Use TActivityPoint.Progress';
  end;
  TActivityDisplayModeHelper = record helper for TActivityDisplayMode
  const
    admOverlay = TActivityDisplayMode.Overlay deprecated 'Use TActivityDisplayMode.Overlay';
    admDocked = TActivityDisplayMode.Docked deprecated 'Use TActivityDisplayMode.Docked';
  end;
  {$ENDIF}
  {$ENDREGION}
  TPointCount = 1..30;
  TAnimationEnergy = 1..200;
  TAnimationTime = 1..100000;
  TAnimationDelay = 0..100000;
  TTimerInterval = 1..1000;

  TEsActivityBar = class(TEsCustomControl)
  const
    DefaultEnegry = 30;
  private
    AnimationTimer: TTimer;
    OldTime: DWORD;
    FVerticalSpace: Word;
    FPointColor: TColor;
    FAutoHide: Boolean;
    FActive: Boolean;
    FPlacement: TActivityPlacement;
    FPointSpace: Word;
    FPointCount: TPointCount;
    Pos: Double;
    FAnimationType: TActivityAnimationType;
    FAnimationTime: TAnimationTime;
    FTimerInterval: TTimerInterval;
    FAnimationDelay: TAnimationDelay;
    FPointType: TActivityPointType;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FHorizontalSpace: Word;
    FDisplayMode: TActivityDisplayMode;
    procedure SetVerticalSpace(const Value: Word);
    procedure SetPointColor(const Value: TColor);
    procedure SetAutoHide(const Value: Boolean);
    procedure SetActive(const Value: Boolean);
    procedure SetPlacement(const Value: TActivityPlacement);
    procedure SetPointSpace(const Value: Word);
    procedure SetPointCount(const Value: TPointCount);
    procedure SetAnimationType(const Value: TActivityAnimationType);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetAnimationEnergy(const Value: TAnimationEnergy);
    function GetAnimationEnergy: TAnimationEnergy;
    procedure SetTimerInterval(const Value: TTimerInterval);
    procedure SetAnimationDelay(const Value: TAnimationDelay);
    procedure SetPointType(const Value: TActivityPointType);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetHorizontalSpace(const Value: Word);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetDisplayMode(const Value: TActivityDisplayMode);
    function GetAlign: TAlign;
    function GetAnchors: TAnchors;
    procedure SetAlign(const Value: TAlign);
    procedure SetAnchors(const Value: TAnchors);
    function GetVisible: Boolean;
    function IsVisibleStored: Boolean;
    procedure SetVisible(const Value: Boolean);
    function AnchorsStored: Boolean;
  protected
    StartPos: Double;
    EndPos: Double;
    NormEnergy: Double;
    procedure CalcPos; dynamic;
    procedure ResetAnimation; dynamic;
    function GetStartPos: double; dynamic;
    function GetEndPos: double; dynamic;
    procedure AnimationExecuteHandler(Sender: TObject); dynamic;
    procedure DelayExecuteHandler(Sender: TObject); dynamic;
    function FillWidth: Integer; dynamic;
    function PointWidth: Integer; virtual;
    function GetPointPos(RealX: Double; Lenght: Integer): Integer; virtual;
    function GetScreenPos(X: Double; Number: Integer): Integer; virtual;
    procedure Paint; override;
    {$IFDEF VER240UP}
    procedure UpdateStyleElements; override;
    {$ENDIF}
    procedure DoPlacement; dynamic;
    procedure Loaded; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure Deactivate;
  published
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property Anchors: TAnchors read GetAnchors write SetAnchors stored AnchorsStored default [akTop, akLeft];
    property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored default True;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property AnimationTime: TAnimationTime read FAnimationTime write FAnimationTime default 4000;
    property AnimationDelay: TAnimationDelay read FAnimationDelay write SetAnimationDelay default 500;
    property AnimationType: TActivityAnimationType read FAnimationType write SetAnimationType default TActivityAnimationType.WindowsX;
    property AnimationEnergy: TAnimationEnergy read GetAnimationEnergy write SetAnimationEnergy default DefaultEnegry;
    property VerticalSpace: Word read FVerticalSpace write SetVerticalSpace default 0;
    property HorizontalSpace: Word read FHorizontalSpace write SetHorizontalSpace default 0;
    property PointSpace: Word read FPointSpace write SetPointSpace default 12;
    property PointCount: TPointCount read FPointCount write SetPointCount default 5;
    property PointColor: TColor read FPointColor write SetPointColor default clHighlight;
    property PointType: TActivityPointType read FPointType write SetPointType default TActivityPointType.Box;
    property TimerInterval: TTimerInterval read FTimerInterval write SetTimerInterval default 33;
    property AutoHide: Boolean read FAutoHide write SetAutoHide default False;
    property Active: Boolean read FActive write SetActive default False;
    property Placement: TActivityPlacement read FPlacement write SetPlacement default TActivityPlacement.None;
    property DisplayMode: TActivityDisplayMode read FDisplayMode write SetDisplayMode default TActivityDisplayMode.Overlay;
    property Color;
    //
    property IsCachedBuffer;// TEsCustomControl
    property IsCachedBackground;// TEsCustomControl
    property IsDrawHelper default True;// TEsCustomControl
    property IsOpaque;// TEsCustomControl
    //
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Constraints;
    property ParentBackground;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property OnClick;
    property OnCanResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnPaint;// TEsCustomControl
    property OnPainting;// TEsCustomControl
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
  System.Math, System.TypInfo, WinApi.GdipObj, WinApi.GdipApi, Vcl.Consts, Vcl.Themes, ES.ExGdiPlus, ES.Utils;

{ TEsActivityBar }

procedure TEsActivityBar.Activate;
begin
  if FActive then
    Exit;

  if FAutoHide then
    Inherited Visible := True;
  Self.BringToFront;

  FActive := True;

  if AnimationTimer = nil then
    AnimationTimer := TTimer.Create(nil);

  ResetAnimation;
  AnimationTimer.OnTimer := AnimationExecuteHandler;
  AnimationTimer.Interval := FTimerInterval;
  AnimationTimer.Enabled := True;
  OldTime := GetTickCount;

  Invalidate;
end;

function TEsActivityBar.AnchorsStored: Boolean;
begin
  Result := Anchors <> AnchorAlign[Align];
end;

procedure TEsActivityBar.AnimationExecuteHandler(Sender: TObject);
var
  NowTime: DWORD;
begin
  NowTime := GetTickCount;
  Pos := Pos + 0.001 * (NowTime - OldTime) * (1000 / FAnimationTime) * (EndPos - StartPos);
  OldTime := NowTime;

  if Pos > EndPos then
  begin
    Pos := StartPos;
    if (FAnimationDelay  <> 0) and (AnimationType <> TActivityAnimationType.Sin) then
    begin
      TTimer(Sender).Enabled := False;
      TTimer(Sender).OnTimer := DelayExecuteHandler;
      TTimer(Sender).Interval := FAnimationDelay;
      TTimer(Sender).Enabled := True;
    end;
  end;
  Repaint;
end;

procedure TEsActivityBar.CalcPos;
begin
  EndPos := GetEndPos;
  StartPos := GetStartPos;
  if Pos > EndPos then
    Pos := EndPos;
  if Pos < StartPos  then
    Pos := StartPos;
end;

procedure TEsActivityBar.CMEnabledChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TEsActivityBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 7;
  IsFullSizeBuffer := True;
  FPointColor := clHighlight;
  FAutoHide := False;
  FActive := False;
  FPlacement := TActivityPlacement.None;
  FPointSpace := 12;
  FPointCount := 5;
  NormEnergy := DefaultEnegry / 10;
  FAnimationType := TActivityAnimationType.WindowsX;
  FTimerInterval := 33;
  FAnimationTime := 4000;
  FAnimationDelay := 500;
  FMax := 100;
  FMin := 0;
  FPosition := 0;
  IsDrawHelper := True;
end;

procedure TEsActivityBar.DelayExecuteHandler(Sender: TObject);
begin
  TTimer(Sender).Enabled := False;
  TTimer(Sender).OnTimer := AnimationExecuteHandler;
  if not (csDesigning in ComponentState) then
    TTimer(Sender).Interval := FTimerInterval
  else
    TTimer(Sender).Interval := 200;
  OldTime := GetTickCount;
  TTimer(Sender).Enabled := True;
end;

procedure TEsActivityBar.Deactivate;
begin
  if not FActive then
    Exit;

  if FAutoHide then
    Inherited Visible := False;

  FActive := False;
  FreeAndNil(AnimationTimer);

  Invalidate;
end;

destructor TEsActivityBar.Destroy;
begin
  AnimationTimer.Free;
  inherited;
end;

procedure TEsActivityBar.DoPlacement;
begin
  if (Parent = nil) or (csLoading in ComponentState) then
    Exit;

  if Placement <> TActivityPlacement.None then
  begin
    if FDisplayMode = TActivityDisplayMode.Overlay then
    begin
      Inherited Align := alNone;
      if Placement = TActivityPlacement.Top then
      begin
        Top := 0;
        Left := 0;
        Width := Parent.ClientWidth;
        Inherited Anchors := [akLeft, akTop, akRight];
      end
      else
      begin
        Top := Parent.ClientHeight - Height;
        Left := 0;
        Width := Parent.ClientWidth;
        Inherited Anchors := [akLeft, akBottom, akRight];
      end;
    end else
    begin
      if Placement = TActivityPlacement.Top then
      begin
        Inherited Align := alTop;
        Top := 0;
      end
      else
      begin
        Inherited Align := alBottom;
        Top := Parent.ClientHeight - Height;
      end;
    end;
  end;
end;

function TEsActivityBar.GetStartPos: double;
const
  MaxInteration = 25;
var
  n: Integer;
  f, d: Double;
begin
  case AnimationType of
    TActivityAnimationType.Bar:
      Result := (FillWidth * -0.5) / (ClientWidth - HorizontalSpace * 2);
    TActivityAnimationType.Sin:
      Result := -pi * 1;
    TActivityAnimationType.WindowsX:
      begin
        f := 1;
        d := 0.5;

        n := 0;
        repeat
          inc(n);
          if (GetScreenPos(f - d, FPointCount - 1) < -PointWidth) and (n < MaxInteration) then
          begin
            d := d / 2;
            continue;
          end else
            f := f - d;
        until not((GetScreenPos(f, FPointCount - 1) > -PointWidth) and (n < MaxInteration));

        Result := f;
      end;
    else
      Result := 0;
  end;
end;

function TEsActivityBar.GetEndPos: double;
begin
  case AnimationType of
    TActivityAnimationType.Bar:
      Result := 1 - GetStartPos;
    TActivityAnimationType.Sin:
      Result := -GetStartPos;
    TActivityAnimationType.WindowsX:
      Result := 1 - GetStartPos;
    else
      Result := 0;
  end;
end;

function TEsActivityBar.GetVisible: Boolean;
begin
  Result := Inherited Visible;
end;

function TEsActivityBar.IsVisibleStored: Boolean;
begin
  Result := not FAutoHide;
end;

procedure TEsActivityBar.Loaded;
begin
  Inherited;
  if FAutoHide then
    Inherited Visible := FActive;
end;

function TEsActivityBar.GetAlign: TAlign;
begin
  Result := Inherited Align
end;

function TEsActivityBar.GetAnchors: TAnchors;
begin
  Result := Inherited Anchors;
end;

function TEsActivityBar.GetAnimationEnergy: TAnimationEnergy;
begin
  Result := Round(NormEnergy * 10);
end;

function TEsActivityBar.GetScreenPos(X: Double; Number: Integer): Integer;
var
  k: Integer;
  w: Integer;
begin
  w := ClientWidth - HorizontalSpace * 2;
  if AnimationType = TActivityAnimationType.Sin then
    k := 4
  else
    k := 1;
  Result :=
    HorizontalSpace + GetPointPos(
     ((k * FPointSpace + PointWidth) * (Number - (FPointCount - 1) * 0.5)) / w + X, w);
end;

function TEsActivityBar.FillWidth: Integer;
begin
  Result := FPointCount * (PointWidth + FPointSpace);
end;

// StartPos..EndPos => 0..Lenght
function TEsActivityBar.GetPointPos(RealX: Double; Lenght: Integer): Integer;
  function X10(x: double): double;
  const
    Pow = 3;
    // Speed = 2.8;
  begin
    Result := Abs(Power(NormEnergy - x * NormEnergy, Pow));
  end;
const
  StartNorm = 0.40;
  EndNorm = 0.60;
var
  d: Double;
  r: Double;
begin
  case AnimationType of
    TActivityAnimationType.Bar:
      Result := Trunc(RealX * Lenght);
    TActivityAnimationType.Sin:
      Result := Trunc((((1 + cos((RealX))))) * Lenght * 1 * 0.5);
    TActivityAnimationType.WindowsX:
      begin
        if RealX < StartNorm then
        begin
          d := 1 - (StartNorm - RealX) / StartNorm;
          r := RealX * Lenght - X10(d) * Lenght;
        end else
        if RealX > EndNorm then
        begin
          d :=  (RealX - EndNorm) / (1 - EndNorm);
          r := RealX * Lenght + X10(1 - d) * Lenght;
        end else
          r := RealX * Lenght;
        Result := Trunc(r);
      end;
    else
      Result := 0;
  end;
end;

procedure TEsActivityBar.Paint;
var
  x, i: Integer;
  Graphics: TGPGraphics;
  Brush: TGPBrush;
  R: TRect;
  CurrentColor: TColor;
begin
  Graphics := nil;
  Brush := nil;

  try
    if FActive then
    begin
      if IsStyledClientControl(Self) then
        CurrentColor := ClientColorToRgb(PointColor, Self)
      else
        CurrentColor := PointColor;

      if AnimationType = TActivityAnimationType.Progress then
      begin
        if FMax <> FMin then
          x := Trunc((FPosition - FMin) / Abs(FMax - FMin) * (ClientWidth - FHorizontalSpace * 2))
        else
          x := 0;

        if (FHorizontalSpace * 2 <= ClientWidth)and(FVerticalSpace * 2 <= ClientHeight) then
        begin
          Canvas.Brush.Color := CurrentColor;
          Canvas.Pen.Style := psClear;
          if Enabled then
            Canvas.Brush.Style := bsSolid
          else
            Canvas.Brush.Style := bsBDiagonal;
          Canvas.Rectangle(HorizontalSpace, FVerticalSpace,
            x + HorizontalSpace + 1, ClientHeight - FVerticalSpace + 1);
        end;
      end else
      begin
        if HorizontalSpace <> 0 then
          IntersectClipRect(Canvas.Handle, HorizontalSpace, 0, ClientWidth - HorizontalSpace, ClientHeight);
        if PointType = TActivityPointType.Circle then
        begin
          Graphics := TGPGraphics.Create(Canvas.Handle);
          Graphics.SetSmoothingMode(SmoothingModeHighQuality);
          Brush := TGPSolidBrush.Create(ColorToGPColor(CurrentColor));
        end else
        begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := CurrentColor;
        end;

        for i := 0 to FPointCount-1 do
        begin
          x := GetScreenPos(Pos, i);

          R.Left := x - PointWidth div 2;
          R.Top := FVerticalSpace;
          R.Width := PointWidth;
          R.Height := PointWidth;

          if PointType = TActivityPointType.Circle then
            Graphics.FillEllipse(Brush, R.Left - 0.5, R.Top - 0.5, R.Width, R.Height)
          else
            Canvas.FillRect(R);
        end;
      end;
    end;
  finally
    Graphics.Free;
    Brush.Free;
  end;
  inherited;
end;

function TEsActivityBar.PointWidth: Integer;
begin
  Result := ClientHeight - FVerticalSpace * 2;
end;

procedure TEsActivityBar.ResetAnimation;
begin
  CalcPos;
  Pos := StartPos;
end;

procedure TEsActivityBar.SetActive(const Value: Boolean);
begin
  //!
  if FActive <> Value then
  begin
    //FActive := Value;
    if Value then
      Activate
    else
      Deactivate;
  end;
end;

procedure TEsActivityBar.SetAlign(const Value: TAlign);
begin
  if (Placement = TActivityPlacement.None) or (csLoading in ComponentState) then
    Inherited Align := Value;
end;

procedure TEsActivityBar.SetAnimationType(const Value: TActivityAnimationType);
begin
  if FAnimationType <> Value then
  begin
    FAnimationType := Value;
    ResetAnimation;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetAutoHide(const Value: Boolean);
begin
  if FAutoHide <> Value then
  begin
    FAutoHide := Value;

    if Visible then
    begin
      if not FActive then
        Deactivate;
    end else
    begin
      if FActive then
        Activate;
    end;
  end;
end;

procedure TEsActivityBar.SetDisplayMode(const Value: TActivityDisplayMode);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    DoPlacement;
  end;
end;

procedure TEsActivityBar.SetHorizontalSpace(const Value: Word);
begin
  if Value <> FHorizontalSpace then
  begin
    FHorizontalSpace := Value;
    CalcPos;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetAnimationEnergy(const Value: TAnimationEnergy);
begin
  if Value <> AnimationEnergy then
  begin
    NormEnergy := Value / 10;
    CalcPos;
  end;
end;

procedure TEsActivityBar.SetAnchors(const Value: TAnchors);
begin
  if (Placement = TActivityPlacement.None) or (csLoading in ComponentState) then
    Inherited Anchors := Value
end;

procedure TEsActivityBar.SetAnimationDelay(const Value: TAnimationDelay);
begin
  if FAnimationDelay <> Value then
  begin
    FAnimationDelay := Value;
    CalcPos;
  end;
end;

procedure TEsActivityBar.SetPointSpace(const Value: Word);
begin
  if FPointSpace <> Value then
  begin
    FPointSpace := Value;
    CalcPos;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    if FMin > Value then
      FMin := Value;// raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
    FMax := Value;
    if FPosition > Value then
      FPosition := Value;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetMin(const Value: Integer);
begin
  if FMin <> Value then
  begin
    if FMax < Value then
      FMax := Value;
    FMin := Value;
    if FPosition < Value then
      FPosition := Value;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  DoPlacement;
end;

procedure TEsActivityBar.SetPlacement(const Value: TActivityPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    DoPlacement;
  end;
end;

procedure TEsActivityBar.SetPointColor(const Value: TColor);
begin
  if FPointColor <> Value then
  begin
    FPointColor := Value;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetPointCount(const Value: TPointCount);
begin
  if FPointCount <> Value then
  begin
    FPointCount := Value;
    CalcPos;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetPointType(const Value: TActivityPointType);
begin
  if FPointType <> Value then
  begin
    FPointType := Value;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetTimerInterval(const Value: TTimerInterval);
begin
  if FTimerInterval <> Value then
  begin
    FTimerInterval := Value;
    if (AnimationTimer <> nil) and (TMethod(AnimationTimer.OnTimer).Code = @TEsActivityBar.AnimationExecuteHandler) then
      AnimationTimer.Interval := Value;
  end;
end;

procedure TEsActivityBar.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    if Value < FMin then
      FPosition := FMin
    else
    if Value > FMax then
      FPosition := FMax
    else
      FPosition := Value;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetVerticalSpace(const Value: Word);
begin
  if FVerticalSpace <> Value then
  begin
    FVerticalSpace := Value;
    CalcPos;
    Invalidate;
  end;
end;

procedure TEsActivityBar.SetVisible(const Value: Boolean);
begin
  if (not FAutoHide)and(Value <> Visible) then
  begin
    Inherited Visible := Value;
  end;
end;

{$IFDEF VER240UP}
procedure TEsActivityBar.UpdateStyleElements;
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

procedure TEsActivityBar.WMSize(var Message: TWMSize);
begin
  Inherited;
  CalcPos;
end;

initialization
  {$IFDEF VER270UP}
  AddEnumElementAliases(TypeInfo(TActivityPointType), ['ptBox', 'ptCircle']);
  AddEnumElementAliases(TypeInfo(TActivityPlacement), ['apNone', 'apTop', 'apBottom']);
  AddEnumElementAliases(TypeInfo(TActivityAnimationType), ['atWindowsX', 'atBar', 'atSin', 'atProgress']);
  AddEnumElementAliases(TypeInfo(TActivityDisplayMode), ['admOverlay', 'admDocked']);
  {$ENDIF}

finalization
  {$IFDEF VER270UP}
  RemoveEnumElementAliases(TypeInfo(TActivityPointType));
  RemoveEnumElementAliases(TypeInfo(TActivityPlacement));
  RemoveEnumElementAliases(TypeInfo(TActivityAnimationType));
  RemoveEnumElementAliases(TypeInfo(TActivityDisplayMode));
  {$ENDIF}

end.
