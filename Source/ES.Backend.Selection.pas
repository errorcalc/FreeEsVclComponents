{******************************************************************************}
{                           errorsoft Core library                             }
{                           errorsoft(c) 2016-2018                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{     errorsoft@protonmail.ch | habrahabr.ru/user/error1024 | errorsoft.org    }
{                                                                              }
{         Open this on github: github.com/errorcalc/FreeEsVclComponents        }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
// WARNING!!!
// This unit should not contain references to the VCL/FMX or API's.

// *** ONLY INTERNAL USE THIS MODULE! (EsVclComponents and EsFmxComponents) ***
// *** This api, this will never be stable, possible breaking changes       ***

//--------------------------------------------------------------------------------------------------

// Virtual control TBackendSelection is backend for visual controls such as NinePatch selector,
// disagram object selector, photo selector, etc...
//
// WARNING: This module is not completed yet, you should not use it!!!!!
// Tasks:
// 1) realization Constraints and ProprtionalRange
// 2) changes in Command
// 3) testing
// 4) general refactoring

//--------------------------------------------------------------------------------------------------

unit ES.Backend.Selection;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types, ES.Core.Classes, ES.Backend.Controls;

{$SCOPEDENUMS ON}

type
  TGripKind = (None, TopLeft, TopRight, BottomLeft, BottomRight, Left, Top, Right, Bottom, Center, Fill);
  TGripKinds = set of TGripKind.TopLeft..TGripKind.Fill;
  TProportionalKind = (Width, Height);
  TSelectionCommand = (IncX, DecX, IncY, DecY, IncWidth, DecWidth, IncHeight, DecHeight);
  TSelectionChangingEvent = procedure (Sender: TObject; var NewRect: TRect) of object;
  TSelectionZoom = 1..10000;

  TProportionalRange = class(TRange)
  private
    FKind: TProportionalKind;
    procedure SetKind(const Value: TProportionalKind);
  published
    property Kind: TProportionalKind read FKind write SetKind;
  end;

  // only internal use
  TBackendSelection = class(TBackendControl)
  private const
    DefaultGrips = [TGripKind.TopLeft, TGripKind.TopRight, TGripKind.BottomLeft, TGripKind.BottomRight,
      TGripKind.Left, TGripKind.Top, TGripKind.Right, TGripKind.Bottom];
    ProportionalGrips = [TGripKind.TopLeft, TGripKind.TopRight, TGripKind.BottomLeft,
      TGripKind.BottomRight, TGripKind.Left, TGripKind.Top, TGripKind.Right, TGripKind.Bottom];
  private
    HW: Double;
    sx: Integer;
    sy: Integer;
    Capture: TGripKind;
    procedure ChangeBounds(Sender: TObject);
    procedure ChangeSelection(Sender: TObject);
    procedure ChangeConstraints(Sender: TObject);
    procedure ChangeProprtionalRange(Sender: TObject);
  private
    FSelection: TBounds;
    FBounds: TBounds;
    FGripWidth: Integer;
//    FGripType: TGripType;
    FLineWidth: Integer;
    FAllowGrips: TGripKinds;
    FOnChange: TNotifyEvent;
    FOnUpdateView: TNotifyEvent;
    FOnChanging: TSelectionChangingEvent;
    FProportional: Boolean;
    FConstraints: TSizeConstraints;
    FProportionalRange: TProportionalRange;
    FOnChanged: TNotifyEvent;
    procedure SetBounds(const Value: TBounds);
    procedure SetSelection(const Value: TBounds);
    function GetFastMetrics(const Index: Integer): Integer;
    procedure SetFastMetrics(const Index, Value: Integer);
    procedure SetProportional(const Value: Boolean);
    procedure SetConstraints(const Value: TSizeConstraints);
    procedure SetProportionalRange(const Value: TProportionalRange);
    function GetSelectionRect: TRect;
    procedure SetSelectionRect(const Value: TRect);
    function GetProportionalRatio: Single;
    function IsProportionalRatioStored: Boolean;
    procedure SetProportionalRatio(const Value: Single);
  protected
    procedure NormalizeRect(var R: TRect);
    procedure DoProportionalContraction(var R: TRect; IsMainLine: Boolean);
    procedure DoProportionalRange(var R: TRect; DirectWidth, DirectHeight: Boolean);
    procedure Change;
    procedure Changing(var NewRect: TRect);
    procedure UpdateView;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ScaleRect: TRect;
    // ---
    function GripKind(X, Y: Integer): TGripKind;
    function GripPos(GripKind: TGripKind): TPoint;
    // ---
    procedure MouseDown(X, Y: Integer);
    procedure MouseMove(X, Y: Integer);
    procedure MouseUp(X, Y: Integer);
    procedure Command(Command: TSelectionCommand);
    //procedure Normalize; overload;
    property Zoom;
    property Numerator;
    property Denominator;
    // ---
    property Bounds: TBounds read FBounds write SetBounds;
//    property UseBounds: Boolean read FUseBounds write SetUseBounds default True; // переименовать в "Bounds Constraints"
    property Constraints: TSizeConstraints read FConstraints write SetConstraints;
    // свойство "привязка к краям"
    property Selection: TBounds read FSelection write SetSelection;
    property GripWidth: Integer read FGripWidth write FGripWidth default 7;
    property LineWidth: Integer read FLineWidth write FLineWidth default 5;
//    property GripType: TGripType read FGripType write FGripType default TGripType.Square;
    property AllowGrips: TGripKinds read FAllowGrips write FAllowGrips default DefaultGrips;
//    property Scale: Single read FScale write SetScale stored IsScaleStored;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property ProportionalRange: TProportionalRange read FProportionalRange write SetProportionalRange;
    property ProportionalRatio: Single read GetProportionalRatio write SetProportionalRatio stored IsProportionalRatioStored;
    // ---
    property OnSelectionChange: TSelectionChangingEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnUpdateView: TNotifyEvent read FOnUpdateView write FOnUpdateView;
    // fast access
    property Left: Integer Index 0 read GetFastMetrics write SetFastMetrics;
    property Top: Integer Index 1 read GetFastMetrics write SetFastMetrics;
    property Right: Integer Index 2 read GetFastMetrics write SetFastMetrics;
    property Bottom: Integer Index 3 read GetFastMetrics write SetFastMetrics;
    property Width: Integer Index 4 read GetFastMetrics write SetFastMetrics;
    property Height: Integer Index 5 read GetFastMetrics write SetFastMetrics;
    property Rect: TRect read GetSelectionRect write SetSelectionRect;
  end;

implementation

uses
  System.Math;

{ TBackendSelection }

procedure TBackendSelection.ChangeBounds(Sender: TObject);
//var
//  R: TRect;
begin
//  if UseBounds then
//  begin
//    R := Selection.Rect;
//    Normalize(R);
//    Selection.Rect := R;
//  end;
end;

procedure TBackendSelection.ChangeConstraints(Sender: TObject);
begin

end;

procedure TBackendSelection.ChangeProprtionalRange(Sender: TObject);
begin

end;

procedure TBackendSelection.ChangeSelection(Sender: TObject);
//var
//  R: TRect;
begin
//  if UseBounds then
//  begin
//    R := Selection.Rect;
//    Normalize(R);
//    if R <> Selection.Rect then
//    begin
//      Selection.Rect := R;
//      Exit;
//    end;
//  end;
  Change;
end;

procedure TBackendSelection.Changing(var NewRect: TRect);
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self, NewRect);
end;

procedure TBackendSelection.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBackendSelection.Command(Command: TSelectionCommand);
var
  R: TRect;
begin
  R := Selection.Rect;
  case Command of
    TSelectionCommand.IncX:
      if Bounds.IsEmpty or (R.Right < Bounds.Right) then
        R.Offset(1, 0);
    TSelectionCommand.DecX:
      if Bounds.IsEmpty or (R.Left > Bounds.Left) then
        R.Offset(-1, 0);
    TSelectionCommand.IncY:
      if Bounds.IsEmpty or (R.Bottom < Bounds.Bottom) then
        R.Offset(0, 1);
    TSelectionCommand.DecY:
      if Bounds.IsEmpty or (R.Top > Bounds.Top) then
        R.Offset(0, -1);
    TSelectionCommand.IncWidth:
      if Bounds.IsEmpty or (R.Right < Bounds.Right) then
        Inc(R.Right);
    TSelectionCommand.DecWidth:
      if R.Width > 0 then
        Dec(R.Right);
    TSelectionCommand.IncHeight:
      if Bounds.IsEmpty or (R.Bottom < Bounds.Bottom) then
        Inc(R.Bottom);
    TSelectionCommand.DecHeight:
      if R.Height > 0 then
        Dec(R.Bottom);
  end;

  if Proportional then
  begin
    // proportional resize if need
    if Command in [TSelectionCommand.IncWidth, TSelectionCommand.DecWidth] then
      R.Bottom := Trunc(R.Top + R.Width * HW)
    else if Command in [TSelectionCommand.IncHeight, TSelectionCommand.DecHeight] then
      R.Right := Trunc(R.Left + R.Height / HW);

    // propertional range if need
    if not ProportionalRange.IsEmpty then
      DoProportionalRange(R, True, True);

    // proportional constraction if need
    if not Bounds.IsEmpty then
      DoProportionalContraction(R, True);
  end;

  // OnChanging
  Changing(R);
  //
  Selection.Rect := R;
end;

constructor TBackendSelection.Create;
begin
  inherited;

  FBounds := TBounds.Create;
  FConstraints := TSizeConstraints.Create;
  FProportionalRange := TProportionalRange.Create;
  FSelection := TBounds.Create;

  FBounds.OnChange := ChangeBounds;
  FConstraints.OnChange := ChangeConstraints;
  FProportionalRange.OnChange := ChangeProprtionalRange;
  FSelection.OnChange := ChangeSelection;

  FGripWidth := 9;
  FLineWidth := 5;
  HW := 1.0;

  AllowGrips := DefaultGrips;
end;

destructor TBackendSelection.Destroy;
begin
  FBounds.Free;
  FConstraints.Free;
  FProportionalRange.Free;
  FSelection.Free;
  Inherited;
end;

function TBackendSelection.GripPos(GripKind: TGripKind): TPoint;
begin
  case GripKind of
    TGripKind.TopLeft: Result := TPoint.Create(ScaleRect.Left, ScaleRect.Top);
    TGripKind.TopRight: Result := TPoint.Create(ScaleRect.Right, ScaleRect.Top);
    TGripKind.BottomLeft: Result := TPoint.Create(ScaleRect.Left, ScaleRect.Bottom);
    TGripKind.BottomRight: Result := TPoint.Create(ScaleRect.Right, ScaleRect.Bottom);
    TGripKind.Left: Result := TPoint.Create(ScaleRect.Left, 0);
    TGripKind.Top: Result := TPoint.Create(0, ScaleRect.Top);
    TGripKind.Right: Result := TPoint.Create(ScaleRect.Right, 0);
    TGripKind.Bottom: Result := TPoint.Create(0, ScaleRect.Bottom);
    TGripKind.Center: Result := ScaleRect.CenterPoint;
    TGripKind.Fill: Result := ScaleRect.TopLeft;
    else
      Result := TPoint.Create(0, 0);
  end;
end;

function TBackendSelection.IsProportionalRatioStored: Boolean;
begin
  Result := not SameValue(1.0, ProportionalRatio);
end;

function TBackendSelection.GetFastMetrics(const Index: Integer): Integer;
begin
  case Index of
    0: Result := Selection.Left;
    1: Result := Selection.Top;
    2: Result := Selection.Right;
    3: Result := Selection.Bottom;
    4: Result := Selection.Width;
    5: Result := Selection.Height;
    else
      Result := 0;
  end;
end;

function TBackendSelection.GetProportionalRatio: Single;
begin
  Result := HW;
end;

function TBackendSelection.GetSelectionRect: TRect;
begin
  Result := Selection.Rect;
end;

function TBackendSelection.GripKind(X, Y: Integer): TGripKind;
var
  Radius, HalfLine: Integer;
  R: TRect;

  function IsHitGrip(tx, ty: Integer): Boolean;
  begin
//    if GripType = TGripType.Square then
    Result := (X >= tx - Radius) and (X <= tx + Radius) and
      (Y >= ty - Radius) and (Y <= ty + Radius);
//    else
//      Result := TPoint.PointInCircle(Point(tx, ty), Point(sx, sy), Radius);
  end;
begin
  Result := TGripKind.None;
  Radius := GripWidth div 2;
  HalfLine := LineWidth div 2;
  R := ScaleRect;

  // Fill
  if (TGripKind.Fill in AllowGrips) and R.Contains(Point(X, Y)) then
    Result := TGripKind.Fill;

  // Top
  if TGripKind.Top in AllowGrips then
    if (X >= R.Left) and (X <= R.Right) and (Y >= R.Top - HalfLine) and (Y <= R.Top + HalfLine) then
      Result := TGripKind.Top;

  // Bottom
  if TGripKind.Bottom in AllowGrips then
    if (X >= R.Left) and (X <= R.Right) and (Y >= R.Bottom - HalfLine) and (Y <= R.Bottom + HalfLine) then
      Result := TGripKind.Bottom;

  // Left
  if TGripKind.Left in AllowGrips then
    if (Y >= R.Top) and (Y <= R.Bottom) and (X >= R.Left - HalfLine) and (X <= R.Left + HalfLine) then
      if (not (TGripKind.Top in AllowGrips) or (Y - R.Top > X - R.Left)) and
         (not (TGripKind.Bottom in AllowGrips) or (R.Bottom - Y > X - R.Left)) then
        Result := TGripKind.Left;

  // Right
  if TGripKind.Right in AllowGrips then
    if (Y >= R.Top) and (Y <= R.Bottom) and (X >= R.Right - HalfLine) and (X <= R.Right + HalfLine) then
      if (not (TGripKind.Top in AllowGrips) or (Y - R.Top > R.Right - X)) and
         (not (TGripKind.Bottom in AllowGrips) or (R.Bottom - Y > R.Right - X)) then
        Result := TGripKind.Right;

  // TopLeft
  if TGripKind.TopLeft in AllowGrips then
    if IsHitGrip(R.Left, R.Top) then
      Result := TGripKind.TopLeft;

  // BottomLeft
  if TGripKind.BottomLeft in AllowGrips then
    if IsHitGrip(R.Left, R.Bottom) then
      Result := TGripKind.BottomLeft;

  // TopRight
  if TGripKind.TopRight in AllowGrips then
    if IsHitGrip(R.Right, R.Top) then
      Result := TGripKind.TopRight;

  // BottomRight
  if TGripKind.BottomRight in AllowGrips then
    if IsHitGrip(R.Right, R.Bottom) then
      Result := TGripKind.BottomRight;

  // Center
  if TGripKind.Center in AllowGrips then
    if (Result = TGripKind.None) and IsHitGrip(R.CenterPoint.X, R.CenterPoint.Y) then
      Result := TGripKind.Center;
end;

procedure TBackendSelection.MouseDown(X, Y: Integer);
begin
  Capture := GripKind(X, Y);
  sx := X - GripPos(Capture).X;
  sy := Y - GripPos(Capture).Y;
  UpdateView;
end;

procedure TBackendSelection.MouseMove(X, Y: Integer);
var
  lx, ly: Integer;
  R: TRect;
  IsMainLine: Boolean;
begin
  if Capture = TGripKind.None then
    Exit;

  lx := ToReal(X - sx);
  ly := ToReal(Y - sy);

  if Proportional and (Capture in ProportionalGrips) then
  begin
    IsMainLine := False;// for compiler paranoia
    case Capture of
      TGripKind.TopLeft, TGripKind.Left, TGripKind.Top:
      begin
        IsMainLine := True;
        R := Selection.Rect;
        case Capture of
        TGripKind.TopLeft:
          begin
            R.Left := lx; R.Top := ly;
          end;
        TGripKind.Left:
          begin
            R.Left := lx; R.Top := R.Bottom;
          end;
        TGripKind.Top:
          begin
            R.Left := R.Right; R.Top := ly;
          end;
        end;
        R.Left := Min(R.Left, R.Right);
        R.Top := Min(R.Top, R.Bottom);

        if (R.Right - R.Left) * HW > R.Bottom - R.Top then
          R.Top := Trunc(R.Bottom - R.Width * HW)
        else
          R.Left := Trunc(R.Right - R.Height / HW);
      end;
      TGripKind.BottomRight, TGripKind.Right, TGripKind.Bottom:
      begin
        IsMainLine := True;
        R := Selection.Rect;
        case Capture of
        TGripKind.BottomRight:
          begin
            R.Right := lx; R.Bottom := ly;
          end;
        TGripKind.Right:
          begin
            R.Right := lx; R.Bottom := R.Top;
          end;
        TGripKind.Bottom:
          begin
            R.Right := R.Left; R.Bottom := ly;
          end;
        end;
        R.Right := Max(R.Right, R.Left);
        R.Bottom := Max(R.Bottom, R.Top);

        if (R.Right - R.Left) * HW > R.Bottom - R.Top then
          R.Bottom := Trunc(R.Top + R.Width * HW)
        else
          R.Right := Trunc(R.Left + R.Height / HW);
      end;
      TGripKind.TopRight:
      begin
        IsMainLine := False;
        R := Selection.Rect;
        R.Right := Max(lx, R.Left); R.Top := Min(ly, R.Bottom);

        if (R.Right - R.Left) * HW > R.Bottom - R.Top then
          R.Top := Trunc(R.Bottom - R.Width * HW)
        else
          R.Right := Trunc(R.Left + R.Height / HW);
      end;
      TGripKind.BottomLeft:
      begin
        IsMainLine := False;
        R := Selection.Rect;
        R.Left := Min(lx, R.Right); R.Bottom := Max(ly, R.Top);

        if (R.Right - R.Left) * HW > R.Bottom - R.Top then
          R.Bottom := Trunc(R.Top + R.Width * HW)
        else
          R.Left := Trunc(R.Right - R.Height / HW);
      end;
    end;

    if not ProportionalRange.IsEmpty then
      DoProportionalRange(R, Capture in [TGripKind.Right, TGripKind.BottomRight, TGripKind.Bottom, TGripKind.TopRight],
        Capture in [TGripKind.Right, TGripKind.BottomRight, TGripKind.Bottom, TGripKind.BottomLeft]);

    if not Bounds.IsEmpty then
      DoProportionalContraction(R, IsMainLine);
  end else
  begin
    case Capture of
    // TopLeft .. BottomRight
    TGripKind.TopLeft:
      R := TRect.Create({Left}Min(lx, Selection.Right), {Top}Min(ly, Selection.Bottom),
                {Right}Selection.Right, {Bottom}Selection.Bottom);

    TGripKind.TopRight:
      R := TRect.Create({Left}Selection.Left, {Top}Min(ly, Selection.Bottom),
                {Right}Max(lx, Selection.Left), {Bottom}Selection.Bottom);

    TGripKind.BottomLeft:
      R := TRect.Create({Left}Min(lx, Selection.Right), {Top}Selection.Top,
                {Right}Selection.Right, {Bottom}Max(ly, Selection.Top));

    TGripKind.BottomRight:
      R := TRect.Create({Left}Selection.Left, {Top}Selection.Top,
                {Right}Max(lx, Selection.Left), {Bottom}Max(ly, Selection.Top));

    // Left..Bottom
    TGripKind.Left:
      R := TRect.Create({Left}Min(lx, Selection.Right), {Top}Selection.Top,
                {Right}Selection.Right, {Bottom}Selection.Bottom);

    TGripKind.Right:
      R := TRect.Create({Left}Selection.Left, {Top}Selection.Top,
                {Right}Max(lx, Selection.Left), {Bottom}Selection.Bottom);

    TGripKind.Top:
      R := TRect.Create({Left}Selection.Left, {Top}Min(ly, Selection.Bottom),
                {Right}Selection.Right, {Bottom}Selection.Bottom);

    TGripKind.Bottom:
      R := TRect.Create({Left}Selection.Left, {Top}Selection.Top,
                {Right}Selection.Right, {Bottom}Max(ly, Selection.Top));

    // Center..Fill
    TGripKind.Center:
      if Bounds.IsEmpty then
      begin
        R := TRect.Create(0, 0, Selection.Width, Selection.Height);
        R.Offset(lx - R.CenterPoint.X, ly - R.CenterPoint.Y);
      end else
      begin
        R := TRect.Create(0, 0, Selection.Width, Selection.Height);
        R.Offset(EnsureRange(lx - R.CenterPoint.X, Bounds.Left, Bounds.Right - R.Width),
          EnsureRange(ly - R.CenterPoint.Y, Bounds.Top, Bounds.Bottom - R.Height));
      end;

    TGripKind.Fill:
      if Bounds.IsEmpty then
      begin
        R := TRect.Create(0, 0, Selection.Width, Selection.Height);
        R.Offset(lx, ly);
      end else
      begin
        R := TRect.Create(0, 0, Selection.Width, Selection.Height);
        R.Offset(EnsureRange(lx, Bounds.Left, Bounds.Right - R.Width),
          EnsureRange(ly, Bounds.Top, Bounds.Bottom - R.Height));
      end;
    end;
    if not Bounds.IsEmpty then
      NormalizeRect(R);
  end;

  Changing(R);

  Selection.Rect := R;

  UpdateView;
end;

procedure TBackendSelection.MouseUp(X, Y: Integer);
begin
  Capture := TGripKind.None;
  UpdateView;
end;

//procedure TBackendSelection.Normalize;
//var
//  R: TRect;
//begin
//  if not Bounds.IsEmpty then
//  begin
//    R := Selection.Rect;
//    NormalizeRect(R);
//    if R <> Selection.Rect then
//    begin
//      Selection.Rect := R;
//    end;
//  end;
//end;

procedure TBackendSelection.NormalizeRect(var R: TRect);
begin
  if R.Left < Bounds.Left then
    R.Left := Bounds.Left;
  if R.Right > Bounds.Right then
    R.Right := Bounds.Right;
  if R.Top < Bounds.Top then
    R.Top := Bounds.Top;
  if R.Bottom > Bounds.Bottom then
    R.Bottom := Bounds.Bottom;
end;

function TBackendSelection.ScaleRect: TRect;
begin
  Result := TRect.Create(ToScale(Selection.Left), ToScale(Selection.Top), ToScale(Selection.Right), ToScale(Selection.Bottom));
end;

procedure TBackendSelection.SetBounds(const Value: TBounds);
begin
  FBounds.Assign(Value);
end;

procedure TBackendSelection.SetConstraints(const Value: TSizeConstraints);
begin
  FConstraints.Assign(Value);
end;

procedure TBackendSelection.SetFastMetrics(const Index, Value: Integer);
begin
  case Index of
    0: Selection.Left := Value;
    1: Selection.Top := Value;
    2: Selection.Right := Value;
    3: Selection.Bottom := Value;
    4: Selection.Width := Value;
    5: Selection.Height := Value;
  end;
end;

procedure TBackendSelection.SetProportional(const Value: Boolean);
begin
  if Value <> FProportional then
  begin
    FProportional := Value;
  end;
end;

procedure TBackendSelection.SetProportionalRange(const Value: TProportionalRange);
begin
  FProportionalRange.Assign(Value);
end;

procedure TBackendSelection.SetProportionalRatio(const Value: Single);
begin
  HW := Value;
  if SameValue(Value, 0.0) then
    HW := 0.00001;
  // FIXME
end;

procedure TBackendSelection.SetSelection(const Value: TBounds);
begin
  FSelection.Assign(Value);
end;

procedure TBackendSelection.SetSelectionRect(const Value: TRect);
begin
  Selection.Rect := Value;
end;

procedure TBackendSelection.UpdateView;
begin
  if Assigned(FOnUpdateView) then
    FOnUpdateView(Self);
end;

procedure TBackendSelection.DoProportionalContraction(var R: TRect; IsMainLine: Boolean);
var
  t: Integer;
begin
  if R.Left < Bounds.Left then
  begin
    t := Bounds.Left - R.Left;
    R.Offset(t, 0);
    R.Right := R.Right - t;
    if IsMainLine then
    begin
      R.Top := R.Bottom - Trunc(R.Width * HW);
    end else
      R.Height := Trunc(R.Width * HW);
  end;
  if R.Top < Bounds.Top then
  begin
    t := Bounds.Top - R.Top;
    R.Offset(0, t);
    R.Bottom := R.Bottom - t;
    if IsMainLine then
    begin
      R.Left := R.Right - Trunc(R.Height / HW);
    end else
      R.Width := Trunc(R.Height / HW);
  end;
  if R.Right > Bounds.Right then
  begin
    t := R.Right - Bounds.Right;
    R.Offset(-t, 0);
    R.Left := R.Left + t;
    if not IsMainLine then
    begin
      R.Top := R.Bottom - Trunc(R.Width * HW);
    end else
      R.Height := Trunc(R.Width * HW);
  end;
  if R.Bottom > Bounds.Bottom then
  begin
    t := R.Bottom - Bounds.Bottom;
    R.Offset(0, -t);
    R.Top := R.Top + t;
    if not IsMainLine then
    begin
      R.Left := R.Right - Trunc(R.Height / HW);
    end else
      R.Width := Trunc(R.Height / HW);
  end;
end;

procedure TBackendSelection.DoProportionalRange(var R: TRect; DirectWidth, DirectHeight: Boolean);
begin
  if ProportionalRange.Kind = TProportionalKind.Width then
  begin
    if DirectWidth then
    begin
      R.Width := ProportionalRange.Apply(R.Width);
      if DirectHeight then
        R.Height := Trunc(R.Width * HW)
      else
        R.Top := R.Bottom - Trunc(R.Width * HW);
    end else
    begin
      R.Left := R.Left + R.Width - ProportionalRange.Apply(R.Width);
      if DirectHeight then
        R.Height := Trunc(R.Width * HW)
      else
        R.Top := R.Bottom - Trunc(R.Width * HW);
    end;
  end else
  begin
    if DirectHeight then
    begin
      R.Height := ProportionalRange.Apply(R.Height);
      if DirectWidth then
        R.Width := Trunc(R.Height / HW)
      else
        R.Left := R.Right - Trunc(R.Height / HW);
    end else
    begin
      R.Top := R.Top + R.Height - ProportionalRange.Apply(R.Height);
      if DirectWidth then
        R.Width := Trunc(R.Height / HW)
      else
        R.Left := R.Right - Trunc(R.Height / HW);
    end;
  end;
end;

{ TProportionalRange }

procedure TProportionalRange.SetKind(const Value: TProportionalKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    Change;
  end;
end;

end.
