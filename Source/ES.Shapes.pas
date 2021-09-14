{******************************************************************************}
{                                                                              }
{                       EsVclComponents/EsVclCore v4.1                         }
{                           errorsoft(c) 2009-2021                             }
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
{          Вы можете заказать разработку VCL/FMX компонента на заказ.          }
{                                                                              }
{******************************************************************************}
unit ES.Shapes;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, Vcl.Controls, Winapi.Windows, Vcl.Graphics, ES.BaseControls,
  System.Types, Winapi.Messages;

type
  TShapePenStyle = (Solid, Dash, Dot, DashDot, DashDotDot, Clear);

  TShapePen = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FWidth: Integer;
    FColor: TColor;
    FStyle: TShapePenStyle;
    FOpacity: Byte;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TShapePenStyle);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Style: TShapePenStyle read FStyle write SetStyle default TShapePenStyle.Solid;
    property Width: Integer read FWidth write SetWidth default 1;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
  end;

  TShapeBrushStyle = (Solid, Clear, HorizontalGradient, VerticalGradient, Horizontal,
    Vertical, ForwardDiagonal, BackwardDiagonal, Cross, DiagonalCross, HalfHatch, ZigZag);

  TShapeBrush = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColor: TColor;
    FStyle: TShapeBrushStyle;
    FOpacity: Byte;
    FEndColor: TColor;
    FStartColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TShapeBrushStyle);
    procedure SetOpacity(const Value: Byte);
    procedure SetEndColor(const Value: TColor);
    procedure SetStartColor(const Value: TColor);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property StartColor: TColor read FStartColor write SetStartColor default clWhite;
    property EndColor: TColor read FEndColor write SetEndColor default clSilver;
    property Style: TShapeBrushStyle read FStyle write SetStyle default TShapeBrushStyle.Solid;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
  end;

  TShapeKind = (Rectangle, RoundRectangle, Square, RoundSquare, Ellipse, Circle,
    Triangle, Rhomb, Star, Bubble, Arrow);

  TShapeDirection = (Top, Left, Right, Bottom);

  TShapeCorner = (TopLeft, TopRight, BottomLeft, BottomRight);
  TShapeCorners = set of TShapeCorner;
const
  AllShapeCorners = [TShapeCorner.TopLeft, TShapeCorner.TopRight,
    TShapeCorner.BottomLeft, TShapeCorner.BottomRight];

type
  TShapeSide = (Top, Left, Bottom, Right);
  TShapeSides = set of TShapeSide;
const
  AllShapeSides = [TShapeSide.Top, TShapeSide.Left,
    TShapeSide.Bottom, TShapeSide.Right];

type
  TEsShape = class(TEsGraphicControl)
  private
    FOpacity: Byte;
    FShape: TShapeKind;
    FPen: TShapePen;
    FBrush: TShapeBrush;
    FCornerRadius: Integer;
    FDirection: TShapeDirection;
    FCorners: TShapeCorners;
    FSides: TShapeSides;
    procedure StyleChanged(Sender: TObject);
    procedure SetBrush(const Value: TShapeBrush);
    procedure SetOpacity(const Value: Byte);
    procedure SetPen(const Value: TShapePen);
    procedure SetShape(const Value: TShapeKind);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetDirection(const Value: TShapeDirection);
    procedure SetCorners(const Value: TShapeCorners);
    procedure SetSides(const Value: TShapeSides);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    {$IFDEF VER310UP}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Brush: TShapeBrush read FBrush write SetBrush;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Caption;
    property Corners: TShapeCorners read FCorners write SetCorners default AllShapeCorners;
    property Constraints;
    property Padding;
    property ParentShowHint;
    property ParentFont;
    property Pen: TShapePen read FPen write SetPen;
    property Shape: TShapeKind read FShape write SetShape default TShapeKind.Rectangle;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 10;
    property Direction: TShapeDirection read FDirection write SetDirection default TShapeDirection.Top;
    property ShowHint;
    property Touch;
    property Visible;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property Sides: TShapeSides read FSides write SetSides default AllShapeSides;
    property IsDrawHelper default False;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  ES.Utils, ES.ExGdiPlus, Winapi.GDIPAPI, System.Math, Winapi.GDIPOBJ;

// rectangle

function MakeRoundRectanglePath(const Left, Top, Width, Height: Double; Radius: Double;
  const Corners: TShapeCorners; const Sides: TShapeSides): TGPGraphicsPath;
var
  Rect: TGPRectF;
  TopLeftRadius, TopRightRadius, BottomLeftRadius, BottomRightRadius: Double;
begin
  Result := TGPGraphicsPath.Create;

  if (Radius * 2 > Width) or (Radius * 2 > Height) then
    Radius := Min(Width, Height) * 0.5;

  Rect.X := Left;
  Rect.Y := Top;
  Rect.Width := Radius * 2;
  Rect.Height := Radius * 2;

  // TopLeft
  if TShapeCorner.TopLeft in Corners then
    TopLeftRadius := Radius
  else
    TopLeftRadius := 0;
  // TopRight
  if TShapeCorner.TopRight in Corners then
    TopRightRadius := Radius
  else
    TopRightRadius := 0;
  // BottomRight
  if TShapeCorner.BottomRight in Corners then
    BottomRightRadius := Radius
  else
    BottomRightRadius := 0;
  // BottomLeft
  if TShapeCorner.BottomLeft in Corners then
    BottomLeftRadius := Radius
  else
    BottomLeftRadius := 0;

  // TopLeft
  if (TShapeCorner.TopLeft in Corners) and
    ((TShapeSide.Left in Sides) or (TShapeSide.Top in Sides)) then
    Result.AddArc(Rect, 180, 90);
  if TShapeSide.Top in Sides then
    Result.AddLine(MakePoint(Left + TopLeftRadius, Top), MakePoint(Left + Width - TopRightRadius, Top))
  else
    Result.StartFigure;
  // TopRight
  Rect.X := Rect.X + (Width - Radius * 2);
  if (TShapeCorner.TopRight in Corners) and
    ((TShapeSide.Right in Sides) or (TShapeSide.Top in Sides)) then
    Result.AddArc(Rect, 270, 90);
  if TShapeSide.Right in Sides then
    Result.AddLine(MakePoint(Left + Width, Top + TopRightRadius), MakePoint(Left + Width, Top + Height - BottomRightRadius))
  else
    Result.StartFigure;
  // BottomRight
  Rect.Y := Rect.Y + (Height - Radius * 2);
  if (TShapeCorner.BottomRight in Corners) and
    ((TShapeSide.Right in Sides) or (TShapeSide.Bottom in Sides)) then
    Result.AddArc(Rect, 0, 90);
  if TShapeSide.Bottom in Sides then
    Result.AddLine(MakePoint(Left + Width - BottomRightRadius, Top + Height), MakePoint(Left + BottomLeftRadius, Top + Height))
  else
    Result.StartFigure;
  // BottomLeft
  Rect.X := Rect.X - (Width - Radius * 2);
  if (TShapeCorner.BottomLeft in Corners) and
    ((TShapeSide.Left in Sides) or (TShapeSide.Bottom in Sides)) then
    Result.AddArc(Rect, 90, 90);
  if TShapeSide.Left in Sides then
    Result.AddLine(MakePoint(Left, Top + Height - BottomLeftRadius), MakePoint(Left, Top + TopLeftRadius))
  else
    Result.StartFigure;

  if Sides = AllShapeSides then
    Result.CloseFigure;
end;

procedure DrawRoundRectangle(const Graphics: TGPGraphics; const Pen: TGPPen;
  const Left, Top, Width, Height, Radius: Double; const Corners: TShapeCorners;
  const Sides: TShapeSides);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeRoundRectanglePath(Left, Top, Width, Height, Radius, Corners, Sides);
  try
    Graphics.DrawPath(Pen, Path);
  finally
    Path.Free;
  end;
end;

procedure FillRoundRectangle(const Graphics: TGPGraphics; const Brush: TGPBrush;
  const Left, Top, Width, Height, Radius: Double; const Corners: TShapeCorners;
  const Sides: TShapeSides);
var
  Path: TGPGraphicsPath;
  LeftShift, TopShift, RightShift, BottomShift: Double;
begin
  if TShapeSide.Top in Sides then
    TopShift := 0.0
  else
    TopShift := 0.5;
  if TShapeSide.Left in Sides then
    LeftShift := 0.0
  else
    LeftShift := 0.5;
  if TShapeSide.Bottom in Sides then
    BottomShift := 0.0
  else
    BottomShift := 0.5;
  if TShapeSide.Right in Sides then
    RightShift := 0.0
  else
    RightShift := 0.5;

  Path := MakeRoundRectanglePath(Left - LeftShift, Top - TopShift, Width + RightShift + LeftShift,
    Height + BottomShift + TopShift, Radius, Corners, AllShapeSides);
  try
    Graphics.FillPath(Brush, Path);
  finally
    Path.Free;
  end;
end;

// triangle

function MakeTrianglePath(const Left, Top, Width, Height: Double;
  const Direction: TShapeDirection): TGPGraphicsPath;
var
  Points: array [0..2] of TGPPointF;
begin
  Result := TGPGraphicsPath.Create;

  case Direction of
    TShapeDirection.Top:
    begin
      Points[0] := MakePoint(Left + Width * 0.5, Top);
      Points[1] := MakePoint(Left + Width, Top + Height);
      Points[2] := MakePoint(Left, Top + Height);
    end;
    TShapeDirection.Left:
    begin
      Points[0] := MakePoint(Left + Width, Top);
      Points[1] := MakePoint(Left, Top + Height * 0.5);
      Points[2] := MakePoint(Left + Width, Top + Height);
    end;
    TShapeDirection.Right:
    begin
      Points[0] := MakePoint(Left, Top);
      Points[1] := MakePoint(Left + Width, Top + Height * 0.5);
      Points[2] := MakePoint(Left, Top + Height);
    end;
    TShapeDirection.Bottom:
    begin
      Points[0] := MakePoint(Left, Top);
      Points[1] := MakePoint(Left + Width * 0.5, Top + Height);
      Points[2] := MakePoint(Left + Width, Top);
    end;
  end;

  Result.AddPolygon(PGPPointF(@Points[0]), 3);
end;

procedure DrawTriangle(const Graphics: TGPGraphics; const Pen: TGPPen;
  const Left, Top, Width, Height: Double; Direction: TShapeDirection);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeTrianglePath(Left, Top, Width, Height, Direction);
  try
    Graphics.DrawPath(Pen, Path);
  finally
    Path.Free;
  end;
end;

procedure FillTriangle(const Graphics: TGPGraphics; const Brush: TGPBrush;
  const Left, Top, Width, Height: Double; Direction: TShapeDirection);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeTrianglePath(Left, Top, Width, Height, Direction);
  try
    Graphics.FillPath(Brush, Path);
  finally
    Path.Free;
  end;
end;

// star

(*
function MakeStarPath(Left, Top, Width, Height: Double; Direction: TShapeDirection): TGPGraphicsPath;
var
  Points: array [0..9] of TGPPointF;
  Center: TGPPointF;
  Angle: Double;
  I: Integer;
begin
  Result := TGPGraphicsPath.Create;

  Center := MakePoint(Left + Width / 2, Top + Height / 2);

  case Direction of
    TShapeDirection.Top:
      Angle := -Pi * 0.5;
    TShapeDirection.Left:
      Angle := Pi * 1 / 5;
    TShapeDirection.Right:
      Angle := Pi * 2 / 5;
    TShapeDirection.Bottom:
      Angle := Pi * 0.5;
    else
      Angle := 0;
  end;
  I := 0;
  while I < Length(Points) do
  begin
    Points[I] := MakePoint(
      Center.X + Cos(Angle) * Width * 0.5,
      Center.Y + Sin(Angle) * Height * 0.5
    );
    Points[I + 1] := MakePoint(
      Center.X + Cos(Angle + 2 * Pi / Length(Points)) * Width * 0.191,
      Center.Y + Sin(Angle + 2 * Pi / Length(Points)) * Height * 0.191
    );
    Angle := Angle + 4 * Pi / Length(Points);
    I := I + 2;
  end;

  Result.AddPolygon(PGPPointF(@Points[0]), Length(Points));
end;
*)

function MakeStarPath(const Left, Top, Width, Height: Double;
  const Direction: TShapeDirection): TGPGraphicsPath;
var
  Points: array [0..9] of TGPPointF;
  Angle: Double;
  I: Integer;
  MaxX, MaxY, MinX, MinY: Double;
begin
  Result := TGPGraphicsPath.Create;

  case Direction of
    TShapeDirection.Top:
      Angle := -Pi * 0.5;
    TShapeDirection.Left:
      Angle := Pi * 1 / 5;
    TShapeDirection.Right:
      Angle := Pi * 2 / 5;
    TShapeDirection.Bottom:
      Angle := Pi * 0.5;
    else
      Angle := 0;
  end;

  I := 0;
  while I < Length(Points) do
  begin
    Points[I] := MakePoint(
      0.5 + Cos(Angle) * 0.5,
      0.5 + Sin(Angle) * 0.5
    );
    Points[I + 1] := MakePoint(
      0.5 + Cos(Angle + 2 * Pi / Length(Points)) * 0.191,
      0.5 + Sin(Angle + 2 * Pi / Length(Points)) * 0.191
    );
    Angle := Angle + 4 * Pi / Length(Points);
    I := I + 2;
  end;

  MinX := Points[0].X;
  MinY := Points[0].Y;
  MaxX := Points[0].X;
  MaxY := Points[0].Y;
  for I := 0 to High(Points) do
  begin
    if MaxX < Points[I].X then
      MaxX := Points[I].X;
    if MaxY < Points[I].Y then
      MaxY := Points[I].Y;
    if MinX > Points[I].X then
      MinX := Points[I].X;
    if MinY > Points[I].Y then
      MinY := Points[I].Y;
  end;

  for I := 0 to High(Points) do
  begin
    Points[I].X := Left + ((Points[I].X - MinX) / (MaxX - MinX)) * Width;
    Points[I].Y := Top + ((Points[I].Y - MinY) / (MaxY - MinY)) * Height;
  end;

  Result.AddPolygon(PGPPointF(@Points[0]), Length(Points));
end;

procedure DrawStar(const Graphics: TGPGraphics; const Pen: TGPPen;
  const Left, Top, Width, Height: Double; const Direction: TShapeDirection);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeStarPath(Left, Top, Width, Height, Direction);
  try
    Graphics.DrawPath(Pen, Path);
  finally
    Path.Free;
  end;
end;

procedure FillStar(const Graphics: TGPGraphics; const Brush: TGPBrush;
  const Left, Top, Width, Height: Double; const Direction: TShapeDirection);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeStarPath(Left, Top, Width, Height, Direction);
  try
    Graphics.FillPath(Brush, Path);
  finally
    Path.Free;
  end;
end;

// Rhomb

function MakeRhombPath(const Left, Top, Width, Height: Double): TGPGraphicsPath;
var
  Points: array [0..3] of TGPPointF;
begin
  Result := TGPGraphicsPath.Create;

  Points[0] := MakePoint(Left + Width / 2, Top);
  Points[1] := MakePoint(Left + Width, Top + Height / 2);
  Points[2] := MakePoint(Left + Width / 2, Top + Height);
  Points[3] := MakePoint(Left, Top + Height / 2);

  Result.AddPolygon(PGPPointF(@Points[0]), 4);
end;

procedure DrawRhomb(const Graphics: TGPGraphics; const Pen: TGPPen;
  const Left, Top, Width, Height: Double);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeRhombPath(Left, Top, Width, Height);
  try
    Graphics.DrawPath(Pen, Path);
  finally
    Path.Free;
  end;
end;

procedure FillRhomb(const Graphics: TGPGraphics; const Brush: TGPBrush;
  const Left, Top, Width, Height: Double);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeRhombPath(Left, Top, Width, Height);
  try
    Graphics.FillPath(Brush, Path);
  finally
    Path.Free;
  end;
end;

// arrow

function MakeArrowPath(const Left, Top, Width, Height: Double;
  const Direction: TShapeDirection): TGPGraphicsPath;
const
  TailSize = 0.33;
var
  Points: array [0..6] of TGPPointF;
  TipWidth: Double;
  TailWidth: Double;
begin
  Result := TGPGraphicsPath.Create;

  case Direction of
    TShapeDirection.Top:
    begin
      TipWidth := Trunc(Width * 0.5);
      TailWidth := Width * TailSize;
      Points[0] := MakePoint(Trunc(Left + Width * 0.5 - TailWidth * 0.5), Top + Height);
      Points[1] := MakePoint(Trunc(Left + Width * 0.5 - TailWidth * 0.5), Top + TipWidth);
      Points[2] := MakePoint(Left, Top + TipWidth);
      Points[3] := MakePoint(Left + Width * 0.5, Top);
      Points[4] := MakePoint(Left + Width, Top + TipWidth);
      Points[5] := MakePoint(Trunc(Left + Width * 0.5 + TailWidth * 0.5), Top + TipWidth);
      Points[6] := MakePoint(Trunc(Left + Width * 0.5 + TailWidth * 0.5), Top + Height);
    end;
    TShapeDirection.Left:
    begin
      TipWidth := Trunc(Height * 0.5);
      TailWidth := Height * TailSize;
      Points[0] := MakePoint(Left + Width, Trunc(Top + Height * 0.5 - TailWidth * 0.5));
      Points[1] := MakePoint(Left + TipWidth, Trunc(Top + Height * 0.5 - TailWidth * 0.5));
      Points[2] := MakePoint(Left + TipWidth, Top);
      Points[3] := MakePoint(Left, Top + Height * 0.5);
      Points[4] := MakePoint(Left + TipWidth, Top + Height);
      Points[5] := MakePoint(Left + TipWidth, Trunc(Top + Height * 0.5 + TailWidth * 0.5));
      Points[6] := MakePoint(Left + Width, Trunc(Top + Height * 0.5 + TailWidth * 0.5));
    end;
    TShapeDirection.Right:
    begin
      TipWidth := Trunc(Height * 0.5);
      TailWidth := Height * TailSize;
      Points[0] := MakePoint(Left, Trunc(Top + Height * 0.5 - TailWidth * 0.5));
      Points[1] := MakePoint(Left + Width - TipWidth, Trunc(Top + Height * 0.5 - TailWidth * 0.5));
      Points[2] := MakePoint(Left + Width - TipWidth, Top);
      Points[3] := MakePoint(Left + Width, Top + Height * 0.5);
      Points[4] := MakePoint(Left + Width - TipWidth, Top + Height);
      Points[5] := MakePoint(Left + Width - TipWidth, Trunc(Top + Height * 0.5 + TailWidth * 0.5));
      Points[6] := MakePoint(Left, Trunc(Top + Height * 0.5 + TailWidth * 0.5));
    end;
    TShapeDirection.Bottom:
    begin
      TipWidth := Trunc(Width * 0.5);
      TailWidth := Width * TailSize;
      Points[0] := MakePoint(Trunc(Left + Width * 0.5 - TailWidth * 0.5), Top);
      Points[1] := MakePoint(Trunc(Left + Width * 0.5 - TailWidth * 0.5), Top + Height - TipWidth);
      Points[2] := MakePoint(Left, Top + Height - TipWidth);
      Points[3] := MakePoint(Left + Width * 0.5, Top + Height);
      Points[4] := MakePoint(Left + Width, Top + Height - TipWidth);
      Points[5] := MakePoint(Trunc(Left + Width * 0.5 + TailWidth * 0.5), Top + Height - TipWidth);
      Points[6] := MakePoint(Trunc(Left + Width * 0.5 + TailWidth * 0.5), Top);
    end;
  end;

  Result.AddPolygon(PGPPointF(@Points[0]), Length(Points));
end;

procedure DrawArrow(const Graphics: TGPGraphics; const Pen: TGPPen;
  const Left, Top, Width, Height: Double; Direction: TShapeDirection);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeArrowPath(Left, Top, Width, Height, Direction);
  try
    Graphics.DrawPath(Pen, Path);
  finally
    Path.Free;
  end;
end;

procedure FillArrow(const Graphics: TGPGraphics; const Brush: TGPBrush;
  const Left, Top, Width, Height: Double; Direction: TShapeDirection);
var
  Path: TGPGraphicsPath;
begin
  Path := MakeArrowPath(Left, Top, Width, Height, Direction);
  try
    Graphics.FillPath(Brush, Path);
  finally
    Path.Free;
  end;
end;

// pen

function MakeGPPenFormShapePen(Pen: TShapePen; Control: TControl): TGPPen;
var
  GPColor: TGPColor;
begin
  GPColor := ColorToGPColor(BorderColorToRgb(Pen.Color, Control), Pen.Opacity);
  Result := TGPPen.Create(GPColor);
  Result.SetWidth(Pen.Width);
  Result.SetDashCap(DashCapRound);
  case Pen.Style of
    TShapePenStyle.Solid:
      Result.SetDashStyle(DashStyleSolid);
    TShapePenStyle.Dash:
      Result.SetDashStyle(DashStyleDash);
    TShapePenStyle.Dot:
      Result.SetDashStyle(DashStyleDot);
    TShapePenStyle.DashDot:
      Result.SetDashStyle(DashStyleDashDot);
    TShapePenStyle.DashDotDot:
      Result.SetDashStyle(DashStyleDashDotDot);
    TShapePenStyle.Clear:
      Result.SetColor(0);
  end;
end;

// brush

function MakeGPBrushFormShapeBrush(Brush: TShapeBrush; Control: TControl;
  GradientLeft, GradientTop, GradientWidth, GradientHeight: Single): TGPBrush;
var
  GPColor: TGPColor;
begin
  if Brush.Style = TShapeBrushStyle.VerticalGradient then
  begin
    Result := TGPLinearGradientBrush.Create(
      MakePoint(0.0, GradientTop),
      MakePoint(0.0, GradientTop + GradientHeight),
      ColorToGPColor(ClientColorToRgb(Brush.StartColor, Control), Brush.Opacity),
      ColorToGPColor(ClientColorToRgb(Brush.EndColor, Control), Brush.Opacity)
    );
    TGPLinearGradientBrush(Result).SetWrapMode(WrapModeTileFlipXY);
    Exit;
  end
  else if Brush.Style = TShapeBrushStyle.HorizontalGradient then
  begin
    Result := TGPLinearGradientBrush.Create(
      MakePoint(GradientLeft, 0.0),
      MakePoint(GradientLeft + GradientWidth, 0.0),
      ColorToGPColor(ClientColorToRgb(Brush.StartColor, Control), Brush.Opacity),
      ColorToGPColor(ClientColorToRgb(Brush.EndColor, Control), Brush.Opacity)
    );
    TGPLinearGradientBrush(Result).SetWrapMode(WrapModeTileFlipXY);
    Exit;
  end;

  GPColor := ColorToGPColor(ClientColorToRgb(Brush.Color, Control), Brush.Opacity);
  case Brush.Style of
    TShapeBrushStyle.Solid:
      Result := TGPSolidBrush.Create(GPColor);
    TShapeBrushStyle.Clear:
      Result := TGPSolidBrush.Create(0);
    TShapeBrushStyle.Horizontal:
      Result := TGPHatchBrush.Create(HatchStyleHorizontal, GPColor, 0);
    TShapeBrushStyle.Vertical:
      Result := TGPHatchBrush.Create(HatchStyleVertical, GPColor, 0);
    TShapeBrushStyle.ForwardDiagonal:
      Result := TGPHatchBrush.Create(HatchStyleForwardDiagonal, GPColor, 0);
    TShapeBrushStyle.BackwardDiagonal:
      Result := TGPHatchBrush.Create(HatchStyleBackwardDiagonal, GPColor, 0);
    TShapeBrushStyle.Cross:
      Result := TGPHatchBrush.Create(HatchStyleCross, GPColor, 0);
    TShapeBrushStyle.DiagonalCross:
      Result := TGPHatchBrush.Create(HatchStyleDiagonalCross, GPColor, 0);
    TShapeBrushStyle.HalfHatch:
      Result := TGPHatchBrush.Create(HatchStyle50Percent, GPColor, 0);
    TShapeBrushStyle.ZigZag:
      Result := TGPHatchBrush.Create(HatchStyleZigZag, GPColor, 0);
    else
      Result := TGPSolidBrush.Create(GPColor);
  end;
end;

{ TShapePen }

procedure TShapePen.Assign(Source: TPersistent);
begin
  if Source is TShapePen then
  begin
    Self.FOpacity := TShapePen(Source).FOpacity;
    Self.FColor := TShapePen(Source).FColor;
    Self.FStyle := TShapePen(Source).FStyle;
    Self.FWidth := TShapePen(Source).FWidth;
    Changed;
  end else
    inherited;
end;

procedure TShapePen.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TShapePen.Create;
begin
  inherited;
  FOpacity := 255;
  FColor := clBlack;
  FStyle := TShapePenStyle.Solid;
  FWidth := 1;
end;

destructor TShapePen.Destroy;
begin
  inherited;
end;

procedure TShapePen.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TShapePen.SetOpacity(const Value: Byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TShapePen.SetStyle(const Value: TShapePenStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TShapePen.SetWidth(const Value: Integer);
begin
  if (Value <> FWidth) and (Value >= 0) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TShapeBrush }

procedure TShapeBrush.Assign(Source: TPersistent);
begin
  if Source is TShapeBrush then
  begin
    Self.FOpacity := TShapeBrush(Source).FOpacity;
    Self.FColor := TShapeBrush(Source).FColor;
    Self.FStyle := TShapeBrush(Source).FStyle;
    Changed;
  end else
    inherited;
end;

procedure TShapeBrush.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TShapeBrush.Create;
begin
  inherited;
  FOpacity := 255;
  FColor := clWhite;
  FStartColor := clWhite;
  FEndColor := clSilver;
  FStyle := TShapeBrushStyle.Solid;
end;

destructor TShapeBrush.Destroy;
begin
  inherited;
end;

procedure TShapeBrush.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TShapeBrush.SetEndColor(const Value: TColor);
begin
  if Value <> FEndColor then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TShapeBrush.SetStartColor(const Value: TColor);
begin
  if Value <> FStartColor then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

procedure TShapeBrush.SetOpacity(const Value: Byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TShapeBrush.SetStyle(const Value: TShapeBrushStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Changed;
  end;
end;

{ TEsShape }

procedure TEsShape.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TEsShape.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csReplicatable] - [csSetCaption];
  Width := 64;
  Height := 64;

  FPen := TShapePen.Create;
  FPen.OnChange := StyleChanged;
  FBrush := TShapeBrush.Create;
  FBrush.OnChange := StyleChanged;

  FCornerRadius := 10;
  FOpacity := 255;

  FCorners := AllShapeCorners;
  FSides := AllShapeSides;

  Caption := '';
end;

destructor TEsShape.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited;
end;

{$IFDEF VER310UP}
procedure TEsShape.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  FPen.Width := MulDiv(FPen.Width, M, D);
  FCornerRadius := MulDiv(FCornerRadius, M, D);
  inherited;
end;
{$ELSE}
procedure TEsShape.ChangeScale(M, D: Integer);
begin
  FPen.Width := MulDiv(FPen.Width, M, D);
  FCornerRadius := MulDiv(FCornerRadius, M, D);
  inherited;
end;
{$ENDIF}

procedure TEsShape.Paint;
var
  GpGraphics: TGPGraphics;
  GpBrush: TGPBrush;
  GpPen: TGPPen;
  TempBitmap: TBitmap;
  ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight: Double;
  FakePenWidth: Integer;
  TextRect: TRect;
begin
  inherited;

  if (ContentRect.Width <= 0) or (ContentRect.Height <= 0) then
    Exit;

  // trick for best align
  if (Pen.Style <> TShapePenStyle.Clear) or (Pen.Width > 1) then
    FakePenWidth := Pen.Width
  else
    FakePenWidth := 0;

  // calc size for pen width
  ShapeLeft := ContentRect.Left + FakePenWidth div 2 - ((FakePenWidth + 1) mod 2) * 0.5;
  ShapeTop := ContentRect.Top + FakePenWidth div 2 - ((FakePenWidth + 1) mod 2) * 0.5;
  ShapeWidth := ContentRect.Width - FakePenWidth;
  ShapeHeight := ContentRect.Height - FakePenWidth;

  // square
  if Shape in [TShapeKind.Square, TShapeKind.RoundSquare, TShapeKind.Circle, TShapeKind.Star] then
  begin
    if ShapeWidth > ShapeHeight then
    begin
      ShapeLeft := ShapeLeft + Trunc(ShapeWidth - ShapeHeight) div 2;
      ShapeWidth := ShapeHeight;
    end else
    begin
      ShapeTop := ShapeTop + Trunc(ShapeHeight - ShapeWidth) div 2;
      ShapeHeight := ShapeWidth;
    end;
  end;

  GpGraphics := nil;
  GpBrush := nil;
  GpPen := nil;
  TempBitmap := nil;
  try
    if Opacity <> 255 then
    begin
      TempBitmap := TBitmap.Create;
      TempBitmap.SetSize(Width, Height);
      TempBitmap.PixelFormat := pf32bit;
      GpGraphics := TGPGraphics.Create(TempBitmap.Canvas.Handle);
      GpGraphics.Clear(0);
    end else
      GpGraphics := TGPGraphics.Create(Canvas.Handle);

    GpGraphics.SetSmoothingMode(SmoothingModeHighQuality);

    GpPen := MakeGPPenFormShapePen(Pen, Self);
    if Pen.Width = 0 then
      GpPen.SetColor(0);
    GpPen.SetLineJoin(LineJoinMiterClipped);
    GpPen.SetMiterLimit(Pen.Width* 0.5);

    GpBrush := MakeGPBrushFormShapeBrush(Brush, Self, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight);

    case Shape of
      TShapeKind.Rectangle,
      TShapeKind.Square:
      begin
        GpGraphics.FillRectangle(GpBrush, ShapeLeft - 0.5, ShapeTop - 0.5, ShapeWidth + 1, ShapeHeight + 1);
        if Sides = AllShapeSides then
          GpGraphics.DrawRectangle(GpPen, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight)
        else
          DrawRoundRectangle(GpGraphics, GpPen,
            ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, 0, Corners, Sides);
      end;
      TShapeKind.RoundRectangle,
      TShapeKind.RoundSquare:
      begin
        FillRoundRectangle(GpGraphics, GpBrush,
          ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, CornerRadius, Corners, Sides);
        DrawRoundRectangle(GpGraphics, GpPen,
          ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, CornerRadius, Corners, Sides);
      end;
      TShapeKind.Ellipse,
      TShapeKind.Circle:
      begin
        GpGraphics.FillEllipse(GpBrush, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight);
        GpGraphics.DrawEllipse(GpPen, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight);
      end;
      TShapeKind.Triangle:
      begin
        FillTriangle(GpGraphics, GpBrush, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, Direction);
        DrawTriangle(GpGraphics, GpPen, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, Direction);
      end;
      TShapeKind.Star:
      begin
        FillStar(GpGraphics, GpBrush, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, Direction);
        DrawStar(GpGraphics, GpPen, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, Direction);
      end;
      TShapeKind.Rhomb:
      begin
        FillRhomb(GpGraphics, GpBrush, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight);
        DrawRhomb(GpGraphics, GpPen, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight);
      end;
      TShapeKind.Bubble:
      begin
        if ContentRect.Width > ContentRect.Height then
        begin
          FillRoundRectangle(GpGraphics, GpBrush,
            ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, ShapeHeight * 0.5, Corners, Sides);
          DrawRoundRectangle(GpGraphics, GpPen,
            ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, ShapeHeight * 0.5, Corners, Sides);
        end else
        begin
          FillRoundRectangle(GpGraphics, GpBrush,
            ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, ShapeWidth * 0.5, Corners, Sides);
          DrawRoundRectangle(GpGraphics, GpPen,
            ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, ShapeWidth * 0.5, Corners, Sides);
        end;
      end;
      TShapeKind.Arrow:
      begin
        FillArrow(GpGraphics, GpBrush, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, Direction);
        DrawArrow(GpGraphics, GpPen, ShapeLeft, ShapeTop, ShapeWidth, ShapeHeight, Direction);
      end;
    end;

    if TempBitmap <> nil then
      Canvas.Draw(0, 0, TempBitmap, Opacity);
  finally
    GpGraphics.Free;
    GpBrush.Free;
    GpPen.Free;
    TempBitmap.Free;
  end;

  if Caption <> '' then
  begin
    TextRect := ContentRect;
    Canvas.Font := Font;
    Canvas.Font.Color := FontColorToRgb(Font.Color, Self);
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, Caption, -1, TextRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
end;

procedure TEsShape.SetBrush(const Value: TShapeBrush);
begin
  FBrush.Assign(Value);
end;

procedure TEsShape.SetCornerRadius(const Value: Integer);
begin
  if Value <> FCornerRadius then
  begin
    FCornerRadius := Value;
    StyleChanged(Self);
  end;
end;

procedure TEsShape.SetCorners(const Value: TShapeCorners);
begin
  if Value <> FCorners then
  begin
    FCorners := Value;
    StyleChanged(Self);
  end;
end;

procedure TEsShape.SetDirection(const Value: TShapeDirection);
begin
  if Value <> FDirection then
  begin
    FDirection := Value;
    StyleChanged(Self);
  end;
end;

procedure TEsShape.SetOpacity(const Value: Byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    StyleChanged(Self);
  end;
end;

procedure TEsShape.SetPen(const Value: TShapePen);
begin
  FPen.Assign(Value);
end;

procedure TEsShape.SetShape(const Value: TShapeKind);
begin
  if Value <> FShape then
  begin
    FShape := Value;
    StyleChanged(Self);
  end;
end;

procedure TEsShape.SetSides(const Value: TShapeSides);
begin
  if Value <> FSides then
  begin
    FSides := Value;
    StyleChanged(Self);
  end;
end;

procedure TEsShape.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

end.
