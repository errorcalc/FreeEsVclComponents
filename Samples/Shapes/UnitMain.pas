unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, ES.BaseControls,
  ES.Shapes, ES.Layouts, Vcl.ComCtrls, Vcl.StdCtrls, ES.Switch, Vcl.ControlList;

type
  TFormMain = class(TForm)
    TimerAnimate: TTimer;
    EsLayoutShapes: TEsLayout;
    EsShapeAni11: TEsShape;
    EsShapeAni10: TEsShape;
    EsShapeAni9: TEsShape;
    EsShapeAni8: TEsShape;
    EsShapeAni1: TEsShape;
    EsShapeAni7: TEsShape;
    EsShapeAni6: TEsShape;
    EsShapeAni5: TEsShape;
    EsShapeAni4: TEsShape;
    EsShapeAni3: TEsShape;
    EsShapeAni2: TEsShape;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    EsShapeAni12: TEsShape;
    EsShapeAni13: TEsShape;
    EsShapeAni14: TEsShape;
    EsShapeAni15: TEsShape;
    EsLayout1: TEsLayout;
    EsShape1: TEsShape;
    ShapeRoundRect: TEsShape;
    CheckBoxTopLeft: TCheckBox;
    CheckBoxBottomLeft: TCheckBox;
    CheckBoxBottomRight: TCheckBox;
    CheckBoxTopRight: TCheckBox;
    CheckBoxRight: TCheckBox;
    CheckBoxLeft: TCheckBox;
    CheckBoxBottom: TCheckBox;
    CheckBoxTop: TCheckBox;
    TrackBarRadius: TTrackBar;
    Label1: TLabel;
    EsSwitchScale2X: TEsSwitch;
    EsLayout2: TEsLayout;
    EsPanel1: TEsPanel;
    EsShape2: TEsShape;
    EsShape3: TEsShape;
    EsShape5: TEsShape;
    EsPanel2: TEsPanel;
    EsShape6: TEsShape;
    EsShape7: TEsShape;
    EsShape9: TEsShape;
    Label2: TLabel;
    Label3: TLabel;
    EsPanel3: TEsPanel;
    EsShape10: TEsShape;
    EsShape11: TEsShape;
    EsShape13: TEsShape;
    EsPanel4: TEsPanel;
    EsShape14: TEsShape;
    EsShape15: TEsShape;
    EsShape17: TEsShape;
    EsShape16: TEsShape;
    EsShape12: TEsShape;
    EsShape8: TEsShape;
    EsShape4: TEsShape;
    Label4: TLabel;
    Label5: TLabel;
    TabSheet4: TTabSheet;
    ControlList: TControlList;
    LabelProduct: TLabel;
    EsShapeAviable: TEsShape;
    EsShapeStar1: TEsShape;
    EsShapeStar2: TEsShape;
    EsShapeStar3: TEsShape;
    EsShapeStar4: TEsShape;
    EsShapeStar5: TEsShape;
    EsShape18: TEsShape;
    Label6: TLabel;
    EsShape19: TEsShape;
    Label7: TLabel;
    procedure TimerAnimateTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EsSwitchScale2XClick(Sender: TObject);
    procedure CheckBoxRoundRectClick(Sender: TObject);
    procedure TrackBarRadiusChange(Sender: TObject);
    procedure ControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
  private
    SpeedArray: array of TPoint;
    IsUpdateRoundRectCheckboxes: Boolean;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  Vcl.Themes, Vcl.StyleAPI;

{$R *.dfm}

procedure TFormMain.CheckBoxRoundRectClick(Sender: TObject);
begin
  if IsUpdateRoundRectCheckboxes then
    Exit;

  if CheckBoxTop.Checked then
    ShapeRoundRect.Sides := ShapeRoundRect.Sides + [TShapeSide.Top]
  else
    ShapeRoundRect.Sides := ShapeRoundRect.Sides - [TShapeSide.Top];

  if CheckBoxLeft.Checked then
    ShapeRoundRect.Sides := ShapeRoundRect.Sides + [TShapeSide.Left]
  else
    ShapeRoundRect.Sides := ShapeRoundRect.Sides - [TShapeSide.Left];

  if CheckBoxBottom.Checked then
    ShapeRoundRect.Sides := ShapeRoundRect.Sides + [TShapeSide.Bottom]
  else
    ShapeRoundRect.Sides := ShapeRoundRect.Sides - [TShapeSide.Bottom];

  if CheckBoxRight.Checked then
    ShapeRoundRect.Sides := ShapeRoundRect.Sides + [TShapeSide.Right]
  else
    ShapeRoundRect.Sides := ShapeRoundRect.Sides - [TShapeSide.Right];

  if CheckBoxTopLeft.Checked then
    ShapeRoundRect.Corners := ShapeRoundRect.Corners + [TShapeCorner.TopLeft]
  else
    ShapeRoundRect.Corners := ShapeRoundRect.Corners - [TShapeCorner.TopLeft];

  if CheckBoxTopRight.Checked then
    ShapeRoundRect.Corners := ShapeRoundRect.Corners + [TShapeCorner.TopRight]
  else
    ShapeRoundRect.Corners := ShapeRoundRect.Corners - [TShapeCorner.TopRight];

  if CheckBoxBottomLeft.Checked then
    ShapeRoundRect.Corners := ShapeRoundRect.Corners + [TShapeCorner.BottomLeft]
  else
    ShapeRoundRect.Corners := ShapeRoundRect.Corners - [TShapeCorner.BottomLeft];

  if CheckBoxBottomRight.Checked then
    ShapeRoundRect.Corners := ShapeRoundRect.Corners + [TShapeCorner.BottomRight]
  else
    ShapeRoundRect.Corners := ShapeRoundRect.Corners - [TShapeCorner.BottomRight];
end;

procedure TFormMain.ControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
var
  Aviable: Integer;
  Count: Integer;
begin
  // aviable
  Aviable := (AIndex mod 2 + AIndex mod 3 + AIndex mod 5 + 6) mod 5;
  if Aviable = 0 then
    EsShapeAviable.Brush.Color := clSkyBlue
  else if Aviable = 1 then
    EsShapeAviable.Brush.Color := clBlue
  else
    EsShapeAviable.Brush.Color := clNone;
  // name
  LabelProduct.Caption := 'Product ' + (AIndex + 1).ToString;
  // raiting
  Count := (AIndex mod 8 + AIndex mod 3 + AIndex mod 6 + AIndex mod 5 + AIndex mod 7 + 9) mod 6;
  if Count >= 1 then
    EsShapeStar1.Brush.Color := clYellow
  else
    EsShapeStar1.Brush.Color := clWhite;
  if Count >= 2 then
    EsShapeStar2.Brush.Color := clYellow
  else
    EsShapeStar2.Brush.Color := clWhite;
  if Count >= 3 then
    EsShapeStar3.Brush.Color := clYellow
  else
    EsShapeStar3.Brush.Color := clWhite;
  if Count >= 4 then
    EsShapeStar4.Brush.Color := clYellow
  else
    EsShapeStar4.Brush.Color := clWhite;
  if Count >= 5 then
    EsShapeStar5.Brush.Color := clYellow
  else
    EsShapeStar5.Brush.Color := clWhite;
end;

procedure TFormMain.EsSwitchScale2XClick(Sender: TObject);
begin
  if TEsSwitch(Sender).Checked then
    Self.ScaleForPPI(Self.GetDesignDpi * 2)
  else
    Self.ScaleForPPI(Self.GetDesignDpi div 2);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  //TStyleManager.Initialize;
  //TStyleManager.TrySetStyle('Windows');
  // Shapes page
  SetLength(SpeedArray, EsLayoutShapes.ControlCount);
  for I := 0 to EsLayoutShapes.ControlCount - 1 do
  begin
    repeat
      SpeedArray[I].X := 3 - Random(7);
    until SpeedArray[I].X <> 0;
    repeat
      SpeedArray[I].Y := 3 - Random(7);
    until SpeedArray[I].Y <> 0;
  end;

  // RoundRectangle page
  IsUpdateRoundRectCheckboxes := True;
  try
    CheckBoxTop.Checked := TShapeSide.Top in ShapeRoundRect.Sides;
    CheckBoxLeft.Checked := TShapeSide.Left in ShapeRoundRect.Sides;
    CheckBoxBottom.Checked := TShapeSide.Bottom in ShapeRoundRect.Sides;
    CheckBoxRight.Checked := TShapeSide.Right in ShapeRoundRect.Sides;
    CheckBoxTopLeft.Checked := TShapeCorner.TopLeft in ShapeRoundRect.Corners;
    CheckBoxTopRight.Checked := TShapeCorner.TopRight in ShapeRoundRect.Corners;
    CheckBoxBottomLeft.Checked := TShapeCorner.BottomLeft in ShapeRoundRect.Corners;
    CheckBoxBottomRight.Checked := TShapeCorner.BottomRight in ShapeRoundRect.Corners;
  finally
    IsUpdateRoundRectCheckboxes := False;
  end;
  TrackBarRadius.Position := ShapeRoundRect.CornerRadius;

  Self.SetFocusedControl(TrackBarRadius);
end;

procedure TFormMain.TimerAnimateTimer(Sender: TObject);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to EsLayoutShapes.ControlCount - 1 do
  begin
    Control := EsLayoutShapes.Controls[I];

    Control.Left := Control.Left + SpeedArray[I].X;
    Control.Top := Control.Top + SpeedArray[I].Y;

    if Control.Left < 8 then
      SpeedArray[I].X := Random(3) + 1;
    if Control.Top < 8 then
      SpeedArray[I].Y := Random(3) + 1;
    if Control.Left > EsLayoutShapes.Width - Control.Width - 8 then
      SpeedArray[I].X := -Random(3) - 1;
    if Control.Top > EsLayoutShapes.Height - Control.Height - 8 then
      SpeedArray[I].Y := -Random(3) - 1;
  end;
end;

procedure TFormMain.TrackBarRadiusChange(Sender: TObject);
begin
  ShapeRoundRect.CornerRadius := TrackBarRadius.Position;
end;

end.
