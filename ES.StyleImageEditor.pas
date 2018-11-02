unit ES.StyleImageEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ToolWin,
  Vcl.ComCtrls, Es.CfxClasses, Vcl.Imaging.PngImage, Es.Backend.Selection, Es.Images;

{$SCOPEDENUMS on}

type
  TStyleImageEditor = class(TForm)
    CancelButton: TButton;
    OkButton: TButton;
    PlusSpeedButton: TSpeedButton;
    MinusSpeedButton: TSpeedButton;
    ZoomComboBox: TComboBox;
    DispScrollBox: TScrollBox;
    FooterPanel: TPanel;
    HeaderPanel: TPanel;
    ZoomLabel: TLabel;
    LoadButton: TButton;
    Panel1: TPanel;
    Disp: TPaintBox;
    procedure PlusSpeedButtonClick(Sender: TObject);
    procedure MinusSpeedButtonClick(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ZoomComboBoxChange(Sender: TObject);
    procedure DispPaint(Sender: TObject);
    procedure DispMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DispMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure DispMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FBitmap: TBitmap;
    d: THeaderStyle;
    FImageMargins: TImageMargins;
    procedure SetBitmap(const Value: TBitmap);
    procedure ChangeSelection(Sender: TObject);
    procedure SetImageMargins(const Value: TImageMargins);
    function GetImageRect: TRect;
  private type
    TGrip = (None, TopLeft, TopRight, BottomLeft, BottomRight, Left, Top, Right, Bottom);
  private
    Selection: TBackendSelection;
    Sx, Sy: Integer;
    CaptureGrip: TGrip;
    GripImage: TPngImage;
    procedure BitmapChange(Sender: TObject);
    procedure ImageMarginsChange(Sender: TObject);
    function GetZoom: Integer;
    procedure SetZoom(const Value: Integer);
    property ImageRect: TRect read GetImageRect;
    property Zoom: Integer read GetZoom write SetZoom;
  public
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;
  end;


implementation

{$R *.dfm}

uses
  Es.ExGraphics, System.Math;

const
  GripRadius = 4;
  LineWidth = 2;

procedure TStyleImageEditor.BitmapChange(Sender: TObject);
begin
  Disp.Width := Bitmap.Width * Zoom;
  Disp.Height := Bitmap.Height * Zoom;
  Selection.Bounds.SetBounds(0, 0, Bitmap.Width-4, Bitmap.Height-4);
  Disp.Invalidate;
end;

procedure TStyleImageEditor.ChangeSelection(Sender: TObject);
begin
  Disp.Invalidate;
  AtomicIncrement(Sx);
end;

procedure TStyleImageEditor.DispMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  CaptureGrip := TGrip.TopLeft;//TestGrip(X, Y, Sx, Sy);
  Selection.MouseDown(X, Y);
end;

procedure TStyleImageEditor.DispMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Lx, Ly: Integer;
begin
  if CaptureGrip = TGrip.None then
//    if TestGrip(X, Y, Sx, Sy) <> TGrip.None then
//      case TestGrip(X, Y, Sx, Sy) of
//        TGrip.TopLeft, TGrip.BottomRight:
//          TPaintBox(Sender).Cursor := crSizeNWSE;
//
//        TGrip.BottomLeft, TGrip.TopRight:
//          TPaintBox(Sender).Cursor := crSizeNESW;
//
//        TGrip.Left, TGrip.Right:
//          TPaintBox(Sender).Cursor := crSizeWE;
//      end
    if Selection.GripKind(X, Y) <> TGripKind.None then
      case Selection.GripKind(X, Y) of
        TGripKind.Top, TGripKind.Bottom: TPaintBox(Sender).Cursor := crSizeNS;
        TGripKind.Right, TGripKind.Left: TPaintBox(Sender).Cursor := crSizeWE;
        TGripKind.TopLeft, TGripKind.BottomRight: TPaintBox(Sender).Cursor := crSizeNWSE;
        TGripKind.TopRight, TGripKind.BottomLeft: TPaintBox(Sender).Cursor := crSizeNESW;
        TGripKind.Center, TGripKind.Fill: TPaintBox(Sender).Cursor := crSizeAll;
      end
    else
      TPaintBox(Sender).Cursor := crDefault
  else
  begin
    Selection.MouseMove(X, Y);
//    Lx := (X + Sx) div Zoom;
//    Ly := (Y + Sy) div Zoom;
//    case CaptureGrip of
//      TStyleImageEditor.TGrip.TopLeft:
//        ImageRect := Rect(Max(0, Min(Lx, ImageRect.Right)), Max(0, Min(Ly, ImageRect.Bottom)), ImageRect.Right, ImageRect.Bottom);
//
//      TStyleImageEditor.TGrip.TopRight:
//        ImageRect := Rect(ImageRect.Left, Max(0, Min(Ly, ImageRect.Bottom)), Min(Max(Lx, ImageRect.Left), Bitmap.Width), ImageRect.Bottom);
//
//      TStyleImageEditor.TGrip.BottomLeft:
//        ImageRect := Rect(Max(0, Min(Lx, ImageRect.Right)), ImageRect.Top, ImageRect.Right, Min(Max(Ly, ImageRect.Top), Bitmap.Height));
//
//      TStyleImageEditor.TGrip.BottomRight:
//        ImageRect := Rect(ImageRect.Left, ImageRect.Top, Min(Max(Lx, ImageRect.Left), Bitmap.Width), Min(Max(Ly, ImageRect.Top), Bitmap.Height));
//
//      TStyleImageEditor.TGrip.Left:
//        ImageRect := Rect(Max(0, Min(Lx, ImageRect.Right)), ImageRect.Top, ImageRect.Right, ImageRect.Bottom);
//
//      TStyleImageEditor.TGrip.Right:
//        ImageRect := Rect(ImageRect.Left, ImageRect.Top, Min(Bitmap.Width, Max(Lx, ImageRect.Left)), ImageRect.Bottom);
//    end;
  end;
end;

procedure TStyleImageEditor.DispMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  CaptureGrip := TGrip.None;
//  if CaptureGrip = TGrip.None then
//    if TestGrip(X, Y, Sx, Sy) <> TGrip.None then
//      case TestGrip(X, Y, Sx, Sy) of
//        TGrip.TopLeft, TGrip.BottomRight:
//          TPaintBox(Sender).Cursor := crSizeNWSE;
//
//        TGrip.BottomLeft, TGrip.TopRight:
//          TPaintBox(Sender).Cursor := crSizeNESW;
//
//        TGrip.Left:
//          TPaintBox(Sender).Cursor := crSizeWE;
//      end
//    else
//      TPaintBox(Sender).Cursor := crDefault;
  Selection.MouseUp(X, Y);
end;

procedure TStyleImageEditor.DispPaint(Sender: TObject);
  procedure DrawBox(Canvas: TCanvas; X, Y, Width: Integer);
  begin
    Canvas.Ellipse(X - Width, Y - Width, X + Width, Y + Width);
    //Canvas.Draw(X - GripImage.Width div 2, Y - GripImage.Height div 2, GripImage);
  end;
var
  State: ICanvasSaver;
begin
  Caption := Random(100000).toString;
  TPaintBox(Sender).Canvas.StretchDraw(Rect(0, 0, TEsImageControl(Sender).Width, TEsImageControl(Sender).Height), Bitmap);

//  TPaintBox(Sender).Canvas.DrawTransparentFrame(
//    Rect((0 + ImageMargins.Left) * Zoom, (0 + ImageMargins.Top) * Zoom,
//      (Bitmap.Width - ImageMargins.Right) * Zoom, (Bitmap.Height - ImageMargins.Bottom) * Zoom),
//    RGB(0, 0, 0), RGB(255, 255, 255), 200, '12 21 ');
  //TPaintBox(Sender).Canvas.DrawChessFrame(Rect((0 + ImageMargins.Left) * Zoom, (0 + ImageMargins.Top) * Zoom,
  //  (Bitmap.Width - ImageMargins.Right) * Zoom, (Bitmap.Height - ImageMargins.Bottom) * Zoom), clBlack, clWhite);
  State := TPaintBox(Sender).Canvas.SaveState([TBrush, TPen]);
  TPaintBox(Sender).Canvas.Pen.Style := psDot;
  TPaintBox(Sender).Canvas.Pen.Mode := pmNot;
  TPaintBox(Sender).Canvas.Brush.Style := bsClear;
//  TPaintBox(Sender).Canvas.Rectangle(Rect((0 + ImageMargins.Left) * Zoom, (0 + ImageMargins.Top) * Zoom,
//    (Bitmap.Width - ImageMargins.Right) * Zoom, (Bitmap.Height - ImageMargins.Bottom) * Zoom));
  TPaintBox(Sender).Canvas.Line(Selection.Selection.Left * Zoom, 0, Selection.Selection.Left * Zoom, Bitmap.Height * Zoom);
  if Selection.Selection.Left <> Selection.Selection.Right then
    TPaintBox(Sender).Canvas.Line(Selection.Selection.Right * Zoom - 1, 0, Selection.Selection.Right * Zoom - 1, Bitmap.Height * Zoom);
  TPaintBox(Sender).Canvas.Line(0, Selection.Selection.Top * Zoom, Bitmap.Width * Zoom, Selection.Selection.Top * Zoom);
  if Selection.Selection.Top <> Selection.Selection.Bottom then
    TPaintBox(Sender).Canvas.Line(0, Selection.Selection.Bottom * Zoom - 1, Bitmap.Width * Zoom, Selection.Selection.Bottom * Zoom - 1);
  TPaintBox(Sender).Canvas.RestoreState(State);

//  DrawBox(TPaintBox(Sender).Canvas,
//    Selection.Selection.Left * Zoom, Selection.Selection.Top * Zoom, 4);
//  DrawBox(TPaintBox(Sender).Canvas,
//    (Bitmap.Width - Selection.Selection.Right) * Zoom - 1, Selection.Selection.Top * Zoom, 4);
//  DrawBox(TPaintBox(Sender).Canvas,
//    (Bitmap.Width - Selection.Selection.Right) * Zoom - 1, (Bitmap.Height - Selection.Selection.Bottom) * Zoom - 1, 4);
//  DrawBox(TPaintBox(Sender).Canvas,
//    Selection.Selection.Left * Zoom, (Bitmap.Height - Selection.Selection.Bottom) * Zoom - 1, 4);
end;

procedure TStyleImageEditor.FormCreate(Sender: TObject);
begin
  Selection := TBackendSelection.Create;
  Selection.OnChange := ChangeSelection;
  Selection.Selection.SetBounds(10, 20, 30, 50);
  Selection.ProportionalRange.Min := 10;
  Selection.ProportionalRange.Max := 40;
  Selection.ProportionalRange.Kind := TProportionalKind.Width;
  Selection.ProportionalRatio := 1.3;
  Selection.Proportional := True;

  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChange;
  FImageMargins := TImageMargins.Create(nil);
  FImageMargins.SetBounds(0, 0, 0, 0);
  FImageMargins.OnChange := ImageMarginsChange;

  Zoom := ZoomComboBox.ItemIndex + 1;





  Bitmap.SetSize(100, 100);

  //Selection.LineWidth := 30;

  //GripImage := TPngImage.Create;
  //GripImage.LoadFromResourceName(hInstance, 'EsGripImage9x9');

  // tes
  //Bitmap.Canvas.Ellipse(0, 0, 100, 200);
  //
end;

procedure TStyleImageEditor.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
  FImageMargins.Free;

  GripImage.Free;
  Selection.Free;
end;

procedure TStyleImageEditor.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := True;

  case Msg.CharCode of
    VK_ADD, ord('P'):
      PlusSpeedButton.Click;

    VK_SUBTRACT, ord('M'):
      MinusSpeedButton.Click;

    VK_LEFT:
      if GetKeyState(VK_CONTROL) < 0 then
        Selection.Command(TSelectionCommand.DecX)
        //FImageMargins.SetBounds(FImageMargins.Left - 1, FImageMargins.Top, FImageMargins.Right + 1, FImageMargins.Bottom)
      else if GetKeyState(VK_SHIFT) < 0 then
        Selection.Command(TSelectionCommand.DecWidth);
        //ImageMargins.Right := ImageMargins.Right + 1;

    VK_RIGHT:
      if GetKeyState(VK_CONTROL) < 0 then
        Selection.Command(TSelectionCommand.IncX)
        //FImageMargins.SetBounds(FImageMargins.Left + 1, FImageMargins.Top, FImageMargins.Right - 1, FImageMargins.Bottom)
      else if GetKeyState(VK_SHIFT) < 0 then
        Selection.Command(TSelectionCommand.IncWidth);
        //ImageMargins.Right := ImageMargins.Right - 1;

    VK_UP:
      if GetKeyState(VK_CONTROL) < 0 then
        FImageMargins.SetBounds(FImageMargins.Left, FImageMargins.Top - 1, FImageMargins.Right, FImageMargins.Bottom + 1)
      else if GetKeyState(VK_SHIFT) < 0 then
        ImageMargins.Bottom := ImageMargins.Bottom + 1;

    VK_DOWN:
      if GetKeyState(VK_CONTROL) < 0 then
        FImageMargins.SetBounds(FImageMargins.Left, FImageMargins.Top + 1, FImageMargins.Right, FImageMargins.Bottom - 1)
      else if GetKeyState(VK_SHIFT) < 0 then
        ImageMargins.Bottom := ImageMargins.Bottom - 1;

    else
      Handled := False;
  end;
end;

procedure TStyleImageEditor.ImageMarginsChange(Sender: TObject);
begin
  //Selection.Selection.Rect := ImageRect;
   Disp.Invalidate;
end;

function TStyleImageEditor.GetImageRect: TRect;
begin
  Result := Rect(ImageMargins.Left, ImageMargins.Top, Bitmap.Width - ImageMargins.Right, Bitmap.Height - ImageMargins.Bottom);
end;

function TStyleImageEditor.GetZoom: Integer;
begin
  Result := Selection.Numerator;
end;

procedure TStyleImageEditor.MinusSpeedButtonClick(Sender: TObject);
begin
  if ZoomComboBox.ItemIndex > 0 then
    ZoomComboBox.ItemIndex := ZoomComboBox.ItemIndex - 1;
  ZoomComboBoxChange(nil);
end;

procedure TStyleImageEditor.PlusSpeedButtonClick(Sender: TObject);
begin
  if ZoomComboBox.ItemIndex < ZoomComboBox.Items.Count then
    ZoomComboBox.ItemIndex := ZoomComboBox.ItemIndex + 1;
  ZoomComboBoxChange(nil);
end;

procedure TStyleImageEditor.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TStyleImageEditor.SetImageMargins(const Value: TImageMargins);
begin
  FImageMargins.Assign(Value);
end;

procedure TStyleImageEditor.SetZoom(const Value: Integer);
begin
  Selection.Numerator := Value;
 // TEsImageControl(Disp).RecreateBitmap;
end;

procedure TStyleImageEditor.ZoomComboBoxChange(Sender: TObject);
begin
  Zoom := ZoomComboBox.ItemIndex + 1;
  BitmapChange(nil);
end;

end.
