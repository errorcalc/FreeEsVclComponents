unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg, ES.BaseControls,
  ES.Images, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TFormMain = class(TForm)
    ImageSource: TEsImageControl;
    LabelSource: TLabel;
    LabelProcessed: TLabel;
    ImageProcessed: TEsImageControl;
    RadioGroupMode: TRadioGroup;
    TimerAnimation: TTimer;
    ButtonSpeedTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupModeClick(Sender: TObject);
    procedure TimerAnimationTimer(Sender: TObject);
    procedure ButtonSpeedTestClick(Sender: TObject);
  private
    procedure Process();
    procedure ProcessInvert(const Bitmap: TBitmap);
    procedure ProcessGrayscale(const Bitmap: TBitmap);
    procedure ProcessAlpha(const Bitmap: TBitmap);
    procedure ProcessNoise(const Bitmap: TBitmap);
    procedure ProcessSwapChannels(const Bitmap: TBitmap);
    procedure ProcessOnlyRGB(const Bitmap: TBitmap);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  ES.BitmapPixels, Math;

{$R *.dfm}

{$POINTERMATH ON}
procedure TFormMain.ButtonSpeedTestClick(Sender: TObject);
var
  I: Integer;
  Bitmap: TBitmap;
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
  Color: TRGBQuad;
  PColor: PRGBQuad;
  Time1, Time2, Time3: Int64;
begin
  // test1
  // make bitmap buffer
  Bitmap := TBitmap.Create();
  try
    // assign source graphic to bitmap
    Bitmap.Assign(ImageSource.Picture.Graphic);

    // test
    Time1 := GetTickCount64();
    for I := 0 to 20 - 1 do
      for Y := 0 to Bitmap.Height - 1 do
      begin
        for X := 0 to Bitmap.Width - 1 do
        begin
          // read pixel
          Color := TRGBQuad(Bitmap.Canvas.Pixels[X, Y]);

          Color.rgbBlue := (Y * 255) div Bitmap.Height;
          Color.rgbRed := (X * 255) div Bitmap.Width;

          // write pixel
          Bitmap.Canvas.Pixels[X, Y] := TColor(Color);
        end;
      end;
    Time1 := Int64(GetTickCount64()) - Time1;
    // end test

    ImageProcessed.Picture.Assign(Bitmap);
  finally
    // free buffer
    Bitmap.Free();
  end;

  // test2
  // make bitmap buffer
  Bitmap := TBitmap.Create();
  try
    // assign source graphic to bitmap
    Bitmap.Assign(ImageSource.Picture.Graphic);
    Bitmap.PixelFormat := pf32bit;

    // test
    Time2 := GetTickCount64();
    for I := 0 to 20 - 1 do
      for Y := 0 to Bitmap.Height - 1 do
      begin
        PColor := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          // read pixel
          Color := PColor[X];

          Color.rgbBlue := (Y * 255) div Bitmap.Height;
          Color.rgbRed := (X * 255) div Bitmap.Width;
          Color.rgbReserved := 255;

          // write pixel
          PColor[X] := TRGBQuad(Color);
        end;
      end;
    Time2 := Int64(GetTickCount64()) - Time2;
    // end test

    ImageProcessed.Picture.Assign(Bitmap);
  finally
    // free buffer
    Bitmap.Free();
  end;

  // test3
  // make bitmap buffer
  Bitmap := TBitmap.Create();
  try
    // assign source graphic to bitmap
    Bitmap.Assign(ImageSource.Picture.Graphic);

    // test
    Time3 := GetTickCount64();
    Data.Map(Bitmap, TAccessMode.ReadWrite, True);
    try
      for I := 0 to 20 - 1 do
        for Y := 0 to Data.Height - 1 do
        begin
          for X := 0 to Data.Width - 1 do
          begin
            // read pixel
            Pixel := Data.GetPixelUnsafe(X, Y);

            Pixel.B := (Y * 255) div Data.Height;
            Pixel.R := (X * 255) div Data.Width;
            Pixel.A := 255;

            // write pixel
            Data.SetPixelUnsafe(X, Y, Pixel);
          end;
        end;
    finally
      Data.Unmap();
    end;
    Time3 := Int64(GetTickCount64()) - Time3;
    // end test

    ImageProcessed.Picture.Assign(Bitmap);
  finally
    // free buffer
    Bitmap.Free();
  end;

  ShowMessage(Format(
    'Canvas.Pixels: %dms, %f pixels per second'#13+
    'Bitmap.Scanline: %dms, %f pixels per second'#13+
    'TBitmapData: %dms, %f pixels per second',
    [Time1, ImageSource.Picture.Width * ImageSource.Picture.Height * 20 / (Time1 / 1000),
     Time2, ImageSource.Picture.Width * ImageSource.Picture.Height * 20 / (Time2 / 1000),
     Time3, ImageSource.Picture.Width * ImageSource.Picture.Height * 20 / (Time3 / 1000)]));
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  RadioGroupMode.ItemIndex := 0;
  Process();
end;

procedure TFormMain.Process();
var
  Bitmap: TBitmap;
begin
  // make bitmap buffer
  Bitmap := TBitmap.Create();
  try
    // assign source graphic to bitmap
    Bitmap.Assign(ImageSource.Picture.Graphic);

    // process
    case RadioGroupMode.ItemIndex of
      0: ProcessInvert(Bitmap);
      1: ProcessGrayscale(Bitmap);
      2: ProcessAlpha(Bitmap);
      3: ProcessNoise(Bitmap);
      4: ProcessSwapChannels(Bitmap);
      5: ProcessOnlyRGB(Bitmap)
    end;

    ImageProcessed.Picture.Assign(Bitmap);
  finally
    // free buffer
    Bitmap.Free();
  end;
end;

procedure TFormMain.ProcessAlpha(const Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
  Value: Integer;
begin
  // begin RGBA access to pixels
  Data.Map(Bitmap, TAccessMode.ReadWrite, True);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        // read pixel
        Pixel := Data[X, Y];

        // calc fun alpha gradient
        Value := Trunc(255 * (3 * Sin(Sin(X * 19 / Data.Width) * Sin(Y * 19 / Data.Height)) * 0.5 + 0.5) * (X / Data.Width));

        if Value < 0 then
          Value := 0;

        if Value > 255 then
          Value := 255;

        // change alpha
        Pixel.A := Value;

        // write pixel
        Data[X, Y] := Pixel;
      end;
    end;
  finally
    // end access to pixels
    Data.Unmap();
  end;
end;

procedure TFormMain.ProcessGrayscale(const Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
  Value: Integer;
begin
  // begin RGB access to pixels
  Data.Map(Bitmap, TAccessMode.ReadWrite, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        // read pixel
        Pixel := Data[X, Y];

        // make grayscale value
        Value := ((Pixel.R * 54) + (Pixel.G * 184) + (Pixel.B * 18)) div 256;

        // write pixel
        Data[X, Y] := MakePixel(Value, Value, Value);
      end;
    end;
  finally
    // end access to pixels
    Data.Unmap();
  end;
end;

procedure TFormMain.ProcessInvert(const Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
begin
  // begin RGB access to pixels
  Data.Map(Bitmap, TAccessMode.ReadWrite, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        // read pixel
        Pixel := Data[X, Y];

        // invert channels
        Pixel.R := 255 - Pixel.R;
        Pixel.G := 255 - Pixel.G;
        Pixel.B := 255 - Pixel.B;

        // write pixel
        Data[X, Y] := Pixel;
      end;
    end;
  finally
    // end access to pixels
    Data.Unmap();
  end;
end;

procedure TFormMain.ProcessNoise(const Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
begin
  // begin RGB access to pixels
  Data.Map(Bitmap, TAccessMode.ReadWrite, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        // read pixel
        Pixel := Data[X, Y];

        // noise channels
        Pixel.R := ClipPixelValue(Pixel.R + Random(141) - 70);
        Pixel.G := ClipPixelValue(Pixel.G + Random(141) - 70);
        Pixel.B := ClipPixelValue(Pixel.B + Random(141) - 70);

        // write pixel
        Data[X, Y] := Pixel;
      end;
    end;
  finally
    // end access to pixels
    Data.Unmap();
  end;
end;

procedure TFormMain.ProcessOnlyRGB(const Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
begin
  // begin RGB access to pixels
  Data.Map(Bitmap, TAccessMode.ReadWrite, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        // read pixel
        Pixel := Data[X, Y];

        // clip channels
        Pixel.R := IfThen(Pixel.R < 128, 0, 255);
        Pixel.G := IfThen(Pixel.G < 128, 0, 255);
        Pixel.B := IfThen(Pixel.B < 128, 0, 255);

        // write pixel
        Data[X, Y] := Pixel;
      end;
    end;
  finally
    // end access to pixels
    Data.Unmap();
  end;
end;

procedure TFormMain.ProcessSwapChannels(const Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel, NewPixel: TPixelRec;
begin
  // begin RGB access to pixels
  Data.Map(Bitmap, TAccessMode.ReadWrite, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        // read pixel
        Pixel := Data[X, Y];

        // noise channels
        NewPixel.R := Pixel.G;
        NewPixel.G := Pixel.B;
        NewPixel.B := Pixel.R;

        // write pixel
        Data[X, Y] := NewPixel;
      end;
    end;
  finally
    // end access to pixels
    Data.Unmap();
  end;
end;

procedure TFormMain.RadioGroupModeClick(Sender: TObject);
begin
  if RadioGroupMode.ItemIndex <= 5 then
  begin
    TimerAnimation.Enabled := False;
    Process();
  end else
    TimerAnimation.Enabled := True;
end;

{$Q-}
{$R-}
procedure Plasm(Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Pixel: TPixelRec;
  t: Double;
begin
  t := GetTickCount64() * 0.0004;
  Data.Map(Bitmap, TAccessMode.Write, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        Pixel.R := Byte(Trunc(
          100 + 100 * (Sin(X * Cos(Y * 0.049 + t * 0.3) * 0.01) + Cos(X * 0.0123 - Y * 0.09))));
        Pixel.G := 0;
        Pixel.B := Byte(Trunc(
          Pixel.R + 100 * (Sin(X * Cos(X * 0.039 + t * 0.72) * 0.022 - t) + Sin(X * 0.01 - Y * 0.029 - t * 0.02))));
        Data.SetPixel(X, Y, Pixel);
      end;
    end;
  finally
    Data.Unmap();
  end;
end;

procedure Noise(Bitmap: TBitmap);
var
  Data: TBitmapData;
  X, Y: Integer;
  Value: Byte;
begin
  Data.Map(Bitmap, TAccessMode.Write, False);
  try
    for Y := 0 to Data.Height - 1 do
    begin
      for X := 0 to Data.Width - 1 do
      begin
        Value := Random(255);
        Data[X, Y] := TPixelRec.Create(Value, Value, Value);
      end;
    end;
  finally
    Data.Unmap();
  end;
end;
{$Q+}
{$R+}

procedure TFormMain.TimerAnimationTimer(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create(150, 150);
  try
    if RadioGroupMode.ItemIndex = 6 then
      Plasm(Bitmap)
    else
      Noise(Bitmap);
    ImageProcessed.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

end.
