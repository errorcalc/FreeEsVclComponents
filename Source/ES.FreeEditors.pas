{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2015-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{     errorsoft@protonmail.ch | habrahabr.ru/user/error1024 | errorsoft.org    }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{                                                                              }
{ Вы можете заказать разработку VCL/FMX компонента на заказ                    }
{ You can order the development of VCL/FMX components to order                 }
{******************************************************************************}
unit ES.FreeEditors;

interface

uses
  DesignEditors, DesignIntf, Classes, Windows, Graphics, PngImage, PicEdit, ImgList, VclEditors, Types;

type
  TEsPngPropertyFix = class(TGraphicProperty)
    procedure Edit; override;
  end;

  TEsPicturePropertyFix = class(TPictureProperty)
    procedure Edit; override;
  end;

  TEsCustomImageIndexProperty = class(TIntegerProperty, ICustomPropertyListDrawing)
  private const
    MaxWidth = 64;
    Border = 2;
  protected
    // rewrite me
    function GetImageList: TCustomImageList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;

    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

implementation

uses
  SysUtils, Math, TypInfo, ES.ExGraphics;

{TEsPngPropertyFix}

procedure TEsPngPropertyFix.Edit;
begin
  TPicture.RegisterFileFormat('PNG', 'ErrorSoft fix PNG loader', TPngImage);
  inherited;
  TPicture.UnregisterGraphicClass(TPngImage);
end;

{ TEsCustomImageIndexProperty }

function TEsCustomImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paDialog] -
    [paSortList, paMultiSelect, paAutoUpdate, paSubProperties, paReadOnly];
end;

function TEsCustomImageIndexProperty.GetImageList: TCustomImageList;
begin
  if TypInfo.GetPropInfo(GetComponent(0), 'Images') <> nil then
    Result := TCustomImageList(TypInfo.GetObjectProp(GetComponent(0), 'Images'))
  else
  if TypInfo.GetPropInfo(GetComponent(0), 'ImageList') <> nil then
    Result := TCustomImageList(TypInfo.GetObjectProp(GetComponent(0), 'ImageList'))
  else
    Result := nil;
end;

procedure TEsCustomImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  if GetImageList <> nil then
    for I := 0 to GetImageList.Count - 1 do
      Proc(IntToStr(I));
end;

procedure TEsCustomImageIndexProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  R: TRect;
  Index, H: Integer;
  ClipRegion: HRGN;
begin
  R := ARect;
  try
    Index := StrToInt(Value);
    ACanvas.FillRect(R);
    if (GetImageList <> nil) and (Index > -1) then
    begin
      H := R.Bottom - R.Top;
      R.Right := R.Left + Min(MaxWidth, GetImageList.Width);

      ClipRegion := CreateRectRgn(R.Left + Border, R.Top + Border,
        R.Left + Border + Min(MaxWidth, GetImageList.Width), R.Top + Border + H);
      try
        SelectClipRgn(ACanvas.Handle, ClipRegion);
        GetImageList.Draw(ACanvas, R.Left + Border, R.Top + Border, Index, True);
      finally
        SelectClipRgn(ACanvas.Handle, 0);
        DeleteObject(ClipRegion);
      end;

      R := Rect(R.Right + 4, R.Top, ARect.Right, R.Bottom);
    end;
  finally
    DefaultPropertyListDrawValue(Value, ACanvas, R, ASelected);
  end;
end;

procedure TEsCustomImageIndexProperty.ListMeasureHeight(const Value: string; ACanvas: TCanvas;
  var AHeight: Integer);
begin
  if GetImageList <> nil then
    AHeight := Max(ACanvas.TextHeight('Wg|'), Min(GetImageList.Height, MaxWidth) + Border * 2);
end;

procedure TEsCustomImageIndexProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas;
  var AWidth: Integer);
begin
 if GetImageList <> nil then
    AWidth := AWidth + Min(GetImageList.Height, MaxWidth) + Border * 2;
end;

{ TEsPicturePropertyFix }

type
  TRawPngImage = class(TPngImage)
  end;

procedure TEsPicturePropertyFix.Edit;
var
  Png: TPngImage;
  Pic: TPicture;
begin
//  PictureEditor := TPictureEditor.Create(nil);
//  try
//    TPicture.RegisterFileFormat('PNG', 'ErrorSoft fix PNG loader', TRawPngImage);
//    PictureEditor.Picture := TPicture(Pointer(GetOrdValue));
//
//    if PictureEditor.Execute then
//    begin
//      if PictureEditor.Picture.Graphic is TRawPngImage then
//      begin
//        Png := TPngImage.Create;
//        try
//          Png.Assign(PictureEditor.Picture.Graphic);
//          PictureEditor.Picture.Assign(Png);
//        finally
//          Png.Free;
//        end;
//      end;
//      SetOrdValue(Longint(PictureEditor.Picture));
//    end;
//
//  finally
//    PictureEditor.Free;
//    TPicture.UnregisterGraphicClass(TRawPngImage);
//  end;

  Inherited;
  Pic := TPicture(Pointer(GetOrdValue));
  if (Pic.Graphic is TBitmap) and Pic.Graphic.ClassNameIs('TPNGGraphic') then
  begin
    Png := TPngImage.Create;
    try
      BitmapAssignToPngImage(Png, TBitmap(Pic.Graphic), False);
      Pic.Assign(Png);
    finally
      Png.Free;
    end;
  end;
end;

end.
