{******************************************************************************}
{                             ESComponents for VCL                             }
{                            ErrorSoft(c) 2012-2015                            }
{                                                                              }
{            errorsoft@mail.ch | errorsoft-demoscene.narod.ru                  }
{ errorsoft@protonmail.ch | github.com/errorcalc | habrahabr.ru/user/error1024 }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.CfxClasses;

interface
uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Imaging.PngImage,
  ES.Vcl.ExGraphics;

type
  TImageAlign = (iaTopLeft, iaTopRight, iaBottomRight, iaBottomLeft, iaCenter, iaLeft, iaRight, iaTop, iaBottom);
  TTextLayout = (tlTop, tlCenter, tlBottom);

  TImageMargins = class(TMargins)
  private
    function GetRect: TRect; inline;
  protected
    class procedure InitDefaults(Margins: TMargins); override;
  public
    property Rect: TRect read GetRect;
  published
    property Left default 0;
    property Top default 0;
    property Right default 0;
    property Bottom default 0;
  end;

  TNinePathObject = class
  private
    FBitmap: TBitmap;
    FOverlay: TBitmap;
    FMargins: TImageMargins;
    FOnNeedRepaint: TNotifyEvent;
    FOverlayAlign: TImageAlign;
    FOverlaySpace: Boolean;
    FOverlayMargins: TImageMargins;
    FContentSpace: Boolean;
    FControl: TControl;
    FOnMarginsChange: TNotifyEvent;
    procedure BoundsChange(Sender: TObject);
    procedure SetMargins(const Value: TImageMargins);
    procedure SetBitMap(const Value: TBitmap);
    procedure SetOverlay(const Value: TBitmap);
    procedure SetOverlayAlign(const Value: TImageAlign);
    procedure SetOverlaySpace(const Value: Boolean);
    procedure SetOverlayMargins(const Value: TImageMargins);
    procedure SetContentSpace(const Value: Boolean);
    procedure SetControl(const Value: TControl);
  protected
    ContentRect: TRect;
    property ContentSpace: Boolean read FContentSpace write SetContentSpace;
    procedure NeedRepaint;
  public
    property Control: TControl read FControl write SetControl;
    property Bitmap: TBitmap read FBitmap write SetBitMap;
    property Overlay: TBitmap read FOverlay write SetOverlay;
    property OverlayAlign: TImageAlign read FOverlayAlign write SetOverlayAlign;
    property OverlayMargins: TImageMargins read FOverlayMargins write SetOverlayMargins;
    property OverlaySpace: Boolean read FOverlaySpace write SetOverlaySpace;
    property Margins: TImageMargins read FMargins write SetMargins;
    property OnNeedRepaint: TNotifyEvent read FOnNeedRepaint write FOnNeedRepaint;
    property OnMarginsChange: TNotifyEvent read FOnMarginsChange write FOnMarginsChange;
    procedure Draw(Canvas: TCanvas; Rect: TRect; Alpha: byte); virtual;
    procedure AssignImage(G: TGraphic);
    procedure AssignOverlay(G: TGraphic);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TTextNinePathObject = class(TNinePathObject)
  private
    FShowCaption: Boolean;
    FTextAlignment: TAlignment;
    FTextDistance: Integer;
    FTextMultiline: Boolean;
    FTextLayout: TTextLayout;
    procedure SetShowCaption(const Value: Boolean);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextDistance(const Value: Integer);
    procedure SetTextMultiline(const Value: Boolean);
    procedure SetTextLayout(const Value: TTextLayout);
  public
    constructor Create; override;
    property ContentSpace;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment;
    property TextLayout: TTextLayout read FTextLayout write SetTextLayout;
    property TextDistance: Integer read FTextDistance write SetTextDistance;
    property TextMultiline: Boolean read FTextMultiline write SetTextMultiline;
    procedure Draw(Canvas: TCanvas; Rect: TRect; Text: String; Alpha: byte); reintroduce; overload;
  end;

function IsStyledTextControl(Control: TControl): Boolean;

implementation

uses
  Vcl.Themes, ES.Vcl.BaseControls;

function IsStyledTextControl(Control: TControl): Boolean;
begin
  Result := False;

  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := (seFont in Control.StyleElements) and TStyleManager.IsCustomStyleActive;
  end;
end;

{ TImageMargins }

function TImageMargins.GetRect: TRect;
begin
  Result := System.Classes.Rect(Left, Top, Right, Bottom);
end;

class procedure TImageMargins.InitDefaults(Margins: TMargins);
begin
  // zero values on default
end;

{ TNinePathObject }

procedure TNinePathObject.BoundsChange(Sender: TObject);
begin
  if Assigned(OnNeedRepaint) then
    OnNeedRepaint(Self);
  if Assigned(OnMarginsChange) then
    OnMarginsChange(Self);
end;

constructor TNinePathObject.Create;
begin
  inherited;
  FMargins := TImageMargins.Create(nil);
  FMargins.OnChange := BoundsChange;
  FOverlayMargins := TImageMargins.Create(nil);
  FOverlayMargins.OnChange := BoundsChange;
  FBitmap := TBitMap.Create;
  FBitmap.PixelFormat := pf32bit;
  FBitmap.AlphaFormat := afPremultiplied;
  FOverlay := TBitMap.Create;
  FOverlay.PixelFormat := pf32bit;
  FOverlay.AlphaFormat := afPremultiplied;
  FOverlayAlign := iaTopLeft;
  // FContentSpace := True;
end;

destructor TNinePathObject.Destroy;
begin
  FMargins.Free;
  FOverlayMargins.Free;
  FBitMap.Free;
  FOverlay.Free;
  inherited;
end;

procedure TNinePathObject.Draw(Canvas: TCanvas; Rect: TRect; Alpha: byte);
var
  R: TRect;
begin
  Canvas.DrawNinePath(Rect, FMargins.Rect, FBitmap, Alpha);

  // for painting child class
  if (Self.ClassType <> TNinePathObject) and ContentSpace then
  begin
    ContentRect.Left := Rect.Left + FMargins.Left;
    ContentRect.Top := Rect.Top + FMargins.Top;
    ContentRect.Right := Rect.Right - FMargins.Right;
    ContentRect.Bottom := Rect.Bottom - FMargins.Bottom;
  end
  else
    ContentRect := Rect;

  if Assigned(FOverlay) then
  begin
    R := Rect;

    if FOverlaySpace then
    begin
      R.Left := R.Left + FMargins.Left;
      R.Top := R.Top + FMargins.Top;
      R.Right := R.Right - FMargins.Right;
      R.Bottom := R.Bottom - FMargins.Bottom;
    end;

    R.TopLeft.Offset(OverlayMargins.Rect.TopLeft);
    R.BottomRight.Offset(TPoint.Zero - OverlayMargins.Rect.BottomRight);

    case FOverlayAlign of
      iaTopLeft:
        Canvas.Draw(R.Left, R.Top, FOverlay, Alpha);

      iaTopRight:
        Canvas.Draw(R.Right - FOverlay.Width, R.Top, FOverlay, Alpha);

      iaBottomRight:
        Canvas.Draw(R.Right - FOverlay.Width, R.Bottom - FOverlay.Height, FOverlay, Alpha);

      iaBottomLeft:
        Canvas.Draw(R.Left, R.Bottom - FOverlay.Height, FOverlay, Alpha);

      iaCenter:
        Canvas.Draw(R.Left + R.Width div 2 - FOverlay.Width div 2, R.Top + R.Height div 2 - FOverlay.Height div 2, FOverlay, Alpha);

      iaLeft:
        Canvas.Draw(R.Left, R.Top + R.Height div 2 - FOverlay.Height div 2, FOverlay, Alpha);

      iaRight:
        Canvas.Draw(R.Left + R.Width - FOverlay.Width, R.Top + R.Height div 2 - FOverlay.Height div 2, FOverlay, Alpha);

      iaTop:
        Canvas.Draw(R.Left + R.Width div 2 - FOverlay.Width div 2, R.Top, FOverlay, Alpha);

      iaBottom:
        Canvas.Draw(R.Left + R.Width div 2 - FOverlay.Width div 2, R.Top + R.Height - FOverlay.Height, FOverlay, Alpha);
    end;

    case FOverlayAlign of
      iaLeft: ContentRect.Left := R.Left + FOverlay.Width + OverlayMargins.Right;
      iaRight: ContentRect.Right := R.Right - FOverlay.Width - OverlayMargins.Left;
      iaTop: ContentRect.Top := R.Top + FOverlay.Height - OverlayMargins.Bottom;
      iaBottom: ContentRect.Bottom := R.Bottom - FOverlay.Height - OverlayMargins.Top;
    end;
  end

  // Canvas.Brush.Color := Random(255*255*255);
  // Canvas.Rectangle(Canvas.ClipRect);
  // Canvas.Font.Color := clRed;
  // Canvas.TextOut(Canvas.ClipRect.Left+30, Canvas.ClipRect.Top,
  //   IntToStr(Canvas.ClipRect.Left)+','+IntToStr(Canvas.ClipRect.Top) + '-'+IntToStr(Canvas.ClipRect.Width)+','+IntToStr(Canvas.ClipRect.Height));
end;

procedure TNinePathObject.NeedRepaint;
begin
  if Assigned(OnNeedRepaint) then
    OnNeedRepaint(Self);
end;

procedure TNinePathObject.SetBitMap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  NeedRepaint;
end;

procedure TNinePathObject.SetContentSpace(const Value: Boolean);
begin
  if Value <> FContentSpace then
  begin
    FContentSpace := Value;
    NeedRepaint;
  end;
end;

procedure TNinePathObject.SetControl(const Value: TControl);
begin
  FControl := Value;
  NeedRepaint;
end;

procedure TNinePathObject.SetMargins(const Value: TImageMargins);
begin
  FMargins.Assign(Value);
end;

procedure TNinePathObject.AssignImage(G: TGraphic);
begin
  FBitmap.SetSize(G.Width, G.Height);
  FBitmap.Canvas.Brush.Color := 0;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  FBitmap.Canvas.Draw(0, 0, G);

  NeedRepaint;
end;

procedure TNinePathObject.AssignOverlay(G: TGraphic);
begin
  FOverlay.SetSize(G.Width, G.Height);
  FOverlay.Canvas.Brush.Color := 0;
  FOverlay.Canvas.FillRect(Rect(0, 0, FOverlay.Width, FOverlay.Height));
  FOverlay.Canvas.Draw(0, 0, G);

  NeedRepaint;
end;

procedure TNinePathObject.SetOverlay(const Value: TBitmap);
begin
  FOverlay.Assign(Value);
  NeedRepaint;
end;

procedure TNinePathObject.SetOverlayAlign(const Value: TImageAlign);
begin
  if Value <> FOverlayAlign then
  begin
    FOverlayAlign := Value;
    if Assigned(FOverlay) then
      NeedRepaint;
  end;
end;

procedure TNinePathObject.SetOverlayMargins(const Value: TImageMargins);
begin
  FOverlayMargins.Assign(Value);
  if Assigned(FOverlay) then
    NeedRepaint;
end;

procedure TNinePathObject.SetOverlaySpace(const Value: Boolean);
begin
  if Value <> FOverlaySpace then
  begin
    FOverlaySpace := Value;
    NeedRepaint;
  end;
end;

{ TTextNinePathObject }

constructor TTextNinePathObject.Create;
begin
  inherited;
  FTextAlignment := taCenter;
  FTextLayout := tlCenter;
  FTextDistance := 2;
  OverlaySpace := True;
  ContentSpace := True;
  OverlayAlign := iaLeft;
end;

procedure TTextNinePathObject.Draw(Canvas: TCanvas; Rect: TRect; Text: String; Alpha: byte);
const
  D: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);//TStyleFont = (sfPanelTextDisabled, sfPanelTextNormal);
var
  R, Temp: TRect;
  Format: TTextFormat;
  procedure CalcRect(var Rect: TRect);
  var
    T: TTextFormat;
  begin
    Rect.Offset(TPoint.Zero - Rect.TopLeft);
    T := Format;
    T := T + [tfCalcRect, tfWordEllipsis];
    if IsStyledTextControl(Control) then
      Canvas.DrawThemeText(StyleServices.GetElementDetails(D[Control.Enabled]),
        Rect, Text, T)
    else
      Canvas.TextRect(Rect, Text, T);
  end;
begin
  inherited Draw(Canvas, Rect, Alpha);

  if (not ShowCaption) or (Control = nil) then
    exit;

  //Canvas.FrameRect(ContentRect);

  R := ContentRect;

  if TextMultiline then
    Format := [tfWordBreak, tfEndEllipsis]
  else
    Format := [tfSingleLine];

  case TextAlignment of
    taLeftJustify:
      begin
        Format := Format + [];
        if TextMultiline then
          R.Inflate(-FTextDistance, -FTextDistance)
        else
          R.Left := R.Left + FTextDistance;
      end;
    taRightJustify:
      begin
        Format := Format + [tfRight];
        if TextMultiline then
          R.Inflate(-FTextDistance, -FTextDistance)
        else
          R.Right := R.Right - FTextDistance;
      end;
    taCenter:
      begin
        Format := Format + [tfCenter];
        //R := Rect;
        if TextMultiline then
        begin
          R := ContentRect;
          R.Inflate(-FTextDistance, -FTextDistance);
        end
        else
        if not Overlay.Empty then
        begin
//          case FOverlayAlign of
//            iaLeft: R.Left := ContentRect.Left;
//            iaRight: R.Right := ContentRect.Right;
//            iaTop: R.Top := ContentRect.Top;
//            iaBottom: R.Bottom := ContentRect.Bottom;
//          end;
          R.Inflate(-FTextDistance, 0);
        end else
        begin
          //R.Inflate(-FTextDistance, 0);
          R.Left := Rect.Left;
          R.Right := Rect.Right;
        end
      end;
  end;

  case TextLayout of
    tlCenter:
      if not TextMultiline then
        Format := Format + [tfVerticalCenter]
      else
      begin
        Temp := R;
        CalcRect(Temp);
        R.Top := R.Top + (R.Height div 2) - (Temp.Height div 2);
        R.Height := Temp.Height;
      end;
    tlBottom:
      if not TextMultiline then
      begin
        Format := Format + [tfBottom];
        R.Bottom := R.Bottom - FTextDistance;
      end else
      begin
        Temp := R;
        CalcRect(Temp);
        R.Top := R.Top + (R.Height - Temp.Height) - FTextDistance;
      end;
    tlTop:
      if not TextMultiline then
      begin
        R.Top := R.Top + FTextDistance;
      end else
      begin
        Temp := R;
        CalcRect(Temp);
        R.Height := Temp.Height;
      end;
  end;

  if (Control <> nil)and(csDesigning in Control.ComponentState)and
     (Control is TEsCustomControl) then
  begin
    if TEsCustomControl(Control).IsDrawHelper then
      Canvas.DrawChessFrame(R, $009900, $F0FF40);
    //Canvas.DrawChessFrame(ContentRect, clRed, $C0C0C0);
  end;

  Canvas.Brush.Style := bsClear;
  if IsStyledTextControl(Control) then
    Canvas.DrawThemeText(StyleServices.GetElementDetails(D[Control.Enabled]),
      R, Text, Format)
  else
    if Control.Enabled then
      Canvas.TextRect(R, Text, Format)
    else
    begin
      R.Offset(1, 1);
      Canvas.Font.Color := clBtnHighlight;
      Canvas.TextRect(R, Text, Format);
      R.Offset(-1, -1);
      Canvas.Font.Color := clBtnShadow;
      Canvas.TextRect(R, Text, Format);
    end;
  Canvas.Brush.Style := bsSolid;
end;

procedure TTextNinePathObject.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    NeedRepaint;
  end;
end;

procedure TTextNinePathObject.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    if FShowCaption then
       NeedRepaint;
  end;
end;

procedure TTextNinePathObject.SetTextDistance(const Value: Integer);
begin
  if FTextDistance <> Value then
  begin
    FTextDistance := Value;
    if FShowCaption then
       NeedRepaint;
  end;
end;

procedure TTextNinePathObject.SetTextLayout(const Value: TTextLayout);
begin
  if FTextLayout <> Value then
  begin
    FTextLayout := Value;
    if FShowCaption then
       NeedRepaint;
  end;
end;

procedure TTextNinePathObject.SetTextMultiline(const Value: Boolean);
begin
  if FTextMultiline <> Value then
  begin
    FTextMultiline := Value;
    if FShowCaption then
       NeedRepaint;
  end;
end;

end.
