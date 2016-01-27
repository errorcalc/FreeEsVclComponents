{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2012-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.CfxClasses;

interface

{$IF CompilerVersion >= 23}
{$DEFINE VER230UP}
{$IFEND}
{$IF CompilerVersion >= 24}
{$DEFINE VER240UP}
{$IFEND}

uses
  Windows, Classes, Controls, Graphics, PngImage,
  ES.Vcl.ExGraphics, Messages, Generics.Collections, Dialogs, StdCtrls;

type
//  {$scopedenums on}
  TVertLayout = (vlTop, vlCenter, vlBottom);
  THorzLayout = (hlLeft, hlCenter, hlRight);
  TImageAlign = (iaTopLeft, iaTopRight, iaBottomRight, iaBottomLeft, iaCenter, iaLeft, iaRight, iaTop, iaBottom);
//  {$scopedenums off}

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

  TStyleMargins = class(TImageMargins)
  published
    property Left nodefault;
    property Top nodefault;
    property Right nodefault;
    property Bottom nodefault;
  end;

  TStylePadding = class(TPadding)
  published
    property Left nodefault;
    property Right nodefault;
    property Top nodefault;
    property Bottom nodefault;
  end;

  // Internal use only
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
    // FOnMarginsChange: TNotifyEvent;
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
    // property OnMarginsChange: TNotifyEvent read FOnMarginsChange write FOnMarginsChange;
    procedure Draw(Canvas: TCanvas; Rect: TRect; Alpha: byte); virtual;
    procedure AssignImage(G: TGraphic);
    procedure AssignOverlay(G: TGraphic);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  // Internal use only
  TTextNinePathObject = class(TNinePathObject)
  private
    FShowCaption: Boolean;
    FTextAlignment: TAlignment;
    FTextDistance: Integer;
    FTextMultiline: Boolean;
    FTextLayout: TVertLayout;
    procedure SetShowCaption(const Value: Boolean);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextDistance(const Value: Integer);
    procedure SetTextMultiline(const Value: Boolean);
    procedure SetTextLayout(const Value: TVertLayout);
  public
    constructor Create; override;
    property ContentSpace;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment;
    property TextLayout: TVertLayout read FTextLayout write SetTextLayout;
    property TextDistance: Integer read FTextDistance write SetTextDistance;
    property TextMultiline: Boolean read FTextMultiline write SetTextMultiline;
    procedure Draw(Canvas: TCanvas; Rect: TRect; Text: String; Alpha: byte); reintroduce; overload;
  end;

  // Internal use only
  TFixPngImage = class(TPngImage)
  protected
    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
  public
    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPalette); override;
    procedure Assign(Source: TPersistent); override;
  end;

  // Internal use only
  TStyleNinePath = class(TPersistent)
  private
    ImageList: TList<TPngImage>;
    BitmapList: TList<TBitmap>;
    FImageMargins: TStyleMargins;
    FOverlayAlign: TImageAlign;
    FOverlayMargins: TStyleMargins;
    FOnChange: TNotifyEvent;
    FControl: TControl;
    FStateCount: Integer;
    FIsDefaultValues: Boolean;
    FUpdateCount: Integer;
    FImageMode: TStretchMode;
    FIsDefaultImages: Boolean;
    procedure SetImageMargins(const Value: TStyleMargins);
    procedure SetOverlayAlign(const Value: TImageAlign);
    procedure SetOverlayMargins(const Value: TStyleMargins);
    function RequestImage(Index: Integer): TPngImage;
    procedure ImageChange(Sender: TObject);
    function GetBitmap(Index: Integer): TBitmap;
    function GetOverlayBitmap(Index: Integer): TBitmap;
    procedure SetImageMode(const Value: TStretchMode);
    procedure SetIsDefaultValues(const Value: Boolean);
    procedure SetIsDefaultImages(const Value: Boolean);
    //
    procedure ChangeNotify;
    procedure SetIsDefaultStyle(const Value: Boolean);
    function GetIsDefaultStyle: Boolean;
  protected
    procedure Change;
    procedure ChangeMargins(Sender: TObject);// use for XXXMargins.OnChange
    function IsStyleStored: Boolean;
    // get/set
    property StateCount: Integer read FStateCount;
    function RequestBitmap(Index: Integer): TBitmap;
    function RequestOverlayBitmap(Index: Integer): TBitmap;
    function GetSuitedBitmap(Index: Integer): TBitmap;
    function GetSuitedOverlayBitmap(Index: Integer): TBitmap;
    procedure AssignTo(Dest: TPersistent); override;
    //
    procedure DoDraw(Canvas: TCanvas; Rect: TRect; Bitmap: TBitmap;
      OverlayBitmap: TBitmap; Mode: TStretchMode; Alpha: Byte = 255); virtual;
    //----------------------------------------------------------------------------------------------
    // Indexed properties:
    function GetImage(const Index: Integer): TPngImage;
    procedure SetImage(const Index: Integer; const Image: TPngImage);
    function GetOverlay(const Index: Integer): TPngImage;
    procedure SetOverlay(const Index: Integer; const Image: TPngImage);
    // PLEASE REALIZE THIS:
    function GetStateCount: Integer; dynamic; abstract;
    function GetStylePrefix: string; dynamic;// if use IsDefaultStyle or LoadDefaultStyle
    // to published
    property ImageMargins: TStyleMargins read FImageMargins write SetImageMargins stored IsStyleStored;
    property ImageMode: TStretchMode read FImageMode write SetImageMode stored IsStyleStored default TStretchMode.smNormal;
    property OverlayAlign: TImageAlign read FOverlayAlign write SetOverlayAlign stored IsStyleStored;
    property OverlayMargins: TStyleMargins read FOverlayMargins write SetOverlayMargins stored IsStyleStored;
    //----------------------------------------------------------------------------------------------
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; Rect: TRect; StateIndex: Integer; Alpha: Byte = 255); virtual;
    procedure Clear; dynamic;
    procedure UpdateImages; dynamic;
    //
    procedure AssignDefaultValues; dynamic;
    procedure AssignDefaultImages; dynamic;
    procedure AssignDefaultStyle;
    //
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsPresented(Index: Integer): Boolean;
    //
    property Control: TControl read FControl write FControl;
    property Bitmap[Index: Integer]: TBitmap read GetBitmap;
    property OverlayBitmap[Index: Integer]: TBitmap read GetOverlayBitmap;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    //
    /// <summary>  This property is deprecated, please use IsDefaultValues/IsDefaultImages </summary>
    property IsDefaultStyle: Boolean read GetIsDefaultStyle write SetIsDefaultStyle stored False;
  published
    // default res style
    property IsDefaultValues: Boolean read FIsDefaultValues write SetIsDefaultValues stored False default True;
    property IsDefaultImages: Boolean read FIsDefaultImages write SetIsDefaultImages default True;
  end;

  // Internal use only
  // TODO: need rewrite for use CreateTimerQueueTimer...
  TEsTimer = class
  private
    HidenWindow: HWND;
    FOnTimer: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure UpdateTimer;
    procedure WndProc(var Message: TMessage);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure DoTimer; dynamic;
  public
    procedure Start;
    procedure Stop;
    procedure Reset;
    constructor Create(Interval: Cardinal = 1000; OnTimer: TNotifyEvent = nil; Enabled: Boolean = False);
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  // Internal use only
  TAnimationMode = (amLinear, amCubic);

  // Internal use only
  TCustomAnimation = class
  private
    FMode: TAnimationMode;
    FDuration: Cardinal;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    Timer: TEsTimer;
    StartTime: Cardinal;
    FNormalizedTime: Single;
    FReverseMode: Boolean;// 0..1
    procedure SetDuration(const Value: Cardinal);
    procedure SetMode(const Value: TAnimationMode);
    procedure SetReverseMode(const Value: Boolean);
  protected
    function GetCurrentTime: Single; virtual;
    procedure TimerProc(Sender: TObject); virtual;
    procedure Process; virtual;
  public
    procedure Start; dynamic;
    procedure Stop; dynamic;
    constructor Create; dynamic;
    destructor Destroy; override;
    property Duration: Cardinal read FDuration write SetDuration;
    property Mode: TAnimationMode read FMode write SetMode;
    property ReverseMode: Boolean read FReverseMode write SetReverseMode;
    property CurrentTime: Single read GetCurrentTime;// 0..1
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  // Internal use only
  TIntegerAnimation = class(TCustomAnimation)
  private
    FStartValue: Integer;
    FEndValue: Integer;
    function GetValue: Integer;
    procedure SetEndValue(const Value: Integer);
    procedure SetStartValue(const Value: Integer);
  public
    constructor Create; override;
    property StartValue: Integer read FStartValue write SetStartValue;
    property EndValue: Integer read FEndValue write SetEndValue;
    property Value: Integer read GetValue;
  end;

implementation

uses
  Themes, ES.Vcl.BaseControls, ES.Vcl.Utils, SysUtils, TypInfo;

{$REGION 'Delphi 2010/XE support'}
{$ifndef VER230UP}

type
  TRectHelper = record helper for TRect
  private
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;
    procedure Inflate(const DX, DY: Integer); overload;
  published
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  TPointHelper = record helper for TPoint
  public
    procedure Offset(const DX, DY : Integer); overload;
    procedure Offset(const Point: TPoint); overload;
  end;

{ TPointHelper }

procedure TPointHelper.Offset(const DX, DY: Integer);
begin
  Inc(Self.X, DX);
  Inc(Self.Y, DY);
end;

procedure TPointHelper.Offset(const Point: TPoint);
begin
  Inc(Self.X, Point.X);
  Inc(Self.Y, Point.Y);
end;

{ TRectHelper }

function TRectHelper.GetHeight: Integer;
begin
  Result := Self.Bottom - Self.Top;
end;

function TRectHelper.GetWidth: Integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectHelper.Inflate(const DX, DY: Integer);
begin
  Self.Left := Self.Left - DX;
  Self.Top := Self.Top - DY;
  Self.Right := Self.Right + DX;
  Self.Bottom := Self.Bottom + DY;
end;

procedure TRectHelper.Offset(const DX, DY: Integer);
begin
  Self.Left := Self.Left + DX;
  Self.Top := Self.Top + DY;
  Self.Right := Self.Right + DX;
  Self.Bottom := Self.Bottom + DY;
end;

procedure TRectHelper.Offset(const Point: TPoint);
begin
  Self.Left := Self.Left + Point.X;
  Self.Top := Self.Top + Point.Y;
  Self.Right := Self.Right + Point.X;
  Self.Bottom := Self.Bottom + Point.Y;
end;

procedure TRectHelper.SetHeight(const Value: Integer);
begin
  Self.Bottom := Self.Top + Value;
end;

procedure TRectHelper.SetWidth(const Value: Integer);
begin
  Self.Right := Self.Left + Value;
end;

{$endif}
{$ENDREGION}

{ TImageMargins }

function TImageMargins.GetRect: TRect;
begin
  Result := Classes.Rect(Left, Top, Right, Bottom);
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
//  if Assigned(OnMarginsChange) then
//    OnMarginsChange(Self);
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
  FOverlayAlign := TImageAlign.iaTopLeft;
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
    R.BottomRight.Offset(-OverlayMargins.Rect.Right, -OverlayMargins.Rect.Bottom);

    case FOverlayAlign of
      TImageAlign.iaTopLeft:
        Canvas.Draw(R.Left, R.Top, FOverlay, Alpha);

      TImageAlign.iaTopRight:
        Canvas.Draw(R.Right - FOverlay.Width, R.Top, FOverlay, Alpha);

      TImageAlign.iaBottomRight:
        Canvas.Draw(R.Right - FOverlay.Width, R.Bottom - FOverlay.Height, FOverlay, Alpha);

      TImageAlign.iaBottomLeft:
        Canvas.Draw(R.Left, R.Bottom - FOverlay.Height, FOverlay, Alpha);

      TImageAlign.iaCenter:
        Canvas.Draw(R.Left + R.Width div 2 - FOverlay.Width div 2, R.Top + R.Height div 2 - FOverlay.Height div 2, FOverlay, Alpha);

      TImageAlign.iaLeft:
        Canvas.Draw(R.Left, R.Top + R.Height div 2 - FOverlay.Height div 2, FOverlay, Alpha);

      TImageAlign.iaRight:
        Canvas.Draw(R.Left + R.Width - FOverlay.Width, R.Top + R.Height div 2 - FOverlay.Height div 2, FOverlay, Alpha);

      TImageAlign.iaTop:
        Canvas.Draw(R.Left + R.Width div 2 - FOverlay.Width div 2, R.Top, FOverlay, Alpha);

      TImageAlign.iaBottom:
        Canvas.Draw(R.Left + R.Width div 2 - FOverlay.Width div 2, R.Top + R.Height - FOverlay.Height, FOverlay, Alpha);
    end;

    case FOverlayAlign of
      TImageAlign.iaLeft: ContentRect.Left := R.Left + FOverlay.Width + OverlayMargins.Right;
      TImageAlign.iaRight: ContentRect.Right := R.Right - FOverlay.Width - OverlayMargins.Left;
      TImageAlign.iaTop: ContentRect.Top := R.Top + FOverlay.Height - OverlayMargins.Bottom;
      TImageAlign.iaBottom: ContentRect.Bottom := R.Bottom - FOverlay.Height - OverlayMargins.Top;
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
  FTextLayout := TVertLayout.vlCenter;
  FTextDistance := 0;
  OverlaySpace := True;
  ContentSpace := True;
  OverlayAlign := TImageAlign.iaLeft;
end;

procedure TTextNinePathObject.Draw(Canvas: TCanvas; Rect: TRect; Text: String; Alpha: byte);
{$ifdef VER230UP}
const
  D: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);//TStyleFont = (sfPanelTextDisabled, sfPanelTextNormal);
{$endif}
var
  R, Temp: TRect;
  Format: TTextFormat;
  procedure CalcRect(var Rect: TRect);
  var
    T: TTextFormat;
  begin
    Rect.Offset(-Rect.Top, -Rect.Left);
    T := Format;
    T := T + [tfCalcRect{$ifdef VER230UP}, tfWordEllipsis{$endif}];
    {$ifdef VER230UP}
    if IsStyledFontControl(Control) then
      Canvas.DrawThemeText(StyleServices.GetElementDetails(D[Control.Enabled]),
        Rect, Text, T)
    else
    {$endif}
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
    TVertLayout.vlCenter:
      if not TextMultiline then
        Format := Format + [tfVerticalCenter]
      else
      begin
        Temp := R;
        CalcRect(Temp);
        R.Top := R.Top + (R.Height div 2) - (Temp.Height div 2);
        R.Height := Temp.Height;
      end;
    TVertLayout.vlBottom:
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
    TVertLayout.vlTop:
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
  {$ifdef VER230UP}
  if IsStyledFontControl(Control) then
    Canvas.DrawThemeText(StyleServices.GetElementDetails(D[Control.Enabled]),
      R, Text, Format)
  else
  {$endif}
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

procedure TTextNinePathObject.SetTextLayout(const Value: TVertLayout);
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

{ TStateNinePath }

procedure TStyleNinePath.AssignDefaultValues;
begin

end;

procedure TStyleNinePath.AssignTo(Dest: TPersistent);
begin
  if Dest is TStyleNinePath then
  begin
    AssignPersistent(Dest, Self);
    //TStyleNinePath(Dest).Change;
  end
  else
    inherited;
end;

procedure TStyleNinePath.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TStyleNinePath.Change;
begin
  if FUpdateCount <> 0 then
    Exit;

  FIsDefaultValues := False;

  ChangeNotify;
end;

procedure TStyleNinePath.ChangeMargins(Sender: TObject);
begin
  Change;
end;

procedure TStyleNinePath.ChangeNotify;
begin
  if Assigned(FOnChange) and not((Control <> nil) and (csLoading in Control.ComponentState)) then
    FOnChange(Self);
end;

procedure TStyleNinePath.Clear;
var
  I: Integer;
begin
  for I := 0 to ImageList.Count - 1 do
    if ImageList[I] <> nil then
    begin
      ImageList[I].Free;
      ImageList[I] := nil;
    end;

  for I := 0 to BitmapList.Count - 1 do
    if BitmapList[I] <> nil then
    begin
      BitmapList[I].Free;
      BitmapList[I] := nil;
    end;
end;

constructor TStyleNinePath.Create;
begin
  inherited;
  ImageList := TList<TPngImage>.Create;
  BitmapList := TList<TBitmap>.Create;

  FStateCount := GetStateCount;

  ImageList.Count := FStateCount * 2;
  BitmapList.Count := FStateCount * 2;

  FImageMargins := TStyleMargins.Create(nil);
  FImageMargins.OnChange := ChangeMargins;
  FOverlayMargins := TStyleMargins.Create(nil);
  FOverlayMargins.OnChange := ChangeMargins;

  FIsDefaultValues := True;
  FIsDefaultImages := True;

  BeginUpdate;
  try
    AssignDefaultValues;
  finally
    EndUpdate;
  end;
end;

destructor TStyleNinePath.Destroy;
begin
  Clear;
  ImageList.Free;
  BitmapList.Free;
  FImageMargins.Free;
  FOverlayMargins.Free;
  inherited;
end;

procedure TStyleNinePath.Draw(Canvas: TCanvas; Rect: TRect; StateIndex: Integer; Alpha: Byte = 255);
begin
  DoDraw(Canvas, Rect, GetSuitedBitmap(StateIndex), GetSuitedOverlayBitmap(StateIndex), ImageMode, Alpha);
end;

procedure TStyleNinePath.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
  end;
end;

function TStyleNinePath.GetBitmap(Index: Integer): TBitmap;
begin
  if (Index < 0) or (Index >= StateCount) then
    raise Exception.CreateFmt('GetBitmap(%d) - Index of bounds!', [Index]);
  Result := RequestBitmap(Index);
end;

function TStyleNinePath.GetImage(const Index: Integer): TPngImage;
begin
  Result := RequestImage(Index);
  // TPngImage not support OnChange event, because we need delete cache in Getter :(
//  if (BitmapList[Index] <> nil) then
//  begin
//    BitmapList[Index].Free;
//    BitmapList[Index] := nil;
//  end;
end;

function TStyleNinePath.GetIsDefaultStyle: Boolean;
begin
  Result := IsDefaultValues and IsDefaultImages;
end;

function TStyleNinePath.GetOverlayBitmap(Index: Integer): TBitmap;
begin
  if (Index < StateCount) or (Index >= StateCount * 2) then
    raise Exception.CreateFmt('GetOverlayBitmap(%d) - Index of bounds!', [Index]);
  Result := RequestBitmap(Index + StateCount);
end;

function TStyleNinePath.GetOverlay(const Index: Integer): TPngImage;
begin
  Result := RequestImage(Index + StateCount);
  // TPngImage not support OnChange event, because we need delete cache in Getter :(
//  if (BitmapList[Index + StateCount] <> nil) then
//  begin
//    BitmapList[Index + StateCount].Free;
//    BitmapList[Index + StateCount] := nil;
//  end;
end;

function TStyleNinePath.GetStylePrefix: string;
begin
  Result := Self.ClassName;
end;

function TStyleNinePath.GetSuitedBitmap(Index: Integer): TBitmap;
var
  I: Integer;
begin
  if (Index < 0) or (Index >= StateCount) then
    raise Exception.CreateFmt('GetSuitedBitmap(%d) - Index of bounds!', [Index]);
  for I := Index downto 0 do
  begin
    if (ImageList[I] <> nil) or (BitmapList[I] <> nil) then
    begin
      //if not BitmapList[I].Empty then
      begin
        Result := RequestBitmap(I);
        exit;
      end;
    end;
  end;

  Result := BitmapList[Index];
end;

function TStyleNinePath.GetSuitedOverlayBitmap(Index: Integer): TBitmap;
var
  I: Integer;
begin
  Index := Index + StateCount;
  if (Index < StateCount) or (Index >= StateCount * 2) then
    raise Exception.CreateFmt('GetSuitedOverlayBitmap(%d) - Index of bounds!', [Index]);
  for I := Index downto StateCount do
  begin
    if (ImageList[I] <> nil) or (BitmapList[I] <> nil) then
    begin
      //if not BitmapList[I].Empty then
      begin
        Result := RequestBitmap(I);
        exit;
      end;
    end;
  end;

  Result := BitmapList[Index];
end;

procedure TStyleNinePath.ImageChange(Sender: TObject);
var
  Index: Integer;
begin
  if FIsDefaultImages and (FUpdateCount = 0) then
  begin
    FIsDefaultImages := False;
    UpdateImages;
  end;

  Index := ImageList.IndexOf(TPngImage(Sender));

  //
  if BitmapList[Index] = nil then
  begin
    if not TPngImage(Sender).Empty then
      RequestBitmap(Index).Assign(TPngImage(Sender));
  end else
  begin
    if not TPngImage(Sender).Empty then
      BitmapList[Index].Assign(TPngImage(Sender))
    else
    begin
      BitmapList[Index].Free;
      BitmapList[Index] := nil;
    end;
  end;

  Change;
end;

procedure TStyleNinePath.DoDraw(Canvas: TCanvas; Rect: TRect; Bitmap: TBitmap;
      OverlayBitmap: TBitmap; Mode: TStretchMode; Alpha: Byte = 255);
var
  R: TRect;
  ContentRect: TRect;
begin
  if Assigned(Bitmap) then
    Canvas.DrawNinePath(Rect, FImageMargins.Rect, Bitmap, Mode, Alpha);

//  // for painting child class
//  if (Self.ClassType <> TNinePathObject) and ContentSpace then
//  begin
//    ContentRect.Left := Rect.Left + FMargins.Left;
//    ContentRect.Top := Rect.Top + FMargins.Top;
//    ContentRect.Right := Rect.Right - FMargins.Right;
//    ContentRect.Bottom := Rect.Bottom - FMargins.Bottom;
//  end
//  else
  ContentRect := Rect;

  if Assigned(OverlayBitmap) then
  begin
    R := Rect;

//    if FOverlaySpace then
//    begin
//      R.Left := R.Left + FMargins.Left;
//      R.Top := R.Top + FMargins.Top;
//      R.Right := R.Right - FMargins.Right;
//      R.Bottom := R.Bottom - FMargins.Bottom;
//    end;

    R.TopLeft.Offset(OverlayMargins.Rect.TopLeft);
    R.BottomRight.Offset(-OverlayMargins.Rect.Right, -OverlayMargins.Rect.Bottom);

    case FOverlayAlign of
      TImageAlign.iaTopLeft:
        Canvas.Draw(R.Left, R.Top, OverlayBitmap, Alpha);

      TImageAlign.iaTopRight:
        Canvas.Draw(R.Right - OverlayBitmap.Width, R.Top, OverlayBitmap, Alpha);

      TImageAlign.iaBottomRight:
        Canvas.Draw(R.Right - OverlayBitmap.Width, R.Bottom - OverlayBitmap.Height, OverlayBitmap, Alpha);

      TImageAlign.iaBottomLeft:
        Canvas.Draw(R.Left, R.Bottom - OverlayBitmap.Height, OverlayBitmap, Alpha);

      TImageAlign.iaCenter:
        Canvas.Draw(R.Left + R.Width div 2 - OverlayBitmap.Width div 2, R.Top + R.Height div 2 - OverlayBitmap.Height div 2, OverlayBitmap, Alpha);

      TImageAlign.iaLeft:
        Canvas.Draw(R.Left, R.Top + R.Height div 2 - OverlayBitmap.Height div 2, OverlayBitmap, Alpha);

      TImageAlign.iaRight:
        Canvas.Draw(R.Left + R.Width - OverlayBitmap.Width, R.Top + R.Height div 2 - OverlayBitmap.Height div 2, OverlayBitmap, Alpha);

      TImageAlign.iaTop:
        Canvas.Draw(R.Left + R.Width div 2 - OverlayBitmap.Width div 2, R.Top, OverlayBitmap, Alpha);

      TImageAlign.iaBottom:
        Canvas.Draw(R.Left + R.Width div 2 - OverlayBitmap.Width div 2, R.Top + R.Height - OverlayBitmap.Height, OverlayBitmap, Alpha);
    end;

//    case FOverlayAlign of
//      iaLeft: ContentRect.Left := R.Left + OverlayBitmap.Width + OverlayMargins.Right;
//      iaRight: ContentRect.Right := R.Right - OverlayBitmap.Width - OverlayMargins.Left;
//      iaTop: ContentRect.Top := R.Top + OverlayBitmap.Height - OverlayMargins.Bottom;
//      iaBottom: ContentRect.Bottom := R.Bottom - OverlayBitmap.Height - OverlayMargins.Top;
//    end;
  end;
end;

function TStyleNinePath.IsPresented(Index: Integer): Boolean;
begin
  Result := False;

  if (Index < 0) or (Index >= StateCount) then
    exit;

  if (ImageList[Index] <> nil) and (not ImageList[Index].Empty) then
    Result := True;

  if (ImageList[Index + StateCount] <> nil) and (not ImageList[Index + StateCount].Empty) then
    Result := True;

  if (BitmapList[Index] <> nil) and (not BitmapList[Index].Empty) then
    Result := True;

  if (BitmapList[Index + StateCount] <> nil) and (not BitmapList[Index + StateCount].Empty) then
    Result := True;
end;

function TStyleNinePath.IsStyleStored: Boolean;
begin
  Result := not (FIsDefaultValues and FIsDefaultImages);
end;

procedure TStyleNinePath.AssignDefaultImages;
var
  StylePrefix: string;
  Name: string;
  I: Integer;
  Data: PTypeData;
  Info: PTypeInfo;
  PropList: PPropList;
  Png: TPngImage;
  hInstance: HINST;
begin
  Clear;

  FIsDefaultImages := True;

  StylePrefix := GetStylePrefix;

  hInstance := FindClassHInstance(Self.ClassType);
  Data := GetTypeData(Self.ClassType.ClassInfo);
  GetMem(PropList, Data.PropCount * SizeOf(Pointer));
  BeginUpdate;
  try
    GetPropInfos(ClassType.ClassInfo, PropList);
    for I := 0 to Data.PropCount - 1 do
    begin
      if IsStoredProp(Self, PropList[i]) then
      begin
        Info := PropList[i]^.PropType^;
        if (Info^.Kind = tkClass) and (string(Info^.Name) = TPngImage.ClassName) then
        begin
          Name := UpperCase(StylePrefix + '_' + string(PropList[i]^.Name));
          if FindResource(hInstance, PChar(Name), RT_RCDATA) <> 0 then
          begin
            Png := TPngImage(GetObjectProp(Self, string(PropList[i]^.Name)));
            Png.LoadFromResourceName(hInstance, PChar(Name));
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, Data.PropCount * SizeOf(Pointer));
    EndUpdate;
  end;

  UpdateImages;

  for I := 0 to ImageList.Count - 1 do
    if ImageList[I] <> nil then
    begin
      ImageList[I].Free;
      ImageList[I] := nil;
    end;
end;

function TStyleNinePath.RequestBitmap(Index: Integer): TBitmap;
begin
  if BitmapList[Index] = nil then
  begin
    BitmapList[Index] := TBitmap.Create;
    BitmapList[Index].PixelFormat := pf32bit;
    BitmapList[Index].AlphaFormat := afDefined;

    if (ImageList[Index] <> nil) and not ImageList[Index].Empty then
      BitmapList[Index].Assign(ImageList[Index]);
  end;

  Result := BitmapList[Index];
end;

function TStyleNinePath.RequestImage(Index: Integer): TPngImage;
begin
  if ImageList[Index] = nil then
  begin
    ImageList[Index] := TFixPngImage.Create;
    ImageList[Index].OnChange := ImageChange;
  end;

  Result := ImageList[Index];
end;

function TStyleNinePath.RequestOverlayBitmap(Index: Integer): TBitmap;
begin
  result := RequestBitmap(Index + StateCount);
end;

procedure TStyleNinePath.AssignDefaultStyle;
begin
  AssignDefaultImages;
  AssignDefaultValues;
  ChangeNotify;
end;

procedure TStyleNinePath.SetIsDefaultValues(const Value: Boolean);
begin
  if Value <> FIsDefaultValues then
  begin
    FIsDefaultValues := Value;
    if Value then
    begin
      //Clear;
      BeginUpdate;
      try
        AssignDefaultValues;
      finally
        EndUpdate;
      end;
      //LoadDefaultImages;
      //Change;
      ChangeNotify;
    end;
  end;
end;

procedure TStyleNinePath.SetImage(const Index: Integer; const Image: TPngImage);
begin
  RequestImage(Index).Assign(Image);
  // ImageChange(RequestImage(Index));
  // Change;
end;

procedure TStyleNinePath.SetImageMargins(const Value: TStyleMargins);
begin
  FImageMargins.Assign(Value);
end;


procedure TStyleNinePath.SetImageMode(const Value: TStretchMode);
begin
  if FImageMode <> Value then
  begin
    FImageMode := Value;
    Change;
  end;
end;

procedure TStyleNinePath.SetIsDefaultImages(const Value: Boolean);
begin
  if Value <> FIsDefaultImages then
  begin
    FIsDefaultImages := Value;
    if Value then
    begin
      Clear;
      AssignDefaultImages;
    end else
      Clear;
    ChangeNotify;
  end;
end;

procedure TStyleNinePath.SetIsDefaultStyle(const Value: Boolean);
begin
  IsDefaultImages := Value;
  IsDefaultValues := Value;
end;

procedure TStyleNinePath.SetOverlayAlign(const Value: TImageAlign);
begin
  if FOverlayAlign <> Value then
  begin
    FOverlayAlign := Value;
    Change;
  end;
end;

procedure TStyleNinePath.SetOverlay(const Index: Integer; const Image: TPngImage);
begin
  RequestImage(Index + StateCount).Assign(Image);
  // ImageChange(RequestImage(Index));
  // Change;
end;

procedure TStyleNinePath.SetOverlayMargins(const Value: TStyleMargins);
begin
  FOverlayMargins.Assign(Value);
end;

procedure TStyleNinePath.UpdateImages;
var
  I: Integer;
begin
  for I := 0 to ImageList.Count - 1 do
  begin
    if (ImageList[I] <> nil)and not ImageList[I].Empty then
    begin
      RequestBitmap(I).Assign(ImageList[I]);
    end else
    begin
      BitmapList[I].Free;
      BitmapList[I] := nil;
    end;
  end;
end;

{ TEsTimer }

constructor TEsTimer.Create(Interval: Cardinal = 1000; OnTimer: TNotifyEvent = nil; Enabled: Boolean = False);
begin
  FInterval := Interval;
  FEnabled := Enabled;
  FOnTimer := OnTimer;
  UpdateTimer;
end;

destructor TEsTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  inherited;
end;

procedure TEsTimer.DoTimer;
begin
  if Assigned(OnTimer) then
    OnTimer(Self);
end;

procedure TEsTimer.Reset;
begin
  if FEnabled then
    UpdateTimer;
end;

procedure TEsTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TEsTimer.SetInterval(const Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TEsTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TEsTimer.Start;
begin
  if not FEnabled then
  begin
    FEnabled := True;
    UpdateTimer;
  end;
end;

procedure TEsTimer.Stop;
begin
  if FEnabled then
  begin
    FEnabled := False;
    UpdateTimer;
  end;
end;

procedure TEsTimer.UpdateTimer;
begin
  if FEnabled then
  begin
    if HidenWindow = 0 then
    begin
      HidenWindow := AllocateHWnd(WndProc);
    end;
    KillTimer(HidenWindow, 1);
    if SetTimer(HidenWindow, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create('Error: No timers!');
  end
  else
    if HidenWindow <> 0 then
    begin
      KillTimer(HidenWindow, 1);
      DeallocateHWnd(HidenWindow);
      HidenWindow := 0;
    end;
end;

procedure TEsTimer.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_TIMER then
    try
      DoTimer;
    except
    end
  else
    Message.Result := DefWindowProc(HidenWindow, Message.Msg, Message.WParam, Message.LParam);
end;

{ TCustomAnimation }

constructor TCustomAnimation.Create;
begin
  Timer := TEsTimer.Create(10, TimerProc, False);
  //FOnProcess := OnProcess;
  FNormalizedTime := 0;
  FMode := amLinear;
  //Timer.Enabled := Enabled;
end;

destructor TCustomAnimation.Destroy;
begin
  Timer.Free;
  inherited;
end;

function TCustomAnimation.GetCurrentTime: Single;
var
  t: Single;
begin
  Result := 0;// for compiler paranoia

  if ReverseMode then
    t := 1 - FNormalizedTime
  else
    t := FNormalizedTime;

  case FMode of
    amLinear: Result := t;
    amCubic: Result := t * t * t; //( 1 - Sqrt(t * t * t * t * t));
  end;

  if ReverseMode then
    Result := 1 - Result;
end;

procedure TCustomAnimation.Process;
begin
  if Assigned(OnProcess) then
    OnProcess(Self);
end;

procedure TCustomAnimation.SetDuration(const Value: Cardinal);
begin
  if Value = 0 then
    FDuration := 1
  else
    FDuration := Value;
end;

procedure TCustomAnimation.SetMode(const Value: TAnimationMode);
begin
  FMode := Value;
end;

procedure TCustomAnimation.SetReverseMode(const Value: Boolean);
begin
  FReverseMode := Value;
end;

procedure TCustomAnimation.Start;
begin
  StartTime := GetTickCount;
  FNormalizedTime := 0;
  Timer.Start;
  Process;
end;

procedure TCustomAnimation.Stop;
begin
  Timer.Stop;
  FNormalizedTime := 1;
  Process;
  if Assigned(OnFinish) then
    OnFinish(Self);
end;

procedure TCustomAnimation.TimerProc(Sender: TObject);
var
  t: Cardinal;
begin
  t := GetTickCount;
  if t < StartTime + FDuration then
  begin
    FNormalizedTime := (t - StartTime) / FDuration;
    Process;
  end else
  begin
    Stop;
  end;
end;

{ TIntegerAnimation }

constructor TIntegerAnimation.Create;
begin
  inherited;

end;

function TIntegerAnimation.GetValue: Integer;
begin
  Result := Trunc(StartValue * (1 - CurrentTime) + EndValue * CurrentTime + 0.5);
end;

procedure TIntegerAnimation.SetEndValue(const Value: Integer);
begin
  if FEndValue <> Value then
  begin
    FEndValue := Value;
//    Process;
  end;
end;

procedure TIntegerAnimation.SetStartValue(const Value: Integer);
begin
  if FStartValue <> Value then
  begin
    FStartValue := Value;
//    Process;
  end;
end;

{ TFixPngImage }

procedure TFixPngImage.Assign(Source: TPersistent);
begin
  inherited;
  Changed(Self);
end;

procedure TFixPngImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPalette);
begin
  inherited;
  Changed(Self);
end;

procedure TFixPngImage.LoadFromFile(const Filename: String);
begin
  inherited;
  Changed(Self);
end;

procedure TFixPngImage.LoadFromStream(Stream: TStream);
begin
  inherited;
  Changed(Self);
end;

procedure TFixPngImage.SetHeight(Value: Integer);
begin
  inherited;
  Changed(Self);
end;

procedure TFixPngImage.SetWidth(Value: Integer);
begin
  inherited;
  Changed(Self);
end;

end.
