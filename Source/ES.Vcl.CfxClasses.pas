{******************************************************************************}
{                             ESComponents for VCL                             }
{                            ErrorSoft(c) 2012-2015                            }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.CfxClasses;

interface

{$IF CompilerVersion >= 24}
{$DEFINE VER240UP}
{$IFEND}

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Imaging.PngImage,
  ES.Vcl.ExGraphics, Winapi.Messages;

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
  Vcl.Themes, ES.Vcl.BaseControls, ES.Vcl.Utils;

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
    R.BottomRight.Offset(-OverlayMargins.Rect.Right, -OverlayMargins.Rect.Bottom);

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
  FTextDistance := 0;
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
    Rect.Offset(-Rect.Top, -Rect.Left);
    T := Format;
    T := T + [tfCalcRect, tfWordEllipsis];
    if IsStyledFontControl(Control) then
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
  if IsStyledFontControl(Control) then
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

end.
