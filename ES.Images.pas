{******************************************************************************}
{                            EsVclComponents v2.0                              }
{                           ErrorSoft(c) 2009-2016                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{         Open this on github: github.com/errorcalc/FreeEsVclComponents        }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
unit ES.Images;

{$SCOPEDENUMS ON}
{$IF CompilerVersion >= 27} {$DEFINE SUPPORT_ENUMS_ALIASES} {$IFEND}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics,
  WinApi.Messages, ES.ExGraphics, ES.BaseControls, ES.CfxClasses, Vcl.ImgList, System.UITypes;

type
  TImageStretch = (None, Center, Fit, Fill, Uniform, Mixed);
  {$REGION 'deprecated names'}
  {$IFDEF SUPPORT_ENUMS_ALIASES}
  TImageStretchHelper = record helper for TImageStretch
  const
    isNone = TImageStretch.None deprecated 'Use TImageStretch.None';
    isCenter = TImageStretch.Center deprecated 'Use TImageStretch.Center';
    isFit = TImageStretch.Fit deprecated 'Use TImageStretch.Fil';
    isFill = TImageStretch.Fill deprecated 'Use TImageStretch.Fill';
    isUniform = TImageStretch.Uniform deprecated 'Use TImageStretch.Uniform';
    isMixed = TImageStretch.Mixed deprecated 'Use TImageStretch.Mixed';
  end;
  {$ENDIF}
  {$ENDREGION}

  /// <summary> ONLY INTERNAL USE! </summary>
  TImageProxy = class(TComponent)
  private
    FSmoth: Boolean;
    FPicture: TPicture;
    FImages: TCustomImageList;
    FStretch: TImageStretch;
    FImageIndex: TImageIndex;
    FOnProgress: TProgressEvent;
    FOnChange: TNotifyEvent;
    FIncrementalDisplay: Boolean;
    FBackgroundColor: TColor;
    FTransparentGraphic: Boolean;
    FTransparent: Boolean;
    FOpacity: Byte;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetStretch(const Value: TImageStretch);
    procedure SetPicture(const Value: TPicture);
    procedure SetSmoth(const Value: Boolean);
    //
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    //
  private
    ImageChangeLink: TChangeLink;
    function HasImageList: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure PictureProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const Rect: TRect; const Msg: string);
    procedure ImageListChange(Sender: TObject);
    procedure Change;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetTransparentGraphic(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetOpacity(const Value: Byte);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetRect(R: TRect): TRect; virtual;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    //
    procedure Draw(Canvas: TCanvas; ARect: TRect);
    //
    property Picture: TPicture read FPicture write SetPicture;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Stretch: TImageStretch read FStretch write SetStretch default TImageStretch.None;
    property Smoth: Boolean read FSmoth write SetSmoth default True;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property TransparentGraphic: Boolean read FTransparentGraphic write SetTransparentGraphic default False;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    //
    property ImageWidth: Integer read GetImageWidth;
    property ImageHeight: Integer read GetImageHeight;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TEsImage = class(TEsGraphicControl)
  private
    ImageProxy: TImageProxy;
    DrawCount: Integer;
    FDoubleBuffered: Boolean;
    FOnPaint: TPaintEvent;
    FOnPainting: TPaintEvent;
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
    function GetStretch: TImageStretch;
    procedure SetStretch(const Value: TImageStretch);
    function GetColor: TColor;
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    function GetIncrementalDisplay: Boolean;
    function GetOnProgress: TProgressEvent;
    function GetSmoth: Boolean;
    function GetTransparent: Boolean;
    function GetTransparentGraphic: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIncrementalDisplay(const Value: Boolean);
    procedure SetOnProgress(const Value: TProgressEvent);
    procedure SetSmoth(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetTransparentGraphic(const Value: Boolean);
    function GetCanvas: TCanvas;
    function GetOpacity: Byte;
    procedure SetOpacity(const Value: Byte);
    procedure SetDoubleBuffered(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure ImageProxyChange(Sender: TObject);
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
    procedure BeginDraw;
    procedure EndDraw;
    procedure RecreateBitmap;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    //
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered default False;
    property Picture: TPicture read GetPicture write SetPicture;
    property Stretch: TImageStretch read GetStretch write SetStretch default TImageStretch.None;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property Smoth: Boolean read GetSmoth write SetSmoth default True;
    property IncrementalDisplay: Boolean read GetIncrementalDisplay write SetIncrementalDisplay default False;
    property Color: TColor read GetColor write SetColor default clWhite;
    property TransparentGraphic: Boolean read GetTransparentGraphic write SetTransparentGraphic default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property Opacity: Byte read GetOpacity write SetOpacity default 255;
    property IsDrawHelper default True;
    //
    property Padding;
    property PopupMenu;
    property AlignWithMargins;
    property Margins;
    property ShowHint;
    {$if CompilerVersion > 23}
    property StyleElements;
    {$ifend}
    property Touch;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnProgress: TProgressEvent read GetOnProgress write SetOnProgress;
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnPainting: TPaintEvent read FOnPainting write FOnPainting;
    property OnStartDock;
    property OnStartDrag;
  end;

  TEsImageControl = class(TEsBaseLayout)
  private
    Painting: Boolean;
    ImageProxy: TImageProxy;
    DrawCount: Integer;
    FFrameWidth: TFrameWidth;
    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    function GetOpacity: Byte;
    function GetCanvas: TCanvas;
    function GetIncrementalDisplay: Boolean;
    function GetOnProgress: TProgressEvent;
    function GetPicture: TPicture;
    function GetSmoth: Boolean;
    function GetStretch: TImageStretch;
    function GetTransparentGraphic: Boolean;
    procedure SetOpacity(const Value: Byte);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIncrementalDisplay(const Value: Boolean);
    procedure SetOnProgress(const Value: TProgressEvent);
    procedure SetPicture(const Value: TPicture);
    procedure SetSmoth(const Value: Boolean);
    procedure SetStretch(const Value: TImageStretch);
    procedure SetTransparentGraphic(const Value: Boolean);
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure ImageProxyChange(Sender: TObject);
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CalcContentMargins(var Margins: TContentMargins); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
    procedure BeginDraw;
    procedure EndDraw;
    procedure RecreateBitmap;
  published
    property Align;
    property Anchors;
    property AutoSize;
    // property BufferedChildrens;// TEsCustomControl
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property BorderWidth;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property IsCachedBuffer;// TEsCustomControl
    property IsCachedBackground;// TEsCustomControl
    property IsFullSizeBuffer;// TEsCustomControl
    //
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.None;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
    //
    property Picture: TPicture read GetPicture write SetPicture;
    property Stretch: TImageStretch read GetStretch write SetStretch default TImageStretch.None;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property Smoth: Boolean read GetSmoth write SetSmoth default True;
    property IncrementalDisplay: Boolean read GetIncrementalDisplay write SetIncrementalDisplay default False;
    property Color default clBtnFace;//nodefault;
    property TransparentGraphic: Boolean read GetTransparentGraphic write SetTransparentGraphic default False;
    property Transparent;
    property Opacity: Byte read GetOpacity write SetOpacity default 255;
    property IsDrawHelper default True;
    //
    property Padding;
    property PopupMenu;
    property AlignWithMargins;
    property Margins;
    property ShowHint;
    {$if CompilerVersion > 23}
    property StyleElements;
    {$ifend}
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnProgress: TProgressEvent read GetOnProgress write SetOnProgress;
    property OnPaint;
    property OnPainting;
    property OnStartDock;
    property OnStartDrag;
    property OnCanResize;
    property OnEnter;
    property OnExit;
    property OnResize;
  end;

implementation

uses
  ES.ExGdiPlus, System.Math, ES.Utils, Vcl.Themes, System.TypInfo;

procedure DrawDesignImageFrame(Canvas: TCanvas; Rect: TRect);
begin
  Canvas.Brush.Style := bsClear;

  Canvas.Pen.Color := RGB(67, 67, 67);
  Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(Rect);

  Canvas.Pen.Color := RGB(160, 160, 255);
  Canvas.Pen.Style := psDot;
  Canvas.Rectangle(Rect);
end;

{ TImageProxy }

constructor TImageProxy.Create;
begin
  inherited Create(nil);

  ImageChangeLink := TChangeLink.Create;
  ImageChangeLink.OnChange := ImageListChange;

  FSmoth := True;
  FImageIndex := -1;
  FBackgroundColor := clWhite;
  FTransparent := True;
  FOpacity := 255;

  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := PictureProgress;
end;

destructor TImageProxy.Destroy;
begin
  ImageChangeLink.Free;
  FPicture.Free;
  inherited;
end;

procedure TImageProxy.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TImageProxy.Draw(Canvas: TCanvas; ARect: TRect);
var
  R: TRect;
  Bitmap: TBitmap;
begin
  R := GetRect(ARect);

  if not Transparent then
  begin
    Canvas.Brush.Color := ColorToRgb(BackgroundColor);
    Canvas.FillRect(ARect);
  end;

  if HasImageList then
  begin
    if (R.Width = ImageWidth) and (R.Height = ImageHeight) and (Opacity = 255) then
      Images.Draw(Canvas, R.Left, R.Top, ImageIndex)
    else
    begin
      Bitmap := TBitmap.Create;
      try
        // generate color mask if need
        if Images.ColorDepth <> cd32bit then
        begin
          Bitmap.Canvas.Brush.Color := RGB(255, 0, 255);
          Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
          Bitmap.TransparentColor := RGB(255, 0, 255);
          Bitmap.Transparent := True;
        end;

        if ImageListGetBitmap(Images, ImageIndex, Bitmap) then
          if (not Smoth){ or (Opacity <> 255)} or ((Bitmap.PixelFormat <> pf32bit) and TransparentGraphic) then
            Canvas.StretchDraw(R, Bitmap, Opacity)
          else
            Canvas.DrawHighQuality(R, Bitmap, Opacity);
      finally
        Bitmap.Free;
      end;
    end;
  end else
  begin
    if (R.Width = ImageWidth) and (R.Height = ImageHeight) then
      if Opacity = 255 then
        Canvas.Draw(R.Left, R.Top, Picture.Graphic)
      else
        Canvas.Draw(R.Left, R.Top, Picture.Graphic, Opacity)
    else
      if (not Smoth){ or (Opacity <> 255)} or TransparentGraphic then
        Canvas.StretchDraw(R, Picture.Graphic, Opacity)
      else
        Canvas.DrawHighQuality(R, Picture.Graphic, Opacity);
  end;
end;

function TImageProxy.GetImageHeight: Integer;
begin
  if HasImageList then
    Result := Images.Height
  else
    Result := Picture.Height;
end;

function TImageProxy.GetImageWidth: Integer;
begin
  if HasImageList then
    Result := Images.Width
  else
    Result := Picture.Width;
end;

function TImageProxy.GetRect(R: TRect): TRect;
var
  dw, dh: Integer;
  w, h: Integer;
  WidthHeight: Real;
begin
  dw := R.Width;
  dh := R.Height;

  if ((Stretch in [TImageStretch.Fit, TImageStretch.Uniform]) or
    ((Stretch = TImageStretch.Mixed) and ((dw < ImageWidth) or (dh < ImageHeight)))) and
    ((ImageWidth <> 0) and (ImageHeight <> 0))  then
  begin
    WidthHeight := ImageWidth / ImageHeight;

    if dh > Round(dw / WidthHeight) then
    begin
      w := dw;
      h := Round(dw / WidthHeight);
    end else
    begin
      h := dh;
      w := Round(dh * WidthHeight);
    end;
  end else
  if Stretch = TImageStretch.Fill then
  begin
    w := dw;
    h := dh;
  end else
  begin
    w := ImageWidth;
    h := ImageHeight;
  end;

  Result := Rect(R.Left, R.Top, R.Left + w, R.Top + h);

  if Stretch in [TImageStretch.Fit, TImageStretch.Mixed, TImageStretch.Center] then
  begin
    Result.Offset((dw - w) div 2, (dh - h) div 2);
  end;
end;

function TImageProxy.HasImageList: Boolean;
begin
  Result := FImages <> nil;
end;

procedure TImageProxy.ImageListChange(Sender: TObject);
begin
  Change;
end;

procedure TImageProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited;
  if (AComponent = FImages) and (Operation = opRemove) then
  begin
    FImages := nil;
    Change;
  end;
end;

procedure TImageProxy.PictureChanged(Sender: TObject);
begin
  if Assigned(Picture.Graphic) and not ((Picture.Graphic is TMetaFile) or (Picture.Graphic is TIcon)) then
    Picture.Graphic.Transparent := TransparentGraphic;
  Change;
end;

procedure TImageProxy.PictureProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte;
  RedrawNow: Boolean; const Rect: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
    Change;

  if Assigned(OnProgress) then
    OnProgress(Sender, Stage, PercentDone, RedrawNow, Rect, Msg);
end;

procedure TImageProxy.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Change;
  end;
end;

procedure TImageProxy.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Change;
  end;
end;

procedure TImageProxy.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TImageProxy.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(ImageChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(ImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    Change;
  end;
end;

procedure TImageProxy.SetStretch(const Value: TImageStretch);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Change;
  end;
end;

procedure TImageProxy.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TImageProxy.SetSmoth(const Value: Boolean);
begin
  if FSmoth <> Value then
  begin
    FSmoth := Value;
    Change;
  end;
end;

procedure TImageProxy.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;

    Change;
  end;
end;

procedure TImageProxy.SetTransparentGraphic(const Value: Boolean);
begin
  if FTransparentGraphic <> Value then
  begin
    FTransparentGraphic := Value;
    PictureChanged(Self);
  end;
end;

{ TEsImage }

constructor TEsImage.Create(AOwner: TComponent);
begin
  inherited;
  ImageProxy := TImageProxy.Create;
  ImageProxy.OnChange := ImageProxyChange;
  IsDrawHelper := True;
  Width := 100;
  Height := 100;
end;

destructor TEsImage.Destroy;
begin
  ImageProxy.Free;
  inherited;
end;

procedure TEsImage.BeginDraw;
begin
  Inc(DrawCount);
end;

procedure TEsImage.EndDraw;
begin
  if DrawCount > 0 then
    Dec(DrawCount);

  if DrawCount = 0 then
    ImageProxyChange(nil);
end;

function TEsImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;

  if (ImageProxy.ImageWidth = 0) or (ImageProxy.ImageHeight = 0) then
    Exit;

  if Align in [alNone, alLeft, alRight] then
  begin
    NewWidth := ImageProxy.ImageWidth + ContentMargins.Width;
  end;
  if Align in [alNone, alTop, alBottom] then
  begin
    NewHeight := ImageProxy.ImageHeight + ContentMargins.Height;
  end;
end;

function TEsImage.GetColor: TColor;
begin
  Result := ImageProxy.BackgroundColor;
end;

function TEsImage.GetOpacity: Byte;
begin
  Result := ImageProxy.Opacity;
end;

function TEsImage.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Picture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  if Picture.Graphic is TBitmap then
    Result := Picture.Bitmap.Canvas
  else
    raise EInvalidOperation.Create('Only TBitmap Has Canvas!');
end;

function TEsImage.GetImageIndex: TImageIndex;
begin
  Result := ImageProxy.ImageIndex;
end;

function TEsImage.GetImages: TCustomImageList;
begin
  Result := ImageProxy.Images;
end;

function TEsImage.GetIncrementalDisplay: Boolean;
begin
  Result := ImageProxy.IncrementalDisplay;
end;

function TEsImage.GetStretch: TImageStretch;
begin
  Result := ImageProxy.Stretch;
end;

function TEsImage.GetOnProgress: TProgressEvent;
begin
  Result := ImageProxy.OnProgress;
end;

function TEsImage.GetPicture: TPicture;
begin
  Result := ImageProxy.Picture;
end;

function TEsImage.GetSmoth: Boolean;
begin
  Result := ImageProxy.Smoth;
end;

function TEsImage.GetTransparent: Boolean;
begin
  Result := ImageProxy.Transparent;
end;

function TEsImage.GetTransparentGraphic: Boolean;
begin
  Result := ImageProxy.TransparentGraphic;
end;

procedure TEsImage.ImageProxyChange(Sender: TObject);
var
  W, H: Integer;
begin
  if DrawCount <> 0 then
    Exit;
  if AutoSize then
  begin
    W := Width;
    H := Height;
    CanAutoSize(W, H);
    if (Width <> W) or (Height <> H) then
      AdjustSize;
  end;
  Invalidate;
end;

procedure TEsImage.Loaded;
begin
  inherited;
  if Autosize then
    AdjustSize;
end;

procedure TEsImage.Paint;
var
  Bitmap: TBitmap;
  C: TCanvas;
begin
  if csDesigning in ComponentState then
  begin
    DrawDesignImageFrame(inherited Canvas, ClientRect);
    if IsDrawHelper then
      DrawControlHelper(Self, [hoPadding]);
  end;

  Bitmap := nil;
  try
    if FDoubleBuffered then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.SetSize(Max(0, Width), Max(0, Height));
      if Transparent then
      begin
        Bitmap.Canvas.Brush.Color := ColorToRgb(Color);
        Bitmap.Canvas.FillRect(ClientRect);
      end;
      C := Bitmap.Canvas;
    end
    else
      C := inherited Canvas;

    if (csDesigning in ComponentState) and IsDrawHelper and (Stretch <> TImageStretch.Fill) and
      (ImageProxy.ImageWidth * ImageProxy.ImageHeight <> 0) and not AutoSize then
    begin
      // WTF? It is okey logic: DrawTransparentFrame is slowly procedure... (if use not space symbols)
      if (ImageProxy.GetRect(ContentRect).Width <= 48) or (ImageProxy.GetRect(ContentRect).Height <= 48) then
        (inherited Canvas).DrawTransparentFrame(ImageProxy.GetRect(ContentRect), clBlack, clWhite, 160, '11-22-')
      else
        (inherited Canvas).DrawTransparentFrame(ImageProxy.GetRect(ContentRect), clBlack, clWhite, 160, '11---22---');
    end;

    if Assigned(FOnPainting) then
      FOnPainting(Self, Canvas, ClientRect);
    //
    ImageProxy.Draw(C, ContentRect);
    //
    if Assigned(FOnPaint) then
      FOnPaint(Self, Canvas, ClientRect);

    if Bitmap <> nil then
      inherited Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TEsImage.RecreateBitmap;
begin
  if Picture.Bitmap <> nil then
  begin
    Picture.Assign(nil);
    GetCanvas;
  end;
end;

procedure TEsImage.SetOpacity(const Value: Byte);
begin
  ImageProxy.Opacity := Value;
end;

procedure TEsImage.SetColor(const Value: TColor);
begin
  ImageProxy.BackgroundColor := Value;
end;

procedure TEsImage.SetDoubleBuffered(const Value: Boolean);
begin
  if FDoubleBuffered <> Value then
  begin
    FDoubleBuffered := Value;
    Invalidate;
  end;
end;

procedure TEsImage.SetImageIndex(const Value: TImageIndex);
begin
  ImageProxy.ImageIndex := Value;
end;

procedure TEsImage.SetImages(const Value: TCustomImageList);
begin
  ImageProxy.Images := Value;
end;

procedure TEsImage.SetIncrementalDisplay(const Value: Boolean);
begin
  ImageProxy.IncrementalDisplay := Value;
end;

procedure TEsImage.SetStretch(const Value: TImageStretch);
begin
  ImageProxy.Stretch := Value;
end;

procedure TEsImage.SetOnProgress(const Value: TProgressEvent);
begin
  ImageProxy.OnProgress := Value;
end;

procedure TEsImage.SetPicture(const Value: TPicture);
begin
  ImageProxy.Picture := Value;
end;

procedure TEsImage.SetSmoth(const Value: Boolean);
begin
  ImageProxy.Smoth := Value;
end;

procedure TEsImage.SetTransparent(const Value: Boolean);
begin
  ImageProxy.Transparent := Value;
  if Transparent then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

procedure TEsImage.SetTransparentGraphic(const Value: Boolean);
begin
  ImageProxy.TransparentGraphic := Value;
end;

{ TEsImageControl }

constructor TEsImageControl.Create(AOwner: TComponent);
begin
  inherited;
  ImageProxy := TImageProxy.Create;
  ImageProxy.OnChange := ImageProxyChange;
  IsDrawHelper := True;
  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
  // ParentColor := False;
  Color := clBtnFace;
  Transparent := True;
  Width := 100;
  Height := 100;
end;

destructor TEsImageControl.Destroy;
begin
  ImageProxy.Free;
  inherited;
end;

procedure TEsImageControl.BeginDraw;
begin
  Inc(DrawCount);
end;

procedure TEsImageControl.EndDraw;
begin
  if DrawCount > 0 then
    Dec(DrawCount);

  if DrawCount = 0 then
    ImageProxyChange(nil);
end;

procedure TEsImageControl.CalcContentMargins(var Margins: TContentMargins);
begin
  inherited;
  if FrameStyle <> TExFrameStyle.None then
  begin
    Margins.Inflate(GetFrameWidth(FrameStyle, FrameWidth), GetFrameWidth(FrameStyle, FrameWidth));
  end;
end;

function TEsImageControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;

  if (ImageProxy.ImageWidth = 0) or (ImageProxy.ImageHeight = 0) then
    Exit;

  if Align in [alNone, alLeft, alRight] then
    NewWidth := ImageProxy.ImageWidth + ContentMargins.Width;
  if Align in [alNone, alTop, alBottom] then
    NewHeight := ImageProxy.ImageHeight + ContentMargins.Height;
end;

function TEsImageControl.GetOpacity: Byte;
begin
  Result := ImageProxy.Opacity;
end;

function TEsImageControl.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Picture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  if Picture.Graphic is TBitmap then
    Result := Picture.Bitmap.Canvas
  else
    raise EInvalidOperation.Create('Only TBitmap Has Canvas!');
end;

function TEsImageControl.GetImageIndex: TImageIndex;
begin
  Result := ImageProxy.ImageIndex;
end;

function TEsImageControl.GetImages: TCustomImageList;
begin
  Result := ImageProxy.Images;
end;

function TEsImageControl.GetIncrementalDisplay: Boolean;
begin
  Result := ImageProxy.IncrementalDisplay;
end;

function TEsImageControl.GetOnProgress: TProgressEvent;
begin
  Result := ImageProxy.OnProgress;
end;

function TEsImageControl.GetPicture: TPicture;
begin
  Result := ImageProxy.Picture;
end;

function TEsImageControl.GetSmoth: Boolean;
begin
  Result := ImageProxy.Smoth;
end;

function TEsImageControl.GetStretch: TImageStretch;
begin
  Result := ImageProxy.Stretch;
end;

function TEsImageControl.GetTransparentGraphic: Boolean;
begin
  Result := ImageProxy.TransparentGraphic;
end;

procedure TEsImageControl.ImageProxyChange(Sender: TObject);
var
  W, H: Integer;
begin
  if DrawCount <> 0 then
    Exit;
  if AutoSize then
  begin
    W := Width;
    H := Height;
    CanAutoSize(W, H);
    if (Width <> W) or (Height <> H) then
      AdjustSize;
  end;
  if not Painting then
    Invalidate;
end;

procedure TEsImageControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    Click;
end;

procedure TEsImageControl.Loaded;
begin
  inherited;
  if AutoSize then
    AdjustSize;
end;

procedure TEsImageControl.Paint;
var
  CalcFrameWidth: Integer;
begin
  if csDesigning in ComponentState then
  begin
    CalcFrameWidth := GetFrameWidth(FrameStyle, FrameWidth);
    DrawDesignImageFrame(inherited Canvas, ClientRect);
    if IsDrawHelper then
      DrawControlHelper(inherited Canvas,
        Rect(CalcFrameWidth, CalcFrameWidth, ClientWidth - CalcFrameWidth, ClientHeight - CalcFrameWidth),
        BorderWidth, Padding, [hoPadding, hoBorder]);
  end;

  if (csDesigning in ComponentState) and IsDrawHelper and (Stretch <> TImageStretch.Fill) and
    (ImageProxy.ImageWidth * ImageProxy.ImageHeight <> 0) and not AutoSize then
  begin
    // WTF? It is okey logic: DrawTransparentFrame is slowly procedure... (if use not space symbols)
    if (ImageProxy.GetRect(ContentRect).Width <= 48) or (ImageProxy.GetRect(ContentRect).Height <= 48) then
      (inherited Canvas).DrawTransparentFrame(ImageProxy.GetRect(ContentRect), clBlack, clWhite, 160, '11-22-')
    else
      (inherited Canvas).DrawTransparentFrame(ImageProxy.GetRect(ContentRect), clBlack, clWhite, 160, '11---22---');
  end;

  ImageProxy.Draw(inherited Canvas, ContentRect);

  if FrameStyle <> TExFrameStyle.None then
    if IsStyledBorderControl(Self) then
      DrawFrame(inherited Canvas, ClientRect, FrameStyle, FrameWidth, StyleServices.GetSystemColor(FrameColor),
        StyleServices.GetSystemColor(clBtnHighlight), StyleServices.GetSystemColor(clBtnShadow))
    else
      DrawFrame(inherited Canvas, ClientRect, FrameStyle, FrameWidth, FrameColor, clBtnHighlight, clBtnShadow);

  if Focused then
  begin
    (inherited Canvas).Brush.Color := TColor($FF000000);
    (inherited Canvas).DrawFocusRect(ContentRect);
  end;
end;

procedure TEsImageControl.PaintWindow(DC: HDC);
begin
  Painting := True;
  try
    inherited;
  finally
    Painting := False;
  end;
end;

procedure TEsImageControl.RecreateBitmap;
begin
  if Picture.Bitmap <> nil then
  begin
    Picture.Assign(nil);
    GetCanvas;
    Invalidate;
  end;
end;

procedure TEsImageControl.SetOpacity(const Value: Byte);
begin
  ImageProxy.Opacity := Value;
end;

procedure TEsImageControl.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TEsImageControl.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TEsImageControl.SetFrameWidth(const Value: TFrameWidth);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TEsImageControl.SetImageIndex(const Value: TImageIndex);
begin
  ImageProxy.ImageIndex := Value;
end;

procedure TEsImageControl.SetImages(const Value: TCustomImageList);
begin
  ImageProxy.Images := Value;
end;

procedure TEsImageControl.SetIncrementalDisplay(const Value: Boolean);
begin
  ImageProxy.IncrementalDisplay := Value;
end;

procedure TEsImageControl.SetOnProgress(const Value: TProgressEvent);
begin
  ImageProxy.OnProgress := Value;
end;

procedure TEsImageControl.SetPicture(const Value: TPicture);
begin
  ImageProxy.Picture := Value;
end;

procedure TEsImageControl.SetSmoth(const Value: Boolean);
begin
  ImageProxy.Smoth := Value;
end;

procedure TEsImageControl.SetStretch(const Value: TImageStretch);
begin
  ImageProxy.Stretch := Value;
end;

procedure TEsImageControl.SetTransparentGraphic(const Value: Boolean);
begin
  ImageProxy.TransparentGraphic := Value;
end;

procedure TEsImageControl.WMKillFocus(var Message: TWMKillFocus);
begin
  Inherited;
  Invalidate;
end;

procedure TEsImageControl.WMSetFocus(var Message: TWMSetFocus);
begin
  Inherited;
  Invalidate;
end;

initialization
  {$IFDEF SUPPORT_ENUMS_ALIASES}
  AddEnumElementAliases(TypeInfo(TImageStretch), ['isNone', 'isCenter', 'isFit', 'isFill', 'isUniform' , 'isMixed']);
  {$ENDIF}

finalization
  {$IFDEF SUPPORT_ENUMS_ALIASES}
  RemoveEnumElementAliases(TypeInfo(TImageStretch));
  {$ENDIF}

end.
