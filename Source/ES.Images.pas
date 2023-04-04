{******************************************************************************}
{                                                                              }
{                       EsVclComponents/EsVclCore v4.4                         }
{                           errorsoft(c) 2009-2023                             }
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
{                                                                              }
{******************************************************************************}
unit ES.Images;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics,
  WinApi.Messages, ES.ExGraphics, ES.BaseControls, ES.CfxClasses, Vcl.ImgList,
  System.UITypes
  {$IFDEF VER340UP}, Vcl.BaseImageCollection, Vcl.ImageCollection, System.Messaging{$ENDIF};

type
  TImageStretch = (None, Center, Fit, Fill, Uniform, Mixed, FitFill);

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
    ImageChangeLink: TChangeLink;
    FBackgroundColor: TColor;
    FImageIndex: TImageIndex;
    {$IFDEF VER340UP}
    FImageName: TImageName;
    {$ENDIF}
    FImages: TCustomImageList;
    FIncrementalDisplay: Boolean;
    FOpacity: Byte;
    FPicture: TPicture;
    FSmoth: Boolean;
    FStretch: TImageStretch;
    FTransparent: Boolean;
    FTransparentGraphic: Boolean;
    FOnProgress: TProgressEvent;
    FOnChange: TNotifyEvent;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetImageIndex(const Value: TImageIndex);
    {$IFDEF VER340UP}
    procedure SetImageName(const Value: TImageName);
    {$ENDIF}
    procedure SetImages(const Value: TCustomImageList);
    procedure SetOpacity(const Value: Byte);
    procedure SetPicture(const Value: TPicture);
    procedure SetSmoth(const Value: Boolean);
    procedure SetStretch(const Value: TImageStretch);
    procedure SetTransparent(const Value: Boolean);
    procedure SetTransparentGraphic(const Value: Boolean);
  private
    function HasImageList: Boolean;
    procedure Change;
    procedure ImageListChange(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure PictureProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const Rect: TRect; const Msg: string);
  protected
    function GetRect(R: TRect): TRect;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function ImageHeight: Integer;
    function ImageWidth: Integer;
    procedure Draw(Canvas: TCanvas; ARect: TRect);
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    {$IFDEF VER340UP}
    property ImageName: TImageName read FImageName write SetImageName;
    {$ENDIF}
    property Images: TCustomImageList read FImages write SetImages;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Picture: TPicture read FPicture write SetPicture;
    property Smoth: Boolean read FSmoth write SetSmoth default True;
    property Stretch: TImageStretch read FStretch write SetStretch default TImageStretch.None;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property TransparentGraphic: Boolean read FTransparentGraphic write SetTransparentGraphic default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TImageInterpolationMode = (HighQualityCubic, Fant, Linear, Cubic, NearestNeighbor);

  {$IFDEF VER340UP}
  /// <summary> ONLY INTERNAL USE! </summary>
  TVirtualImageProxy = class(TComponent)
  private
    CollectionChangedMessageID: Integer;
    FImageCollection: TCustomImageCollection;
    FImageHeight: Integer;
    FImageIndex: TImageIndex;
    FImageName: string;
    FImageWidth: Integer;
    FInterpolationMode: TImageInterpolationMode;
    FOnChange: TNotifyEvent;
    FOpacity: Byte;
    FStretch: TImageStretch;
    procedure SetImageCollection(const Value: TCustomImageCollection);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageName(const Value: string);
    procedure SetImageWidth(const Value: Integer);
    procedure SetInterpolationMode(const Value: TImageInterpolationMode);
    procedure SetOpacity(const Value: Byte);
    procedure SetStretch(const Value: TImageStretch);
  private
    function GetRect(R: TRect): TRect;
    function HasImageCollection: Boolean;
    procedure Change;
    procedure CollectionChangedMessageHandler(const Sender: TObject;
      const Message: System.Messaging.TMessage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; ARect: TRect); virtual;
    property ImageCollection: TCustomImageCollection read FImageCollection write SetImageCollection;
    property ImageHeight: Integer read FImageHeight write SetImageHeight default 0;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageName: string read FImageName write SetImageName;
    property ImageWidth: Integer read FImageWidth write SetImageWidth default 0;
    property InterpolationMode: TImageInterpolationMode read FInterpolationMode
      write SetInterpolationMode default TImageInterpolationMode.HighQualityCubic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Stretch: TImageStretch read FStretch write SetStretch default TImageStretch.Fit;
  end;
  {$ENDIF}

  TEsImage = class(TEsGraphicControl)
  private
    DrawCount: Integer;
    ImageProxy: TImageProxy;
    FDoubleBuffered: Boolean;
    FOnPaint: TPaintEvent;
    FOnPainting: TPaintEvent;
    FFrameWidth: TFrameWidth;
    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    FBorderWidth: Integer;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    function GetCanvas: TCanvas;
    //function GetColor: TColor;
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    function GetIncrementalDisplay: Boolean;
    function GetOnProgress: TProgressEvent;
    function GetOpacity: Byte;
    function GetPicture: TPicture;
    function GetSmoth: Boolean;
    function GetStretch: TImageStretch;
    function GetTransparent: Boolean;
    function GetTransparentGraphic: Boolean;
    //procedure SetColor(const Value: TColor);
    procedure SetDoubleBuffered(const Value: Boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIncrementalDisplay(const Value: Boolean);
    procedure SetOnProgress(const Value: TProgressEvent);
    procedure SetOpacity(const Value: Byte);
    procedure SetPicture(const Value: TPicture);
    procedure SetSmoth(const Value: Boolean);
    procedure SetStretch(const Value: TImageStretch);
    procedure SetTransparent(const Value: Boolean);
    procedure SetTransparentGraphic(const Value: Boolean);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
    procedure SetBorderWidth(const Value: Integer);
    {$IFDEF VER340UP}
    function GetImageName: TImageName;
    function IsImageNameStored: Boolean;
    procedure SetImageName(const Value: TImageName);
    {$ENDIF}
  protected
    procedure CalcContentMargins(var Margins: TContentMargins); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    {$IFDEF VER310UP}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    procedure ImageProxyChange(Sender: TObject);
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginDraw;
    procedure EndDraw;
    procedure RecreateBitmap;
    property Canvas: TCanvas read GetCanvas;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property AutoSize;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property Color default clBtnFace;//: TColor read GetColor write SetColor default clBtnFace;
    property Constraints;
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.None;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    {$IFDEF VER340UP}
    property ImageName: TImageName read GetImageName write SetImageName stored IsImageNameStored;
    {$ENDIF}
    property Images: TCustomImageList read GetImages write SetImages;
    property IncrementalDisplay: Boolean read GetIncrementalDisplay write SetIncrementalDisplay default False;
    property IsDrawHelper default True;
    property Margins;
    property Opacity: Byte read GetOpacity write SetOpacity default 255;
    property Padding;
    property ParentShowHint;
    property ParentColor;
    property Picture: TPicture read GetPicture write SetPicture;
    property PopupMenu;
    property ShowHint;
    property Smoth: Boolean read GetSmoth write SetSmoth default True;
    property Stretch: TImageStretch read GetStretch write SetStretch default TImageStretch.None;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property Touch;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property TransparentGraphic: Boolean read GetTransparentGraphic write SetTransparentGraphic default False;
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
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnPainting: TPaintEvent read FOnPainting write FOnPainting;
    property OnProgress: TProgressEvent read GetOnProgress write SetOnProgress;
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
    function GetCanvas: TCanvas;
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    function GetIncrementalDisplay: Boolean;
    function GetOnProgress: TProgressEvent;
    function GetOpacity: Byte;
    function GetPicture: TPicture;
    function GetSmoth: Boolean;
    function GetStretch: TImageStretch;
    function GetTransparentGraphic: Boolean;
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIncrementalDisplay(const Value: Boolean);
    procedure SetOnProgress(const Value: TProgressEvent);
    procedure SetOpacity(const Value: Byte);
    procedure SetPicture(const Value: TPicture);
    procedure SetSmoth(const Value: Boolean);
    procedure SetStretch(const Value: TImageStretch);
    procedure SetTransparentGraphic(const Value: Boolean);
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    {$IFDEF VER340UP}
    function GetImageName: TImageName;
    function IsImageNameStored: Boolean;
    procedure SetImageName(const Value: TImageName);
    {$ENDIF}
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CalcContentMargins(var Margins: TContentMargins); override;
    procedure ImageProxyChange(Sender: TObject);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginDraw;
    procedure EndDraw;
    procedure RecreateBitmap;
    property Canvas: TCanvas read GetCanvas;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property AutoSize;
    property BorderWidth;
    property Color default clBtnFace;//nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.None;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    {$IFDEF VER340UP}
    property ImageName: TImageName read GetImageName write SetImageName stored IsImageNameStored;
    {$ENDIF}
    property Images: TCustomImageList read GetImages write SetImages;
    property IncrementalDisplay: Boolean read GetIncrementalDisplay write SetIncrementalDisplay default False;
    property IsCachedBackground;// TEsCustomControl
    property IsCachedBuffer;// TEsCustomControl
    property IsDrawHelper default True;
    property IsFullSizeBuffer;// TEsCustomControl
    property Margins;
    property Opacity: Byte read GetOpacity write SetOpacity default 255;
    property Padding;
    property ParentColor default True;
    property ParentShowHint;
    property Picture: TPicture read GetPicture write SetPicture;
    property PopupMenu;
    property ShowHint;
    property Smoth: Boolean read GetSmoth write SetSmoth default True;
    property Stretch: TImageStretch read GetStretch write SetStretch default TImageStretch.None;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property Touch;
    property Transparent;
    property TransparentGraphic: Boolean read GetTransparentGraphic write SetTransparentGraphic default False;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnPainting;
    property OnProgress: TProgressEvent read GetOnProgress write SetOnProgress;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  {$IFDEF VER340UP}
  TEsCustomVirtualImage = class(TEsGraphicControl)
  private
    ImageProxy: TVirtualImageProxy;
    FDoubleBuffered: Boolean;
    FColor: TColor;
    FTransparent: Boolean;
    FOnPaint: TPaintEvent;
    FOnPainting: TPaintEvent;
    FFrameWidth: TFrameWidth;
    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    FBorderWidth: Integer;
    function GetImageCollection: TCustomImageCollection;
    function GetImageHeight: Integer;
    function GetImageIndex: TImageIndex;
    function GetImageName: TImageName;
    function GetImageWidth: Integer;
    function GetInterpolationMode: TImageInterpolationMode;
    function GetOpacity: Byte;
    function GetStretch: TImageStretch;
    function IsImageNameStored: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetDoubleBuffered(const Value: Boolean);
    procedure SetImageCollection(const Value: TCustomImageCollection);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageName(const Value: TImageName);
    procedure SetImageWidth(const Value: Integer);
    procedure SetInterpolationMode(const Value: TImageInterpolationMode);
    procedure SetOpacity(const Value: Byte);
    procedure SetStretch(const Value: TImageStretch);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
    procedure SetBorderWidth(const Value: Integer);
  protected
    procedure CalcContentMargins(var Margins: TContentMargins); override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure ImageProxyChange(Sender: TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    /// <summary> The color that is visible under the semi-transparent image </summary>
    property Color: TColor read FColor write SetColor default clBtnFace;
    /// <summary> Allows for less blinking with an opaque background </summary>
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered default False;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.None;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
    /// <summary> Component with source images </summary>
    property ImageCollection: TCustomImageCollection read GetImageCollection write SetImageCollection;
    /// <summary> Manually set height </summary>
    property ImageHeight: Integer read GetImageHeight write SetImageHeight default 0;
    /// <summary> Index in source image collection </summary>
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    /// <summary> Name in source image collection </summary>
    property ImageName: TImageName read GetImageName write SetImageName stored IsImageNameStored;
    /// <summary> Manually set width </summary>
    property ImageWidth: Integer read GetImageWidth write SetImageWidth default 0;
    /// <summary> Interpolation mode affects the quality and speed of scaling </summary>
    property InterpolationMode: TImageInterpolationMode read GetInterpolationMode write SetInterpolationMode default TImageInterpolationMode.HighQualityCubic;
    /// <summary> IsDrawHelper specifies the drawing of the design time helper </summary>
    property IsDrawHelper default True;
    /// <summary> Transparency level for the output image </summary>
    property Opacity: Byte read GetOpacity write SetOpacity default 255;
    /// <summary> Image location </summary>
    property Stretch: TImageStretch read GetStretch write SetStretch default TImageStretch.Fit;
    /// <summary> Transparent specifies the draw background </summary>
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    /// <summary> Called before drawing </summary>
    property OnPainting: TPaintEvent read FOnPainting write FOnPainting;
    /// <summary> Called after drawing </summary>
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
  end;

  TEsVirtualImage = class(TEsCustomVirtualImage)
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property BorderWidth;// +TEsCustomVirtualImage
    property Color;// +TEsCustomVirtualImage
    property Constraints;
    property DoubleBuffered;// +TEsCustomVirtualImage
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FrameColor;// +TEsCustomVirtualImage
    property FrameStyle;// +TEsCustomVirtualImage
    property FrameWidth;// +TEsCustomVirtualImage
    property ImageCollection;// +TEsCustomVirtualImage
    property ImageHeight;// +TEsCustomVirtualImage
    property ImageIndex;// +TEsCustomVirtualImage
    property ImageName;// +TEsCustomVirtualImage
    property ImageWidth;// +TEsCustomVirtualImage
    property InterpolationMode;// +TEsCustomVirtualImage
    property IsDrawHelper;// +TEsCustomVirtualImage
    property Margins;
    property Opacity;// +TEsCustomVirtualImage
    property Padding;// +TEsCustomVirtualImage
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Stretch;// +TEsCustomVirtualImage
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property Touch;
    property Transparent;// +TEsCustomVirtualImage
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
    property OnPaint;
    property OnPainting;
    property OnStartDock;
    property OnStartDrag;
  end;

  TEsCustomVirtualImageControl = class(TEsBaseLayout)
  private
    ImageProxy: TVirtualImageProxy;
    FFrameWidth: TFrameWidth;
    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    function GetImageCollection: TCustomImageCollection;
    function GetImageHeight: Integer;
    function GetImageIndex: TImageIndex;
    function GetImageName: TImageName;
    function GetImageWidth: Integer;
    function GetInterpolationMode: TImageInterpolationMode;
    function GetOpacity: Byte;
    function GetStretch: TImageStretch;
    function IsImageNameStored: Boolean;
    procedure SetImageCollection(const Value: TCustomImageCollection);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageName(const Value: TImageName);
    procedure SetImageWidth(const Value: Integer);
    procedure SetInterpolationMode(const Value: TImageInterpolationMode);
    procedure SetOpacity(const Value: Byte);
    procedure SetStretch(const Value: TImageStretch);
  private
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetFrameWidth(const Value: TFrameWidth);
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure CalcContentMargins(var Margins: TContentMargins); override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure ImageProxyChange(Sender: TObject);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnShadow;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default TExFrameStyle.None;
    property FrameWidth: TFrameWidth read FFrameWidth write SetFrameWidth default 1;
    /// <summary> Component with source images </summary>
    property ImageCollection: TCustomImageCollection read GetImageCollection write SetImageCollection;
    /// <summary> Manually set height </summary>
    property ImageHeight: Integer read GetImageHeight write SetImageHeight default 0;
    /// <summary> Index in source image collection </summary>
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    /// <summary> Name in source image collection </summary>
    property ImageName: TImageName read GetImageName write SetImageName stored IsImageNameStored;
    /// <summary> Manually set width </summary>
    property ImageWidth: Integer read GetImageWidth write SetImageWidth default 0;
    /// <summary> Interpolation mode affects the quality and speed of scaling </summary>
    property InterpolationMode: TImageInterpolationMode read GetInterpolationMode write SetInterpolationMode default TImageInterpolationMode.HighQualityCubic;
    /// <summary> IsDrawHelper specifies the drawing of the design time helper </summary>
    property IsDrawHelper default True;
    /// <summary> Transparency level for the output image </summary>
    property Opacity: Byte read GetOpacity write SetOpacity default 255;
    /// <summary> Image location </summary>
    property Stretch: TImageStretch read GetStretch write SetStretch default TImageStretch.Fit;
  end;

  TEsVirtualImageControl = class(TEsCustomVirtualImageControl)
    property Align;
    property AlignWithMargins;
    property Anchors;
    property BorderWidth;
    property Color default clBtnFace;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property ImageCollection;// +TEsCustomVirtualImageControl
    property ImageHeight;// +TEsCustomVirtualImageControl
    property ImageIndex;// +TEsCustomVirtualImageControl
    property ImageName;// +TEsCustomVirtualImageControl
    property ImageWidth;// +TEsCustomVirtualImageControl
    property InterpolationMode;// +TEsCustomVirtualImageControl
    property IsCachedBackground;// TEsCustomControl
    property IsCachedBuffer;// TEsCustomControl
    property IsDrawHelper default True;
    property IsFullSizeBuffer;// TEsCustomControl
    property Margins;
    property Opacity;// +TEsCustomVirtualImageControl
    property ParentColor default True;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Stretch;// +TEsCustomVirtualImageControl
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property Touch;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnPainting;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;
  {$ENDIF}

function CalcImageRect(const R: TRect; const ImageWidth, ImageHeight: Integer;
  const Stretch: TImageStretch): TRect;

implementation

uses
  ES.ExGdiPlus, System.Math, ES.Utils, Vcl.Themes, System.TypInfo;

function CalcImageRect(const R: TRect; const ImageWidth, ImageHeight: Integer;
  const Stretch: TImageStretch): TRect;
var
  dw, dh: Integer;
  w, h: Integer;
  WidthHeight: Real;
begin
  dw := R.Width;
  dh := R.Height;

  if ((Stretch in [TImageStretch.Fit, TImageStretch.Uniform]) or
    ((Stretch = TImageStretch.Mixed) and ((dw < ImageWidth) or (dh < ImageHeight)))) and
    ((ImageWidth <> 0) and (ImageHeight <> 0)) then
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
  end
  else if (Stretch = TImageStretch.FitFill) and ((ImageWidth <> 0) and (ImageHeight <> 0)) then
  begin
    WidthHeight := ImageWidth / ImageHeight;

    if dh < Round(dw / WidthHeight) then
    begin
      w := dw;
      h := Round(dw / WidthHeight);
    end else
    begin
      h := dh;
      w := Round(dh * WidthHeight);
    end;
  end
  else if Stretch = TImageStretch.Fill then
  begin
    w := dw;
    h := dh;
  end else
  begin
    w := ImageWidth;
    h := ImageHeight;
  end;

  Result := Rect(R.Left, R.Top, R.Left + w, R.Top + h);

  if Stretch in [TImageStretch.Fit, TImageStretch.Mixed, TImageStretch.Center,
    TImageStretch.FitFill] then
  begin
    Result.Offset((dw - w) div 2, (dh - h) div 2);
  end;
end;

procedure DrawDesignImageFrame(Canvas: TCanvas; Rect: TRect; IsWinControl: Boolean);
begin
  Canvas.Brush.Style := bsClear;

  Canvas.Pen.Color := RGB(67, 67, 67);
  if IsWinControl then
    Canvas.Pen.Style := psSolid
  else
    Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(Rect);

  Canvas.Pen.Color := RGB(160, 160, 255);
  Canvas.Pen.Style := psDot;
  Canvas.Rectangle(Rect);
end;

procedure DrawDesignVirtualImageFrame(Canvas: TCanvas; Rect: TRect; IsWinControl: Boolean);
begin
  Canvas.Brush.Style := bsClear;

  Canvas.Pen.Color := RGB(67, 67, 67);
  if IsWinControl then
    Canvas.Pen.Style := psSolid
  else
    Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(Rect);

  Canvas.Pen.Color := RGB(255, 100, 80);
  Canvas.Pen.Style := psDot;
  Canvas.Rectangle(Rect);
end;

{$IFDEF VER340UP}
procedure DrawDesignStretchHint(const Canvas: TCanvas; ARect: TRect; const Proxy: TVirtualImageProxy);
var
  HintString: string;
  Bitmap: TBitmap;
  R: TRect;
begin
  if ((Proxy.ImageWidth <= 0) or (Proxy.ImageHeight <= 0)) and
    not (Proxy.Stretch in [TImageStretch.Fit, TImageStretch.Fill]) then
  begin
    Bitmap := TBitmap.Create;
    try
      // text settings
      Canvas.Font.Color := clBlack;
      Canvas.Font.Size := 7;
      Canvas.Brush.Style := bsClear;
      HintString := 'If (ImageWidth = 0) or (ImageHeight = 0) then only these Stretch are valid: Fit, Fill';
      R := ARect;
      Canvas.TextRect(R, HintString, [tfWordBreak, tfCalcRect]);
      // bg
      Bitmap.SetSize(ARect.Width, R.Height);
      Bitmap.Canvas.Brush.Color := clYellow;
      Bitmap.Canvas.FillRect(TRect.Create(0, 0, Bitmap.Width, Bitmap.Height));
      // draw
      Canvas.Draw(ARect.Left, ARect.Top, Bitmap, 120);
      Canvas.TextRect(ARect, HintString, [tfWordBreak]);
    finally
      Bitmap.Free;
    end;
  end;
end;
{$ENDIF}

{ TImageProxy }

constructor TImageProxy.Create;
begin
  inherited Create(nil);

  ImageChangeLink := TChangeLink.Create;
  ImageChangeLink.OnChange := ImageListChange;

  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := PictureProgress;

  FSmoth := True;
  FImageIndex := -1;
  FBackgroundColor := clWhite;
  FTransparent := True;
  FOpacity := 255;
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
  if HasImageList then
  begin
    R := GetRect(ARect);
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
    R := GetRect(ARect);
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

function TImageProxy.ImageHeight: Integer;
begin
  if HasImageList then
    Result := Images.Height
  else
    Result := Picture.Height;
end;

function TImageProxy.ImageWidth: Integer;
begin
  if HasImageList then
    Result := Images.Width
  else
    Result := Picture.Width;
end;

function TImageProxy.GetRect(R: TRect): TRect;
begin
  Result := CalcImageRect(R, ImageWidth, ImageHeight, Stretch);
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
    {$IFDEF VER340UP}
    // TVirtualImageList support
    if (FImages <> nil) and FImages.IsImageNameAvailable then
      FImageName := FImages.GetNameByIndex(FImageIndex);
    {$ENDIF}
    Change;
  end;
end;

{$IFDEF VER340UP}
procedure TImageProxy.SetImageName(const Value: TImageName);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    if (FImages <> nil) and FImages.IsImageNameAvailable then
      FImageIndex := FImages.GetIndexByName(FImageName);
    Change;
  end;
end;
{$ENDIF}

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
  ImageProxy.BackgroundColor := Color;
  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
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

procedure TEsImage.CalcContentMargins(var Margins: TContentMargins);
begin
  inherited;
  if FrameStyle <> TExFrameStyle.None then
  begin
    Margins.Inflate(GetFrameWidth(FrameStyle, FrameWidth), GetFrameWidth(FrameStyle, FrameWidth));
  end;
  if BorderWidth <> 0 then
  begin
    Margins.Inflate(BorderWidth, BorderWidth);
  end;
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

procedure TEsImage.CMColorChanged(var Message: TMessage);
begin
  ImageProxy.BackgroundColor := Color;
  inherited;
end;

{$IFDEF VER310UP}
procedure TEsImage.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
end;
{$ELSE}
procedure TEsImage.ChangeScale(M, D: Integer);
begin
  inherited;
end;
{$ENDIF}

{function TEsImage.GetColor: TColor;
begin
  Result := ImageProxy.BackgroundColor;
end;}

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

{$IFDEF VER340UP}
function TEsImage.GetImageName: TImageName;
begin
  Result := ImageProxy.ImageName;
end;
{$ENDIF}

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

{$IFDEF VER340UP}
function TEsImage.IsImageNameStored: Boolean;
begin
  Result := ImageProxy.ImageName <> '';
end;
{$ENDIF}

procedure TEsImage.Loaded;
begin
  inherited;
  if Autosize then
    AdjustSize;
end;

procedure TEsImage.Paint;
var
  Bitmap: TBitmap;
  CurrentCanvas: TCanvas;
begin
  Bitmap := nil;
  try
    if FDoubleBuffered then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.SetSize(Max(0, Width), Max(0, Height));
      CurrentCanvas := Bitmap.Canvas;
      CurrentCanvas.FillRect(ClientRect, ClientColorToRgb(Color, Self));
    end else
    begin
      CurrentCanvas := inherited Canvas;
      if not Transparent then
        CurrentCanvas.FillRect(ClientRect, ClientColorToRgb(Color, Self));
    end;

    if (csDesigning in ComponentState) and IsDrawHelper and
      (ImageProxy.ImageWidth * ImageProxy.ImageHeight <> 0) then
    begin
      CurrentCanvas.Pen.Color := clBlue;
      CurrentCanvas.Pen.Style := psSolid;
      CurrentCanvas.DrawCorners(ImageProxy.GetRect(ContentRect), 10);
    end;

    if FrameStyle <> TExFrameStyle.None then
      if IsStyledBorderControl(Self) then
        DrawFrame(CurrentCanvas, Self, ClientRect, FrameStyle, FrameWidth, FrameColor,
          clBtnHighlight, clBtnShadow)
      else
        DrawFrame(CurrentCanvas, nil, ClientRect, FrameStyle, FrameWidth, FrameColor, clBtnHighlight, clBtnShadow);

    if Assigned(FOnPainting) then
      FOnPainting(Self, CurrentCanvas, ContentRect);
    ImageProxy.Draw(CurrentCanvas, ContentRect);
    if Assigned(FOnPaint) then
      FOnPaint(Self, CurrentCanvas, ContentRect);

    if csDesigning in ComponentState then
    begin
      DrawDesignImageFrame(CurrentCanvas, ClientRect, False);
      if IsDrawHelper then
        DrawControlHelper(CurrentCanvas, ClientRect, 0, Padding, [hoPadding]);
    end;

    if Bitmap <> nil then
      inherited Canvas.Draw(ClientRect.Left, ClientRect.Top, Bitmap);
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

procedure TEsImage.SetBorderWidth(const Value: Integer);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

{procedure TEsImage.SetColor(const Value: TColor);
begin
  ImageProxy.BackgroundColor := Value;
end;}

procedure TEsImage.SetDoubleBuffered(const Value: Boolean);
begin
  if FDoubleBuffered <> Value then
  begin
    FDoubleBuffered := Value;
    if Value then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];
    Invalidate;
  end;
end;

procedure TEsImage.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TEsImage.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Invalidate;
  end;
end;

procedure TEsImage.SetFrameWidth(const Value: TFrameWidth);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Invalidate;
  end;
end;

procedure TEsImage.SetImageIndex(const Value: TImageIndex);
begin
  ImageProxy.ImageIndex := Value;
end;

{$IFDEF VER340UP}
procedure TEsImage.SetImageName(const Value: TImageName);
begin
  ImageProxy.ImageName := Value;
end;
{$ENDIF}

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
  //Color := clBtnFace;
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

{$IFDEF VER340UP}
function TEsImageControl.GetImageName: TImageName;
begin
  Result := ImageProxy.ImageName;
end;
{$ENDIF}

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

{$IFDEF VER340UP}
function TEsImageControl.IsImageNameStored: Boolean;
begin
  Result := ImageProxy.ImageName <> '';
end;
{$ENDIF}

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
    DrawDesignImageFrame(inherited Canvas, ClientRect, True);
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
      DrawFrame(inherited Canvas, Self, ClientRect, FrameStyle, FrameWidth, FrameColor,
        clBtnHighlight, clBtnShadow)
    else
      DrawFrame(inherited Canvas, nil, ClientRect, FrameStyle, FrameWidth, FrameColor, clBtnHighlight, clBtnShadow);

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

{$IFDEF VER340UP}
procedure TEsImageControl.SetImageName(const Value: TImageName);
begin
  ImageProxy.ImageName := Value;
end;
{$ENDIF}

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

{$IFDEF VER340UP}

{ TVirtualImageProxy }

procedure TVirtualImageProxy.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TVirtualImageProxy.CollectionChangedMessageHandler(
  const Sender: TObject; const Message: System.Messaging.TMessage);
var
  ImageName: string;
begin
  if TImageCollectionChangedMessage(Message).Collection = FImageCollection then
  begin
    if FImageName <> '' then
    begin
      ImageName := FImageCollection.GetNameByIndex(FImageIndex);
      if ImageName <> FImageName then
        FImageIndex := FImageCollection.GetIndexByName(FImageName);
    end;
  end;
end;

constructor TVirtualImageProxy.Create;
begin
  inherited Create(nil);
  CollectionChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(
    TImageCollectionChangedMessage, CollectionChangedMessageHandler);

  FOpacity := 255;
  FImageIndex := -1;
  FInterpolationMode := TImageInterpolationMode.HighQualityCubic;
  FStretch := TImageStretch.Fit;
end;

destructor TVirtualImageProxy.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(
    TImageCollectionChangedMessage, CollectionChangedMessageID);
  ImageCollection := nil;
  inherited;
end;

procedure TVirtualImageProxy.Draw(Canvas: TCanvas; ARect: TRect);
  function MakeCleanBitmap(const Width, Height: Integer): TBitmap;
  var
    X, Y: Integer;
    Scanline: PCardinal;
  begin
    Result := TBitmap.Create;
    try
      Result.PixelFormat := pf32bit;
      Result.SetSize(Width, Height);
      for Y  := 0 to Height - 1 do
      begin
        Scanline := PCardinal(Result.ScanLine[Y]);
        for X := 0 to Width - 1 do
          {$POINTERMATH ON}
          (Scanline + X)^ := 0;
          {$POINTERMATH OFF}
      end;
    except
      Result.Free;
      raise;
    end;
  end;

var
  R: TRect;
  Bitmap: TBitmap;
  OldMode: TImageCollectionInterpolationMode;
  VclImageCollection: TImageCollection;
begin
  if not HasImageCollection then
    Exit;

  // If width and height are available, we can provide all stretch modes
  if (ImageWidth > 0) and (ImageHeight > 0) then
    R := GetRect(ARect)
  else
  begin
    R := ARect;
  end;

  // set InterpolationMode for TImageCollection
  OldMode := icIMModeNearestNeighbor;// compiler paranoia
  if ImageCollection is TImageCollection then
  begin
    VclImageCollection := TImageCollection(ImageCollection);
    OldMode := VclImageCollection.InterpolationMode;
    case InterpolationMode of
      TImageInterpolationMode.HighQualityCubic:
        VclImageCollection.InterpolationMode := icIMModeHighQualityCubic;
      TImageInterpolationMode.Fant:
        VclImageCollection.InterpolationMode := icIMFant;
      TImageInterpolationMode.Linear:
        VclImageCollection.InterpolationMode := icIMLinear;
      TImageInterpolationMode.Cubic:
        VclImageCollection.InterpolationMode := icIMCubic;
      TImageInterpolationMode.NearestNeighbor:
        VclImageCollection.InterpolationMode := icIMModeNearestNeighbor;
    end;
  end else
    VclImageCollection := nil;

  try
    if Opacity = 255 then
      ImageCollection.Draw(Canvas, R, ImageIndex, Stretch <> TImageStretch.Fill)
    else
    begin
      Bitmap := MakeCleanBitmap(R.Width, R.Height);
      try
        ImageCollection.Draw(Bitmap.Canvas, TRect.Create(0, 0, R.Width, R.Height),
          ImageIndex, Stretch <> TImageStretch.Fill);
        Canvas.Draw(R.Left, R.Top, Bitmap, Opacity);
      finally
        Bitmap.Free;
      end;
    end;
  finally
    // restore InterpolationMode
    if VclImageCollection <> nil then
      VclImageCollection.InterpolationMode := OldMode;
  end;
end;

function TVirtualImageProxy.GetRect(R: TRect): TRect;
begin
  Result := CalcImageRect(R, ImageWidth, ImageHeight, Stretch);
end;

function TVirtualImageProxy.HasImageCollection: Boolean;
begin
  Result := ImageCollection <> nil;
end;

procedure TVirtualImageProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FImageCollection) and (Operation = opRemove) then
  begin
    FImageCollection := nil;
    Change;
  end;
end;

procedure TVirtualImageProxy.SetImageCollection(const Value: TCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    if FImageCollection <> nil then
      FImageCollection.RemoveFreeNotification(Self);
    FImageCollection := Value;
    if FImageCollection <> nil then
      FImageCollection.FreeNotification(Self);
    if not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
      Change;
  end;
end;

procedure TVirtualImageProxy.SetImageHeight(const Value: Integer);
begin
  if (Value >= 0) and (Value <> FImageHeight) then
  begin
    FImageHeight := Value;
    Change;
  end;
end;

procedure TVirtualImageProxy.SetImageWidth(const Value: Integer);
begin
  if (Value >= 0) and (Value <> FImageWidth) then
  begin
    FImageWidth := Value;
    Change;
  end;
end;

procedure TVirtualImageProxy.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FImageCollection <> nil then
      FImageName := FImageCollection.GetNameByIndex(FImageIndex);
    Change;
  end;
end;

procedure TVirtualImageProxy.SetImageName(const Value: string);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    if FImageCollection <> nil then
      FImageIndex := FImageCollection.GetIndexByName(FImageName);
    Change;
  end;
end;

procedure TVirtualImageProxy.SetInterpolationMode(const Value: TImageInterpolationMode);
begin
  if FInterpolationMode <> Value then
  begin
    FInterpolationMode := Value;
    Change;
  end;
end;

procedure TVirtualImageProxy.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Change;
  end;
end;

procedure TVirtualImageProxy.SetStretch(const Value: TImageStretch);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Change;
  end;
end;

{$ENDIF}

{$IFDEF VER340UP}

{ TEsCustomVirtualImage }

constructor TEsCustomVirtualImage.Create(AOwner: TComponent);
begin
  inherited;
  ImageProxy := TVirtualImageProxy.Create;
  ImageProxy.OnChange := ImageProxyChange;
  Color := clBtnFace;
  IsDrawHelper := True;
  Transparent := True;
  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
  Width := 100;
  Height := 100;
end;

destructor TEsCustomVirtualImage.Destroy;
begin
  ImageProxy.Free;
  inherited;
end;

procedure TEsCustomVirtualImage.CalcContentMargins(var Margins: TContentMargins);
begin
  inherited;
  if FrameStyle <> TExFrameStyle.None then
  begin
    Margins.Inflate(GetFrameWidth(FrameStyle, FrameWidth), GetFrameWidth(FrameStyle, FrameWidth));
  end;
  if BorderWidth <> 0 then
  begin
    Margins.Inflate(BorderWidth, BorderWidth);
  end;
end;

function TEsCustomVirtualImage.GetImageCollection: TCustomImageCollection;
begin
  Result := ImageProxy.ImageCollection;
end;

function TEsCustomVirtualImage.GetImageHeight: Integer;
begin
  Result := ImageProxy.ImageHeight;
end;

function TEsCustomVirtualImage.GetImageIndex: TImageIndex;
begin
  Result := ImageProxy.ImageIndex;
end;

function TEsCustomVirtualImage.GetImageName: TImageName;
begin
  Result := ImageProxy.ImageName;
end;

function TEsCustomVirtualImage.GetImageWidth: Integer;
begin
  Result := ImageProxy.ImageWidth;
end;

function TEsCustomVirtualImage.GetInterpolationMode: TImageInterpolationMode;
begin
  Result := ImageProxy.InterpolationMode;
end;

function TEsCustomVirtualImage.GetOpacity: Byte;
begin
  Result := ImageProxy.Opacity;
end;

function TEsCustomVirtualImage.GetStretch: TImageStretch;
begin
  Result := ImageProxy.Stretch;
end;

{$IFDEF VER310UP}
procedure TEsCustomVirtualImage.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  ImageProxy.ImageWidth := MulDiv(ImageProxy.ImageWidth, M, D);
  ImageProxy.ImageHeight := MulDiv(ImageProxy.ImageHeight, M, D);
  inherited;
end;
{$ELSE}
procedure TEsCustomVirtualImage.ChangeScale(M, D: Integer);
begin
  ImageProxy.ImageWidth := MulDiv(ImageProxy.ImageWidth, M, D);
  ImageProxy.ImageHeight := MulDiv(ImageProxy.ImageHeight, M, D);
  inherited;
end;
{$ENDIF}

procedure TEsCustomVirtualImage.ImageProxyChange(Sender: TObject);
begin
  Invalidate;
end;

function TEsCustomVirtualImage.IsImageNameStored: Boolean;
begin
  Result := ImageProxy.ImageName <> '';
end;

procedure TEsCustomVirtualImage.Paint;
var
  Bitmap: TBitmap;
  CurrentCanvas: TCanvas;
begin
  Bitmap := nil;
  try
    if FDoubleBuffered then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.SetSize(Max(0, Width), Max(0, Height));
      CurrentCanvas := Bitmap.Canvas;
      CurrentCanvas.FillRect(ClientRect, ClientColorToRgb(Color, Self));
    end else
    begin
      CurrentCanvas := inherited Canvas;
      if not Transparent then
        CurrentCanvas.FillRect(ClientRect, ClientColorToRgb(Color, Self));
    end;

    if (csDesigning in ComponentState) and IsDrawHelper and
      (ImageProxy.ImageWidth * ImageProxy.ImageHeight <> 0) then
    begin
      CurrentCanvas.Pen.Color := clBlue;
      CurrentCanvas.Pen.Style := psSolid;
      CurrentCanvas.DrawCorners(ImageProxy.GetRect(ContentRect), 10);
    end;

    if FrameStyle <> TExFrameStyle.None then
      if IsStyledBorderControl(Self) then
        DrawFrame(CurrentCanvas, Self, ClientRect, FrameStyle, FrameWidth, FrameColor,
          clBtnHighlight, clBtnShadow)
      else
        DrawFrame(CurrentCanvas, nil, ClientRect, FrameStyle, FrameWidth, FrameColor, clBtnHighlight, clBtnShadow);


    if Assigned(FOnPainting) then
      FOnPainting(Self, CurrentCanvas, ClientRect);
    ImageProxy.Draw(CurrentCanvas, ContentRect);
    if Assigned(FOnPaint) then
      FOnPaint(Self, CurrentCanvas, ClientRect);

    if csDesigning in ComponentState then
    begin
      DrawDesignVirtualImageFrame(CurrentCanvas, ClientRect, False);
      if IsDrawHelper then
        DrawControlHelper(CurrentCanvas, ClientRect, 0, Padding, [hoPadding]);
      DrawDesignStretchHint(CurrentCanvas, ClientRect, ImageProxy);
    end;

    if Bitmap <> nil then
      inherited Canvas.Draw(ClientRect.Left, ClientRect.Top, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TEsCustomVirtualImage.SetBorderWidth(const Value: Integer);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImage.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImage.SetDoubleBuffered(const Value: Boolean);
begin
  if FDoubleBuffered <> Value then
  begin
    FDoubleBuffered := Value;
    if Value then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImage.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImage.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImage.SetFrameWidth(const Value: TFrameWidth);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImage.SetImageCollection(const Value: TCustomImageCollection);
begin
  ImageProxy.ImageCollection := Value;
end;

procedure TEsCustomVirtualImage.SetImageHeight(const Value: Integer);
begin
  ImageProxy.ImageHeight := Value;
end;

procedure TEsCustomVirtualImage.SetImageIndex(const Value: TImageIndex);
begin
  ImageProxy.ImageIndex := Value;
end;

procedure TEsCustomVirtualImage.SetImageName(const Value: TImageName);
begin
  ImageProxy.ImageName := Value;
end;

procedure TEsCustomVirtualImage.SetImageWidth(const Value: Integer);
begin
  ImageProxy.ImageWidth := Value;
end;

procedure TEsCustomVirtualImage.SetInterpolationMode(const Value: TImageInterpolationMode);
begin
  ImageProxy.InterpolationMode := Value;
end;

procedure TEsCustomVirtualImage.SetOpacity(const Value: Byte);
begin
  ImageProxy.Opacity := Value;
end;

procedure TEsCustomVirtualImage.SetStretch(const Value: TImageStretch);
begin
  ImageProxy.Stretch := Value;
end;

procedure TEsCustomVirtualImage.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

{ TEsCustomVirtualImageControl }

constructor TEsCustomVirtualImageControl.Create(AOwner: TComponent);
begin
  inherited;
  ImageProxy := TVirtualImageProxy.Create;
  ImageProxy.OnChange := ImageProxyChange;
  //Color := clBtnFace;
  IsDrawHelper := True;
  Transparent := True;
  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
  Width := 100;
  Height := 100;
end;

destructor TEsCustomVirtualImageControl.Destroy;
begin
  ImageProxy.Free;
  inherited;
end;

procedure TEsCustomVirtualImageControl.CalcContentMargins(
  var Margins: TContentMargins);
begin
  inherited;
  if FrameStyle <> TExFrameStyle.None then
  begin
    Margins.Inflate(GetFrameWidth(FrameStyle, FrameWidth), GetFrameWidth(FrameStyle, FrameWidth));
  end;
end;

{$IFDEF VER310UP}
procedure TEsCustomVirtualImageControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  ImageProxy.ImageWidth := MulDiv(ImageProxy.ImageWidth, M, D);
  ImageProxy.ImageHeight := MulDiv(ImageProxy.ImageHeight, M, D);
  inherited;
end;
{$ELSE}
procedure TEsCustomVirtualImageControl.ChangeScale(M, D: Integer);
begin
  ImageProxy.ImageWidth := MulDiv(ImageProxy.ImageWidth, M, D);
  ImageProxy.ImageHeight := MulDiv(ImageProxy.ImageHeight, M, D);
  inherited;
end;
{$ENDIF}

function TEsCustomVirtualImageControl.GetImageCollection: TCustomImageCollection;
begin
  Result := ImageProxy.ImageCollection;
end;

function TEsCustomVirtualImageControl.GetImageHeight: Integer;
begin
  Result := ImageProxy.ImageHeight;
end;

function TEsCustomVirtualImageControl.GetImageIndex: TImageIndex;
begin
  Result := ImageProxy.ImageIndex;
end;

function TEsCustomVirtualImageControl.GetImageName: TImageName;
begin
  Result := ImageProxy.ImageName;
end;

function TEsCustomVirtualImageControl.GetImageWidth: Integer;
begin
  Result := ImageProxy.ImageWidth;
end;

function TEsCustomVirtualImageControl.GetInterpolationMode: TImageInterpolationMode;
begin
  Result := ImageProxy.InterpolationMode;
end;

function TEsCustomVirtualImageControl.GetOpacity: Byte;
begin
  Result := ImageProxy.Opacity;
end;

function TEsCustomVirtualImageControl.GetStretch: TImageStretch;
begin
  Result := ImageProxy.Stretch;
end;

procedure TEsCustomVirtualImageControl.ImageProxyChange(Sender: TObject);
begin
  Invalidate;
end;

function TEsCustomVirtualImageControl.IsImageNameStored: Boolean;
begin
  Result := ImageProxy.ImageName <> '';
end;

procedure TEsCustomVirtualImageControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    Click;
end;

procedure TEsCustomVirtualImageControl.Paint;
begin
  if (csDesigning in ComponentState) and IsDrawHelper and
    (ImageProxy.ImageWidth * ImageProxy.ImageHeight <> 0) then
  begin
    Canvas.Pen.Color := clBlue;
    Canvas.Pen.Style := psSolid;
    Canvas.DrawCorners(ImageProxy.GetRect(ContentRect), 10);
  end;

  if FrameStyle <> TExFrameStyle.None then
    if IsStyledBorderControl(Self) then
      DrawFrame(inherited Canvas, Self, ClientRect, FrameStyle, FrameWidth, FrameColor,
        clBtnHighlight, clBtnShadow)
    else
      DrawFrame(inherited Canvas, nil, ClientRect, FrameStyle, FrameWidth, FrameColor, clBtnHighlight, clBtnShadow);

  ImageProxy.Draw(Canvas, ContentRect);

  if csDesigning in ComponentState then
  begin
    DrawDesignVirtualImageFrame(Canvas, ClientRect, True);
    if IsDrawHelper then
      DrawControlHelper(Canvas, ClientRect, 0, Padding, [hoPadding]);
    DrawDesignStretchHint(Canvas, ClientRect, ImageProxy);
  end;

  if Focused then
  begin
    Canvas.Brush.Color := TColor($FF000000);
    Canvas.DrawFocusRect(ContentRect);
  end;
end;

procedure TEsCustomVirtualImageControl.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImageControl.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImageControl.SetFrameWidth(const Value: TFrameWidth);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TEsCustomVirtualImageControl.SetImageCollection(
  const Value: TCustomImageCollection);
begin
  ImageProxy.ImageCollection := Value;
end;

procedure TEsCustomVirtualImageControl.SetImageHeight(const Value: Integer);
begin
  ImageProxy.ImageHeight := Value;
end;

procedure TEsCustomVirtualImageControl.SetImageIndex(const Value: TImageIndex);
begin
  ImageProxy.ImageIndex := Value;
end;

procedure TEsCustomVirtualImageControl.SetImageName(const Value: TImageName);
begin
  ImageProxy.ImageName := Value;
end;

procedure TEsCustomVirtualImageControl.SetImageWidth(const Value: Integer);
begin
  ImageProxy.ImageWidth := Value;
end;

procedure TEsCustomVirtualImageControl.SetInterpolationMode(
  const Value: TImageInterpolationMode);
begin
  ImageProxy.InterpolationMode := Value;
end;

procedure TEsCustomVirtualImageControl.SetOpacity(const Value: Byte);
begin
  ImageProxy.Opacity := Value;
end;

procedure TEsCustomVirtualImageControl.SetStretch(const Value: TImageStretch);
begin
  ImageProxy.Stretch := Value;
end;

procedure TEsCustomVirtualImageControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TEsCustomVirtualImageControl.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

{$ENDIF}

initialization
  {$IFDEF VER270UP}
  AddEnumElementAliases(TypeInfo(TImageStretch), ['isNone', 'isCenter', 'isFit', 'isFill', 'isUniform' , 'isMixed']);
  {$ENDIF}

finalization
  {$IFDEF VER270UP}
  RemoveEnumElementAliases(TypeInfo(TImageStretch));
  {$ENDIF}

end.
