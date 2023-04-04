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
unit ES.NinePatch;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Imaging.PngImage,
  WinApi.Messages, ES.ExGraphics, ES.BaseControls, ES.Layouts, ES.CfxClasses;

type
  TEsNinePatchImage = class(TEsGraphicControl)
  private
    NinePatch: TTextNinePatchObject;
    FImage: TPngImage;
    FOpacity: Byte;
    FOverlay: TPngImage;
    procedure NeedRepaint(Sender: TObject);
    procedure SetImageMargins(const Value: TImageMargins);
    function GetImageMargins: TImageMargins;
    procedure SetImage(const Value: TPngImage);
    procedure PictureChange(Sender: TObject);
    procedure SetOpacity(const Value: byte);
    procedure SetOverlay(const Value: TPngImage);
    procedure SetOverlayAlign(const Value: TImageAlign);
    function GetOverlayAlign: TImageAlign;
    function GetOverlaySpace: Boolean;
    procedure SetOverlaySpace(const Value: Boolean);
    function GetOverlayMargins: TImageMargins;
    procedure SetOverlayMargins(const Value: TImageMargins);
    // text
    function GetTextAlignment: TAlignment;
    procedure SetTextAlignment(const Value: TAlignment);
    function GetTextLayout: TVertLayout;
    procedure SetTextLayout(const Value: TVertLayout);
    function GetTextDistance: Integer;
    procedure SetTextDistance(const Value: Integer);
    function GetTextMultiline: Boolean;
    procedure SetTextMultiline(const Value: Boolean);
    function GetShowCaption: Boolean;
    procedure SetShowCaption(const Value: Boolean);
    //
    procedure CMControlChange(var Message: TMessage); message CM_CONTROLCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    {$IFDEF VER240UP}
    procedure UpdateStyleElements; override;
    {$ENDIF}
    // Text
    property TextAlignment: TAlignment read GetTextAlignment write SetTextAlignment default taCenter;
    property TextLayout: TVertLayout read GetTextLayout write SetTextLayout default TVertLayout.Center;
    property TextDistance: Integer read GetTextDistance write SetTextDistance default 0;
    property TextMultiline: Boolean read GetTextMultiline write SetTextMultiline default False;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ImageMargins: TImageMargins read GetImageMargins write SetImageMargins;
    property Image: TPngImage read FImage write SetImage;
    property Overlay: TPngImage read FOverlay write SetOverlay;
    property OverlayAlign: TImageAlign read GetOverlayAlign write SetOverlayAlign;
    property Opacity: byte read FOpacity write SetOpacity default 255;
    property OverlaySpace: Boolean read GetOverlaySpace write SetOverlaySpace default False;
    property OverlayMargins: TImageMargins read GetOverlayMargins write SetOverlayMargins;
    //
    property IsDrawHelper;
    //
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Touch;
    property Visible;
    property DragCursor;
    property DragKind;
    property DragMode;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
  end;

  TEsImageLabel = class(TEsNinePatchImage)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OverlayAlign default TImageAlign.Left;
    property OverlaySpace default True;
    property TextAlignment;
    property TextLayout;
    property TextDistance;
    property TextMultiline;
    property ShowCaption default True;
    property Caption;
    //
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentBiDiMode;
    property ParentFont;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
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
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

  TEsCustomImageLayout = class(TEsCustomLayout)
  private
    NinePatch: TTextNinePatchObject;
    FImage: TPngImage;
    FOpacity: Byte;
    FOverlay: TPngImage;
    FPaddingWithImage: Boolean;
    FPadding: TPadding;
    procedure SetImageMargins(const Value: TImageMargins);
    function GetImageMargins: TImageMargins;
    procedure SetImage(const Value: TPngImage);
    procedure PictureChange(Sender: TObject);
    procedure SetOpacity(const Value: byte);
    procedure SetOverlay(const Value: TPngImage);
    procedure SetOverlayAlign(const Value: TImageAlign);
    function GetOverlayAlign: TImageAlign;
    procedure SetOverlaySpace(const Value: Boolean);
    function GetOverlaySpace: Boolean;
    function GetOverlayMargins: TImageMargins;
    procedure SetOverlayMargins(const Value: TImageMargins);
    // Text
    function GetShowCaption: Boolean; 
    procedure SetShowCaption(const Value: Boolean);
    function GetTextDistance: Integer;
    procedure SetTextDistance(const Value: Integer);
    function GetTextAlignment: TAlignment;
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextMultiline(const Value: Boolean);
    function GetTextMultiline: Boolean;
    function GetTextLayout: TVertLayout;
    procedure SetTextLayout(const Value: TVertLayout);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    // Padding
    procedure SetPaddingWithImage(const Value: Boolean);
    procedure SetPadding(const Value: TPadding);
    function IsPaddingStored: Boolean;
    function GetPadding: TPadding;
    procedure PaddingChange(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure NeedRepaint(Sender: TObject);
    procedure ImageMarginsChange(Sender: TObject);
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure UpdateText; override;
    {$IFDEF VER240UP}
    procedure UpdateStyleElements; override;
    {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    property PaddingWithImage: Boolean read FPaddingWithImage write SetPaddingWithImage default False;
    property ImageMargins: TImageMargins read GetImageMargins write SetImageMargins;
    property Image: TPngImage read FImage write SetImage;
    property Overlay: TPngImage read FOverlay write SetOverlay;
    property OverlayAlign: TImageAlign read GetOverlayAlign write SetOverlayAlign default TImageAlign.TopLeft;
    property OverlaySpace: Boolean read GetOverlaySpace write SetOverlaySpace default False;
    property OverlayMargins: TImageMargins read GetOverlayMargins write SetOverlayMargins;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Padding: TPadding read GetPadding write SetPadding stored IsPaddingStored;
    // Text
    property TextAlignment: TAlignment read GetTextAlignment write SetTextAlignment default taCenter;
    property TextLayout: TVertLayout read GetTextLayout write SetTextLayout default TVertLayout.Center;
    property TextDistance: Integer read GetTextDistance write SetTextDistance default 0;
    property TextMultiline: Boolean read GetTextMultiline write SetTextMultiline default False;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption default False;
  end;

  TEsImageLayout = class(TEsCustomImageLayout)
  published
    property ImageMargins;
    property Image;
    property Overlay;
    property OverlayAlign;
    property OverlaySpace;
    property OverlayMargins;
    property Opacity;
    // property AlignWithImageBounds;
    property PaddingWithImage;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BufferedChildren;// TEsCustomControl
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property IsCachedBuffer;// TEsCustomControl
    property IsCachedBackground;// TEsCustomControl
    property IsDrawHelper;// TEsCustomControl
    property IsOpaque;// TEsCustomControl
    property IsFullSizeBuffer;// TEsCustomControl
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentBufferedChildren;// TEsCustomControl
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    {$IFDEF VER240UP}
    property StyleElements;
    {$ENDIF}
    {$IFDEF VER340UP}
    property StyleName;
    {$ENDIF}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnPaint;// TEsCustomControl
    property OnPainting;// TEsCustomControl
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TEsImageStaticText = class(TEsImageLayout)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OverlayAlign default TImageAlign.Left;
    property OverlaySpace default True;
    property TextAlignment;
    property TextLayout;
    property TextDistance;
    property TextMultiline;
    property ShowCaption default True;
    property Caption;
  end;

implementation

uses
  Vcl.Themes;

procedure DrawHelper(Canvas: TCanvas; Width, Height: Integer);
var
  Saver: ICanvasSaver;
begin
  Saver := Canvas.SaveState([TPen]);
  try
    Canvas.Pen.Mode := pmNot;
    Canvas.DrawCorners(Rect(0, 0, Width, Height), 10);
  finally
    Canvas.RestoreState(Saver);
  end;
end;

{ TEsNinePatchImage }

procedure TEsNinePatchImage.CMControlChange(var Message: TMessage);
begin
  Inherited;
end;

procedure TEsNinePatchImage.CMEnabledChanged(var Message: TMessage);
begin
  Inherited;
  Invalidate;
end;

procedure TEsNinePatchImage.CMTextChanged(var Message: TMessage);
begin
  Inherited;
  Invalidate;
end;

constructor TEsNinePatchImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NinePatch := TTextNinePatchObject.Create;
  NinePatch.OnNeedRepaint := NeedRepaint;
  NinePatch.OverlayAlign := TImageAlign.TopLeft;
  NinePatch.OverlaySpace := False;
  NinePatch.Control := Self;
  FImage := TFixPngImage.Create;
  FImage.OnChange := PictureChange;
  FOverlay := TFixPngImage.Create;
  FOverlay.OnChange := PictureChange;
  FOpacity := 255;
end;

destructor TEsNinePatchImage.Destroy;
begin
  FImage.Free;
  FOverlay.Free;
  NinePatch.Free;
  inherited;
end;

function TEsNinePatchImage.GetImageMargins: TImageMargins;
begin
  Result := NinePatch.Margins;
end;

function TEsNinePatchImage.GetOverlayMargins: TImageMargins;
begin
  Result := NinePatch.OverlayMargins;
end;

function TEsNinePatchImage.GetOverlayAlign: TImageAlign;
begin
  Result := NinePatch.OverlayAlign;
end;

function TEsNinePatchImage.GetOverlaySpace: Boolean;
begin
  Result := NinePatch.OverlaySpace;
end;

function TEsNinePatchImage.GetShowCaption: Boolean;
begin
  Result := NinePatch.ShowCaption;
end;

function TEsNinePatchImage.GetTextAlignment: TAlignment;
begin
  Result := NinePatch.TextAlignment;
end;

function TEsNinePatchImage.GetTextDistance: Integer;
begin
  Result := NinePatch.TextDistance;
end;

function TEsNinePatchImage.GetTextLayout: TVertLayout;
begin
  Result := NinePatch.TextLayout;
end;

function TEsNinePatchImage.GetTextMultiline: Boolean;
begin
  Result := NinePatch.TextMultiline;
end;

procedure TEsNinePatchImage.Loaded;
begin
  inherited;
  if FImage <> nil then
    NinePatch.AssignImage(FImage);
  if FOverlay <> nil then
    NinePatch.AssignOverlay(FOverlay);
end;

procedure TEsNinePatchImage.NeedRepaint(Sender: TObject);
begin
  Invalidate;
end;

procedure TEsNinePatchImage.Paint;
begin
  Canvas.Font := Font;
  NinePatch.Draw(Canvas, Rect(0, 0, Width, Height), Caption, FOpacity);

  if (csDesigning in ComponentState) and not IsDrawHelper then
    DrawHelper(Canvas, Width, Height);

  inherited;
end;

procedure TEsNinePatchImage.PictureChange(Sender: TObject);
begin
  NinePatch.AssignImage(FImage);
  NinePatch.AssignOverlay(FOverlay);
end;

procedure TEsNinePatchImage.SetOpacity(const Value: byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Invalidate;
  end;
end;

procedure TEsNinePatchImage.SetImage(const Value: TPngImage);
begin
  FImage.Assign(Value);
  NinePatch.AssignImage(FImage);
end;

procedure TEsNinePatchImage.SetImageMargins(const Value: TImageMargins);
begin
  NinePatch.Margins := Value;
end;

procedure TEsNinePatchImage.SetOverlayMargins(const Value: TImageMargins);
begin
  NinePatch.OverlayMargins := Value;
end;

procedure TEsNinePatchImage.SetOverlay(const Value: TPngImage);
begin
  FOverlay.Assign(Value);
  NinePatch.AssignOverlay(FOverlay);
end;

procedure TEsNinePatchImage.SetOverlayAlign(const Value: TImageAlign);
begin
  NinePatch.OverlayAlign := Value;
end;

procedure TEsNinePatchImage.SetOverlaySpace(const Value: Boolean);
begin
  if NinePatch.OverlaySpace <> Value then
  begin
    NinePatch.OverlaySpace := Value;
  end;
end;

procedure TEsNinePatchImage.SetShowCaption(const Value: Boolean);
begin
  NinePatch.ShowCaption := Value;
end;

procedure TEsNinePatchImage.SetTextAlignment(const Value: TAlignment);
begin
  NinePatch.TextAlignment := Value;
end;

procedure TEsNinePatchImage.SetTextDistance(const Value: Integer);
begin
  NinePatch.TextDistance := Value;
end;

procedure TEsNinePatchImage.SetTextLayout(const Value: TVertLayout);
begin
  NinePatch.TextLayout := Value;
end;

procedure TEsNinePatchImage.SetTextMultiline(const Value: Boolean);
begin
  NinePatch.TextMultiline := Value;
end;

{$IFDEF VER240UP}
procedure TEsNinePatchImage.UpdateStyleElements;
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

{ TEsCustomNinePathLayout }

procedure TEsCustomImageLayout.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if FPaddingWithImage then
  begin
    Rect.Left := Rect.Left + ImageMargins.Left;
    Rect.Right := Rect.Right - ImageMargins.Right;
    Rect.Top := Rect.Top + ImageMargins.Top;
    Rect.Bottom := Rect.Bottom - ImageMargins.Bottom;
  end;
end;

procedure TEsCustomImageLayout.CMEnabledChanged(var Message: TMessage);
begin
  Inherited;
  if NinePatch.ShowCaption then
    Invalidate;
end;

constructor TEsCustomImageLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [{csParentBackground,} csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csPannable, csGestures];

  Color := clBtnFace;

  ParentBackground := True;
  ParentColor := True;
  NinePatch := TTextNinePatchObject.Create;
  NinePatch.OnNeedRepaint := NeedRepaint;
  FImage := TPngImage.Create;
  FImage.OnChange := PictureChange;
  FOverlay := TPngImage.Create;
  FOverlay.OnChange := PictureChange;
  NinePatch.OverlayAlign := TImageAlign.TopLeft;
  NinePatch.OverlaySpace := False;
  FOpacity := 255;
end;

procedure TEsCustomImageLayout.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

destructor TEsCustomImageLayout.Destroy;
begin
  FImage.Free;
  FOverlay.Free;
  NinePatch.Free;
  FPadding.Free;
  inherited;
end;

function TEsCustomImageLayout.GetImageMargins: TImageMargins;
begin
  Result := NinePatch.Margins;
end;

function TEsCustomImageLayout.GetOverlayMargins: TImageMargins;
begin
  Result := NinePatch.OverlayMargins;
end;

function TEsCustomImageLayout.GetOverlayAlign: TImageAlign;
begin
  Result := NinePatch.OverlayAlign;
end;

function TEsCustomImageLayout.GetOverlaySpace: Boolean;
begin
  Result := NinePatch.OverlaySpace;
end;

function TEsCustomImageLayout.GetPadding: TPadding;
begin
  if FPaddingWithImage then
  begin
    Result := FPadding;
  end
  else
    Result := Inherited Padding;
end;

function TEsCustomImageLayout.GetShowCaption: Boolean;
begin
  Result := NinePatch.ShowCaption;
end;

function TEsCustomImageLayout.GetTextAlignment: TAlignment;
begin
  Result := NinePatch.TextAlignment;
end;

function TEsCustomImageLayout.GetTextDistance: Integer;
begin
  Result := NinePatch.TextDistance;
end;

function TEsCustomImageLayout.GetTextLayout: TVertLayout;
begin
  Result := NinePatch.TextLayout;
end;

function TEsCustomImageLayout.GetTextMultiline: Boolean;
begin
  Result := NinePatch.TextMultiline;
end;

procedure TEsCustomImageLayout.Loaded;
begin
  inherited;
  if FImage <> nil then
    NinePatch.AssignImage(FImage);
  if FOverlay <> nil then
    NinePatch.AssignOverlay(FOverlay);
end;

procedure TEsCustomImageLayout.NeedRepaint(Sender: TObject);
begin
  if FPaddingWithImage then
    if (Padding.Left <> NinePatch.Margins.Left)or(Padding.Right <> NinePatch.Margins.Right)or
      (Padding.Top <> NinePatch.Margins.Top)or(Padding.Bottom <> NinePatch.Margins.Bottom) then
      Padding := TPadding(NinePatch.Margins);
  Invalidate;
end;

procedure TEsCustomImageLayout.ImageMarginsChange(Sender: TObject);
begin
  if FPaddingWithImage then
    Inherited Padding := TPadding(ImageMargins);
end;

function TEsCustomImageLayout.IsPaddingStored: Boolean;
begin
  Result := not FPaddingWithImage;
end;

procedure TEsCustomImageLayout.PaddingChange(Sender: TObject);
begin
  Inherited Padding.Assign(TPadding(Sender));
  NinePatch.Margins.SetBounds(TPadding(Sender).Left, TPadding(Sender).Top,
    TPadding(Sender).Right, TPadding(Sender).Bottom);
end;

procedure TEsCustomImageLayout.Paint;
begin
  Canvas.Font := Font;
  NinePatch.Draw(Canvas, Rect(0, 0, Width, Height), Caption, FOpacity);

  if (csDesigning in ComponentState) and not IsDrawHelper then
    DrawHelper(Canvas, Width, Height);

  inherited;
end;

procedure TEsCustomImageLayout.PictureChange(Sender: TObject);
begin
  NinePatch.AssignImage(FImage);
end;

procedure TEsCustomImageLayout.SetTextAlignment(const Value: TAlignment);
begin
  NinePatch.TextAlignment := Value;
end;

procedure TEsCustomImageLayout.SetTextDistance(const Value: Integer);
begin
  NinePatch.TextDistance := Value;
end;

procedure TEsCustomImageLayout.SetTextLayout(const Value: TVertLayout);
begin
  NinePatch.TextLayout := Value;
end;

procedure TEsCustomImageLayout.SetTextMultiline(const Value: Boolean);
begin
  NinePatch.TextMultiline := Value;
end;

procedure TEsCustomImageLayout.SetOpacity(const Value: byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Invalidate;
  end;
end;

procedure TEsCustomImageLayout.SetImage(const Value: TPngImage);
begin
  FImage.Assign(Value);
  NinePatch.AssignImage(FImage);
end;

procedure TEsCustomImageLayout.SetImageMargins(const Value: TImageMargins);
begin
  NinePatch.Margins := Value;
  if FPaddingWithImage then
    Inherited Padding := TPadding(Value);
end;

procedure TEsCustomImageLayout.SetPaddingWithImage(const Value: Boolean);
begin
  if FPaddingWithImage <> Value then
  begin
    FPaddingWithImage := Value;

    // internal padding
    if Value then
    begin
      FPadding := TPadding.Create(nil);
      FPadding.OnChange := PaddingChange;
      FPadding.SetBounds(ImageMargins.Left, ImageMargins.Top, ImageMargins.Right, ImageMargins.Bottom);
    end else
      FreeAndNil(FPadding);
  end;
end;

procedure TEsCustomImageLayout.SetOverlayMargins(const Value: TImageMargins);
begin
  NinePatch.OverlayMargins := Value;
end;

procedure TEsCustomImageLayout.SetOverlay(const Value: TPngImage);
begin
  FOverlay.Assign(Value);
  NinePatch.AssignOverlay(FOverlay);
end;

procedure TEsCustomImageLayout.SetOverlayAlign(const Value: TImageAlign);
begin
  NinePatch.OverlayAlign := Value;
end;

procedure TEsCustomImageLayout.SetShowCaption(const Value: Boolean);
begin
  NinePatch.ShowCaption := Value;
end;

procedure TEsCustomImageLayout.SetOverlaySpace(const Value: Boolean);
begin
  if NinePatch.OverlaySpace <> Value then
  begin
    NinePatch.OverlaySpace := Value;
  end;
end;

procedure TEsCustomImageLayout.SetPadding(const Value: TPadding);
begin
  if FPaddingWithImage then
  begin
    FPadding.Assign(Value);
  end
  else
    Inherited Padding := Value;
end;

{$IFDEF VER240UP}
procedure TEsCustomImageLayout.UpdateStyleElements;
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

procedure TEsCustomImageLayout.UpdateText;
begin
  inherited;
  Invalidate;
end;

{ TEsImageStatic }

constructor TEsImageStaticText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption] - [csAcceptsControls];
  OverlayAlign := TImageAlign.Left;
  OverlaySpace := True;
  ShowCaption := True;
  NinePatch.Control := Self;
end;

{ TEsImageLabel }

constructor TEsImageLabel.Create(AOwner: TComponent);
begin
  inherited;
  OverlayAlign := TImageAlign.Left;
  OverlaySpace := True;
  ShowCaption := True;
end;

end.
