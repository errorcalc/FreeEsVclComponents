{******************************************************************************}
{                             ESComponents for VCL                             }
{                            ErrorSoft(c) 2012-2015                            }
{                                                                              }
{            errorsoft@mail.ch | errorsoft-demoscene.narod.ru                  }
{ errorsoft@protonmail.ch | github.com/errorcalc | habrahabr.ru/user/error1024 }
{******************************************************************************}
unit ES.Vcl.NinePath;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Imaging.PngImage,
  WinApi.Messages, ES.Vcl.ExGraphics, ES.Vcl.BaseControls, ES.Vcl.Layouts, ES.Vcl.CfxClasses;

type
  TEsNinePathImage = class(TGraphicControl)
  private
    NinePath: TTextNinePathObject;
    FImage: TPngImage;
    FAlpha: byte;
    FOverlay: TPngImage;
    procedure NeedRepaint(Sender: TObject);
    procedure SetImageMargins(const Value: TImageMargins);
    function GetImageMargins: TImageMargins;
    procedure SetImage(const Value: TPngImage);
    procedure PictureChange(Sender: TObject);
    procedure SetAlpha(const Value: byte);
    procedure SetOverlay(const Value: TPngImage);
    procedure SetOverlayAlign(const Value: TImageAlign);
    function GetOverlayAlign: TImageAlign;
    function GetOverlaySpace: Boolean;
    procedure SetOverlaySpace(const Value: Boolean);
    function GetMargins: TImageMargins;
    procedure SetMargins(const Value: TImageMargins);
    // text
    function GetTextAlignment: TAlignment;
    procedure SetTextAlignment(const Value: TAlignment);
    function GetTextLayout: TTextLayout;
    procedure SetTextLayout(const Value: TTextLayout);
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
    procedure UpdateStyleElements; override;
    // Text
    property TextAlignment: TAlignment read GetTextAlignment write SetTextAlignment default taCenter;
    property TextLayout: TTextLayout read GetTextLayout write SetTextLayout default tlCenter;
    property TextDistance: Integer read GetTextDistance write SetTextDistance default 2;
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
    property Alpha: byte read FAlpha write SetAlpha default 255;
    property OverlaySpace: Boolean read GetOverlaySpace write SetOverlaySpace default False;
    property OverlayMargins: TImageMargins read GetMargins write SetMargins;
    //
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

  TEsImageLabel = class(TEsNinePathImage)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OverlayAlign default iaLeft;
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
    property StyleElements;
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
    NinePath: TTextNinePathObject;
    FImage: TPngImage;
    FAlpha: byte;
    FOverlay: TPngImage;
    FAlignWithImageBounds: Boolean;
    procedure SetImageMargins(const Value: TImageMargins);
    function GetImageMargins: TImageMargins;
    procedure SetImage(const Value: TPngImage);
    procedure PictureChange(Sender: TObject);
    procedure SetAlpha(const Value: byte);
    procedure SetOverlay(const Value: TPngImage);
    procedure SetOverlayAlign(const Value: TImageAlign);
    function GetOverlayAlign: TImageAlign;
    procedure SetAlignWithImageBounds(const Value: Boolean);
    procedure SetOverlaySpace(const Value: Boolean);
    function GetOverlaySpace: Boolean;
    function GetMargins: TImageMargins;
    procedure SetMargins(const Value: TImageMargins);
    // Text
    function GetShowCaption: Boolean; 
    procedure SetShowCaption(const Value: Boolean);
    function GetTextDistance: Integer;
    procedure SetTextDistance(const Value: Integer);
    function GetTextAlignment: TAlignment;
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextMultiline(const Value: Boolean);
    function GetTextMultiline: Boolean;
    function GetTextLayout: TTextLayout;
    procedure SetTextLayout(const Value: TTextLayout);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure NeedRepaint(Sender: TObject);
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure UpdateText; override;
    procedure UpdateStyleElements; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    property ImageMargins: TImageMargins read GetImageMargins write SetImageMargins;
    property Image: TPngImage read FImage write SetImage;
    property Overlay: TPngImage read FOverlay write SetOverlay;
    property OverlayAlign: TImageAlign read GetOverlayAlign write SetOverlayAlign default iaTopLeft;
    property OverlaySpace: Boolean read GetOverlaySpace write SetOverlaySpace default False;
    property OverlayMargins: TImageMargins read GetMargins write SetMargins;
    property Alpha: byte read FAlpha write SetAlpha default 255;
    property AlignWithImageBounds: Boolean read FAlignWithImageBounds write SetAlignWithImageBounds default False;
    // Text
    property TextAlignment: TAlignment read GetTextAlignment write SetTextAlignment default taCenter;
    property TextLayout: TTextLayout read GetTextLayout write SetTextLayout default tlCenter;
    property TextDistance: Integer read GetTextDistance write SetTextDistance default 2;
    property TextMultiline: Boolean read GetTextMultiline write SetTextMultiline default False;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption default False;
  end;

  TEsImageLayout = class(TEsCustomImageLayout)
    property ImageMargins;
    property Image;
    property Overlay;
    property OverlayAlign;
    property OverlaySpace;
    property OverlayMargins;
    property Alpha;
    property AlignWithImageBounds;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
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
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
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
    property StyleElements;
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
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TEsLabelLayout = class(TEsImageLayout)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OverlayAlign default iaLeft;
    property OverlaySpace default True;
    property TextAlignment;
    property TextLayout;
    property TextDistance;
    property TextMultiline;
    property ShowCaption default True;
    property Caption;
  end;
  
procedure Register;

implementation

uses
  Vcl.Themes;

procedure Register;
begin
  RegisterComponents('ErrorSoft', [TEsNinePathImage, TEsImageLabel, TEsImageLayout, TEsLabelLayout]);
end;

{ TEsNinePathImage }

procedure TEsNinePathImage.CMControlChange(var Message: TMessage);
begin
  Inherited;
  //NinePath.Control := TControl(Message.WParam);
end;

procedure TEsNinePathImage.CMEnabledChanged(var Message: TMessage);
begin
  Inherited;
  Invalidate;
end;

procedure TEsNinePathImage.CMTextChanged(var Message: TMessage);
begin
  Inherited;
  Invalidate;
end;

constructor TEsNinePathImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NinePath := TTextNinePathObject.Create;
  NinePath.OnNeedRepaint := NeedRepaint;
  NinePath.OverlayAlign := iaTopLeft;
  NinePath.OverlaySpace := False;
  NinePath.Control := Self;
  FImage := TPngImage.Create;
  FImage.OnChange := PictureChange;
  FOverlay := TPngImage.Create;
  FOverlay.OnChange := PictureChange;
  FAlpha := 255;
end;

destructor TEsNinePathImage.Destroy;
begin
  FImage.Free;
  FOverlay.Free;
  NinePath.Free;
  inherited;
end;

function TEsNinePathImage.GetImageMargins: TImageMargins;
begin
  Result := NinePath.Margins;
end;

function TEsNinePathImage.GetMargins: TImageMargins;
begin
  Result := NinePath.OverlayMargins;
end;

function TEsNinePathImage.GetOverlayAlign: TImageAlign;
begin
  Result := NinePath.OverlayAlign;
end;

function TEsNinePathImage.GetOverlaySpace: Boolean;
begin
  Result := NinePath.OverlaySpace;
end;

function TEsNinePathImage.GetShowCaption: Boolean;
begin
  Result := NinePath.ShowCaption;
end;

function TEsNinePathImage.GetTextAlignment: TAlignment;
begin
  Result := NinePath.TextAlignment;
end;

function TEsNinePathImage.GetTextDistance: Integer;
begin
  Result := NinePath.TextDistance;
end;

function TEsNinePathImage.GetTextLayout: TTextLayout;
begin
  Result := NinePath.TextLayout;
end;

function TEsNinePathImage.GetTextMultiline: Boolean;
begin
  Result := NinePath.TextMultiline;
end;

procedure TEsNinePathImage.Loaded;
begin
  inherited;
  if FImage <> nil then
    NinePath.AssignImage(FImage);
  if FOverlay <> nil then
    NinePath.AssignOverlay(FOverlay);
end;

procedure TEsNinePathImage.NeedRepaint(Sender: TObject);
begin
  Invalidate;
end;

procedure TEsNinePathImage.Paint;
begin
  Canvas.Font := Font;
  NinePath.Draw(Canvas, Rect(0, 0, Width, Height), Caption, FAlpha);
  inherited;
  inherited;
end;

procedure TEsNinePathImage.PictureChange(Sender: TObject);
begin
  NinePath.AssignImage(FImage);
end;

procedure TEsNinePathImage.SetAlpha(const Value: byte);
begin
  if Value <> FAlpha then
  begin
    FAlpha := Value;
    Invalidate;
  end;
end;

procedure TEsNinePathImage.SetImage(const Value: TPngImage);
begin
  FImage.Assign(Value);
  NinePath.AssignImage(FImage);
end;

procedure TEsNinePathImage.SetImageMargins(const Value: TImageMargins);
begin
  NinePath.Margins := Value;
end;

procedure TEsNinePathImage.SetMargins(const Value: TImageMargins);
begin
  NinePath.OverlayMargins := Value;
end;

procedure TEsNinePathImage.SetOverlay(const Value: TPngImage);
begin
  FOverlay.Assign(Value);
  NinePath.AssignOverlay(FOverlay);
end;

procedure TEsNinePathImage.SetOverlayAlign(const Value: TImageAlign);
begin
  NinePath.OverlayAlign := Value;
end;

procedure TEsNinePathImage.SetOverlaySpace(const Value: Boolean);
begin
  if NinePath.OverlaySpace <> Value then
  begin
    NinePath.OverlaySpace := Value;
  end;
end;

procedure TEsNinePathImage.SetShowCaption(const Value: Boolean);
begin
  NinePath.ShowCaption := Value;
end;

procedure TEsNinePathImage.SetTextAlignment(const Value: TAlignment);
begin
  NinePath.TextAlignment := Value;
end;

procedure TEsNinePathImage.SetTextDistance(const Value: Integer);
begin
  NinePath.TextDistance := Value;
end;

procedure TEsNinePathImage.SetTextLayout(const Value: TTextLayout);
begin
  NinePath.TextLayout := Value;
end;

procedure TEsNinePathImage.SetTextMultiline(const Value: Boolean);
begin
  NinePath.TextMultiline := Value;
end;

procedure TEsNinePathImage.UpdateStyleElements;
begin
  inherited;
  Invalidate;
end;

{ TEsCustomNinePathLayout }

procedure TEsCustomImageLayout.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if FAlignWithImageBounds then
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
  if NinePath.ShowCaption then
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
  //DoubleBuffered:= true;
  NinePath := TTextNinePathObject.Create;
  NinePath.OnNeedRepaint := NeedRepaint;
  FImage := TPngImage.Create;
  FImage.OnChange := PictureChange;
  FOverlay := TPngImage.Create;
  FOverlay.OnChange := PictureChange;
  NinePath.OverlayAlign := iaTopLeft;
  NinePath.OverlaySpace := False;
  FAlpha := 255;
end;

procedure TEsCustomImageLayout.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

destructor TEsCustomImageLayout.Destroy;
begin
  FImage.Free;
  FOverlay.Free;
  NinePath.Free;
  inherited;
end;

function TEsCustomImageLayout.GetImageMargins: TImageMargins;
begin
  Result := NinePath.Margins;
end;

function TEsCustomImageLayout.GetMargins: TImageMargins;
begin
  Result := NinePath.OverlayMargins;
end;

function TEsCustomImageLayout.GetOverlayAlign: TImageAlign;
begin
  Result := NinePath.OverlayAlign;
end;

function TEsCustomImageLayout.GetOverlaySpace: Boolean;
begin
  Result := NinePath.OverlaySpace;
end;

function TEsCustomImageLayout.GetShowCaption: Boolean;
begin
  Result := NinePath.ShowCaption;
end;

function TEsCustomImageLayout.GetTextAlignment: TAlignment;
begin
  Result := NinePath.TextAlignment;
end;

function TEsCustomImageLayout.GetTextDistance: Integer;
begin
  Result := NinePath.TextDistance;
end;

function TEsCustomImageLayout.GetTextLayout: TTextLayout;
begin
  Result := NinePath.TextLayout;
end;

function TEsCustomImageLayout.GetTextMultiline: Boolean;
begin
  Result := NinePath.TextMultiline;
end;

procedure TEsCustomImageLayout.Loaded;
begin
  inherited;
  if FImage <> nil then
    NinePath.AssignImage(FImage);
  if FOverlay <> nil then
    NinePath.AssignOverlay(FOverlay);
end;

procedure TEsCustomImageLayout.NeedRepaint(Sender: TObject);
begin
  Invalidate;
end;

procedure TEsCustomImageLayout.Paint;
begin
  Canvas.Font := Font;
  NinePath.Draw(Canvas, Rect(0, 0, Width, Height), Caption, FAlpha);
  inherited;
end;

procedure TEsCustomImageLayout.PictureChange(Sender: TObject);
begin
  NinePath.AssignImage(FImage);
end;

procedure TEsCustomImageLayout.SetTextAlignment(const Value: TAlignment);
begin
  NinePath.TextAlignment := Value;
end;

procedure TEsCustomImageLayout.SetTextDistance(const Value: Integer);
begin
  NinePath.TextDistance := Value;
end;

procedure TEsCustomImageLayout.SetTextLayout(const Value: TTextLayout);
begin
  NinePath.TextLayout := Value;
end;

procedure TEsCustomImageLayout.SetTextMultiline(const Value: Boolean);
begin
  NinePath.TextMultiline := Value;
end;

procedure TEsCustomImageLayout.SetAlpha(const Value: byte);
begin
  if Value <> FAlpha then
  begin
    FAlpha := Value;
    Invalidate;
  end;
end;

procedure TEsCustomImageLayout.SetImage(const Value: TPngImage);
begin
  FImage.Assign(Value);
  NinePath.AssignImage(FImage);
end;

procedure TEsCustomImageLayout.SetImageMargins(const Value: TImageMargins);
begin
  NinePath.Margins := Value;
  if FAlignWithImageBounds then Realign;
end;

procedure TEsCustomImageLayout.SetMargins(const Value: TImageMargins);
begin
  NinePath.OverlayMargins := Value;
end;

procedure TEsCustomImageLayout.SetOverlay(const Value: TPngImage);
begin
  FOverlay.Assign(Value);
  NinePath.AssignOverlay(FOverlay);
end;

procedure TEsCustomImageLayout.SetOverlayAlign(const Value: TImageAlign);
begin
  NinePath.OverlayAlign := Value;
end;

procedure TEsCustomImageLayout.SetShowCaption(const Value: Boolean);
begin
  NinePath.ShowCaption := Value;
end;

procedure TEsCustomImageLayout.SetAlignWithImageBounds(const Value: Boolean);
begin
  if FAlignWithImageBounds <> Value then
  begin
    FAlignWithImageBounds := Value;
    Realign;
  end;
end;

procedure TEsCustomImageLayout.SetOverlaySpace(const Value: Boolean);
begin
  if NinePath.OverlaySpace <> Value then
  begin
    NinePath.OverlaySpace := Value;
  end;
end;

procedure TEsCustomImageLayout.UpdateStyleElements;
begin
  Invalidate;
end;

procedure TEsCustomImageLayout.UpdateText;
begin
  inherited;
  Invalidate;
end;

{ TEsTextImageLayout }

constructor TEsLabelLayout.Create(AOwner: TComponent);
begin
  inherited;
  OverlayAlign := iaLeft;
  OverlaySpace := True;
  ShowCaption := True;
  NinePath.Control := Self;
end;

{ TEsImageLabel }

constructor TEsImageLabel.Create(AOwner: TComponent);
begin
  inherited;
  OverlayAlign := iaLeft;
  OverlaySpace := True;
  ShowCaption := True;
end;

end.
