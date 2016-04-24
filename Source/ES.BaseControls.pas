{******************************************************************************}
{                          FreeEsVclComponents/Core                            }
{                           ErrorSoft(c) 2011-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{     errorsoft@protonmail.ch | habrahabr.ru/user/error1024 | errorsoft.org    }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{                                                                              }
{ Вы можете заказать разработку VCL/FMX компонента на заказ                    }
{ You can order the development of VCL/FMX components to order                 }
{******************************************************************************}
{
  This is the base unit, which must remain Delphi 7 support, and it should not
  be dependent on any other units!
}

unit Es.BaseControls;

{$IF CompilerVersion >= 18} {$DEFINE VER180UP} {$IFEND}
{$IF CompilerVersion >= 21} {$DEFINE VER210UP} {$IFEND}
{$IF CompilerVersion >= 23} {$DEFINE VER230UP} {$IFEND}
{$IF CompilerVersion >= 24} {$DEFINE VER240UP} {$IFEND}

// see function CalcClientRect
{$define FAST_CALC_CLIENTRECT}

// see TEsBaseLayout.ContentRect
{$define TEST_CONTROL_CONTENT_RECT}

interface

uses
  Windows, Types, Classes, Controls,
  Graphics, {$IFDEF VER230UP}Themes,{$ENDIF} Messages, Uxtheme, Forms;

const
  CM_ESBASE = CM_BASE + $0800;
  CM_PARENT_BUFFEREDCHILDRENS_CHANGED = CM_ESBASE + 1;

  EsVclCoreVersion = 1.0;

type
  THelperOption = (hoPadding, hoBorder, hoClientRect);
  THelperOptions = set of THelperOption;

  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;

  /// <summary> The best replacement for TCustomControl, supports transparency and without flicker </summary>
  TEsCustomControl = class(TWinControl)
  private
    // anti flicker and transparent magic
    FCanvas: TCanvas;
    CacheBitmap: HBITMAP;// Cache for buffer BitMap
    CacheBackground: HBITMAP;// Cache for background BitMap
    FIsCachedBuffer: Boolean;
    FIsCachedBackground: Boolean;
    StoredCachedBuffer: Boolean;
    StoredCachedBackground: Boolean;
    FBufferedChildrens: Boolean;
    FParentBufferedChildrens: Boolean;
    FIsFullSizeBuffer: Boolean;
    // paint events
    FOnPaint: TPaintEvent;
    FOnPainting: TPaintEvent;
    // draw helper
    FIsDrawHelper: Boolean;
    // transparent mouse
    // FIsTransparentMouse: Boolean;
    // paint
    procedure SetIsCachedBuffer(Value: Boolean);
    procedure SetIsCachedBackground(Value: Boolean);
    procedure SetIsDrawHelper(const Value: Boolean);
    procedure SetIsOpaque(const Value: Boolean);
    function GetIsOpaque: Boolean;
    procedure SetBufferedChildrens(const Value: Boolean);
    procedure SetParentBufferedChildrens(const Value: Boolean);
    function GetTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
    function IsBufferedChildrensStored: Boolean;
    // handle messages
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMParentBufferedChildrensChanged(var Message: TMessage); message CM_PARENT_BUFFEREDCHILDRENS_CHANGED;
    procedure DrawBackgroundForOpaqueControls(DC: HDC);
    // intercept mouse
    // procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    // other
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMTextChanges(var Message: TMessage); message WM_SETTEXT;
  protected
    // paint
    property Canvas: TCanvas read FCanvas;
    procedure DeleteCache;{$IFDEF VER210UP}inline;{$ENDIF}
    procedure Paint; virtual;
    procedure BeginCachedBuffer;{$IFDEF VER210UP}inline;{$ENDIF}
    procedure EndCachedBuffer;{$IFDEF VER210UP}inline;{$ENDIF}
    procedure BeginCachedBackground;{$IFDEF VER210UP}inline;{$ENDIF}
    procedure EndCachedBackground;{$IFDEF VER210UP}inline;{$ENDIF}
    procedure PaintWindow(DC: HDC); override;
    procedure PaintHandler(var Message: TWMPaint);
    // other
    procedure UpdateText; dynamic;
    //
    property ParentBackground default True;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;// analog of ParentBackground
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintTo(DC: HDC; X, Y: Integer);
    procedure UpdateBackground(Repaint: Boolean); overload;
    procedure UpdateBackground; overload;
    // ------------------ Properties for published -------------------------------------------------
    property DoubleBuffered default False;
    {$IFDEF VER210UP}
    property ParentDoubleBuffered default False;
    {$ENDIF}
    // Painting for chidrens classes
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnPainting: TPaintEvent read FOnPainting write FOnPainting;
    // BufferedChildrens
    property ParentBufferedChildrens: Boolean read FParentBufferedChildrens write SetParentBufferedChildrens default True;
    property BufferedChildrens: Boolean read FBufferedChildrens write SetBufferedChildrens stored IsBufferedChildrensStored;
    // External prop
    property IsCachedBuffer: Boolean read FIsCachedBuffer write SetIsCachedBuffer default False;
    property IsCachedBackground: Boolean read FIsCachedBackground write SetIsCachedBackground default False;
    property IsDrawHelper: Boolean read FIsDrawHelper write SetIsDrawHelper default False;
    property IsOpaque: Boolean read GetIsOpaque write SetIsOpaque default False;
    // property IsTransparentMouse: Boolean read FIsTransparentMouse write FIsTransparentMouse default False;
    property IsFullSizeBuffer: Boolean read FIsFullSizeBuffer write FIsFullSizeBuffer default False;
  end;

  {$IFDEF VER180UP}
  TContentMargins = record
  type
    TMarginSize = 0..MaxInt;
  private
    Left: TMarginSize;
    Top: TMarginSize;
    Right: TMarginSize;
    Bottom: TMarginSize;
  public
    function Width: TMarginSize;
    function Height: TMarginSize;
    procedure Inflate(DX, DY: Integer); overload;
    procedure Inflate(DLeft, DTop, DRight, DBottom: Integer); overload;
    procedure Reset;
    constructor Create(Left, Top, Right, Bottom: TMarginSize); overload;
  end;

  /// <summary> ONLY INTERNAL USE! THIS CLASS CAN BE DELETED! (USE TEsCustomControl OR TEsCustomLayot) </summary>
  TEsBaseLayout = class(TEsCustomControl)
  private
    FBorderWidth: TBorderWidth;
    procedure SetBorderWidth(const Value: TBorderWidth);
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    // new
    procedure CalcContentMargins(var Margins: TContentMargins); virtual;
  public
    function ContentRect: TRect; virtual;
    function ContentMargins: TContentMargins; inline;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
  end;

  /// <summary> The GraphicControl, supports Padding and IsDrawHelper property </summary>
  TEsGraphicControl = class(TGraphicControl)
  private
    FPadding: TPadding;
    FIsDrawHelper: Boolean;
    function GetPadding: TPadding;
    procedure SetPadding(const Value: TPadding);
    procedure PaddingChange(Sender: TObject);
    procedure SetIsDrawHelper(const Value: Boolean);
  protected
    procedure Paint; override;
    function HasPadding: Boolean;
    // new
    procedure CalcContentMargins(var Margins: TContentMargins); virtual;
  public
    destructor Destroy; override;
    property Padding: TPadding read GetPadding write SetPadding;
    function ContentRect: TRect; virtual;
    function ContentMargins: TContentMargins; inline;
    property IsDrawHelper: Boolean read FIsDrawHelper write SetIsDrawHelper default False;
  end;

  procedure DrawControlHelper(Control: TControl; Options: THelperOptions); overload;
  procedure DrawControlHelper(Canvas: TCanvas; Rect: TRect; BorderWidth: TBorderWidth;
    Padding: TPadding; Options: THelperOptions); overload;
  {$ENDIF}

  function CalcClientRect(Control: TControl): TRect;

implementation

uses
  SysUtils, TypInfo;

type
  TOpenCtrl = class(TWinControl)
  public
    property BorderWidth;
  end;

// Old delphi support
{$IFNDEF VER210UP}
function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;
{$ENDIF}

{$IFDEF VER210UP} {$REGION 'DrawControlHelper'}
procedure DrawControlHelper(Canvas: TCanvas; Rect: TRect; BorderWidth: TBorderWidth;
  Padding: TPadding; Options: THelperOptions);
  procedure Line(Canvas: TCanvas; x1, y1, x2, y2: Integer);
  begin
    Canvas.MoveTo(x1, y1);
    Canvas.LineTo(x2, y2);
  end;
var
  SaveBk: TColor;
  SavePen, SaveBrush: TPersistent;
begin
  SavePen := nil;
  SaveBrush := nil;

  try
    if Canvas.Handle = 0 then
      Exit;

    // save canvas state
    SavePen := TPen.Create;
    SavePen.Assign(Canvas.Pen);
    SaveBrush := TBrush.Create;
    SaveBrush.Assign(Canvas.Brush);

    Canvas.Pen.Mode := pmNot;
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;

    // ClientRect Helper
    if THelperOption.hoClientRect in Options then
    begin
      SaveBk := SetBkColor(Canvas.Handle, RGB(127,255,255));
      DrawFocusRect(Canvas.Handle, Rect);
      SetBkColor(Canvas.Handle, SaveBk);
    end;

    // Border Helper
    if THelperOption.hoBorder in Options then
    begin
      if (BorderWidth <> 0) and (BorderWidth * 2 <= RectWidth(Rect)) and (BorderWidth * 2 <= RectHeight(Rect)) then
        Canvas.Rectangle(Rect.Left + BorderWidth, Rect.Top + BorderWidth,
          Rect.Right - BorderWidth, Rect.Bottom - BorderWidth);
    end;

    // Padding Helper
    if THelperOption.hoPadding in Options then
    begin
      if (BorderWidth + Padding.Top < RectHeight(Rect) - BorderWidth - Padding.Bottom) and
         (BorderWidth + Padding.Left < RectWidth(Rect) - BorderWidth - Padding.Right) then
      begin
        Canvas.Pen.Style := psDot;

        if Padding.Left <> 0 then
          Line(Canvas, Rect.Left + Padding.Left + BorderWidth, Rect.Top + Padding.Top + BorderWidth,
            Rect.Left + Padding.Left + BorderWidth, Rect.Bottom - Padding.Bottom - BorderWidth - 1);
        if Padding.Top <> 0 then
          Line(Canvas, Rect.Left + Padding.Left + BorderWidth, Rect.Top + Padding.Top + BorderWidth,
            Rect.Right - Padding.Right - BorderWidth - 1, Rect.Top + Padding.Top + BorderWidth);
        if Padding.Right <> 0 then
          Line(Canvas, Rect.Right - Padding.Right - BorderWidth - 1, Rect.Top + Padding.Top + BorderWidth,
            Rect.Right - Padding.Right - BorderWidth - 1, Rect.Bottom - Padding.Bottom - BorderWidth - 1);
        if Padding.Bottom <> 0 then
          Line(Canvas, Rect.Left + Padding.Left + BorderWidth, Rect.Bottom - Padding.Bottom - BorderWidth - 1,
            Rect.Right - Padding.Right - BorderWidth - 1, Rect.Bottom - Padding.Bottom - BorderWidth - 1);
      end;
    end;

    Canvas.Pen.Assign(SavePen);
    Canvas.Brush.Assign(SaveBrush);
  finally
    SavePen.Free;
    SaveBrush.Free;
  end;
end;

procedure DrawControlHelper(Control: TControl; Options: THelperOptions);
var
  Canvas: TCanvas;
  Padding: TPadding;
  BorderWidth: Integer;
  MyCanvas: Boolean;
begin
  MyCanvas := False;
  Canvas := nil;
  Padding := nil;
  BorderWidth := 0;

  // if win control
  if Control is TWinControl then
  begin
    // get padding
    Padding := TWinControl(Control).Padding;
    // get canvas
    if Control is TEsCustomControl then
      Canvas := TEsCustomControl(Control).Canvas
    else
    begin
      MyCanvas := True;
      Canvas := TControlCanvas.Create;
      TControlCanvas(Canvas).Control := Control;
    end;
    // get border width
    if Control is TEsBaseLayout then
      BorderWidth := TEsBaseLayout(Control).BorderWidth
    else
      BorderWidth := TOpenCtrl(Control).BorderWidth;
  end else
  if Control is TGraphicControl then
  begin
    // get canvas
    Canvas := TEsGraphicControl(Control).Canvas;
    if Control is TEsGraphicControl then
      Padding := TEsGraphicControl(Control).Padding;
  end;

  try
    DrawControlHelper(Canvas, Control.ClientRect, BorderWidth, Padding, Options);
  finally
    if MyCanvas then
      Canvas.Free;
  end;
end;
{$ENDREGION} {$ENDIF}

function IsStyledClientControl(Control: TControl): Boolean;
begin
  Result := False;

  {$IFDEF VER230UP}
  if Control = nil then
    Exit;

  if StyleServices.Enabled then
  begin
    Result := {$ifdef VER240UP}(seClient in Control.StyleElements) and{$endif}
      TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function CalcClientRect(Control: TControl): TRect;
var
  {$ifdef FAST_CALC_CLIENTRECT}
  Info: TWindowInfo;
  {$endif}
  IsFast: Boolean;
begin
  {$ifdef FAST_CALC_CLIENTRECT}
  IsFast := True;
  {$else}
  IsFast := False;
  {$endif}

  Result := Rect(0, 0, Control.Width, Control.Height);

  // Only TWinControl's has non client area
  if not (Control is TWinControl) then
    Exit;

  // Fast method not work for controls not having Handle
  if not TWinControl(Control).Handle <> 0 then
    IsFast := False;

  if IsFast then
  begin
    ZeroMemory(@Info, SizeOf(TWindowInfo));
    Info.cbSize := SizeOf(TWindowInfo);
    GetWindowInfo(TWinControl(Control).Handle, info);
    Result.Left := Info.rcClient.Left - Info.rcWindow.Left;
    Result.Top := Info.rcClient.Top - Info.rcWindow.Top;
    Result.Right := -Info.rcWindow.Left + Info.rcClient.Right;
    Result.Top := -Info.rcWindow.Top + Info.rcClient.Bottom;
  end else
  begin
    Control.Perform(WM_NCCALCSIZE, 0, LParam(@Result));
  end;
end;

procedure DrawParentImage(Control: TControl; DC: HDC; InvalidateParent: Boolean = False);
var
  ClientRect: TRect;
  P: TPoint;
  SaveIndex: Integer;
begin
  if Control.Parent = nil then
    Exit;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, P);

  // if control has non client border then need additional offset viewport
  ClientRect := Control.ClientRect;
  if (ClientRect.Right <> Control.Width) or (ClientRect.Bottom <> Control.Height) then
  begin
    ClientRect := CalcClientRect(Control);
    SetViewportOrgEx(DC, P.X - Control.Left - ClientRect.Left, P.Y - Control.Top - ClientRect.Top, nil);
  end else
    SetViewportOrgEx(DC, P.X - Control.Left, P.Y - Control.Top, nil);

  IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);

  Control.Parent.Perform(WM_ERASEBKGND, DC, 0);
  // Control.Parent.Perform(WM_PAINT, DC, 0);
  Control.Parent.Perform(WM_PRINTCLIENT, DC, PRF_CLIENT);

  RestoreDC(DC, SaveIndex);

  if InvalidateParent then
    if not (Control.Parent is TCustomControl) and not (Control.Parent is TCustomForm) and
       not (csDesigning in Control.ComponentState)and not (Control.Parent is TEsCustomControl) then
    begin
      Control.Parent.Invalidate;
    end;

  SetViewportOrgEx(DC, P.X, P.Y, nil);
end;

{ TESCustomControl }

procedure BitMapDeleteAndNil(var BitMap: HBITMAP);{$IFDEF VER210UP}inline;{$ENDIF}
begin
  if BitMap <> 0 then
  begin
    DeleteObject(BitMap);
    BitMap := 0;
  end;
end;

procedure TEsCustomControl.BeginCachedBackground;
begin
  if CacheBackground <> 0 then BitMapDeleteAndNil(CacheBackground);
  StoredCachedBackground := FIsCachedBackground;
  FIsCachedBackground := True;
end;

procedure TEsCustomControl.BeginCachedBuffer;
begin
  if CacheBitmap <> 0 then BitMapDeleteAndNil(CacheBitmap);
  StoredCachedBuffer := FIsCachedBuffer;
  FIsCachedBuffer := True;
end;

procedure TEsCustomControl.CMParentBufferedChildrensChanged(var Message: TMessage);
begin
  if FParentBufferedChildrens then
  begin
    if Parent <> nil then
    begin
      if Parent is TEsCustomControl then
        BufferedChildrens := TEsCustomControl(Parent).BufferedChildrens
      else
        BufferedChildrens := False;
    end;
    FParentBufferedChildrens := True;
  end;
end;

procedure TEsCustomControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateText;
end;

procedure TEsCustomControl.WMTextChanges(var Message: TMessage);
begin
  Inherited;
  UpdateText;
end;


constructor TEsCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
  {$IFDEF VER210UP}
  ParentDoubleBuffered := False;
  {$ENDIF}
  FParentBufferedChildrens := True;// !!
  CacheBitmap := 0;
  CacheBackground := 0;
  FIsCachedBuffer := False;
  FIsCachedBackground := False;
end;

procedure TEsCustomControl.DeleteCache;
begin
  if CacheBitmap <> 0 then BitMapDeleteAndNil(CacheBitmap);
  if CacheBackground <> 0 then BitMapDeleteAndNil(CacheBackground);
end;

destructor TEsCustomControl.Destroy;
begin
  FCanvas.Free;
  DeleteCache;
  inherited;
end;

procedure TEsCustomControl.DrawBackgroundForOpaqueControls(DC: HDC);
var
  i: integer;
  Control: TControl;
  Prop: Pointer;
begin
  for i := 0 to ControlCount - 1 do
  begin
    Control := Controls[i];
    if (Control is TGraphicControl) and (csOpaque in Control.ControlStyle) and Control.Visible and
       (not (csDesigning in ComponentState) or not (csNoDesignVisible in ControlStyle)
       {$IFDEF VER210UP}or not (csDesignerHide in Control.ControlState){$ENDIF})
    then
    begin
      // Necessary to draw a background if the component has a Property 'Transparent' and also has a Property 'Color'
      Prop := GetPropInfo(Control.ClassInfo, 'Transparent');
      if Prop <> nil then
      begin
        Prop := GetPropInfo(Control.ClassInfo, 'Color');
        if Prop = nil then
          FillRect(DC, Rect(Control.Left, Control.Top, Control.Left + Control.Width, Control.Top + Control.Height), Brush.Handle);
      end;
    end;
//    if (Control is TGraphicControl) and (Control is TSpeedButton) and (csOpaque in Control.ControlStyle) and
//      Control.Visible and (not (csDesigning in ComponentState) or not (csDesignerHide in Control.ControlState) and
//      not (csNoDesignVisible in ControlStyle)) then
//        FillRect(DC, Rect(Control.Left, Control.Top, Control.Left + Control.Width, Control.Top + Control.Height), Brush.Handle);
  end;
end;

procedure TEsCustomControl.EndCachedBackground;
begin
  FIsCachedBackground := StoredCachedBackground;
end;

procedure TEsCustomControl.EndCachedBuffer;
begin
  FIsCachedBuffer := StoredCachedBuffer;
end;

function TEsCustomControl.GetIsOpaque: Boolean;
begin
  Result := csOpaque in ControlStyle;
end;

function TEsCustomControl.GetTransparent: Boolean;
begin
  Result := ParentBackground;
end;

procedure TEsCustomControl.Paint;
var
  SaveBk: TColor;
begin
  // for Design time
  if IsDrawHelper and(csDesigning in ComponentState) then
  begin
    SaveBk := SetBkColor(Canvas.Handle, RGB(127,255,255));
    DrawFocusRect(Canvas.Handle, Self.ClientRect);
    SetBkColor(Canvas.Handle, SaveBk);
  end;
end;

// Main crap located here:
{ TODO -cCRITICAL : 22.02.2013:
 Perhaps in this method double buffering has errors
 need test this and eliminate duplication of code!!! }
procedure TEsCustomControl.PaintHandler(var Message: TWMPaint);
var
  PS: TPaintStruct;
  BufferDC: HDC;
  BufferBitMap: HBITMAP;
  UpdateRect: TRect;
  SaveViewport: TPoint;
  Region: HRGN;
  DC: HDC;
  IsBeginPaint: Boolean;
begin
  BufferBitMap := 0;
  Region := 0;
  IsBeginPaint := Message.DC = 0;

  if IsBeginPaint then
  begin
    DC := BeginPaint(Handle, PS);
    {$IFDEF VER230UP}
    if TStyleManager.IsCustomStyleActive and not FIsCachedBuffer then
      UpdateRect := ClientRect
      // I had to use a crutch to ClientRect, due to the fact that
      // VCL.Styles.TCustomStyle.DoDrawParentBackground NOT use relative coordinates,
      // ie ignores SetViewportOrgEx!
      // This function uses ClientToScreen and ScreenToClient for coordinates calculation!
    else
    {$endif}
      UpdateRect := PS.rcPaint;
  end
  else
  begin
    DC := Message.DC;
    {$IFDEF VER230UP}
    if TStyleManager.IsCustomStyleActive and not FIsCachedBuffer then
      UpdateRect := ClientRect
    else
    {$endif}
      if GetClipBox(DC, UpdateRect) = ERROR then
        UpdateRect := ClientRect;
  end;

  //------------------------------------------------------------------------------------------------
  // Dublicate code, see PaintWindow
  //------------------------------------------------------------------------------------------------
  // if control not double buffered then create or assign buffer
  if not DoubleBuffered then
  begin
    BufferDC := CreateCompatibleDC(DC);
    // CreateCompatibleDC(DC) return 0 if Drawing takes place to MemDC(buffer):
    // return <> 0 => need to double buffer || return = 0 => no need to double buffer
    if BufferDC <> 0 then
    begin
      // Using the cache if possible
      if FIsCachedBuffer or FIsFullSizeBuffer then
      begin
        // Create cache if need
        if CacheBitmap = 0 then
        begin
          BufferBitMap := CreateCompatibleBitmap(DC, ClientWidth, ClientHeight);
          // Assign to cache if need
          if FIsCachedBuffer then
            CacheBitmap := BufferBitMap;
        end
        else
          BufferBitMap := CacheBitmap;

        // Assign region for minimal overdraw
        Region := CreateRectRgnIndirect(UpdateRect);//0, 0, UpdateRect.Width, UpdateRect.Height);
        SelectClipRgn(BufferDC, Region);
      end
      else
        // Create buffer
        BufferBitMap := CreateCompatibleBitmap(DC,
          UpdateRect.Right - UpdateRect.Left, UpdateRect.Bottom - UpdateRect.Top);
      // Select buffer bitmap
      SelectObject(BufferDC, BufferBitMap);
      // [change coord], if need
      // Moving update region to the (0,0) point
      if not(FIsCachedBuffer or FIsFullSizeBuffer) then
      begin
        GetViewportOrgEx(BufferDC, SaveViewport);
        SetViewportOrgEx(BufferDC, -UpdateRect.Left + SaveViewport.X, -UpdateRect.Top + SaveViewport.Y, nil);
      end;
    end
    else
      BufferDC := DC;
  end
  else
    BufferDC := DC;
  //------------------------------------------------------------------------------------------------

  // DEFAULT HANDLER:
  Message.DC := BufferDC;
  inherited PaintHandler(Message);

  //------------------------------------------------------------------------------------------------
  // Dublicate code, see PaintWindow
  //------------------------------------------------------------------------------------------------
  // draw to window
  if not DoubleBuffered then
  begin
    if not(FIsCachedBuffer or FIsFullSizeBuffer) then
    begin
      // [restore coord], if need
      SetViewportOrgEx(BufferDC, SaveViewport.X, SaveViewport.Y, nil);
      BitBlt(DC, UpdateRect.Left, UpdateRect.Top, RectWidth(UpdateRect), RectHeight(UpdateRect), BufferDC, 0, 0, SRCCOPY);
    end
    else
    begin
      BitBlt(DC, UpdateRect.Left, UpdateRect.Top, RectWidth(UpdateRect), RectHeight(UpdateRect), BufferDC,
        UpdateRect.Left, UpdateRect.Top, SRCCOPY);
    end;
  end;

  if BufferDC <> DC then
    DeleteObject(BufferDC);
  if Region <> 0 then
    DeleteObject(Region);
  // delete buufer, if need
  if not FIsCachedBuffer and (BufferBitMap <> 0) then
    DeleteObject(BufferBitMap);
  //------------------------------------------------------------------------------------------------

  // end paint, if need
  if IsBeginPaint then
    EndPaint(Handle, PS);
end;

procedure TEsCustomControl.PaintTo(DC: HDC; X, Y: Integer);
begin
//  if DoubleBuffered then
//    FillRect(DC, Rect(X, Y, X + Width, Y + Height), Brush.Handle);
  inherited PaintTo(DC, X, Y);
end;

{$ifdef VER210UP} {$REGION 'BACKUP'}
(*
// Main magic located here:
procedure TESCustomControl.PaintWindow(DC: HDC);
var
  BufferDC, TempDC: HDC;
  BufferBitMap: HBITMAP;
  UpdateRect: TRect;
  SaveViewport: TPoint;
  Region: HRGN;
begin
  //UpdateRect := Rect(0, 0, Width, Height);
  //GetClipBox(DC, UpdateRect);
  if GetClipBox(DC, UpdateRect) = ERROR then
    UpdateRect := Rect(0, 0, Width, Height);

  if not DoubleBuffered then
  begin
    BufferDC := CreateCompatibleDC(DC);
    // for bitmap context
    if BufferDC = 0 then
      BufferDC := DC
    else
    begin
      if FCachedBuffer then
      begin
        if CacheBuffer = 0 then
          CacheBuffer := CreateCompatibleBitmap(DC, Width, Height);
        BufferBitMap := CacheBuffer;
        Region := CreateRectRgn(0, 0, UpdateRect.Width, UpdateRect.Height);
        SelectClipRgn(BufferDC, Region);
      end
      else
        BufferBitMap := CreateCompatibleBitmap(DC, UpdateRect.Width, UpdateRect.Height);
      SelectObject(BufferDC, BufferBitMap);
    end;
  end
  else
    BufferDC := DC;

  // change coord
  if (not DoubleBuffered){ and (not FCachedBuffer)} then
  begin
    GetViewportOrgEx(BufferDC, SaveViewport);
    SetViewportOrgEx(BufferDC, -UpdateRect.Left + SaveViewport.X, -UpdateRect.Top + SaveViewport.Y, nil);
  end;

  if not(csOpaque in ControlStyle) then
    if ParentBackground then
    begin
      if FCachedBackground then
      begin
        if CacheBackground = 0 then
        begin
          TempDC := CreateCompatibleDC(DC);
          CacheBackground := CreateCompatibleBitmap(DC, Width, Height);
          SelectObject(TempDC, CacheBackground);
          DrawParentImage(Self, TempDC, False);
          DeleteDC(TempDC);
        end;
        TempDC := CreateCompatibleDC(BufferDC);
        SelectObject(TempDC, CacheBackground);
        BitBlt(BufferDC, 0, 0, UpdateRect.Width, UpdateRect.Height, TempDC, 0, 0, SRCCOPY);
        DeleteDC(TempDC);
      end
      else
        DrawParentImage(Self, BufferDC, False);
    end else
      if (not DoubleBuffered) then
        FillRect(BufferDC, Rect(0, 0, Width, Height), Brush.Handle);

  FCanvas.Lock;
  try
    Canvas.Handle := BufferDC;
    TControlCanvas(Canvas).UpdateTextFlags;
    Paint;
    //Canvas.Brush.Color := Random(256*256*256);
    //Canvas.FillRect(Updaterect);
  finally
    FCanvas.Handle := 0;
    FCanvas.Unlock;
  end;

  if IsDrawHelper and(csDesigning in ComponentState) then
  begin
    SetBkColor(BufferDC, RGB(127,255,255));
    DrawFocusRect(BufferDC, self.ClientRect);//self.ClientRect);// for Design
  end;

  // restore coord
  if (not DoubleBuffered){ and (not FCachedBuffer)} then
    SetViewportOrgEx(BufferDC, SaveViewport.X, SaveViewport.Y, nil);

  if not DoubleBuffered then
  begin
    if not FCachedBuffer then
      BitBlt(DC, UpdateRect.Left, UpdateRect.Top, UpdateRect.Width, UpdateRect.Height, BufferDC, 0, 0, SRCCOPY)
    else
    begin
      //BitBlt(DC, UpdateRect.Left, UpdateRect.Top, UpdateRect.Width, UpdateRect.Height, BufferDC, UpdateRect.Left, UpdateRect.Top, SRCCOPY);
      BitBlt(DC, UpdateRect.Left, UpdateRect.Top, UpdateRect.Width, UpdateRect.Height, BufferDC, 0, 0, SRCCOPY);
      DeleteObject(Region);
    end;
    DeleteDC(BufferDC);
  end;

  if not FCachedBuffer and (BufferBitMap <> 0) then DeleteObject(BufferBitMap);
end;
*)
{$ENDREGION} {$endif}

{ TODO -cMAJOR : 22.02.2013:
 See: PaintHandler,
 need eliminate duplication of code!!! }
procedure TEsCustomControl.PaintWindow(DC: HDC);
var
  TempDC: HDC;
  UpdateRect: TRect;
  //---
  BufferDC: HDC;
  BufferBitMap: HBITMAP;
  Region: HRGN;
  SaveViewport: TPoint;
  BufferedThis: Boolean;
begin
  BufferBitMap := 0;
  Region := 0;

  if GetClipBox(DC, UpdateRect) = ERROR then
    UpdateRect := ClientRect;

  BufferedThis := not BufferedChildrens;

  if BufferedThis then
  begin
  //------------------------------------------------------------------------------------------------
  // Dublicate code, see PaintHandler
  //------------------------------------------------------------------------------------------------
    // if control not double buffered then create or assign buffer
    if not DoubleBuffered then
    begin
      BufferDC := CreateCompatibleDC(DC);
      // CreateCompatibleDC(DC) return 0 if Drawing takes place to MemDC(buffer):
      // return <> 0 => need to double buffer || return = 0 => no need to double buffer
      if BufferDC <> 0 then
      begin
        // Using the cache if possible
        if FIsCachedBuffer or FIsFullSizeBuffer then
        begin
          // Create cache if need
          if CacheBitmap = 0 then
          begin
            BufferBitMap := CreateCompatibleBitmap(DC, ClientWidth, ClientHeight);
            // Assign to cache if need
            if FIsCachedBuffer then
              CacheBitmap := BufferBitMap;
          end
          else
            BufferBitMap := CacheBitmap;

          // Assign region for minimal overdraw
          Region := CreateRectRgnIndirect(UpdateRect);//0, 0, UpdateRect.Width, UpdateRect.Height);
          SelectClipRgn(BufferDC, Region);
        end
        else
          // Create buffer
          BufferBitMap := CreateCompatibleBitmap(DC, RectWidth(UpdateRect), RectHeight(UpdateRect));
        // Select buffer bitmap
        SelectObject(BufferDC, BufferBitMap);
        // [change coord], if need
        // Moving update region to the (0,0) point
        if not(FIsCachedBuffer or FIsFullSizeBuffer) then
        begin
          GetViewportOrgEx(BufferDC, SaveViewport);
          SetViewportOrgEx(BufferDC, -UpdateRect.Left + SaveViewport.X, -UpdateRect.Top + SaveViewport.Y, nil);
        end;
      end
      else
        BufferDC := DC;
    end
    else
      BufferDC := DC;
  //------------------------------------------------------------------------------------------------
  end else
    BufferDC := DC;

  if not(csOpaque in ControlStyle) then
    if ParentBackground then
    begin
      if FIsCachedBackground then
      begin
        if CacheBackground = 0 then
        begin
          TempDC := CreateCompatibleDC(DC);
          CacheBackground := CreateCompatibleBitmap(DC, ClientWidth, ClientHeight);
          SelectObject(TempDC, CacheBackground);
          DrawParentImage(Self, TempDC, False);
          DeleteDC(TempDC);
        end;
        TempDC := CreateCompatibleDC(BufferDC);
        SelectObject(TempDC, CacheBackground);
        if not FIsCachedBuffer then
          BitBlt(BufferDC, UpdateRect.Left, UpdateRect.Top, RectWidth(UpdateRect), RectHeight(UpdateRect), TempDC, 0, 0, SRCCOPY)
        else
          BitBlt(BufferDC, UpdateRect.Left, UpdateRect.Top, RectWidth(UpdateRect), RectHeight(UpdateRect), TempDC,
            UpdateRect.Left, UpdateRect.Top, SRCCOPY);
        DeleteDC(TempDC);
      end
      else
        DrawParentImage(Self, BufferDC, False);
    end else
      if (not DoubleBuffered or (DC <> 0)) then
        if not IsStyledClientControl(Self) then
          FillRect(BufferDC, ClientRect, Brush.Handle)
        else
        begin
          SetDCBrushColor(BufferDC,
            ColorToRGB({$ifdef VER230UP}StyleServices.GetSystemColor(Color){$else}Color{$endif}));
          FillRect(BufferDC, ClientRect, GetStockObject(DC_BRUSH));
        end;

  FCanvas.Lock;
  try
    Canvas.Handle := BufferDC;
    TControlCanvas(Canvas).UpdateTextFlags;

    if Assigned(FOnPainting) then
      FOnPainting(Self, Canvas, ClientRect);
    Paint;
    if Assigned(FOnPaint) then
      FOnPaint(Self, Canvas, ClientRect);
  finally
    FCanvas.Handle := 0;
    FCanvas.Unlock;
  end;

  if BufferedThis then
  begin
  //------------------------------------------------------------------------------------------------
  // Dublicate code, see PaintHandler
  //------------------------------------------------------------------------------------------------
    // draw to window
    if not DoubleBuffered then
    begin
      if not(FIsCachedBuffer or FIsFullSizeBuffer) then
      begin
        // [restore coord], if need
        SetViewportOrgEx(BufferDC, SaveViewport.X, SaveViewport.Y, nil);
        BitBlt(DC, UpdateRect.Left, UpdateRect.Top, RectWidth(UpdateRect), RectHeight(UpdateRect), BufferDC, 0, 0, SRCCOPY);
      end
      else
      begin
        BitBlt(DC, UpdateRect.Left, UpdateRect.Top, RectWidth(UpdateRect), RectHeight(UpdateRect), BufferDC,
          UpdateRect.Left, UpdateRect.Top, SRCCOPY);
      end;
    end;

    if (BufferDC <> DC) then
      DeleteObject(BufferDC);
    if Region <> 0 then
      DeleteObject(Region);
    // delete buufer, if need
    if not FIsCachedBuffer and (BufferBitMap <> 0) then
      DeleteObject(BufferBitMap);
  //------------------------------------------------------------------------------------------------
  end;
end;

function TEsCustomControl.IsBufferedChildrensStored: Boolean;
begin
  Result := not ParentBufferedChildrens;
end;

procedure TEsCustomControl.SetBufferedChildrens(const Value: Boolean);
begin
  if Value <> FBufferedChildrens then
  begin
    FBufferedChildrens := Value;
    FParentBufferedChildrens := False;
    NotifyControls(CM_PARENT_BUFFEREDCHILDRENS_CHANGED);
  end;
end;

procedure TEsCustomControl.SetIsCachedBackground(Value: Boolean);
begin
  if Value <> FIsCachedBackground then
  begin
    FIsCachedBackground := Value;
    if not FIsCachedBackground then BitMapDeleteAndNil(CacheBackground);
  end;
end;

procedure TEsCustomControl.SetIsCachedBuffer(Value: Boolean);
begin
  if Value <> FIsCachedBuffer then
  begin
    FIsCachedBuffer := Value;
    if not FIsCachedBuffer then BitMapDeleteAndNil(CacheBitmap);
  end;
end;

procedure TEsCustomControl.SetIsDrawHelper(const Value: Boolean);
begin
  FIsDrawHelper := Value;
  if csDesigning in ComponentState then Invalidate;
end;

procedure TEsCustomControl.SetIsOpaque(const Value: Boolean);
begin
  if Value <> (csOpaque in ControlStyle) then
  begin
    if Value then
    begin
      ControlStyle := ControlStyle + [csOpaque];
    end else
    begin
      ControlStyle := ControlStyle - [csOpaque];
    end;
    Invalidate;
  end;
end;

procedure TEsCustomControl.SetParentBufferedChildrens(const Value: Boolean);
begin
  //FParentBufferedChildrens := Value;
  if Value <> FParentBufferedChildrens then
  begin
//    if (Parent <> nil) and Value then
//    begin
//      if Parent is TESCustomControl then
//        BufferedChildrens := TESCustomControl(Parent).BufferedChildrens
//      else
//        BufferedChildrens := False;
//    end
//    else
//      if Value then
//        BufferedChildrens := False;
//    FParentBufferedChildrens := Value;
    FParentBufferedChildrens := Value;
    if (Parent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENT_BUFFEREDCHILDRENS_CHANGED, 0, 0);
  end;
end;

procedure TEsCustomControl.SetTransparent(const Value: Boolean);
begin
  ParentBackground := Value;
end;

procedure TEsCustomControl.UpdateBackground;
begin
  UpdateBackground(true);
end;

procedure TEsCustomControl.UpdateText;
begin
end;

procedure TEsCustomControl.UpdateBackground(Repaint: Boolean);
begin
  // Delete cache background
  if CacheBackground <> 0 then BitMapDeleteAndNil(CacheBackground);
  if Repaint then Invalidate;
end;

procedure TEsCustomControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if DoubleBuffered {and not(csOpaque in ControlStyle)} then
  begin
    Inherited;
    Message.Result := 1;
    exit;
  end;
  if ControlCount <> 0 then
    DrawBackgroundForOpaqueControls(Message.DC);
  Message.Result := 1;
end;

//procedure TEsCustomControl.WMNCHitTest(var Message: TWMNCHitTest);
//begin
//  if (FIsTransparentMouse) and not(csDesigning in ComponentState) then
//    Message.Result := HTTRANSPARENT
//  else
//    inherited;
//end;

procedure TEsCustomControl.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  if BufferedChildrens and (not FDoubleBuffered or (Message.DC <> 0)) then
  begin
    PaintHandler(Message)// My new PaintHandler
  end
  else
    inherited;// WMPaint(Message);
  ControlState := ControlState - [csCustomPaint];
end;

procedure TEsCustomControl.WMSize(var Message: TWMSize);
begin
  DeleteCache;
  inherited;
end;

procedure TEsCustomControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if not (csOpaque in ControlStyle) and ParentBackground{ and not CachedBackground }then
    Invalidate;
  Inherited;
end;

{$IFDEF VER180UP}
{ TEsBaseLayout }

procedure TEsBaseLayout.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if BorderWidth <> 0 then
  begin
    InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));
  end;
end;

procedure TEsBaseLayout.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if (csDesigning in ComponentState) and IsDrawHelper then
    Invalidate;
end;

procedure TEsBaseLayout.CalcContentMargins(var Margins: TContentMargins);
begin
  Margins.Create(Padding.Left, Padding.Top, Padding.Right, Padding.Bottom);
  if BorderWidth <> 0 then
    Margins.Inflate(Integer(BorderWidth), Integer(BorderWidth));
end;

function TEsBaseLayout.ContentMargins: TContentMargins;
begin
  Result.Reset;
  CalcContentMargins(Result);
end;

function TEsBaseLayout.ContentRect: TRect;
var
  ContentMargins: TContentMargins;
begin
  Result := ClientRect;

  ContentMargins.Reset;
  CalcContentMargins(ContentMargins);

  Inc(Result.Left, ContentMargins.Left);
  Inc(Result.Top, ContentMargins.Top);
  Dec(Result.Right, ContentMargins.Right);
  Dec(Result.Bottom, ContentMargins.Bottom);

  {$ifdef TEST_CONTROL_CONTENT_RECT}
  if Result.Left > Result.Right then
    Result.Right := Result.Left;
  if Result.Top > Result.Bottom then
    Result.Bottom := Result.Top;
  {$endif}
end;

procedure TEsBaseLayout.Paint;
begin
  if (csDesigning in ComponentState) and IsDrawHelper then
    DrawControlHelper(Self, [hoBorder, hoPadding, hoClientRect]);
end;

procedure TEsBaseLayout.SetBorderWidth(const Value: TBorderWidth);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Realign;
    Invalidate;
  end;
end;

{ TEsGraphicControl }

procedure TEsGraphicControl.CalcContentMargins(var Margins: TContentMargins);
begin
  if FPadding <> nil then
    Margins.Create(Padding.Left, Padding.Top, Padding.Right, Padding.Bottom)
  else
    Margins.Reset;
end;

function TEsGraphicControl.ContentMargins: TContentMargins;
begin
  Result.Reset;
  CalcContentMargins(Result);
end;

function TEsGraphicControl.ContentRect: TRect;
var
  ContentMargins: TContentMargins;
begin
  Result := ClientRect;

  ContentMargins.Reset;
  CalcContentMargins(ContentMargins);

  Inc(Result.Left, ContentMargins.Left);
  Inc(Result.Top, ContentMargins.Top);
  Dec(Result.Right, ContentMargins.Right);
  Dec(Result.Bottom, ContentMargins.Bottom);

  {$ifdef TEST_CONTROL_CONTENT_RECT}
  if Result.Left > Result.Right then
    Result.Right := Result.Left;
  if Result.Top > Result.Bottom then
    Result.Bottom := Result.Top;
  {$endif}
end;

destructor TEsGraphicControl.Destroy;
begin
  FPadding.Free;
  inherited;
end;

function TEsGraphicControl.GetPadding: TPadding;
begin
  if FPadding = nil then
  begin
    FPadding := TPadding.Create(nil);
    FPadding.OnChange := PaddingChange;
  end;
  Result := FPadding;
end;

function TEsGraphicControl.HasPadding: Boolean;
begin
  Result := FPadding <> nil;
end;

procedure TEsGraphicControl.PaddingChange(Sender: TObject);
begin
  AdjustSize;
  Invalidate;
  if (FPadding.Left = 0) and (FPadding.Top = 0) and (FPadding.Right = 0) and (FPadding.Bottom = 0) then
    FreeAndNil(FPadding);
end;

procedure TEsGraphicControl.Paint;
begin
  if (csDesigning in ComponentState) and IsDrawHelper then
    DrawControlHelper(Self, [hoPadding, hoClientRect]);
end;

procedure TEsGraphicControl.SetIsDrawHelper(const Value: Boolean);
begin
  if FIsDrawHelper <> Value then
  begin
      FIsDrawHelper := Value;
      if csDesigning in ComponentState then
        Invalidate;
  end;
end;

procedure TEsGraphicControl.SetPadding(const Value: TPadding);
begin
  Padding.Assign(Value);
end;

{ TContentMargins }

constructor TContentMargins.Create(Left, Top, Right, Bottom: TMarginSize);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

procedure TContentMargins.Reset;
begin
  Left := 0;
  Top := 0;
  Right := 0;
  Bottom := 0;
end;

function TContentMargins.Height: TMarginSize;
begin
  Result := Top + Bottom;
end;

procedure TContentMargins.Inflate(DX, DY: Integer);
begin
  Inc(Left, DX);
  Inc(Right, DX);
  Inc(Top, DY);
  Inc(Bottom, DY);
end;

procedure TContentMargins.Inflate(DLeft, DTop, DRight, DBottom: Integer);
begin
  Inc(Left, DLeft);
  Inc(Right, DRight);
  Inc(Top, DTop);
  Inc(Bottom, DBottom);
end;

function TContentMargins.Width: TMarginSize;
begin
  Result := Left + Right;
end;
{$ENDIF}

end.


