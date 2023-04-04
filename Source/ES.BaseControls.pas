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
{                                                                              }
{ LICENSE:                                                                     }
{ This library is Open Source software, can be used this in commercial         }
{ projects, modify, and distribute as source code or binary files.             }
{ ===                                                                          }
{ This library licensed at two license: GNU GPLv2 & Modified MIT License(MIT)  }
{ You can choose one of two license.                                           }
{ 1. GNU GPL v2: https://www.gnu.org/licenses/gpl2.html                        }
{ 2. Modified MIT License (MIT):                                               }
{ ===                                                                          }
{ Modified MIT License (MIT)                                                   }
{ Copyright (c) 2009-2021 Peter Sokolov, errorsoft.org, errorsoft(c)           }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"),        }
{ to deal in the Software without restriction, including without limitation    }
{ the rights to use, copy, modify, merge, publish, distribute, sublicense,     }
{ and/or sell copies of the Software, and to permit persons to whom the        }
{ Software is furnished to do so, subject to the following conditions:         }
{ 1. The above copyright notice and this permission notice shall be included   }
{    in all copies or substantial portions of the Software.                    }
{ 2. Do not have to sell this library as a standalone components library or    }
{    as part of another components library.                                    }
{    (you can agree with me on licensing).                                     }
{ 3. Desirable specify the use of this library in your software                }
{    (for example: about window).                                              }
{ ===                                                                          }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      }
{ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{******************************************************************************}

// -----------------------------------------------------------------------------
// This module/file does not depend on other units/files ErrorsoftVclComponents,
// you can use it separately from other units in your project.
// Support Delphi since Delphi XE2 and Windows 7, older version this file with
// support Delphi 7 and Windows XP can be downloaded here:
//   https://github.com/errorcalc/FreeEsVclComponents/tree/Version2/Source
// -----------------------------------------------------------------------------

unit ES.BaseControls;

interface

// {$I EsDefines.inc}
{$REGION 'CompilerVersionDefines'}
  // I use multiple definitions to Code Completion work well.
  // Compiler verisons
  // XE2
  {$IFDEF VER230}{$DEFINE VER230UP}{$ENDIF}
  // XE3
  {$IFDEF VER240}{$DEFINE VER230UP}{$DEFINE VER240UP}{$ENDIF}
  // XE4
  {$IFDEF VER250}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$ENDIF}
  // XE5
  {$IFDEF VER260}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}{$ENDIF}
  // XE6
  {$IFDEF VER270}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$ENDIF}
  // XE7
  {$IFDEF VER280}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$ENDIF}
  // XE8
  {$IFDEF VER290}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$ENDIF}
  // XE10.0 Seattle
  {$IFDEF VER300}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$ENDIF}
  // XE10.1 Berlin
  {$IFDEF VER310}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$DEFINE VER310UP}{$ENDIF}
  // XE10.2 Tokyo
  {$IFDEF VER320}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$DEFINE VER310UP}
  {$DEFINE VER320UP}{$ENDIF}
  // XE10.3 Rio
  {$IFDEF VER330}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$DEFINE VER310UP}
  {$DEFINE VER320UP}{$DEFINE VER330UP}{$ENDIF}
  // XE10.4 Sydney
  {$IFDEF VER340}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$DEFINE VER310UP}
  {$DEFINE VER320UP}{$DEFINE VER330UP}{$DEFINE VER340UP}{$ENDIF}
  // XE11 Alexandria
  {$IFDEF VER350}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$DEFINE VER310UP}
  {$DEFINE VER320UP}{$DEFINE VER330UP}{$DEFINE VER340UP}{$DEFINE VER350UP}{$ENDIF}
  // Next versions
  {$IF CompilerVersion >= 34}{$DEFINE VER230UP}{$DEFINE VER240UP}{$DEFINE VER250UP}{$DEFINE VER260UP}
  {$DEFINE VER270UP}{$DEFINE VER280UP}{$DEFINE VER290UP}{$DEFINE VER300UP}{$DEFINE VER310UP}
  {$DEFINE VER320UP}{$DEFINE VER330UP}{$DEFINE VER340UP}{$DEFINE VER350UP}{$IFEND}
  // Vcl
  {$IFDEF VER240UP}{$DEFINE STYLE_ELEMENTS}{$ENDIF}
  {$IFDEF VER330UP}{$DEFINE VIRTUAL_IMAGE}{$ENDIF}
  {$IFDEF VER340UP}{$DEFINE STYLE_NAME}{$ENDIF}
{$ENDREGION}

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

// see function CalcClientRect
{$define FAST_CALC_CLIENTRECT}

// see TEsBaseLayout.ContentRect
{$define TEST_CONTROL_CONTENT_RECT}

uses
  System.Classes, System.Types, WinApi.Windows, WinApi.Messages, Vcl.Controls,
  Vcl.Forms, Vcl.Graphics;

const
  CM_ESBASE = CM_BASE + $0800;
  CM_PARENT_BUFFEREDCHILDRENS_CHANGED = CM_ESBASE + 1;

  EsVclCoreVersion = 4.4;

type
  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;

  /// <summary>
  /// TEsCustomControl this is the best replacement for TCustomControl,
  /// supports transparency and without flicker.
  /// The component is double buffered by default.
  /// If the standard DoubleBuffered property will be activated,
  /// then improved double buffered has been deactivated!
  /// The DoubleBuffered property is retained for backward compatibility only.
  /// If the descendant class is a container, then to double buffering graphic children,
  /// activate the property BufferedChildren.
  /// </summary>
  TEsCustomControl = class(TWinControl)
  private type
    TPaintBuffer = record
      Rect: TRect;
      DC: HDC;
      BufferDC: HDC;
      Bitmap: HBITMAP;
      OldBitmap: HBITMAP;
      WindowOrg: TPoint;
    end;
  private
    // Paint
    FCanvas: TCanvas;
    FOnPaint: TPaintEvent;
    FOnPainting: TPaintEvent;
    FIsDrawHelper: Boolean;
    // Anti flicker and transparent magic
    FBufferedChildren: Boolean;
    FParentBufferedChildren: Boolean;
    FIsCachedBuffer: Boolean;
    FIsFullSizeBuffer: Boolean;
    FIsCachedBackground: Boolean;
    CacheBitmap: HBITMAP;// Cache for buffer BitMap
    CacheBackground: HBITMAP;// Cache for background BitMap
    // ----- Set/Get -----
    procedure SetIsCachedBuffer(Value: Boolean);
    procedure SetIsCachedBackground(Value: Boolean);
    procedure SetIsDrawHelper(const Value: Boolean);
    procedure SetIsOpaque(const Value: Boolean);
    function GetIsOpaque: Boolean;
    procedure SetBufferedChildren(const Value: Boolean);
    procedure SetParentBufferedChildren(const Value: Boolean);
    function GetTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
    function IsBufferedChildrenStored: Boolean;
    procedure SetIsFullSizeBuffer(const Value: Boolean);
    // ----- Handle paint oriented messages/functions -----
    procedure CMParentBufferedChildrensChanged(var Message: TMessage); message CM_PARENT_BUFFEREDCHILDRENS_CHANGED;
    procedure CustomPaintHandler(var Message: TWMPaint);
    /// Starts custom double buffering
    function BeginCustomBufferedPaint(DC: HDC; Rect: TRect): TPaintBuffer;
    /// Ends custom double buffering
    procedure EndCustomBufferedPaint(PaintBuffer: TPaintBuffer);
    procedure DeleteBitmapCache;
    procedure DrawBackgroundForOpaqueControls(DC: HDC);
    procedure FillBackground(Handle: THandle);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    // Handle other messages
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMTextChanges(var Message: TMessage); message WM_SETTEXT;
    // Fix - will be removed in future releases
    procedure FixBufferedChildren(Reader: TReader);
    procedure FixParentBufferedChildren(Reader: TReader);
  protected
    /// <summary>
    /// The canvas is similar to the canvas in TCustomControl,
    /// the canvas can be used ONLY in Paint, OnPaint, OnPainting
    /// </summary>
    property Canvas: TCanvas read FCanvas;
    /// <summary>
    /// Descendants must override this method for custom rendering
    /// </summary>
    procedure Paint; virtual;
    /// <summary>
    /// Descendants must override this method for custom background rendering
    /// </summary>
    procedure DrawBackground(DC: HDC); virtual;
    /// <summary>
    /// This procedure calls a change in the text in a control
    /// </summary>
    procedure UpdateText; dynamic;
    /// You should not override this method but inheritors
    procedure PaintWindow(DC: HDC); override;
    // Fix - will be removed in future releases
    procedure DefineProperties(Filer: TFiler); override;
    {$IFDEF STYLE_ELEMENTS}
    procedure UpdateStyleElements; override;
    {$ENDIF}
    property ParentBackground default True;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Call this to update the background when there is a use background cache
    /// </summary>
    procedure UpdateBackground(Repaint: Boolean = True);
    // ------------------ Properties for published -----------------------------
    /// <summary>
    /// Standard double buffering, this control uses its own improved buffering, therefore this property is False by default.
    /// If standard DoubleBuffering is enabled then improved buffering of child graphics controls has been disabled.
    /// </summary>
    property DoubleBuffered default False;
    /// <summary>
    /// See the DoubleBuffering property description for details.
    /// </summary>
    property ParentDoubleBuffered default False;
    /// <summary>
    /// OnPaint event handler, called after the control content has been rendered.
    /// </summary>
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    /// <summary>
    /// OnPainting event handler, called before the control content has been rendered.
    /// </summary>
    property OnPainting: TPaintEvent read FOnPainting write FOnPainting;
    /// <summary>
    /// ParentDoubleBuffered property similar to ParentDoubleBuffered property but related to BufferedChildren.
    /// </summary>
    property ParentBufferedChildren: Boolean read FParentBufferedChildren write SetParentBufferedChildren default True;
    /// <summary>
    /// BufferedChildren is an improved double buffering that suppresses flickering for child graphics controls.
    /// </summary>
    property BufferedChildren: Boolean read FBufferedChildren write SetBufferedChildren stored IsBufferedChildrenStored;
    /// <summary>
    /// If IsCachedBuffer is true, then the double buffering buffer will be persisted between draw calls, this is faster,
    /// but causes more memory consumption.
    /// </summary>
    property IsCachedBuffer: Boolean read FIsCachedBuffer write SetIsCachedBuffer default False;
    /// <summary>
    /// IsCachedBackground allows you to persist the background image between draw calls.
    /// Accelerates rendering, but when the background changes, you must manually call UpdateBackground.
    /// </summary>
    property IsCachedBackground: Boolean read FIsCachedBackground write SetIsCachedBackground default False;
    /// <summary>
    /// If True then control will be drawing the halt-tone frame, helps to more accurately position the control in the design time.
    /// </summary>
    property IsDrawHelper: Boolean read FIsDrawHelper write SetIsDrawHelper default False;
    /// <summary>
    /// IsOpaque are analogue [csOpaque] in ControlStyle.
    /// </summary>
    property IsOpaque: Boolean read GetIsOpaque write SetIsOpaque default False;
    /// <summary>
    /// If the property is active, then the bitmap buffer will be for the entire size of the control.
    /// More cpu and mem usage, but for some code that does not take into account the context shift, this is a solution to problems.
    /// </summary>
    property IsFullSizeBuffer: Boolean read FIsFullSizeBuffer write SetIsFullSizeBuffer default False;
  end;

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
    procedure AdjustContentRect(var Rect: TRect); virtual;
    procedure Paint; override;
    // new
    procedure CalcContentMargins(var Margins: TContentMargins); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function ContentRect: TRect; virtual;
    function ContentMargins: TContentMargins; inline;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BufferedChildren default True;
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
    {$IFDEF STYLE_ELEMENTS}
    procedure UpdateStyleElements; override;
    {$ENDIF}
    // new
    procedure CalcContentMargins(var Margins: TContentMargins); virtual;
  public
    destructor Destroy; override;
    property Padding: TPadding read GetPadding write SetPadding;
    function ContentRect: TRect; virtual;
    function ContentMargins: TContentMargins; inline;
    property IsDrawHelper: Boolean read FIsDrawHelper write SetIsDrawHelper default False;
  end;

type
  THelperOption = (hoPadding, hoBorder, hoClientRect);
  THelperOptions = set of THelperOption;

  procedure DrawControlHelper(Control: TControl; Options: THelperOptions; FrameWidth: Integer = 0); overload;
  procedure DrawControlHelper(Canvas: TCanvas; Rect: TRect; BorderWidth: TBorderWidth;
    Padding: TPadding; Options: THelperOptions); overload;

  function CalcClientRect(Control: TControl): TRect;

  procedure DrawParentImage(Control: TControl; DC: HDC; InvalidateParent: Boolean = False);

implementation

uses
  System.SysUtils, System.TypInfo, WinApi.Uxtheme, Winapi.DwmApi, Vcl.Themes;

type
  TOpenCtrl = class(TWinControl)
  public
    property BorderWidth;
  end;

{$REGION 'DrawControlHelper'}
procedure DrawControlHelper(Canvas: TCanvas; Rect: TRect; BorderWidth: TBorderWidth;
  Padding: TPadding; Options: THelperOptions);
  procedure Line(Canvas: TCanvas; x1, y1, x2, y2: Integer);
  begin
    Canvas.MoveTo(x1, y1);
    Canvas.LineTo(x2, y2);
  end;
var
  SaveBk: Cardinal;
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
    if (THelperOption.hoPadding in Options) and (Padding <> nil) then
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

procedure DrawControlHelper(Control: TControl; Options: THelperOptions;
  FrameWidth: Integer = 0);
var
  Canvas: TCanvas;
  Padding: TPadding;
  BorderWidth: Integer;
  MyCanvas: Boolean;
  R: TRect;
begin
  MyCanvas := False;
  Canvas := nil;
  Padding := nil;
  BorderWidth := 0;

  // if win control
  if Control is TWinControl then
  begin
    TOpenCtrl(Control).AdjustClientRect(R);

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
  end
  else if Control is TEsGraphicControl then
  begin
    // get canvas
    Canvas := TEsGraphicControl(Control).Canvas;
    Padding := TEsGraphicControl(Control).Padding;
  end;

  try
    R := Control.ClientRect;
    R.Inflate(-FrameWidth, -FrameWidth);
    DrawControlHelper(Canvas, R, BorderWidth, Padding, Options);
  finally
    if MyCanvas then
      Canvas.Free;
  end;
end;
{$ENDREGION}

function IsStyledClientControl(Control: TControl): Boolean;
begin
  Result := False;

  {$IFDEF STYLE_NAME}
  if StyleServices(Control).Enabled then
  begin
    Result := (seClient in Control.StyleElements) and
              (not StyleServices(Control).IsSystemStyle);
  end;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := {$IFDEF STYLE_ELEMENTS}(seClient in Control.StyleElements) and{$ENDIF}
      TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function IsStyledBorderControl(Control: TControl): Boolean;
begin
  Result := False;

  {$IFDEF STYLE_NAME}
  if StyleServices(Control).Enabled then
  begin
    Result := (seBorder in Control.StyleElements) and
              (not StyleServices(Control).IsSystemStyle);
  end;
  {$ELSE}
  if StyleServices.Enabled then
  begin
    Result := {$IFDEF STYLE_ELEMENTS}(seBorder in Control.StyleElements) and{$ENDIF}
      TStyleManager.IsCustomStyleActive;
  end;
  {$ENDIF}
end;

function CalcClientRect(Control: TControl): TRect;
var
  {$IFDEF FAST_CALC_CLIENTRECT}
  Info: TWindowInfo;
  {$ENDIF}
  IsFast: Boolean;
begin
  {$IFDEF FAST_CALC_CLIENTRECT}
  IsFast := True;
  {$ELSE}
  IsFast := False;
  {$ENDIF}

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

procedure BitmapDeleteAndNil(var Bitmap: HBITMAP);
begin
  if Bitmap <> 0 then
  begin
    DeleteObject(Bitmap);
    Bitmap := 0;
  end;
end;

{ TEsCustomControl }

constructor TEsCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // cache
  CacheBitmap := 0;
  CacheBackground := 0;

  // init
  ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
  ParentDoubleBuffered := False;

  // canvas
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  // new props
  FParentBufferedChildren := True;
  FBufferedChildren := False;
  FIsCachedBuffer := False;
  FIsCachedBackground := False;
  FIsFullSizeBuffer := False;
  FIsDrawHelper := False;
end;

destructor TEsCustomControl.Destroy;
begin
  FCanvas.Free;
  DeleteBitmapCache;
  inherited;
end;

procedure TEsCustomControl.CMParentBufferedChildrensChanged(var Message: TMessage);
begin
  if FParentBufferedChildren then
  begin
    if Parent <> nil then
    begin
      if Parent is TEsCustomControl then
        BufferedChildren := TEsCustomControl(Parent).BufferedChildren
      else
        BufferedChildren := False;
    end;
    FParentBufferedChildren := True;
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

// temp fix
procedure TEsCustomControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('BufferedChildrens', FixBufferedChildren, nil, False);
  Filer.DefineProperty('ParentBufferedChildrens', FixParentBufferedChildren, nil, False);
end;

procedure TEsCustomControl.DeleteBitmapCache;
begin
  BitmapDeleteAndNil(CacheBitmap);
  BitmapDeleteAndNil(CacheBackground);
end;

procedure TEsCustomControl.DrawBackground(DC: HDC);
begin
  DrawParentImage(Self, DC, False);
end;

// hack for bad graphic controls
procedure TEsCustomControl.DrawBackgroundForOpaqueControls(DC: HDC);
var
  I: integer;
  Control: TControl;
  Prop: Pointer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control is TGraphicControl) and (csOpaque in Control.ControlStyle) and Control.Visible and
       (not (csDesigning in ComponentState) or not (csNoDesignVisible in ControlStyle)
        or not (csDesignerHide in Control.ControlState))
    then
    begin
      // Necessary to draw a background if the control has a Property 'Transparent' and hasn't a Property 'Color'
      Prop := GetPropInfo(Control.ClassInfo, 'Transparent');
      if Prop <> nil then
      begin
        Prop := GetPropInfo(Control.ClassInfo, 'Color');
        if Prop = nil then
          FillRect(DC, Rect(Control.Left, Control.Top, Control.Left + Control.Width, Control.Top + Control.Height), Brush.Handle);
      end;
    end;
  end;
end;

procedure TEsCustomControl.FillBackground(Handle: THandle);
var
  RGBColor, OldColor: TColor;
begin
  if not IsStyledClientControl(Self) then
  begin
    FillRect(Handle, ClientRect, Brush.Handle)
  end else
  begin
    {$IFDEF STYLE_NAME}
    RGBColor := StyleServices(Self).GetSystemColor(Color);
    {$ELSE}
    RGBColor := StyleServices.GetSystemColor(Color);
    {$ENDIF}
    if RGBColor and $FF000000 <> 0 then
      RGBColor := ColorToRGB(Color);

    OldColor := SetDCBrushColor(Handle, RGBColor);
    FillRect(Handle, ClientRect, GetStockObject(DC_BRUSH));
    SetDCBrushColor(Handle, OldColor);
  end;
end;

procedure TEsCustomControl.FixBufferedChildren(Reader: TReader);
begin
  BufferedChildren := Reader.ReadBoolean;
end;

procedure TEsCustomControl.FixParentBufferedChildren(Reader: TReader);
begin
  ParentBufferedChildren := Reader.ReadBoolean;
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
  SaveBk: Cardinal;
begin
  // for Design time
  if IsDrawHelper and(csDesigning in ComponentState) then
  begin
    SaveBk := SetBkColor(Canvas.Handle, RGB(127,255,255));
    DrawFocusRect(Canvas.Handle, Self.ClientRect);
    SetBkColor(Canvas.Handle, SaveBk);
  end;
end;

function TEsCustomControl.BeginCustomBufferedPaint(DC: HDC; Rect: TRect): TPaintBuffer;
var
  Region: HRGN;
begin
  Result := Default(TPaintBuffer);
  Result.Rect := Rect;
  Result.DC := DC;
  Result.BufferDC := CreateCompatibleDC(DC);

  // Check DC
  if Result.BufferDC = 0 then
  begin
    Result.BufferDC := DC;
    Exit;
  end;

  // Full size buffer makes sense for IsCachedBuffer and IsFullSizeBuffer
  if FIsCachedBuffer or FIsFullSizeBuffer then
  begin
    // Load bitmap from cache if possible, or create new bitmap
    if FIsCachedBuffer then
    begin
      // Recreate bitmap cache if need
      if CacheBitmap = 0 then
        CacheBitmap := CreateCompatibleBitmap(Result.DC, ClientWidth, ClientHeight);
      // Select cache bitmap
      Result.OldBitmap := SelectObject(Result.BufferDC, CacheBitmap);
    end else
    begin
      // Make bitmap
      Result.Bitmap := CreateCompatibleBitmap(Result.DC, ClientWidth, ClientHeight);
      // Select bitmap
      Result.OldBitmap := SelectObject(Result.BufferDC, Result.Bitmap);
    end;
    // Assign region for minimal overdraw
    Region := CreateRectRgnIndirect(Result.Rect);//0, 0, UpdateRect.Width, UpdateRect.Height);
    if Region <> ERROR then
    begin
      SelectClipRgn(Result.BufferDC, Region);
      // The region can be deleted immediately
      DeleteObject(Region);
    end;
  end else
  begin
    // Make bitmap
    Result.Bitmap := CreateCompatibleBitmap(DC, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
    // Select bitmap
    Result.OldBitmap := SelectObject(Result.BufferDC, Result.Bitmap);
    // Move viewport
    GetWindowOrgEx(Result.BufferDC, Result.WindowOrg);
    SetWindowOrgEx(Result.BufferDC,
      Result.Rect.Left + Result.WindowOrg.X, Result.Rect.Top + Result.WindowOrg.Y, nil);
  end;
end;

procedure TEsCustomControl.EndCustomBufferedPaint(PaintBuffer: TPaintBuffer);
begin
  if FIsCachedBuffer or FIsFullSizeBuffer then
  begin
    // Full size draw
    BitBlt(PaintBuffer.DC,
      PaintBuffer.Rect.Left, PaintBuffer.Rect.Top,
      PaintBuffer.Rect.Right - PaintBuffer.Rect.Left, PaintBuffer.Rect.Bottom - PaintBuffer.Rect.Top,
      PaintBuffer.BufferDC,
      PaintBuffer.Rect.Left, PaintBuffer.Rect.Top,
      SRCCOPY);
  end else
  begin
    SetWindowOrgEx(PaintBuffer.BufferDC, PaintBuffer.WindowOrg.X, PaintBuffer.WindowOrg.Y, nil);
    BitBlt(PaintBuffer.DC,
      PaintBuffer.Rect.Left, PaintBuffer.Rect.Top,
      PaintBuffer.Rect.Right - PaintBuffer.Rect.Left, PaintBuffer.Rect.Bottom - PaintBuffer.Rect.Top,
      PaintBuffer.BufferDC,
      0, 0,
      SRCCOPY);

   // SetBkColor(PaintBuffer.DC, RGB(127,255,255));
   // DrawFocusRect(PaintBuffer.DC, PaintBuffer.Rect);
  end;
  // Select old bitmap to buffer
  SelectObject(PaintBuffer.BufferDC, PaintBuffer.OldBitmap);
  // Free bitmap if need
  if PaintBuffer.Bitmap <> 0 then
    DeleteObject(PaintBuffer.Bitmap);
  // Delete buffer dc if need
  if PaintBuffer.BufferDC <> PaintBuffer.DC then
    DeleteDC(PaintBuffer.BufferDC);
end;

function TEsCustomControl.IsBufferedChildrenStored: Boolean;
begin
  Result := not ParentBufferedChildren;
end;

{$IFDEF STYLE_ELEMENTS}
procedure TEsCustomControl.UpdateStyleElements;
begin
  Invalidate;
end;
{$ENDIF}

procedure TEsCustomControl.SetBufferedChildren(const Value: Boolean);
begin
  if Value <> FBufferedChildren then
  begin
    FBufferedChildren := Value;
    FParentBufferedChildren := False;
    NotifyControls(CM_PARENT_BUFFEREDCHILDRENS_CHANGED);
  end;
end;

procedure TEsCustomControl.SetIsCachedBackground(Value: Boolean);
begin
  if Value <> FIsCachedBackground then
  begin
    FIsCachedBackground := Value;
    DeleteBitmapCache;
  end;
end;

procedure TEsCustomControl.SetIsCachedBuffer(Value: Boolean);
begin
  if Value <> FIsCachedBuffer then
  begin
    FIsCachedBuffer := Value;
    DeleteBitmapCache;
  end;
end;

procedure TEsCustomControl.SetIsDrawHelper(const Value: Boolean);
begin
  if Value <> FIsDrawHelper then
  begin
    FIsDrawHelper := Value;
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TEsCustomControl.SetIsFullSizeBuffer(const Value: Boolean);
begin
  if Value <> FIsFullSizeBuffer then
  begin
    FIsFullSizeBuffer := Value;
  end;
  DeleteBitmapCache;
end;

procedure TEsCustomControl.SetIsOpaque(const Value: Boolean);
begin
  if Value <> (csOpaque in ControlStyle) then
  begin
    if Value then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];
    Invalidate;
  end;
end;

procedure TEsCustomControl.SetParentBufferedChildren(const Value: Boolean);
begin
  if Value <> FParentBufferedChildren then
  begin
    FParentBufferedChildren := Value;

    if (Parent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENT_BUFFEREDCHILDRENS_CHANGED, 0, 0);
  end;
end;

procedure TEsCustomControl.SetTransparent(const Value: Boolean);
begin
  ParentBackground := Value;
end;

procedure TEsCustomControl.UpdateText;
begin
end;

procedure TEsCustomControl.UpdateBackground(Repaint: Boolean);
begin
  // Delete cache background
  BitmapDeleteAndNil(CacheBackground);

  if Repaint then
    Invalidate;
end;

procedure TEsCustomControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if DoubleBuffered then
  begin
    inherited;
  end else
  begin
    if ControlCount <> 0 then
      DrawBackgroundForOpaqueControls(Message.DC);
    Message.Result := 1;
  end;
end;

procedure TEsCustomControl.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
  RectClient, RectWindow: TRect;
  RGBColor, OldColor: TColor;
begin
  if (BevelKind = bkNone) and (BorderWidth > 0) then
  begin
    DC := GetWindowDC(Handle);
    try
      Winapi.Windows.GetClientRect(Handle, RectClient);
      Winapi.Windows.GetWindowRect(Handle, RectWindow);
      MapWindowPoints(0, Handle, RectWindow, 2);
      OffsetRect(RectClient, -RectWindow.Left, -RectWindow.Top);
      OffsetRect(RectWindow, -RectWindow.Left, -RectWindow.Top);
      ExcludeClipRect(DC, RectClient.Left, RectClient.Top, RectClient.Right, RectClient.Bottom);

      if not IsStyledBorderControl(Self) then
      begin
        FillRect(DC, RectWindow, Brush.Handle)
      end else
      begin
        {$IFDEF STYLE_NAME}
        RGBColor := StyleServices(Self).GetSystemColor(Color);
        {$ELSE}
        RGBColor := StyleServices.GetSystemColor(Color);
        {$ENDIF}
        if RGBColor and $FF000000 <> 0 then
          RGBColor := ColorToRGB(Color);

        OldColor := SetDCBrushColor(DC, RGBColor);
        FillRect(DC, RectWindow, GetStockObject(DC_BRUSH));
        SetDCBrushColor(DC, OldColor);
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end else
  begin
    Inherited;
  end;
end;

procedure TEsCustomControl.CustomPaintHandler(var Message: TWMPaint);
var
  // PAINT
  DC: HDC;
  PaintStruct: TPaintStruct;
  // DWM
  DwmBuffer: HPAINTBUFFER;
  DwmDC: HDC;
  // CUSTOM
  CustomBuffer: TPaintBuffer;
begin
  DC := BeginPaint(Handle, PaintStruct);
  try
    if DwmCompositionEnabled and not (IsFullSizeBuffer or IsCachedBuffer) then
    begin
      DwmBuffer := BeginBufferedPaint(DC, PaintStruct.rcPaint, BPBF_COMPOSITED, nil, DwmDC);
      if DwmBuffer <> 0 then
      begin
        try
          // Alt:
          // Perform(WM_ERASEBKGND, DwmDC, DwmDC);
          // Perform(WM_PRINTCLIENT, DwmDC, PRF_CLIENT);
          Message.DC := DwmDC;
          inherited PaintHandler(Message);
          Message.DC := 0;
        finally
          EndBufferedPaint(DwmBuffer, True);
        end;
      end;
    end else
    begin
      CustomBuffer := BeginCustomBufferedPaint(DC, PaintStruct.rcPaint);
      try
        // Alt:
        // Perform(WM_ERASEBKGND, MemDC, MemDC);
        // Message.DC := MemDC;
        // if IsCustomStyleActive then WndProc(TMessage(Message)) else WMPaint(Message);
        // Message.DC := 0;
        Message.DC := CustomBuffer.BufferDC;
        inherited PaintHandler(Message);
        Message.DC := 0;
      finally
        EndCustomBufferedPaint(CustomBuffer);
      end;
    end;
  finally
    EndPaint(Handle, PaintStruct);
  end;
end;

procedure TEsCustomControl.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];

  // Buffered childen aviable only for not DoubleBuffered controls.
  // If Message.DC <> 0 then no buffering is required, the control is already buffered.
  if (BufferedChildren and (not DoubleBuffered) and
    not (csDesigning in ComponentState) { <- fix for designer selection}) and
    (Message.DC = 0) then
  begin
    CustomPaintHandler(Message);// My custom PaintHandler
  end else
  begin
    inherited;
  end;

  ControlState := ControlState - [csCustomPaint];
end;

procedure TEsCustomControl.PaintWindow(DC: HDC);
  procedure DoDrawBackground(DC: HDC; Rect: TRect);
  var
    CacheDC: HDC;
    OldBitmap: HBITMAP;
  begin
    if ParentBackground then
    begin
      if FIsCachedBackground then
      begin
        // Make backround bitmap if need
        if CacheBackground = 0 then
        begin
          CacheDC := CreateCompatibleDC(DC);
          CacheBackground := CreateCompatibleBitmap(DC, ClientWidth, ClientHeight);
          OldBitmap := 0;
          try
            OldBitmap := SelectObject(CacheDC, CacheBackground);
            DrawBackground(CacheDC);
          finally
            SelectObject(CacheDC, OldBitmap);
          end;
          DeleteDC(CacheDC);
        end;
        // Draw background bitmap from cache
        CacheDC := CreateCompatibleDC(DC);
        OldBitmap := 0;
        try
          OldBitmap := SelectObject(CacheDC, CacheBackground);
          BitBlt(DC,
            Rect.Left, Rect.Top,
            Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
            CacheDC,
            Rect.Left, Rect.Top,
            SRCCOPY);
        finally
          SelectObject(CacheDC, OldBitmap);
        end;
        DeleteDC(CacheDC);
      end else
        DrawBackground(DC);
    end else
      FillBackground(DC);
  end;

  procedure DoDraw(DC: HDC);
  begin
    FCanvas.Lock;
    try
      Canvas.Handle := DC;
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
  end;

var
  UpdateRect: TRect;
  // DWM
  DwmBuffer: HPAINTBUFFER;
  DwmDC: HDC;
  // CUSTOM
  CustomBuffer: TPaintBuffer;
begin
  // Get update rect for DC
  if GetClipBox(DC, UpdateRect) = ERROR then
    UpdateRect := ClientRect;

  // If the control is already buffered with BufferedChildren or DoubleBuffered then
  //  no additional buffering is required
  if BufferedChildren or DoubleBuffered or
    (csDesigning in ComponentState) { <- fix for designer selection} then
  begin
    DoDrawBackground(DC, UpdateRect);
    DoDraw(DC);
  end else
  begin
    if (DwmCompositionEnabled and not (IsFullSizeBuffer or IsCachedBuffer)) then
    begin
      DwmBuffer := BeginBufferedPaint(DC, UpdateRect, BPBF_COMPOSITED, nil, DwmDC);
      if DwmBuffer <> 0 then
      begin
        try
          DoDrawBackground(DwmDC, UpdateRect);
          DoDraw(DwmDC);
        finally
          EndBufferedPaint(DwmBuffer, True);
        end;
      end;
    end else
    begin
      CustomBuffer := BeginCustomBufferedPaint(DC, UpdateRect);
      try
        DoDrawBackground(CustomBuffer.BufferDC, UpdateRect);
        DoDraw(CustomBuffer.BufferDC);
      finally
        EndCustomBufferedPaint(CustomBuffer);
      end;
    end;
  end;
end;

procedure TEsCustomControl.WMSize(var Message: TWMSize);
begin
  DeleteBitmapCache;
  inherited;
end;

procedure TEsCustomControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if not (csOpaque in ControlStyle) and ParentBackground then
  begin
    DeleteBitmapCache;
    Invalidate;
  end;
  inherited;
end;

{ TEsBaseLayout }

constructor TEsBaseLayout.Create(AOwner: TComponent);
begin
  inherited;

  FBufferedChildren := True;
end;

procedure TEsBaseLayout.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if BorderWidth <> 0 then
  begin
    InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));
  end;
end;

procedure TEsBaseLayout.AdjustContentRect(var Rect: TRect);
begin
  // nope
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

  Result.Left := Result.Left + ContentMargins.Left;
  Result.Top := Result.Top + ContentMargins.Top;
  Result.Right := Result.Right - ContentMargins.Right;
  Result.Bottom := Result.Bottom - ContentMargins.Bottom;

  AdjustContentRect(Result);

  {$IFDEF TEST_CONTROL_CONTENT_RECT}
  if Result.Left > Result.Right then
    Result.Right := Result.Left;
  if Result.Top > Result.Bottom then
    Result.Bottom := Result.Top;
  {$ENDIF}
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

  {$IFDEF TEST_CONTROL_CONTENT_RECT}
  if Result.Left > Result.Right then
    Result.Right := Result.Left;
  if Result.Top > Result.Bottom then
    Result.Bottom := Result.Top;
  {$ENDIF}
end;

destructor TEsGraphicControl.Destroy;
begin
  FPadding.Free;
  inherited;
end;

{$IFDEF STYLE_ELEMENTS}
procedure TEsGraphicControl.UpdateStyleElements;
begin
  Invalidate;
end;
{$ENDIF}

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
  if (FPadding.Left = 0) and (FPadding.Top = 0) and
     (FPadding.Right = 0) and (FPadding.Bottom = 0) then
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

end.
