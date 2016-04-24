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
unit ES.Layouts;

interface

uses
  Winapi.Messages, Vcl.Controls, System.Classes, System.Types, Vcl.Graphics, ES.BaseControls;

type
  TEsCustomLayout = class(TEsBaseLayout)
  private
    FLocked: Boolean;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property UseDockManager default True;
  public
    constructor Create(AOwner: TComponent); override;
    property Color default clBtnFace;
    property DockManager;
    property Locked: Boolean read FLocked write FLocked default False;
  end;

  TEsLayout = class(TEsCustomLayout)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BufferedChildrens;// TEsCustomControl
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
    property ParentBufferedChildrens;// TEsCustomControl
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
    {$if CompilerVersion > 23}
    property StyleElements;
    {$ifend}
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

implementation

uses
  ES.ExGraphics;

procedure TEsCustomLayout.CMIsToolControl(var Message: TMessage);
begin
  if not FLocked then Message.Result := 1;
end;

constructor TEsCustomLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csParentBackground, csDoubleClicks, csReplicatable, csPannable, csGestures];
  Width := 185;
  Height := 41;
  UseDockManager := True;
end;

procedure TEsCustomLayout.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // nope now
end;

end.
