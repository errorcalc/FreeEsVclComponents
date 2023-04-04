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
unit ES.PaintBox;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
   System.Classes, ES.BaseControls;

type
  TEsPaintBox = class(TEsCustomControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property OnStartDock;
    property OnStartDrag;
    // additional props
    property ParentBackground;
    property OnPaint;
    property IsCachedBuffer;
    property IsOpaque;
  end;

implementation

uses
  Vcl.Graphics, System.Types, ES.ExGraphics;

{ TEsPaintBox }

constructor TEsPaintBox.Create(AOwner: TComponent);
begin
  inherited;
  Width := 105;
  Height := 105;
end;

procedure TEsPaintBox.Paint;
  procedure DrawDesign;
  var
    TempRect: TRect;
    TempString: string;
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
    Canvas.Font.Color := clBlue;
    TempRect := ClientRect;
    TempString := '[TEsPaintBox]';
    Canvas.TextRect(TempRect, TempString,
      [tfCenter, tfVerticalCenter, tfSingleLine]);
  end;

begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if csDesigning in ComponentState then
    DrawDesign;
end;

end.
