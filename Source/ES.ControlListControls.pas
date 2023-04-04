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
unit ES.ControlListControls;

interface

{$SCOPEDENUMS ON}
{$I EsDefines.inc}

{$IFDEF VER350UP}

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Themes,
  Vcl.StdCtrls, WinApi.Windows, Vcl.Styles, WinApi.Messages, Vcl.Graphics,
  Vcl.ControlList;

type
  TCheckBoxAlignment = (Left, Right, CenterNoCaption);

  [ObservableMember('Checked')]
  TEsControlListCheckBox = class(TControlListControl)
  private
    FChecked: Boolean;
    FCheckBoxSize: TSize;
    FReadOnly: Boolean;
    FReadOnlyObserver: Boolean;
    FAlignment: TCheckBoxAlignment;
    procedure ObserverToggle(const AObserver: IObserver; const Value: Boolean);
    procedure CalcCheckBoxSize(Dpi: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure dddd(var Message: TMessage); message CM_UNTHEMECONTROL;
    procedure SetAlignment(const Value: TCheckBoxAlignment);
  protected
    procedure MouseEnter(); override;
    procedure MouseLeave(); override;
    function GetState(): TControlListControlState; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(); override;
    procedure ChangeScale(M, D: Integer; IsDpiChange: Boolean); override;
    // Observer
    function CanObserve(const ID: Integer): Boolean; override;
    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click(); override;
  published
    property Action;
    property Align;
    property Alignment: TCheckBoxAlignment read FAlignment write SetAlignment default TCheckBoxAlignment.Left;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Constraints;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ShowHint;
    property Visible;
    property StyleElements;
    property StyleName;
    property OnClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

{$ENDIF}

implementation

{$IFDEF VER350UP}

{ TControlListCheckBox }

constructor TEsControlListCheckBox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle - [csDoubleClicks];
  FReadOnly := False;
  FChecked := False;

  FReadOnlyObserver := False;
  Width := 97;
  Height := 17;
end;

procedure TEsControlListCheckBox.CalcCheckBoxSize(Dpi: Integer);
var
  Bitmap: TBitmap;
  TestRect: TRect;
  Style: TCustomStyleServices;
begin
  if seClient in StyleElements then
    Style := StyleServices(Self)
  else
    Style := TStyleManager.SystemStyle;

  if Style.Enabled then
  begin
    TestRect := Rect(0, 0, 20, 20);
    if not Style.GetElementSize(Canvas.Handle, Style.GetElementDetails(tbCheckBoxCheckedNormal),
      TestRect, esActual, FCheckBoxSize, Dpi) then
    begin
      FCheckBoxSize.cx := 13;
      FCheckBoxSize.cy := 13;
    end;
  end else
  begin
    Bitmap := TBitmap.Create();
    try
      Bitmap.Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      FCheckBoxSize.cx := Bitmap.Width div 4;
      FCheckBoxSize.cy := Bitmap.Height div 3;
    finally
      Bitmap.Free();
    end;
  end;
end;

function TEsControlListCheckBox.CanObserve(const ID: Integer): Boolean;
begin
  case ID of
    TObserverMapping.EditLinkID,
    TObserverMapping.ControlValueID:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TEsControlListCheckBox.ChangeScale(M, D: Integer; IsDpiChange: Boolean);
begin
  inherited;
  if IsDpiChange then
    CalcCheckBoxSize(M);
end;

procedure TEsControlListCheckBox.Click();
begin
  if (not FReadOnlyObserver) and (not FReadOnly) then
  begin
    FChecked := not FChecked;
    TLinkObservers.ControlChanged(Self);
  end;

  inherited;

  Invalidate();
end;

procedure TEsControlListCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  Invalidate();
end;

procedure TEsControlListCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  Invalidate();
end;

procedure TEsControlListCheckBox.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  CalcCheckBoxSize(CurrentPPI);
end;

procedure TEsControlListCheckBox.dddd(var Message: TMessage);
begin
CalcCheckBoxSize(CurrentPPI);
end;

procedure TEsControlListCheckBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate();
end;

function TEsControlListCheckBox.GetState(): TControlListControlState;
begin
  Result := inherited;

  if (ControlList = nil) and Enabled then
  begin
    if FMouseDown then
      Result := clstPressed
    else
    if FMouseInControl then
      Result := clstHot;
  end;
end;

procedure TEsControlListCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown := True;
  InvalidateWithItem();
end;

procedure TEsControlListCheckBox.MouseEnter();
begin
  inherited;
  InvalidateWithItem();
end;

procedure TEsControlListCheckBox.MouseLeave();
begin
  inherited;
  InvalidateWithItem();
end;

procedure TEsControlListCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown := False;
  InvalidateWithItem();
end;

procedure TEsControlListCheckBox.ObserverAdded(const ID: Integer; const Observer: IObserver);
begin
  if ID = TObserverMapping.EditLinkID then
    Observer.OnObserverToggle := ObserverToggle;
end;

procedure TEsControlListCheckBox.ObserverToggle(const AObserver: IObserver; const Value: Boolean);
var
  EditLinkObserver: IEditLinkObserver;
begin
  if Value then
  begin
    if Supports(AObserver, IEditLinkObserver, EditLinkObserver) then
      FReadOnlyObserver := EditLinkObserver.IsReadOnly;
  end else
    FReadOnlyObserver := True;
end;

procedure TEsControlListCheckBox.Paint();
const
  FaceDetail: array [Boolean, TControlListControlState] of TThemedButton = (
    (tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled),
    (tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled)
  );
  InactiveFaceDetail: array [Boolean, TControlListControlState] of TThemedButton = (
    (tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedDisabled),
    (tbCheckBoxCheckedNormal, tbCheckBoxCheckedNormal, tbCheckBoxCheckedNormal, tbCheckBoxCheckedDisabled)
  );
  Spacing = 3;
var
  Details: TThemedElementDetails;
  Style: TCustomStyleServices;
  DrawFlags: Integer;
  CheckRect: TRect;
  Detail: TThemedButton;
  TextFormat: TTextFormat;
  TextRect: TRect;
  TextSpacing: Integer;
  Temp: string;
  ThemeTextColor: TColor;
begin
  inherited;

  // calc check size if need
  // Since CM_STYLECHANGED is sometimes not called need permanent size calculation,
  // this is a temporary fix until the bug in the VCL is fixed
  // ok code:
  // if FCheckBoxSize.IsZero then
  //   CalcCheckBoxSize(CurrentPPI);
  // fix:
  CalcCheckBoxSize(CurrentPPI);

  // calc check position
  CheckRect.Top := ClientHeight div 2 - FCheckBoxSize.cy div 2;
  CheckRect.Bottom := CheckRect.Top + FCheckBoxSize.cy;
  case FAlignment of
    TCheckBoxAlignment.Left:
      CheckRect.Left := 0;
    TCheckBoxAlignment.Right:
      CheckRect.Left := ClientWidth - FCheckBoxSize.cx;
    TCheckBoxAlignment.CenterNoCaption:
      CheckRect.Left := ClientWidth div 2 - FCheckBoxSize.cx div 2;
  end;
  CheckRect.Right := CheckRect.Left + FCheckBoxSize.cx;

  // draw check
  if seClient in StyleElements then
    Style := StyleServices(Self)
  else
    Style := TStyleManager.SystemStyle;

  if Style.Enabled then
  begin
    if not FReadOnly then
      Detail := FaceDetail[FChecked <> False, State]
    else
      Detail := InactiveFaceDetail[FChecked <> False, State];

    Details := Style.GetElementDetails(Detail);
    Style.DrawElement(Canvas.Handle, Details, CheckRect, nil, CurrentPPI);
  end else
  begin
    DrawFlags := DFCS_BUTTONCHECK;

    if Checked then
      DrawFlags := DrawFlags or DFCS_CHECKED;

    if Enabled then
    begin
      if (State = clstPressed) and not ReadOnly then
        DrawFlags := DrawFlags or DFCS_PUSHED;
    end else
    begin
      DrawFlags := DrawFlags or DFCS_INACTIVE;
    end;

    DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, DrawFlags);
  end;

  // draw text
  if Alignment <> TCheckBoxAlignment.CenterNoCaption then
  begin
    if seFont in StyleElements then
      Style := StyleServices(Self)
    else
      Style := TStyleManager.SystemStyle;

    // calc text rect
    TextSpacing := MulDiv(Spacing, CurrentPPI, 96);
    TextRect.Top := 0;
    TextRect.Bottom := ClientHeight;
    if FAlignment = TCheckBoxAlignment.Left then
    begin
      TextRect.Left := CheckRect.Right + TextSpacing;
      TextRect.Right := ClientWidth;
    end else
    begin
      TextRect.Left := 0;
      TextRect.Right := CheckRect.Left - TextSpacing;
    end;

    TextFormat := [tfSingleLine, tfVerticalCenter, tfExpandTabs, tfExternalLeading, tfWordEllipsis];
    if Alignment = TCheckBoxAlignment.Left then
      Include(TextFormat, tfLeft)
    else
      Include(TextFormat, tfRight);

    // draw
    if Style.Enabled then
    begin
      Canvas.Font := Font;

      //if (seFont in StyleElements) and Style.GetElementColor(Details, ecTextColor, ThemeTextColor) then
      //  Canvas.Font.Color := ThemeTextColor;
      if (not Enabled) and Style.GetElementColor(Details, ecTextColor, ThemeTextColor) then
        Canvas.Font.Color := ThemeTextColor;

      Canvas.Font.Color := Style.GetSystemColor(Canvas.Font.Color);

      Style.DrawText(Canvas.Handle, Details, Caption, TextRect, TextFormat, Canvas.Font.Color);
    end else
    begin
      Canvas.Font := Font;
      Canvas.Brush.Style := bsClear;
      Temp := Caption;
      Canvas.TextRect(TextRect, Temp, TextFormat);
    end;
  end;
end;

procedure TEsControlListCheckBox.SetAlignment(const Value: TCheckBoxAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate();
  end;
end;

procedure TEsControlListCheckBox.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
  end;
end;

procedure TEsControlListCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Invalidate();
  end;
end;

{$ENDIF}

end.
