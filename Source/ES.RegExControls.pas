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
unit ES.RegexControls;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, WinApi.Messages, Vcl.Controls, Vcl.StdCtrls, System.SysUtils, Vcl.Graphics,
  Vcl.ExtCtrls;

const
  DefaultColorIntensity = 50;

type
  TIndicateState = (All, Valid, Invalid, None);
  TColorIntensity = 0..240;

  IRegexIndicator = interface
  ['{0783F276-F4D7-4669-BE9F-113180E51EC3}']
    procedure SetPattern(const Value: string);
    function GetPattern: string;
    procedure SetText(const Value: string);
    function GetText: string;
  end;

  TRegexIndicator = class
  private type
    TOpenControl = class(TWinControl);
  public type
    TGetDefaultColor = reference to function: TColor;
    TChangeEvent = reference to procedure;
  private
    FColorValid: TColor;
    FAllowNeutral: Boolean;
    FColorIntensity: TColorIntensity;
    FColorInvalid: TColor;
    FPattern: string;
    Control: TOpenControl;
    FOnChange: TChangeEvent;
    FOnGetDefaultColor: TGetDefaultColor;
    FIndicateState: TIndicateState;
    procedure SetAllowNeutral(const Value: Boolean);
    procedure SetColorIntensity(const Value: TColorIntensity);
    procedure SetColorInvalid(const Value: TColor);
    procedure SetColorValid(const Value: TColor);
    procedure SetPattern(const Value: string);
    procedure SetIndicateState(const Value: TIndicateState);
  protected
    procedure Change;
    function GetColor: TColor; virtual;
    function GetDefaultColor: TColor;
  public
    constructor Create(Control: TControl); virtual;
    destructor Destroy; override;
    //
    function IsValid: Boolean; virtual;
    function IsNeutral: Boolean; virtual;
    procedure Indicate;
    //
    property ColorValid: TColor read FColorValid write SetColorValid default clDefault;
    property ColorInvalid: TColor read FColorInvalid write SetColorInvalid default clDefault;
    property ColorIntensity: TColorIntensity read FColorIntensity write SetColorIntensity default DefaultColorIntensity;
    property Pattern: string read FPattern write SetPattern;
    // modes
    property AllowNeutral: Boolean read FAllowNeutral write SetAllowNeutral stored True;
    property IndicateState: TIndicateState read FIndicateState write SetIndicateState stored True;
    //
    property Color: TColor read GetColor;
    //
    property OnChange: TChangeEvent read FOnChange write FOnChange;
    property OnGetDefaultColor: TGetDefaultColor read FOnGetDefaultColor write FOnGetDefaultColor;
  end;
  TIndicatorClass = class of TRegexIndicator;

  // TEdit
  TEsRegexEdit = class(TEdit, IRegexIndicator)
  private
    Indicator: TRegexIndicator;
    procedure SetColorValid(const Value: TColor);
    procedure SetColorInvalid(const Value: TColor);
    procedure SetPattern(const Value: string);
    procedure SetAllowNeutral(const Value: Boolean);
    procedure SetColorIntensity(const Value: TColorIntensity);
    function GetAllowNeutral: Boolean;
    function GetColorIntensity: TColorIntensity;
    function GetColorInvalid: TColor;
    function GetColorValid: TColor;
    function GetPattern: string;
    //
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    //
    class constructor Create;
    class destructor Destroy;
    function GetIndicatorState: TIndicateState;
    procedure SetIndicateState(const Value: TIndicateState);
    { IRegexIndicator }
    procedure SetText(const Value: string);
    function GetText: string;
  protected
    procedure Loaded; override;
    procedure Change; override;
    function GetIndicatorClass: TIndicatorClass; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsValid: Boolean;
    function IsNeutral: Boolean;
  published
    property Pattern: string read GetPattern write SetPattern;
    property ColorValid: TColor read GetColorValid write SetColorValid default clDefault;
    property ColorInvalid: TColor read GetColorInvalid write SetColorInvalid default clDefault;
    property ColorIntensity: TColorIntensity read GetColorIntensity write SetColorIntensity default DefaultColorIntensity;
    property AllowNeutral: Boolean read GetAllowNeutral write SetAllowNeutral stored True;
    property IndicateState: TIndicateState read GetIndicatorState write SetIndicateState stored True;
  end;
  // TEdit style hook
  TEsRegexEditStyleHook = class(TEditStyleHook)
  strict protected
  {$IFNDEF VER350UP}
    procedure PaintBackground(Canvas: TCanvas); override;
  {$ENDIF}
  {$IFDEF VER350UP}
    procedure WndProc(var Message: TMessage); override;
  {$ENDIF}
  end;

  // TButtonedEdit
  TEsRegexButtonedEdit = class(TButtonedEdit, IRegexIndicator)
  private
    Indicator: TRegexIndicator;
    FColor: TColor;
    procedure SetColorValid(const Value: TColor);
    procedure SetColorInvalid(const Value: TColor);
    procedure SetPattern(const Value: string);
    procedure SetAllowNeutral(const Value: Boolean);
    procedure SetColorIntensity(const Value: TColorIntensity);
    procedure SetColor(const Value: TColor);
    function GetAllowNeutral: Boolean;
    function GetColorIntensity: TColorIntensity;
    function GetColorInvalid: TColor;
    function GetColorValid: TColor;
    function GetPattern: string;
    //
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    //
    class constructor Create;
    class destructor Destroy;
    function GetIndicatorState: TIndicateState;
    procedure SetIndicateState(const Value: TIndicateState);
    { IRegexIndicator }
    procedure SetText(const Value: string);
    function GetText: string;
  protected
    procedure Loaded; override;
    procedure Change; override;
    function GetIndicatorClass: TIndicatorClass; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsValid: Boolean;
    function IsNeutral: Boolean;
  published
    property Color: TColor read FColor write SetColor default clWindow;
    property Pattern: string read GetPattern write SetPattern;
    property ColorValid: TColor read GetColorValid write SetColorValid default clDefault;
    property ColorInvalid: TColor read GetColorInvalid write SetColorInvalid default clDefault;
    property ColorIntensity: TColorIntensity read GetColorIntensity write SetColorIntensity default DefaultColorIntensity;
    property AllowNeutral: Boolean read GetAllowNeutral write SetAllowNeutral stored True;
    property IndicateState: TIndicateState read GetIndicatorState write SetIndicateState stored True;
  end;

  // TLabeledEdit
  TEsRegexLabeledEdit = class(TLabeledEdit, IRegexIndicator)
  private
    Indicator: TRegexIndicator;
    procedure SetColorValid(const Value: TColor);
    procedure SetColorInvalid(const Value: TColor);
    procedure SetPattern(const Value: string);
    procedure SetAllowNeutral(const Value: Boolean);
    procedure SetColorIntensity(const Value: TColorIntensity);
    function GetAllowNeutral: Boolean;
    function GetColorIntensity: TColorIntensity;
    function GetColorInvalid: TColor;
    function GetColorValid: TColor;
    function GetPattern: string;
    //
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    //
    class constructor Create;
    class destructor Destroy;
    function GetIndicatorState: TIndicateState;
    procedure SetIndicateState(const Value: TIndicateState);
    { IRegexIndicator }
    procedure SetText(const Value: string);
    function GetText: string;
  protected
    procedure Loaded; override;
    procedure Change; override;
    function GetIndicatorClass: TIndicatorClass; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsValid: Boolean;
    function IsNeutral: Boolean;
  published
    property Pattern: string read GetPattern write SetPattern;
    property ColorValid: TColor read GetColorValid write SetColorValid default clDefault;
    property ColorInvalid: TColor read GetColorInvalid write SetColorInvalid default clDefault;
    property ColorIntensity: TColorIntensity read GetColorIntensity write SetColorIntensity default DefaultColorIntensity;
    property AllowNeutral: Boolean read GetAllowNeutral write SetAllowNeutral stored True;
    property IndicateState: TIndicateState read GetIndicatorState write SetIndicateState stored True;
  end;

implementation

uses
  System.Math, System.UITypes, Es.ExGraphics, Es.Utils, System.RegularExpressionsCore, System.RegularExpressions,
  Winapi.Windows, Vcl.Themes;

function GetNormalEditColor(Control: TWinControl): TColor;
begin
  if IsStyledClientControl(Control) then
    Result := GetControlStyle(Control).GetStyleColor(scEdit)
  else
    Result := TEdit(Control).Color;

  Result := ColorToRgb(Result);
end;

{ TRegExIndicator }

procedure TRegexIndicator.Change;
begin
  if Assigned(FOnChange) then
    FOnChange;
end;

constructor TRegexIndicator.Create(Control: TControl);
begin
  FColorValid := clDefault;
  FColorInvalid := clDefault;
  FColorIntensity := DefaultColorIntensity;
  FAllowNeutral := True;
  FIndicateState := TIndicateState.All;
  Self.Control := TOpenControl(Control);
end;

destructor TRegexIndicator.Destroy;
begin
  inherited;
end;

function TRegexIndicator.GetColor: TColor;
var
  Hue: Integer;
  Intensity: Integer;
begin
  if IsNeutral then
    Exit(GetNormalEditColor(Control));

  if IsValid then
  begin
    if (IndicateState <> TIndicateState.All) and (IndicateState <> TIndicateState.Valid) then
      Exit(GetNormalEditColor(Control));
    if ColorValid = clDefault then
      Hue := 80// green
    else
      Exit(ClientColorToRgb(ColorValid, Control));
  end else
  begin
    if (IndicateState <> TIndicateState.All) and (IndicateState <> TIndicateState.Invalid) then
      Exit(GetNormalEditColor(Control));
    if ColorInvalid = clDefault then
      Hue := 0// red
    else
      Exit(ClientColorToRgb(ColorInvalid, Control));
  end;

  Intensity := IfThen(Control.Enabled, ColorIntensity, ColorIntensity div 2);

  Result := HSLToColor(Hue, 240,
    EnsureRange(GetLuminanceColor(GetDefaultColor), Max(0, Intensity), 240 - Intensity));
end;

function TRegexIndicator.GetDefaultColor: TColor;
begin
  if Assigned(FOnGetDefaultColor) then
    Result := FOnGetDefaultColor
  else
    Result := clNone;
end;

procedure TRegexIndicator.Indicate;
begin
  Change;
end;

function TRegexIndicator.IsNeutral: Boolean;
begin
  Result := (Pattern = '') or (AllowNeutral and (Control.Text = ''));
end;

function TRegexIndicator.IsValid: Boolean;
begin
  if Pattern = '' then
    Exit(False);

  try
    Result := TRegEx.IsMatch(Control.Text, Pattern);
  except
    on ERegularExpressionError do Result := False;
  end;
end;

procedure TRegexIndicator.SetAllowNeutral(const Value: Boolean);
begin
  if AllowNeutral <> Value then
  begin
    FAllowNeutral := Value;
    Change;
  end;
end;

procedure TRegexIndicator.SetColorIntensity(const Value: TColorIntensity);
begin
  if FColorIntensity <> Value then
  begin
    FColorIntensity := Value;
    Change;
  end;
end;

procedure TRegexIndicator.SetColorInvalid(const Value: TColor);
begin
  if FColorInvalid <> Value then
  begin
    FColorInvalid := Value;
    Change;
  end;
end;

procedure TRegexIndicator.SetColorValid(const Value: TColor);
begin
  if FColorValid <> Value then
  begin
    FColorValid := Value;
    Change;
  end;
end;

procedure TRegexIndicator.SetIndicateState(const Value: TIndicateState);
begin
  if FIndicateState <> Value then
  begin
    FIndicateState := Value;
    Change;
  end;
end;

procedure TRegexIndicator.SetPattern(const Value: string);
begin
  if FPattern <> Value then
  begin
    FPattern := Value;
    Change;
  end;
end;

{ TEsRegExEdit }

class constructor TEsRegexEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TEsRegexEdit, TEsRegexEditStyleHook);
end;

class destructor TEsRegexEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TEsRegexEdit, TEsRegexEditStyleHook);
end;

constructor TEsRegexEdit.Create(AOwner: TComponent);
begin
  inherited;
  Indicator := GetIndicatorClass.Create(Self);
  Indicator.OnChange := procedure
    begin
      Perform(CM_COLORCHANGED, 0, 0);
    end;
  Indicator.OnGetDefaultColor := function: TColor
    begin
      Result := GetNormalEditColor(Self);
    end;
end;

destructor TEsRegexEdit.Destroy;
begin
  Indicator.Free;
  inherited;
end;

function TEsRegexEdit.GetAllowNeutral: Boolean;
begin
  Result := Indicator.AllowNeutral;
end;

function TEsRegexEdit.GetColorIntensity: TColorIntensity;
begin
  Result := Indicator.ColorIntensity;
end;

function TEsRegexEdit.GetColorInvalid: TColor;
begin
  Result := Indicator.ColorInvalid;
end;

function TEsRegexEdit.GetColorValid: TColor;
begin
  Result := Indicator.ColorValid;
end;

function TEsRegexEdit.GetIndicatorClass: TIndicatorClass;
begin
  Result := TRegexIndicator;
end;

function TEsRegexEdit.GetIndicatorState: TIndicateState;
begin
  Result := Indicator.IndicateState;
end;

function TEsRegexEdit.GetPattern: string;
begin
  Result := Indicator.Pattern;
end;

function TEsRegexEdit.GetText: string;
begin
  Result := Text;
end;

procedure TEsRegexEdit.Change;
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Brush.Color := Indicator.Color;
end;

procedure TEsRegexEdit.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Indicator.Indicate;
end;

function TEsRegexEdit.IsNeutral: Boolean;
begin
  Result := Indicator.IsNeutral;
end;

function TEsRegexEdit.IsValid: Boolean;
begin
  Result := Indicator.IsValid;
end;

procedure TEsRegexEdit.Loaded;
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexEdit.SetAllowNeutral(const Value: Boolean);
begin
  Indicator.AllowNeutral := Value;
end;

procedure TEsRegexEdit.SetColorIntensity(const Value: TColorIntensity);
begin
  Indicator.ColorIntensity := Value;
end;

procedure TEsRegexEdit.SetColorValid(const Value: TColor);
begin
  Indicator.ColorValid := Value;
end;

procedure TEsRegexEdit.SetIndicateState(const Value: TIndicateState);
begin
  Indicator.IndicateState := Value;
end;

procedure TEsRegexEdit.SetPattern(const Value: string);
begin
  Indicator.Pattern := Value;
end;

procedure TEsRegexEdit.SetText(const Value: string);
begin
  Text := Value;
end;

procedure TEsRegexEdit.SetColorInvalid(const Value: TColor);
begin
  Indicator.ColorInvalid := Value;
end;

{ TEsRegExEditStyleHook }

{$IFNDEF VER350UP}
procedure TEsRegexEditStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Brush.Color := Control.Brush.Color;
  inherited;
end;
{$ENDIF}

{$IFDEF VER350UP}
procedure TEsRegexEditStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
    begin
      inherited WndProc(Message);
      Brush.Color := Control.Brush.Color;
      SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
      Message.Result := LRESULT(Brush.Handle);
      Handled := True;
    end;
  else
    inherited WndProc(Message);
  end;
end;
{$ENDIF}

{ TEsRegExButtonedEdit }

class constructor TEsRegexButtonedEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TEsRegexButtonedEdit, TEsRegexEditStyleHook);
end;

class destructor TEsRegexButtonedEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TEsRegexButtonedEdit, TEsRegexEditStyleHook);
end;

constructor TEsRegexButtonedEdit.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clWindow;

  Indicator := GetIndicatorClass.Create(Self);
  Indicator.OnChange := procedure
    begin
      //Perform(CM_COLORCHANGED, 0, 0);
      inherited Color := Indicator.Color;
    end;
  Indicator.OnGetDefaultColor := function: TColor
    begin
      if IsStyledClientControl(Self) then
        Result := StyleServices.GetStyleColor(scEdit)
      else
        Result := Color;

      Result := ColorToRgb(Result);
    end;
end;

destructor TEsRegexButtonedEdit.Destroy;
begin
  Indicator.Free;
  inherited;
end;

function TEsRegexButtonedEdit.GetAllowNeutral: Boolean;
begin
  Result := Indicator.AllowNeutral;
end;

function TEsRegexButtonedEdit.GetColorIntensity: TColorIntensity;
begin
  Result := Indicator.ColorIntensity;
end;

function TEsRegexButtonedEdit.GetColorInvalid: TColor;
begin
  Result := Indicator.ColorInvalid;
end;

function TEsRegexButtonedEdit.GetColorValid: TColor;
begin
  Result := Indicator.ColorValid;
end;

function TEsRegexButtonedEdit.GetIndicatorClass: TIndicatorClass;
begin
  Result := TRegexIndicator;
end;

function TEsRegexButtonedEdit.GetIndicatorState: TIndicateState;
begin
  Result := Indicator.IndicateState;
end;

function TEsRegexButtonedEdit.GetPattern: string;
begin
  Result := Indicator.Pattern;
end;

function TEsRegexButtonedEdit.GetText: string;
begin
  Result := Text;
end;

procedure TEsRegexButtonedEdit.Change;
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexButtonedEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TEsRegexButtonedEdit.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexButtonedEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Indicator.Indicate;
end;

function TEsRegexButtonedEdit.IsNeutral: Boolean;
begin
  Result := Indicator.IsNeutral;
end;

function TEsRegexButtonedEdit.IsValid: Boolean;
begin
  Result := Indicator.IsValid;
end;

procedure TEsRegexButtonedEdit.Loaded;
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexButtonedEdit.SetAllowNeutral(const Value: Boolean);
begin
  Indicator.AllowNeutral := Value;
end;

procedure TEsRegexButtonedEdit.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    Indicator.Indicate;
  end;
end;

procedure TEsRegexButtonedEdit.SetColorIntensity(const Value: TColorIntensity);
begin
  Indicator.ColorIntensity := Value;
end;

procedure TEsRegexButtonedEdit.SetColorValid(const Value: TColor);
begin
  Indicator.ColorValid := Value;
end;

procedure TEsRegexButtonedEdit.SetIndicateState(const Value: TIndicateState);
begin
  Indicator.IndicateState := Value;
end;

procedure TEsRegexButtonedEdit.SetPattern(const Value: string);
begin
  Indicator.Pattern := Value;
end;

procedure TEsRegexButtonedEdit.SetText(const Value: string);
begin
  Text := Value;
end;

procedure TEsRegexButtonedEdit.SetColorInvalid(const Value: TColor);
begin
  Indicator.ColorInvalid := Value;
end;

{ TEsRegExLabeledEdit }

class constructor TEsRegexLabeledEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TEsRegexLabeledEdit, TEsRegexEditStyleHook);
end;

class destructor TEsRegexLabeledEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TEsRegexLabeledEdit, TEsRegexEditStyleHook);
end;

constructor TEsRegexLabeledEdit.Create(AOwner: TComponent);
begin
  inherited;
  Indicator := GetIndicatorClass.Create(Self);
  Indicator.OnChange := procedure
    begin
      Perform(CM_COLORCHANGED, 0, 0);
    end;
  Indicator.OnGetDefaultColor := function: TColor
    begin
      Result := GetNormalEditColor(Self);
    end;
end;

destructor TEsRegexLabeledEdit.Destroy;
begin
  Indicator.Free;
  inherited;
end;

function TEsRegexLabeledEdit.GetAllowNeutral: Boolean;
begin
  Result := Indicator.AllowNeutral;
end;

function TEsRegexLabeledEdit.GetColorIntensity: TColorIntensity;
begin
  Result := Indicator.ColorIntensity;
end;

function TEsRegexLabeledEdit.GetColorInvalid: TColor;
begin
  Result := Indicator.ColorInvalid;
end;

function TEsRegexLabeledEdit.GetColorValid: TColor;
begin
  Result := Indicator.ColorValid;
end;

function TEsRegexLabeledEdit.GetIndicatorClass: TIndicatorClass;
begin
  Result := TRegexIndicator;
end;

function TEsRegexLabeledEdit.GetIndicatorState: TIndicateState;
begin
  Result := Indicator.IndicateState;
end;

function TEsRegexLabeledEdit.GetPattern: string;
begin
  Result := Indicator.Pattern;
end;

function TEsRegexLabeledEdit.GetText: string;
begin
  Result := Text;
end;

procedure TEsRegexLabeledEdit.Change;
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexLabeledEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Brush.Color := Indicator.Color;
end;

procedure TEsRegexLabeledEdit.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexLabeledEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Indicator.Indicate;
end;

function TEsRegexLabeledEdit.IsNeutral: Boolean;
begin
  Result := Indicator.IsNeutral;
end;

function TEsRegexLabeledEdit.IsValid: Boolean;
begin
  Result := Indicator.IsValid;
end;

procedure TEsRegexLabeledEdit.Loaded;
begin
  inherited;
  Indicator.Indicate;
end;

procedure TEsRegexLabeledEdit.SetAllowNeutral(const Value: Boolean);
begin
  Indicator.AllowNeutral := Value;
end;

procedure TEsRegexLabeledEdit.SetColorIntensity(const Value: TColorIntensity);
begin
  Indicator.ColorIntensity := Value;
end;

procedure TEsRegexLabeledEdit.SetColorValid(const Value: TColor);
begin
  Indicator.ColorValid := Value;
end;

procedure TEsRegexLabeledEdit.SetIndicateState(const Value: TIndicateState);
begin
  Indicator.IndicateState := Value;
end;

procedure TEsRegexLabeledEdit.SetPattern(const Value: string);
begin
  Indicator.Pattern := Value;
end;

procedure TEsRegexLabeledEdit.SetText(const Value: string);
begin
  Text := Value;
end;

procedure TEsRegexLabeledEdit.SetColorInvalid(const Value: TColor);
begin
  Indicator.ColorInvalid := Value;
end;

end.
