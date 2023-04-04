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
// WARNING!!!
// This unit should not contain references to the VCL/FMX or API's.

unit ES.Core.Classes;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types;

type
  /// <summary> Analog of TBounds/TMargins, independent of VCL or FMX</summary>
  TBox = class(TPersistent)
  private
    FRight: Integer;
    FBottom: Integer;
    FTop: Integer;
    FLeft: Integer;
    FOnChange: TNotifyEvent;
    IsUpdate: Boolean;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRect(const Value: TRect);
    function GetRect: TRect;
  protected
    procedure InitDefaults; virtual;
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
    procedure SetHeight(const Value: Integer); virtual;
    procedure SetWidth(const Value: Integer); virtual;
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload; virtual;
    constructor Create(Left, Top, Right, Bottom: Integer); overload; virtual;
    constructor Create(Rect: TRect); overload; virtual;
    procedure SetBounds(Left, Top, Right, Bottom: Integer); virtual;
    procedure Reset;
    function IsEmpty: Boolean;
  public
    // main
    property Left: Integer read FLeft write SetLeft default 0;
    property Top: Integer read FTop write SetTop default 0;
    property Right: Integer read FRight write SetRight default 0;
    property Bottom: Integer read FBottom write SetBottom default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // helpers
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Rect: TRect read GetRect write SetRect;
    //
    procedure BeginUpdate;
    procedure EndUpdate(ChangeCall: Boolean);
  end;

  TBounds = class(TBox);
  TMargins = class(TBox);

  /// <summary>DO NOT USE THIS CLASS, CURRENTLY THIS CLASS IS NOT STABLE!</summary>
  TSizeConstraints = class(TPersistent)
  private
    FMinHeight: Integer;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FMaxHeight: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMaxHeight(const Value: Integer);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetMinHeight(const Value: Integer);
    procedure SetMinWidth(const Value: Integer);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(MinWidth, MaxWidth, MinHeight, MaxHeight: Integer); overload;
    function Apply(R: TRect): TRect; overload;
//    function ApplyEx(R: TRect): TRect; overload;
    procedure Apply(var Box: TBox); overload;
//    procedure ApplyEx(var Box: TBox); overload;
    function IsEmpty: Boolean;
    function HasValue: Boolean;
  published
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 0;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>DO NOT USE THIS CLASS, CURRENTLY THIS CLASS IS NOT STABLE!</summary>
  TRange = class(TPersistent)
  private
    FMax: Integer;
    FMin: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Min, Max: Integer); overload;
    function Apply(Value: Integer): Integer;
//    function ApplyEx(Value: Integer): Integer;
    function IsEmpty: Boolean;
    function HasValue: Boolean;
  published
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFileVersion = record
  public
    Major: Word;
    Minor: Word;
    Release: Word;
    Build: Word;
    //
    class operator Equal(const L, R: TFileVersion): Boolean;// =
    class operator NotEqual(const L, R: TFileVersion): Boolean;//	<>
    class operator GreaterThan(const L, R: TFileVersion): Boolean;// >
    class operator GreaterThanOrEqual(const L, R: TFileVersion): Boolean;// >=
    class operator LessThan(const L, R: TFileVersion): Boolean;// <
    class operator LessThanOrEqual(const L, R: TFileVersion): Boolean;// <=
    //
    constructor Create(const VersionStr: string);
    {$IFDEF MSWINDOWS}
    constructor CreateForFile(const FileName: TFileName);
    {$ENDIF}
    function ToString: string;
  end;

{$IFDEF MSWINDOWS}
function GetFileVersion(FileName: string; out Version: TFileVersion): Boolean;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  WinApi.Windows;
{$ENDIF}

{ TBox }

constructor TBox.Create;
begin
  InitDefaults;
end;

procedure TBox.AssignTo(Dest: TPersistent);
begin
  if Dest is TBox then
    with TBox(Dest) do
    begin
      FLeft := Self.FLeft;
      FTop := Self.FTop;
      FRight := Self.FRight;
      FBottom := Self.FBottom;
      Change;
    end
  else
    inherited;
end;

procedure TBox.BeginUpdate;
begin
  IsUpdate := True;
end;

procedure TBox.Change;
begin
  if Assigned(FOnChange) and not IsUpdate then
    FOnChange(Self);
end;

constructor TBox.Create(Left, Top, Right, Bottom: Integer);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

constructor TBox.Create(Rect: TRect);
begin
  Self.Left := Rect.Left;
  Self.Top := Rect.Top;
  Self.Right := Rect.Right;
  Self.Bottom := Rect.Bottom;
end;

procedure TBox.EndUpdate(ChangeCall: Boolean);
begin
  IsUpdate := False;
  if ChangeCall then
    Change;
end;

procedure TBox.InitDefaults;
begin
  // nothing now
end;

function TBox.IsEmpty: Boolean;
begin
  Result := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);
end;

procedure TBox.Reset;
begin
  SetBounds(0, 0, 0, 0);
end;

function TBox.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

function TBox.GetRect: TRect;
begin
  Result := TRect.Create(Left, Top, Right, Bottom);
end;

function TBox.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TBox.SetWidth(const Value: Integer);
begin
  Right := Left + Value;
end;

procedure TBox.SetHeight(const Value: Integer);
begin
  Bottom := Top + Value;
end;

procedure TBox.SetBottom(const Value: Integer);
begin
  if Bottom <> Value then
    SetBounds(Left, Top, Right, Value);
end;

procedure TBox.SetLeft(const Value: Integer);
begin
  if Left <> Value then
    SetBounds(Value, Top, Right, Bottom);
end;

procedure TBox.SetRect(const Value: TRect);
begin
  SetBounds(Value.Left, Value.Top, Value.Right, Value.Bottom);
end;

procedure TBox.SetRight(const Value: Integer);
begin
  if Right <> Value then
    SetBounds(Left, Top, Value, Bottom);
end;

procedure TBox.SetTop(const Value: Integer);
begin
  if Top <> Value then
    SetBounds(Left, Value, Right, Bottom);
end;

procedure TBox.SetBounds(Left, Top, Right, Bottom: Integer);
begin
  Self.FLeft := Left;
  Self.FTop := Top;
  Self.FRight := Right;
  Self.FBottom := Bottom;
  Change;
end;

{ TSizeConstraints }

//procedure TSizeConstraints.Apply(var Box: TBox);
//begin
//  Box.Rect := Apply(Box.Rect);
//end;

function TSizeConstraints.Apply(R: TRect): TRect;
begin
  if IsEmpty then
    Exit(R);

  if (MinWidth <> 0) and (R.Width < MinWidth) then
    R.Width := MinWidth;
  if (MinHeight <> 0) and (R.Height < MinHeight) then
    R.Height := MinHeight;
  if (MaxWidth <> 0) and (R.Width > MaxWidth) then
    R.Width := MaxWidth;
  if (MaxHeight <> 0) and (R.Height > MaxHeight) then
    R.Height := MaxHeight;

  Exit(R);
end;

procedure TSizeConstraints.Apply(var Box: TBox);
begin
  Box.Rect := Apply(Box.Rect);
end;

//function TSizeConstraints.Apply(R: TRect): TRect;
//begin
//  if IsEmpty then
//    Exit(R);
//
//  if R.Width < MinWidth then
//    R.Width := MinWidth;
//  if R.Height < MinHeight then
//    R.Height := MinHeight;
//  if R.Width > MaxWidth then
//    R.Width := MaxWidth;
//  if R.Height > MaxHeight then
//    R.Height := MaxHeight;
//
//  Exit(R);
//end;

procedure TSizeConstraints.AssignTo(Dest: TPersistent);
begin
  if Dest is TSizeConstraints then
    with TSizeConstraints(Dest) do
    begin
      FMinHeight := Self.FMinHeight;
      FMaxHeight := Self.FMaxHeight;
      FMinWidth := Self.MinWidth;
      FMaxWidth := Self.MaxWidth;
      Change;
    end
  else
    inherited;
end;

procedure TSizeConstraints.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSizeConstraints.Create(MinWidth, MaxWidth, MinHeight, MaxHeight: Integer);
begin
  inherited Create;
  FMinHeight := MinHeight;
  FMinWidth := MinWidth;
  FMaxHeight := MaxHeight;
  FMaxWidth := MaxWidth;
end;

function TSizeConstraints.HasValue: Boolean;
begin
  Result := (MinWidth <> 0) or (MinHeight <> 0) or (MaxWidth <> 0) or (MaxHeight <> 0);
end;

function TSizeConstraints.IsEmpty: Boolean;
begin
  Result := (MinWidth = 0) and (MinHeight = 0) and (MaxWidth = 0) and (MaxHeight = 0);
end;

procedure TSizeConstraints.SetMaxHeight(const Value: Integer);
begin
  if FMaxHeight <> Value then
  begin
    FMaxHeight := Value;
    Change;
  end;
end;

procedure TSizeConstraints.SetMaxWidth(const Value: Integer);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    Change;
  end;
end;

procedure TSizeConstraints.SetMinHeight(const Value: Integer);
begin
  if FMinHeight <> Value then
  begin
    FMinHeight := Value;
    Change;
  end;
end;

procedure TSizeConstraints.SetMinWidth(const Value: Integer);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    Change;
  end;
end;

{ TRange }

//function TRange.Apply(Value: Integer): Integer;
//begin
//  if IsEmpty then
//    Exit(Value);
//
//  if Value < Min then
//    Value := Min;
//  if Value > Max then
//    Value := Max;
//
//  Exit(Value);
//end;

function TRange.Apply(Value: Integer): Integer;
begin
  if IsEmpty then
    Exit(Value);

  if (Min <> 0) and (Value < Min) then
    Value := Min;
  if (Max <> 0) and (Value > Max) then
    Value := Max;

  Exit(Value);
end;

procedure TRange.AssignTo(Dest: TPersistent);
begin
  if Dest is TRange then
    with TRange(Dest) do
    begin
      FMin := Self.FMin;
      FMax := Self.FMax;
      Change;
    end
  else
    inherited;
end;

procedure TRange.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TRange.Create(Min, Max: Integer);
begin
  FMin := Min;
  FMax := Max;
end;

function TRange.HasValue: Boolean;
begin
  Result := (Min <> 0) or (Max <> 0);
end;

function TRange.IsEmpty: Boolean;
begin
  Result := (Min = 0) and (Max = 0);
end;

procedure TRange.SetMax(const Value: Integer);
begin
  FMax := Value;
end;

procedure TRange.SetMin(const Value: Integer);
begin
  FMin := Value;
end;

{$IFDEF MSWINDOWS}
function GetFileVersion(FileName: string; out Version: TFileVersion): Boolean;
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := False;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          Version.Major := HiWord(FI.dwFileVersionMS);
          Version.Minor := LoWord(FI.dwFileVersionMS);
          Version.Release := HiWord(FI.dwFileVersionLS);
          Version.Build := LoWord(FI.dwFileVersionLS);
          Result := True;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$ENDIF}

{ TFileVersion }

constructor TFileVersion.Create(const VersionStr: string);
var
  List: TStringList;
begin
  Major := 0;
  Minor := 0;
  Release := 0;
  Build := 0;

  List := TStringList.Create;
  try
    List.Delimiter := '.';
    List.DelimitedText := VersionStr;

    try
      if List.Count >= 1 then
        Major := StrToInt(Trim(List[0]));
      if List.Count >= 2 then
        Minor := StrToInt(Trim(List[1]));
      if List.Count >= 3 then
        Release := StrToInt(Trim(List[2]));
      if List.Count >= 4 then
        Build := StrToInt(Trim(List[3]));
    except
      on EConvertError do ;
    end;
  finally
    List.Free;
  end;
end;

{$IFDEF MSWINDOWS}
constructor TFileVersion.CreateForFile(const FileName: TFileName);
begin
  Self := Default(TFileVersion);
  GetFileVersion(FileName, Self);
end;
{$ENDIF}

class operator TFileVersion.Equal(const L, R: TFileVersion): Boolean;
begin
  Result := (L.Major = R.Major) and (L.Minor = R.Minor) and (L.Release = R.Release) and (L.Build = R.Build);
end;

class operator TFileVersion.GreaterThan(const L, R: TFileVersion): Boolean;
begin
  Result := R < L;
end;

class operator TFileVersion.GreaterThanOrEqual(const L, R: TFileVersion): Boolean;
begin
  Result := R <= L;
end;

class operator TFileVersion.LessThan(const L, R: TFileVersion): Boolean;
begin
  Result := False;

  // Major
  if L.Major > R.Major then Exit(False);
  if L.Major < R.Major then Exit(True);

  // Minor
  if L.Minor > R.Minor then Exit(False);
  if L.Minor < R.Minor then Exit(True);

  // Release
  if L.Release > R.Release then Exit(False);
  if L.Release < R.Release then Exit(True);

  // Build
  if L.Build > R.Build then Exit(False);
  if L.Build < R.Build then Exit(True);
end;

class operator TFileVersion.LessThanOrEqual(const L, R: TFileVersion): Boolean;
begin
  Result := (L < R) or (L = R);
end;

class operator TFileVersion.NotEqual(const L, R: TFileVersion): Boolean;
begin
  Result := not (L = R);
end;

function TFileVersion.ToString: string;
begin
  Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

end.
