{******************************************************************************}
{                           errorsoft Core library                             }
{                           errorsoft(c) 2016-2018                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{     errorsoft@protonmail.ch | habrahabr.ru/user/error1024 | errorsoft.org    }
{                                                                              }
{         Open this on github: github.com/errorcalc/FreeEsVclComponents        }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
// WARNING!!!
// This unit should not contain references to the VCL/FMX or API's.

unit ES.Backend.Controls;

{$I EsDefines.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.Types, System.Classes;

type
  TControlZoom = 1..1000000;
  /// <summary>DO NOT USE THIS CLASS, CURRENTLY THIS CLASS IS NOT STABLE!</summary>
  TBackendControl = class(TInterfacedPersistent)
  private
    FNumerator: Integer;
    FDenominator: Integer;
    function GetZoom: TControlZoom;
    procedure SetDenominator(const Value: Integer);
    procedure SetNumerator(const Value: Integer);
    procedure SetZoom(const Value: TControlZoom);
  protected
    // Scale support
    function ToScale(Value: Integer): Integer;
    function ToReal(Value: Integer): Integer;
    procedure ChangeScale; virtual;
    property Zoom: TControlZoom read GetZoom write SetZoom default 100;
    property Numerator: Integer read FNumerator write SetNumerator;
    property Denominator: Integer read FDenominator write SetDenominator;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TBackendControl }

procedure TBackendControl.ChangeScale;
begin
end;

constructor TBackendControl.Create;
begin
  inherited Create;
  FNumerator := 1;
  FDenominator := 1;
end;

destructor TBackendControl.Destroy;
begin
  inherited;
end;

function TBackendControl.GetZoom: TControlZoom;
begin
  Result := (FNumerator * 100) div FDenominator;
end;

procedure TBackendControl.SetDenominator(const Value: Integer);
begin
  if (Value <> FDenominator) and (Value <> 0) then
  begin
    FDenominator := Value;
    ChangeScale;
  end;
end;

procedure TBackendControl.SetNumerator(const Value: Integer);
begin
  if (Value <> FNumerator) and (Value <> 0) then
  begin
    FNumerator := Value;
    ChangeScale;
  end;
end;

procedure TBackendControl.SetZoom(const Value: TControlZoom);
begin
  if (Value <> GetZoom) or (FDenominator <> 100) then
  begin
    FNumerator := Value;
    FDenominator := 100;
    ChangeScale;
  end;
end;

function TBackendControl.ToReal(Value: Integer): Integer;
begin
  Result := (Value * Denominator) div Numerator;
end;

function TBackendControl.ToScale(Value: Integer): Integer;
begin
  Result := (Value * Numerator) div Denominator;
end;

end.
