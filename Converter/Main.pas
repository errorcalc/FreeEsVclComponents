{******************************************************************************}
{                            EsVclComponents v1.6                              }
{                           ErrorSoft(c) 2009-2016                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{         Open this on github: github.com/errorcalc/FreeEsVclComponents        }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ExtDlgs, System.Generics.Collections;

type
  TPair = record
    Key: string;
    Value: string;
  end;

  TFormConverter = class(TForm)
    LogMemo: TMemo;
    OpenButton: TButton;
    ClearLogButton: TButton;
    TopPanel: TPanel;
    EsLabel: TLabel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearLogButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  private
    Rules: TList<TPair>;
    &File: TStringList;
    procedure Convert;
    procedure Log(s: string);
  public
  end;

var
  FormConverter: TFormConverter;

implementation

{$R *.dfm}

uses
  System.IniFiles, System.StrUtils;

procedure TFormConverter.ClearLogButtonClick(Sender: TObject);
begin
  LogMemo.Lines.Clear;
end;

procedure TFormConverter.Convert;
var
  I, L: Integer;

begin
  for L := 0 to &File.Count - 1 do
  begin
    for I := 0 to Rules.Count - 1 do
      if Pos(Rules[I].Key, &File[L]) <> 0 then
      begin
        &File[L] := StringReplace(&File[L], Rules[I].Key, Rules[I].Value, []);
        Log('Replace "' + Rules[I].Key + '" -> "' + Rules[I].Value + '"');
        Application.ProcessMessages;
      end;
  end;
end;

procedure TFormConverter.FormCreate(Sender: TObject);
  function ReadIdent(S: string; var Index: Integer): string;
  var
    P: Integer;

  begin
    Index := PosEx('"', s, Index);
    if Index <> 0 then
    begin
      P := PosEx('"', s, Index + 1);
      if P <> 0 then
      begin
        Result := Copy(s, Index + 1, P - Index - 1);
        Index := P + 1;
      end;
    end;
  end;

var
  v: TStringList;
  s: string;
  Index: Integer;
  Pair: TPair;

begin
  Rules := TList<TPair>.Create;
  &File := TStringList.Create;
  // load rules
  v := TStringList.Create;
  try
    v.LoadFromFile(GetCurrentDir + '\Rules.ini');

    for s in v do
    begin
      if Pos(';', s) = 0 then
      begin
        Index := 1;
        Pair.Key := ReadIdent(s, Index);
        Pair.Value := ReadIdent(s, Index);
        Rules.Add(Pair);
      end;
    end;

  finally
    v.Free;
  end;

end;

procedure TFormConverter.FormDestroy(Sender: TObject);
begin
  Rules.Free;
  &File.Free;
end;

procedure TFormConverter.Log(s: string);
begin
  LogMemo.Lines.Add(s);
end;

procedure TFormConverter.OpenButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute(Self.Handle) then
  begin
    &File.LoadFromFile(OpenDialog.FileName);
    Log('File loaded');
    &File.SaveToFile(OpenDialog.FileName + '.backup');
    Log('Backup created');
    Log('Start convert');
    Application.ProcessMessages;
    Convert;
    &File.SaveToFile(OpenDialog.FileName);
    Log('Done!');
    Log('============================================');
    Beep;
  end;
end;

end.
