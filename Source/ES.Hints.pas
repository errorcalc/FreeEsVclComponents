{******************************************************************************}
{                            EsVclComponents v3.0                              }
{                           errorsoft(c) 2009-2018                             }
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
unit ES.Hints;

{$SCOPEDENUMS ON}

interface

uses
  Vcl.Controls;

procedure ShowErrorHint(const Control: TControl; const Title, Description: string); overload;
procedure ShowErrorHint(const Control: TControl; const Hint: string); overload;

implementation

{$R 'Cfx\EsVclComponentsCfx.res'}

uses
  Vcl.ImgList;

var
  FErrorHint: TBalloonHint = nil;
  Images: TImageList = nil;

function ErrorHint: TBalloonHint;
begin
  if FErrorHint = nil then
  begin
    FErrorHint := TBalloonHint.Create(nil);
    Images := TImageList.Create(nil);
    Images.Width := 24;
    Images.Height := 24;
    Images.ResInstLoad(HInstance, TResType.rtBitmap, 'ESERRORHINTIMAGE', 0);
    FErrorHint.Images := Images;
    FErrorHint.ImageIndex := 0;
    FErrorHint.Delay := 20;
    FErrorHint.HideAfter := 1000;
  end;

  Result := FErrorHint;
end;

procedure ShowErrorHint(const Control: TControl; const Title, Description: string);
begin
  ErrorHint.Title := Title;
  ErrorHint.Description := Description;
  ErrorHint.ShowHint(Control);
end;

procedure ShowErrorHint(const Control: TControl; const Hint: string);
begin
  ShowErrorHint(Control, '', Hint);
end;

initialization
finalization
  FErrorHint.Free;
  Images.Free;

end.
