{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2015-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{******************************************************************************}
unit ES.Vcl.FreeEditors;

interface
uses
  DesignEditors, DesignIntf, Classes, Graphics, PngImage, PicEdit;

type
  TEsPngPropertyFix = class(TGraphicProperty)
    procedure Edit; override;
  end;

implementation

{TEsPngPropertyFix}

procedure TEsPngPropertyFix.Edit;
begin
  TPicture.RegisterFileFormat('PNG', 'ErrorSoft fix PNG loader', TPngImage);
  inherited;
  TPicture.UnregisterGraphicClass(TPngImage);
end;

end.
