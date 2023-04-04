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
unit ES.RegexEditor;

{$I EsDefines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, ES.BaseControls, ES.Layouts, Vcl.StdCtrls,
  ES.RegexControls, Vcl.ExtCtrls, DesignMenus, DesignEditors, DesignIntf, WinApi.ShellAPI,
  Vcl.Mask;

type
  TEsRegexEditorForm = class(TForm)
    PatternEdit: TLabeledEdit;
    TestEdit: TEsRegexLabeledEdit;
    PatternsButton: TButton;
    FotterLayout: TEsLayout;
    CancelButton: TButton;
    OkButton: TButton;
    PatternsPopup: TPopupMenu;
    LinkLabelEs: TLinkLabel;
    procedure PatternEditChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PatternsButtonClick(Sender: TObject);
    procedure LinkLabelEsLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
  private
    FEdit: IRegexIndicator;
    procedure SetEdit(const Value: IRegexIndicator);
    procedure PatternsPopupClick(Sender: TObject);
  public
    property Edit: IRegexIndicator read FEdit write SetEdit;
  end;

  TEsRegexEditorEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

var
  EsRegexEditorForm: TEsRegexEditorForm;

implementation

{$R *.dfm}

const
  Patterns: array [0..14, 0..1] of string =
  (
    ('Email (most used)', '^(?!\.)(""([^""\r\\]|\\[""\r\\])*""|([-a-z0-9!#$%&''*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)@[a-z0-9][\w\.-]*[a-z0-9]\.[a-z][a-z\.]*[a-z]$'),
    ('Url', '^(((ftp|http|https):\/\/)|(\/)|(..\/))(\w+:{0,1}\w*@)?(\S+)(:[0-9]+)?(\/|\/([\w#!:.?+=&%@!\-\/]))?$'),
    ('-', '-'),
    ('Data (dd.mm.yyyy)', '^((0?[1-9])|((1|2)[1-9])|(3[0,1]))\.((0?[1-9])|(1[0-2]))\.[1,2][0-9][0-9][0-9]$'),
    ('Data (dd/mm/yyyy)', '^((0?[1-9])|((1|2)[1-9])|(3[0,1]))\/((0?[1-9])|(1[0-2]))\/[1,2][0-9][0-9][0-9]$'),
    ('Data (mm.dd.yyyy)', '^((0?[1-9])|(1[0-2]))\.((0?[1-9])|((1|2)[1-9])|(3[0,1]))\.[1,2][0-9][0-9][0-9]$'),
    ('Data (mm.dd.yyyy)', '^((0?[1-9])|(1[0-2]))\/((0?[1-9])|((1|2)[1-9])|(3[0,1]))\/[1,2][0-9][0-9][0-9]$'),
    ('-', '-'),
    ('Time (hh:mm:ss)', '^((0|1)?[0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$'),
    ('-', '-'),
    ('Letters (latin) and numbers', '^[a-zA-Z0-9]+$'),
    ('Identifier (Delphi)', '^[a-zA-Z_][a-zA-Z0-9_]*$'),
    ('-', '-'),
    ('Integer number', '^[+-]?[0-9]+$'),
    ('Float number (ex: 1.2e+12)', '^[+-]?[\d]+($|[\.][\d]+|([\.][\d]+[Ee]|[Ee])[+-]?\d+)$')
  );

{ TEsRegexEditorEditor }

procedure TEsRegexEditorEditor.Edit;
begin
  inherited;
end;

procedure TEsRegexEditorEditor.ExecuteVerb(Index: Integer);
var
  Form: TEsRegexEditorForm;
begin
  Form := TEsRegexEditorForm.Create(nil);
  try
    Form.Edit := Component as IRegexIndicator;
    Form.ShowModal;
    Designer.Modified;
  finally
    Form.Free;
  end;
end;

function TEsRegexEditorEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit Pattern...';
end;

function TEsRegexEditorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{ TEsRegexEditorForm }

procedure TEsRegexEditorForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TEsRegexEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  for I := 0 to High(Patterns) do
  begin
    Item := TMenuItem.Create(Self);
    Item.OnClick := PatternsPopupClick;
    Item.Caption := Patterns[I, 0];
    PatternsPopup.Items.Add(Item);
  end;
end;

procedure TEsRegexEditorForm.LinkLabelEsLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  WinApi.ShellAPI.ShellExecute(0, 'Open', PChar(Link), PChar(''), nil, SW_SHOWNORMAL);
end;

procedure TEsRegexEditorForm.OkButtonClick(Sender: TObject);
begin
  Edit.SetPattern(PatternEdit.Text);
  Close;
end;

procedure TEsRegexEditorForm.PatternEditChange(Sender: TObject);
begin
  TestEdit.Pattern := PatternEdit.Text;
end;

procedure TEsRegexEditorForm.PatternsButtonClick(Sender: TObject);
begin
  PatternsPopup.Popup(PatternsButton.ClientToScreen(Point(0, 0)).X, PatternsButton.ClientToScreen(Point(0, 0)).Y);
end;

procedure TEsRegexEditorForm.PatternsPopupClick(Sender: TObject);
begin
  PatternEdit.Text := Patterns[PatternsPopup.Items.IndexOf(TMenuItem(Sender)), 1];
end;

procedure TEsRegexEditorForm.SetEdit(const Value: IRegexIndicator);
begin
  FEdit := Value;
  PatternEdit.Text := Edit.GetPattern;
  TestEdit.Pattern := Edit.GetPattern;
  TestEdit.Text := Edit.GetText;
end;

end.
