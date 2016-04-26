unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ES.BaseControls, ES.Switch, ES.Layouts,
  ES.NinePatch, Vcl.StdCtrls, Vcl.ActnList, Vcl.ExtCtrls,
  Vcl.ComCtrls {$ifndef VER230},System.Actions{$endif};

type
  TMainForm = class(TForm)
    EsSwitch1: TEsSwitch;
    EsImageLayout1: TEsImageLayout;
    Label1: TLabel;
    EsLayout1: TEsLayout;
    EsSwitch2: TEsSwitch;
    Label2: TLabel;
    Label3: TLabel;
    EsSwitch3: TEsSwitch;
    Label4: TLabel;
    EsSwitch4: TEsSwitch;
    Label5: TLabel;
    EsSwitch5: TEsSwitch;
    EsSwitch6: TEsSwitch;
    Label6: TLabel;
    EsLayout2: TEsLayout;
    EsSwitch7: TEsSwitch;
    Label7: TLabel;
    EsLayout3: TEsLayout;
    EsSwitch8: TEsSwitch;
    EsSwitch9: TEsSwitch;
    Label8: TLabel;
    ActionList1: TActionList;
    Action1: TAction;
    CheckBox1: TCheckBox;
    EsSwitch10: TEsSwitch;
    EsLayout4: TEsLayout;
    cbStyle: TComboBox;
    Label9: TLabel;
    Label10: TLabel;
    StaticText1: TStaticText;
    udFontSize: TUpDown;
    procedure Action1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbStyleChange(Sender: TObject);
    procedure udFontSizeClick(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  //
end;

procedure TMainForm.cbStyleChange(Sender: TObject);
begin
  TStyleManager.SetStyle(TComboBox(Sender).Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Style: string;
begin
  for Style in TStyleManager.StyleNames do
    cbStyle.Items.Add(Style);

  cbStyle.ItemIndex := cbStyle.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  udFontSize.Position := Self.Font.Size;
end;

procedure TMainForm.udFontSizeClick(Sender: TObject; Button: TUDBtnType);
begin
  Self.Font.Size := udFontSize.Position;
end;

end.
