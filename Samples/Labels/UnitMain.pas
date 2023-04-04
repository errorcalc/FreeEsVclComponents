unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ES.Labels, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Mask;

type
  TFormMain = class(TForm)
    GroupBox1: TGroupBox;
    EsVersionLabel: TEsVersionLabel;
    GroupBox2: TGroupBox;
    EditCaption: TLabeledEdit;
    EditAddress: TLabeledEdit;
    EsLinkLabel: TEsLinkLabel;
    Label1: TLabel;
    procedure EditCaptionChange(Sender: TObject);
    procedure EditAddressChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateMe();
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.EditCaptionChange(Sender: TObject);
begin
  UpdateMe;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Update;
end;

procedure TFormMain.UpdateMe();
begin
  EsLinkLabel.Caption := EditCaption.Text;
  EsLinkLabel.Url := EditAddress.Text;
end;

procedure TFormMain.EditAddressChange(Sender: TObject);
begin
  Update;
end;

end.
