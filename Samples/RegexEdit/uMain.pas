unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  {$ifndef VER230 or VER240 or VER250 or VER260 or VER270 or VER280}System.ImageList,{$endif}
  Vcl.ImgList, Vcl.ExtCtrls, ES.RegexControls, Vcl.StdCtrls;

type
  TRegExForm = class(TForm)
    FloatRegexEdit: TEsRegexEdit;
    Label1: TLabel;
    Label2: TLabel;
    IntegerRegexLabeledEdit: TEsRegexLabeledEdit;
    Label3: TLabel;
    EmailRegexButtonedEdit: TEsRegexButtonedEdit;
    ImageList1: TImageList;
    Label4: TLabel;
    TestRegexEdit: TEsRegexEdit;
    Bevel1: TBevel;
    Label5: TLabel;
    Label6: TLabel;
    PatternEdit: TEdit;
    AllowNeutralCheckBox: TCheckBox;
    ColorIntensityEdit: TEsRegexEdit;
    Label7: TLabel;
    ColorValidColorBox: TColorBox;
    Label8: TLabel;
    ColorInvalidColorBox: TColorBox;
    Label9: TLabel;
    Label10: TLabel;
    IndicateStateComboBox: TComboBox;
    Button1: TButton;
    EnableThemesCheckBox: TCheckBox;
    procedure EmailRegexButtonedEditRightButtonClick(Sender: TObject);
    procedure PatternEditChange(Sender: TObject);
    procedure AllowNeutralCheckBoxClick(Sender: TObject);
    procedure ColorIntensityEditChange(Sender: TObject);
    procedure ColorValidColorBoxChange(Sender: TObject);
    procedure ColorInvalidColorBoxChange(Sender: TObject);
    procedure IndicateStateComboBoxChange(Sender: TObject);
    procedure EnableThemesCheckBoxClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegExForm: TRegExForm;

implementation

uses
  WinApi.ShellAPI, Vcl.Themes, Es.Hints;

{$R *.dfm}

procedure TRegExForm.AllowNeutralCheckBoxClick(Sender: TObject);
begin
  TestRegexEdit.AllowNeutral := AllowNeutralCheckBox.Checked;
end;

procedure TRegExForm.EmailRegexButtonedEditRightButtonClick(Sender: TObject);
begin
  if EmailRegexButtonedEdit.IsValid then
    WinApi.ShellAPI.ShellExecute(0, 'Open', PChar('mailto:' + EmailRegexButtonedEdit.Text), PChar(''), nil, SW_SHOWNORMAL)
  else
    ShowErrorHint(TWinControl(Sender), 'Please enter correct Email!');
end;

procedure TRegExForm.EnableThemesCheckBoxClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    TStyleManager.TrySetStyle('Glow')
  else
    TStyleManager.TrySetStyle('Windows');
end;

procedure TRegExForm.IndicateStateComboBoxChange(Sender: TObject);
begin
  TestRegexEdit.IndicateState := TIndicateState(IndicateStateComboBox.ItemIndex);
end;

procedure TRegExForm.Button1Click(Sender: TObject);
begin
  if TestRegexEdit.IsValid then
    ShowMessage('Valid')
  else
    ShowMessage('Invalid');
end;

procedure TRegExForm.ColorIntensityEditChange(Sender: TObject);
begin
  if ColorIntensityEdit.IsValid then
    TestRegexEdit.ColorIntensity := StrToInt(ColorIntensityEdit.Text);
end;

procedure TRegExForm.ColorInvalidColorBoxChange(Sender: TObject);
begin
  TestRegexEdit.ColorInvalid := ColorInvalidColorBox.Selected;
end;

procedure TRegExForm.ColorValidColorBoxChange(Sender: TObject);
begin
  TestRegexEdit.ColorValid := ColorValidColorBox.Selected;
end;

procedure TRegExForm.PatternEditChange(Sender: TObject);
begin
  TestRegexEdit.Pattern := PatternEdit.Text;
end;

end.
