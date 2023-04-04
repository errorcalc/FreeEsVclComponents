unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ControlList, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Data.Bind.Components, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.NumberBox,
  ES.ControlListControls, Data.Bind.ObjectScope, Vcl.Bind.GenData,
  Vcl.Bind.ControlList, Data.Bind.Grid, ES.BaseControls, ES.Images,
  Data.Bind.GenData, Vcl.ExtCtrls, ES.Switch, Vcl.CheckLst, Vcl.Imaging.pngimage;

type
  TIssue = record
    Selected: Boolean;
    IssueNumber: Integer;
    Caption: string;
    IsClient: Boolean;
    IsServer: Boolean;
  end;

  TFormMain = class(TForm)
    BindingsList: TBindingsList;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ControlListIssues: TControlList;
    LabelIssueCaption: TLabel;
    EsControlListCheckBoxIssueNumber: TEsControlListCheckBox;
    EsControlListCheckBoxClient: TEsControlListCheckBox;
    EsControlListCheckBoxServer: TEsControlListCheckBox;
    ControlListButtonOpen: TControlListButton;
    ButtonSelectAll: TButton;
    ButtonClearSelection: TButton;
    ListBoxSelectedIssues: TListBox;
    LabelSelectedIssues: TLabel;
    ControlList1: TControlList;
    PrototypeBindSource: TPrototypeBindSource;
    LabelText: TLabel;
    EsControlListCheckBoxDone: TEsControlListCheckBox;
    EsImage: TEsImage;
    LinkGridToDataSourcePrototypeBindSource1: TLinkGridToDataSource;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldPicture: TLinkPropertyToField;
    LinkPropertyToFieldCaption: TLinkPropertyToField;
    RadioGroupAlignment: TRadioGroup;
    CheckBoxReadOnly: TCheckBox;
    SwitchStyle: TEsSwitch;
    EsImageHint: TEsImage;
    procedure FormCreate(Sender: TObject);
    procedure ControlListIssuesBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure EsControlListCheckBoxIssueNumberClick(Sender: TObject);
    procedure ListBoxSelectedIssuesClick(Sender: TObject);
    procedure ButtonSelectAllClick(Sender: TObject);
    procedure ButtonClearSelectionClick(Sender: TObject);
    procedure ControlListButtonOpenClick(Sender: TObject);
    procedure RadioGroupAlignmentClick(Sender: TObject);
    procedure CheckBoxReadOnlyClick(Sender: TObject);
    procedure SwitchStyleClick(Sender: TObject);
  private
    Issues: TArray<TIssue>;
    procedure UpdateSelected();
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

const
  ConstIssuesTextArray: TArray<string> = [
    'File transfer not working',
    'An error occurs when creating a document on Windows 7',
    'UI not displaying correctly on dark theme',
    'The program crashes when there are many open documents',
    'Incorrect API response',
    'Too slow work with a large nesting of dictionaries',
    'Documentation example doesn''t work',
    'Seems to have a memory leak when running for a long time',
    'Proposal to improve the speed of data collection',
    'Absurd 404 error when using API',
    'Sometimes the message comes only after reconnection',
    'Application crash after update'
  ];

procedure TFormMain.ButtonClearSelectionClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(Issues) do
  begin
    Issues[I].Selected := False;
  end;
  ControlListIssues.Invalidate();
  UpdateSelected();
end;

procedure TFormMain.ButtonSelectAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(Issues) do
  begin
    Issues[I].Selected := True;
  end;
  ControlListIssues.Invalidate();
  UpdateSelected();
end;

procedure TFormMain.CheckBoxReadOnlyClick(Sender: TObject);
begin
  EsControlListCheckBoxIssueNumber.ReadOnly := CheckBoxReadOnly.Checked;
end;

procedure TFormMain.ControlListButtonOpenClick(Sender: TObject);
begin
  ShowMessage('Open Issue #' + Issues[ControlListIssues.ItemIndex].IssueNumber.ToString());
end;

procedure TFormMain.ControlListIssuesBeforeDrawItem(AIndex: Integer;
  ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  // Select check box
  EsControlListCheckBoxIssueNumber.Caption := 'Issue #' + Issues[AIndex].IssueNumber.ToString();
  EsControlListCheckBoxIssueNumber.Checked := Issues[AIndex].Selected;
  // caption
  LabelIssueCaption.Caption := Issues[AIndex].Caption;
  // IsClient read only check box
  EsControlListCheckBoxClient.Checked := Issues[AIndex].IsClient;
  // IsServer read only check box
  EsControlListCheckBoxServer.Checked := Issues[AIndex].IsServer;
end;

procedure TFormMain.EsControlListCheckBoxIssueNumberClick(Sender: TObject);
begin
  // Update check state
  Issues[ControlListIssues.ItemIndex].Selected := EsControlListCheckBoxIssueNumber.Checked;

  UpdateSelected();
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  SetLength(Issues, ControlListIssues.ItemCount);

  for I := 0 to High(Issues) do
  begin
    Issues[I].Selected := (I >= 1) and (I <= 3);
    Issues[I].IssueNumber := I + 1;
    Issues[I].Caption := ConstIssuesTextArray[Random(Length(ConstIssuesTextArray))];
    Issues[I].IsClient := Random(4) <> 0;
    Issues[I].IsServer := Random(4) <> 0;
  end;

  UpdateSelected();

  //TStyleManager.TrySetStyle('Glow');
end;

procedure TFormMain.ListBoxSelectedIssuesClick(Sender: TObject);
begin
  ListBoxSelectedIssues.ClearSelection();
end;

procedure TFormMain.RadioGroupAlignmentClick(Sender: TObject);
begin
  case RadioGroupAlignment.ItemIndex of
    0: EsControlListCheckBoxIssueNumber.Alignment := TCheckBoxAlignment.Left;
    1: EsControlListCheckBoxIssueNumber.Alignment := TCheckBoxAlignment.Right;
    2: EsControlListCheckBoxIssueNumber.Alignment := TCheckBoxAlignment.CenterNoCaption;
  end;
end;

procedure TFormMain.SwitchStyleClick(Sender: TObject);
begin
  if TEsSwitch(Sender).Checked then
    TStyleManager.TrySetStyle('Glow')
  else
    TStyleManager.TrySetStyle('Windows');
end;

procedure TFormMain.UpdateSelected();
var
  I: Integer;
begin
  ListBoxSelectedIssues.Items.BeginUpdate();
  try
    ListBoxSelectedIssues.Items.Clear();
    for I := 0 to High(Issues) do
      if Issues[I].Selected then
        ListBoxSelectedIssues.Items.Add(
          'Issue #' + Issues[I].IssueNumber.ToString() + ' "' + Issues[I].Caption + '"');
  finally
    ListBoxSelectedIssues.Items.EndUpdate();
  end;
end;

end.
