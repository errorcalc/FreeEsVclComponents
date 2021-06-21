unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Es.BaseControls, ES.Images, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.ImgList
  {$ifndef VER230 or VER240 or VER250 or VER260 or VER270 or VER280},System.ImageList{$endif};

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label1: TLabel;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    EsImage1: TEsImage;
    EsImage3: TEsImage;
    Image5: TImage;
    Label2: TLabel;
    Label3: TLabel;
    EsImage2: TEsImage;
    Image1: TImage;
    EsImage4: TEsImage;
    Image6: TImage;
    Image7: TImage;
    EsImage5: TEsImage;
    Shape1: TShape;
    Shape2: TShape;
    ImageList: TImageList;
    ImageListTestImage: TEsImage;
    Timer1: TTimer;
    EsImage6: TEsImage;
    EsImage7: TEsImage;
    EsImage8: TEsImage;
    EsImage9: TEsImage;
    EsImage10: TEsImage;
    EsImage11: TEsImage;
    EsImage12: TEsImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Image8: TImage;
    EsImage13: TEsImage;
    EsImage14: TEsImage;
    Image9: TImage;
    Memo1: TMemo;
    Label13: TLabel;
    Panel1: TPanel;
    EsImage15: TEsImage;
    Label14: TLabel;
    Panel2: TPanel;
    EsImage16: TEsImage;
    Label15: TLabel;
    Panel3: TPanel;
    EsImage17: TEsImage;
    Label16: TLabel;
    Panel4: TPanel;
    EsImage18: TEsImage;
    Label17: TLabel;
    Panel5: TPanel;
    EsImage19: TEsImage;
    Label18: TLabel;
    Panel6: TPanel;
    EsImage20: TEsImage;
    Label19: TLabel;
    Panel7: TPanel;
    EsImage21: TEsImage;
    Panel8: TPanel;
    EsImageControl3: TEsImageControl;
    EsImageControl4: TEsImageControl;
    EsImageControl7: TEsImageControl;
    EsImageControl6: TEsImageControl;
    EsImageControl5: TEsImageControl;
    EsImageControl2: TEsImageControl;
    EsImageControl1: TEsImageControl;
    Label20: TLabel;
    Panel9: TPanel;
    Label21: TLabel;
    EsImageControl14: TEsImageControl;
    EsImageControl8: TEsImageControl;
    EsImageControl9: TEsImageControl;
    Panel10: TPanel;
    Label22: TLabel;
    EsImageControl10: TEsImageControl;
    EsImageControl11: TEsImageControl;
    EsImageControl12: TEsImageControl;
    TabSheet5: TTabSheet;
    EsImageControl13: TEsImageControl;
    TrackBar1: TTrackBar;
    Timer2: TTimer;
    EsImage22: TEsImage;
    LabelIndex: TLabel;
    Shape3: TShape;
    Shape4: TShape;
    procedure Timer1Timer(Sender: TObject);
    procedure EsImageControl14Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
    IsAdd: Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.EsImageControl14Click(Sender: TObject);
begin
  ShowMessage('Press!');
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  ImageListTestImage.ImageIndex := ImageListTestImage.ImageIndex + 1;
  if ImageListTestImage.ImageIndex >= ImageList.Count then
    ImageListTestImage.ImageIndex := 0;
  LabelIndex.Caption := IntToStr(ImageListTestImage.ImageIndex);
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
begin
  if IsAdd then
  begin
    if TrackBar1.Position = TrackBar1.Max then
      IsAdd := False
    else
      TrackBar1.Position := TrackBar1.Position + 4;
  end else
  begin
    if TrackBar1.Position = TrackBar1.Min then
      IsAdd := True
    else
      TrackBar1.Position := TrackBar1.Position - 4;
  end;
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  EsImageControl13.Opacity := TrackBar1.Position;
end;

end.
