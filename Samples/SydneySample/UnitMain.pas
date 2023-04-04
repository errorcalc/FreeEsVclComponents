unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, ES.BaseControls, ES.Switch, Vcl.Themes,
  Vcl.ExtCtrls, ES.Layouts, Vcl.StdCtrls, ES.RegexControls, ES.Labels,
  Vcl.Imaging.pngimage, ES.Images, ES.Indicators, Vcl.BaseImageCollection,
  Vcl.ImageCollection, ES.CfxClasses, ES.NinePatch, Vcl.ControlList,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, ES.ExGraphics,
  Vcl.WinXCtrls, Vcl.TitleBarCtrls;

type
  TFormMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    EsSwitchStyle: TEsSwitch;
    FlowPanel1: TFlowPanel;
    EsPanel1: TEsPanel;
    EsPanel2: TEsPanel;
    EsRegexEdit1: TEsRegexEdit;
    Label1: TLabel;
    EsSwitch1: TEsSwitch;
    Label2: TLabel;
    EsActivityBar1: TEsActivityBar;
    Label3: TLabel;
    EsImageControl1: TEsImageControl;
    Label4: TLabel;
    EsImageControl2: TEsImageControl;
    EsImageControl3: TEsImageControl;
    EsLinkLabel1: TEsLinkLabel;
    EsVersionLabel1: TEsVersionLabel;
    EsPanel3: TEsPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    EsLinkLabel2: TEsLinkLabel;
    EsVersionLabel2: TEsVersionLabel;
    EsPanel4: TEsPanel;
    EsRegexEdit2: TEsRegexEdit;
    EsSwitch2: TEsSwitch;
    EsActivityBar2: TEsActivityBar;
    EsImageControl4: TEsImageControl;
    EsImageControl5: TEsImageControl;
    EsImageControl6: TEsImageControl;
    EsPanel5: TEsPanel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    EsLinkLabel3: TEsLinkLabel;
    EsVersionLabel3: TEsVersionLabel;
    EsPanel6: TEsPanel;
    EsRegexEdit3: TEsRegexEdit;
    EsSwitch3: TEsSwitch;
    EsActivityBar3: TEsActivityBar;
    EsImageControl7: TEsImageControl;
    EsImageControl8: TEsImageControl;
    EsImageControl9: TEsImageControl;
    EsPanel7: TEsPanel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    EsLinkLabel4: TEsLinkLabel;
    EsVersionLabel4: TEsVersionLabel;
    EsPanel8: TEsPanel;
    EsRegexEdit4: TEsRegexEdit;
    EsSwitch4: TEsSwitch;
    EsActivityBar4: TEsActivityBar;
    EsImageControl10: TEsImageControl;
    EsImageControl11: TEsImageControl;
    EsImageControl12: TEsImageControl;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    EsVirtualImageControl: TEsVirtualImageControl;
    ImageCollection: TImageCollection;
    TrackBarImageOpacity: TTrackBar;
    Label17: TLabel;
    ComboBoxImageStretch: TComboBox;
    ComboBoxImageInterpolationMode: TComboBox;
    Label18: TLabel;
    Label19: TLabel;
    ComboBoxImageFrameStyle: TComboBox;
    Label20: TLabel;
    EsImageLayout1: TEsImageLayout;
    CheckBoxImageTransparent: TCheckBox;
    TabSheet3: TTabSheet;
    ControlListOpacity: TControlList;
    EsImageOpacityCL: TEsImage;
    VirtualImageList: TVirtualImageList;
    EsImageLabelOpacityCL: TEsImageLabel;
    EsLinkLabel5: TEsLinkLabel;
    ControlList: TControlList;
    EsImageCL: TEsImage;
    EsImageLabelCL: TEsImageLabel;
    EsPanel9: TEsPanel;
    EsImageErrorsoft: TEsImage;
    EsImageSydnayUpdate: TEsImage;
    EsImageBg1: TEsImage;
    TimerAni: TTimer;
    EsImageVersion: TEsImage;
    EsImageBg2: TEsImage;
    TabSheet4: TTabSheet;
    Panel1: TPanel;
    EsPanelStyleElements: TEsPanel;
    CheckBoxSeFont: TCheckBox;
    CheckBoxSeClient: TCheckBox;
    CheckBoxSeBorder: TCheckBox;
    EsActivityBarOpacity: TEsActivityBar;
    procedure EsSwitchStyleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxImageStretchChange(Sender: TObject);
    procedure ComboBoxImageInterpolationModeChange(Sender: TObject);
    procedure ComboBoxImageFrameStyleChange(Sender: TObject);
    procedure TrackBarImageOpacityChange(Sender: TObject);
    procedure CheckBoxImageTransparentClick(Sender: TObject);
    procedure ControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure ControlListOpacityBeforeDrawItem(AIndex: Integer;
      ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
    procedure TimerAniTimer(Sender: TObject);
    procedure CheckBoxSeFontClick(Sender: TObject);
    procedure CheckBoxSeClientClick(Sender: TObject);
    procedure CheckBoxSeBorderClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    Time: Double;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.CheckBoxImageTransparentClick(Sender: TObject);
begin
  EsVirtualImageControl.Transparent := CheckBoxImageTransparent.Checked;
end;

procedure TFormMain.CheckBoxSeBorderClick(Sender: TObject);
begin
  if CheckBoxSeBorder.Checked then
    EsPanelStyleElements.StyleElements := EsPanelStyleElements.StyleElements + [seBorder]
  else
    EsPanelStyleElements.StyleElements := EsPanelStyleElements.StyleElements - [seBorder];
end;

procedure TFormMain.CheckBoxSeClientClick(Sender: TObject);
begin
  if CheckBoxSeClient.Checked then
    EsPanelStyleElements.StyleElements := EsPanelStyleElements.StyleElements + [seClient]
  else
    EsPanelStyleElements.StyleElements := EsPanelStyleElements.StyleElements - [seClient];
end;

procedure TFormMain.CheckBoxSeFontClick(Sender: TObject);
begin
  if CheckBoxSeFont.Checked then
    EsPanelStyleElements.StyleElements := EsPanelStyleElements.StyleElements + [seFont]
  else
    EsPanelStyleElements.StyleElements := EsPanelStyleElements.StyleElements - [seFont];
end;

procedure TFormMain.ComboBoxImageFrameStyleChange(Sender: TObject);
begin
  EsVirtualImageControl.FrameStyle := TFrameStyle(ComboBoxImageFrameStyle.ItemIndex);
end;

procedure TFormMain.ComboBoxImageInterpolationModeChange(Sender: TObject);
begin
  EsVirtualImageControl.InterpolationMode := TImageInterpolationMode(ComboBoxImageInterpolationMode.ItemIndex);
end;

procedure TFormMain.ComboBoxImageStretchChange(Sender: TObject);
begin
  EsVirtualImageControl.Stretch := TImageStretch(ComboBoxImageStretch.ItemIndex);
end;

procedure TFormMain.ControlListBeforeDrawItem(AIndex: Integer;
  ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  EsImageCL.ImageName := 'emotion' + IntToStr(AIndex mod 8);
  if AIndex mod 8 = 0 then
    EsImageLabelCL.Caption :=
      'This is some long text. Hi, you can see awesome TControlList with errorsoft components.'
  else
    EsImageLabelCL.Caption := 'Short text #' + IntToStr(AIndex);

  EsImageCL.FrameStyle := TFrameStyle(AIndex mod 8);
end;

procedure TFormMain.ControlListOpacityBeforeDrawItem(AIndex: Integer;
  ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  EsImageOpacityCL.Opacity := Byte((AIndex * 13219 + 83919 + AIndex * 32234 + 3423));
  EsImageOpacityCL.Color :=
    RGB(
      Byte(AIndex * 53453 + 1312),
      Byte(AIndex * 65424 + 4234),
      Byte(AIndex * 35225 + 6542)
      );
  EsImageLabelOpacityCL.Caption := 'Opacity = ' + EsImageOpacityCL.Opacity.ToString + #13 +
    'Color = (' + GetRValue(EsImageOpacityCL.Color).ToString + ', ' +
                  GetGValue(EsImageOpacityCL.Color).ToString + ', ' +
                  GetBValue(EsImageOpacityCL.Color).ToString + ')';
end;

procedure TFormMain.EsSwitchStyleClick(Sender: TObject);
begin
  if TEsSwitch(Sender).Checked then
    StyleName := 'Windows10 SlateGray'
  else
    StyleName := 'Windows';
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // TEsVirtualImageControl
  ComboBoxImageInterpolationMode.ItemIndex := Integer(EsVirtualImageControl.InterpolationMode);
  ComboBoxImageStretch.ItemIndex := Integer(EsVirtualImageControl.Stretch);
  ComboBoxImageFrameStyle.ItemIndex := Integer(EsVirtualImageControl.FrameStyle);
  TrackBarImageOpacity.Position := EsVirtualImageControl.Opacity;
  EsActivityBarOpacity.Position := EsVirtualImageControl.Opacity;
  CheckBoxImageTransparent.Checked := EsVirtualImageControl.Transparent;
  // StyleElements
  CheckBoxSeFont.Checked := seFont in EsPanelStyleElements.StyleElements;
  CheckBoxSeClient.Checked := seClient in EsPanelStyleElements.StyleElements;
  CheckBoxSeBorder.Checked := seBorder in EsPanelStyleElements.StyleElements;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  EsSwitchStyle.Left := ClientWidth - EsSwitchStyle.Width - 4;
  EsSwitchStyle.Top := 4;
end;

procedure TFormMain.TimerAniTimer(Sender: TObject);
begin
  EsImageErrorsoft.Opacity := 140 + Trunc(50 + 50 * Sin(Time * 0.12245));
  EsImageSydnayUpdate.Opacity := 140 + Trunc(50 + 50 * Sin(Time * 0.12416));
  EsImageVersion.Opacity := 140 + Trunc(50 + 50 * Sin(Time * 0.12863));
  EsImageBg2.Opacity := 50 + Trunc(15 + 30 * Sin(Time * 0.053982 + Cos(Time * 0.023)));
  EsImageBg1.Opacity := 100 - EsImageBg2.Opacity;
  Time := Time + 2;
end;

procedure TFormMain.TrackBarImageOpacityChange(Sender: TObject);
begin
  EsVirtualImageControl.Opacity := TrackBarImageOpacity.Position;
  EsActivityBarOpacity.Position := TrackBarImageOpacity.Position;
end;

end.
