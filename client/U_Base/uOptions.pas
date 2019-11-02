unit uOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uResources, ComCtrls, IniFiles, uLocalize;

type
  TfrmOptions = class(TForm)
    btnOk: TButton;
    Button2: TButton;
    PageControl: TPageControl;
    tsGeneral: TTabSheet;
    lLLanguage: TLabel;
    cmLanguage: TComboBox;
    cbAutoTabs: TCheckBox;
    cbPersonImages: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.dfm}

procedure TfrmOptions.FormCreate(Sender: TObject);
var sr: TSearchRec;
    res: integer;
begin
  Localize(Self);

  // Fill languages menu
  res := FindFirst(BaseDir + 'Language\*.lng', 0, sr);
  while res = 0 do begin
    cmLanguage.Items.Add(Copy(sr.Name, 1, Length(sr.Name) - 4));
    if (cmLanguage.Items[cmLanguage.Items.Count-1] =
      Config.ReadString('Main', 'Language', '')) then
      cmLanguage.ItemIndex := cmLanguage.Items.Count-1;
    res := FindNext(sr);
  end;
  FindClose(sr);

  cbAutoTabs.Checked := Config.ReadBool('Main', 'AutoTabs', true);
  cbPersonImages.Checked := Config.ReadBool('Main', 'PersonImages', true);
end;

procedure TfrmOptions.btnOkClick(Sender: TObject);
begin
  if (cmLanguage.Text <> Config.ReadString('Main', 'Language', '')) then begin
    Config.WriteString('Main', 'Language', cmLanguage.Text);
    Lang.Free;
    Lang := TMemIniFile.Create(BaseDir + 'Language\' + cmLanguage.Text + '.lng');
  end;

  Config.WriteBool('Main', 'AutoTabs', cbAutoTabs.Checked);
  Config.WriteBool('Main', 'PersonImages', cbPersonImages.Checked);

  ModalResult := mrOk;
end;

procedure TfrmOptions.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
