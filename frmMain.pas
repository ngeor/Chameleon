unit frmMain;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, Buttons;

type

  { TMainForm }

  TMainForm = class(TForm)
    DelayTime: TSpinEdit;
    lblDelayTime: TLabel;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    InformationSheet: TTabSheet;
    DelphiFormSheet: TTabSheet;
    Information: TButton;
    WndValue: TEdit;
    Label3: TLabel;
    PasFileName: TEdit;
    BrowsePas: TButton;
    Label4: TLabel;
    FormName: TEdit;
    SavePas: TButton;
    lblWndValue: TLabel;
    optAutomatic: TRadioButton;
    optManual: TRadioButton;
    RCSheet: TTabSheet;
    Label2: TLabel;
    RCFileName: TEdit;
    BrowseRC: TButton;
    SaveRC: TButton;
    btnAbout: TBitBtn;
    procedure InformationClick(Sender: TObject);
    procedure BrowsePasClick(Sender: TObject);
    procedure BrowseRCClick(Sender: TObject);
    procedure SavePasClick(Sender: TObject);
    procedure PasEditChange(Sender: TObject);
    procedure optManualClick(Sender: TObject);
    procedure optAutomaticClick(Sender: TObject);
    procedure RCFileNameChange(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure SaveRCClick(Sender: TObject);
  private
    procedure GenerateDfmFile(const filename, frmname: string; wnd: HWND;
      PasList: TStrings);
    procedure GeneratePasFile(const filename, frmname: string; PasList: TStrings);
    procedure EnableSavePas;
    procedure EnableWndInput;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses frmResults, Writers, DfmEngine, about1;

{$R *.lfm}

procedure TMainForm.InformationClick(Sender: TObject);
var
  wnd: HWND;
begin
  if optAutomatic.Checked then
  begin
    Application.Minimize;
    Sleep(DelayTime.Value);
    wnd := GetForegroundWindow;
    Application.Restore;
  end
  else
    wnd := StrToInt(WndValue.Text);

  Results.Show;
  Results.TreeView1.Items.Clear;
  Results.GetWinInfo(wnd, nil);
end;

procedure TMainForm.BrowsePasClick(Sender: TObject);
var
  s: string;
begin
  with SaveDialog1 do
  begin
    FileName := '';
    DefaultExt := 'pas';
    Filter := 'Delphi Units|*.pas';
    if Execute then
    begin
      PasFileName.Text := FileName;
      s := ChangeFileExt(ExtractFileName(FileName), '');
      if LowerCase(Copy(s, 1, 3)) = 'frm' then
        s := Copy(s, 4, Length(s) - 3) + 'Form'
      else
        s := s + 'Form';
      FormName.Text := s;
    end;
  end;
end;

procedure TMainForm.BrowseRCClick(Sender: TObject);
begin
  with SaveDialog1 do
  begin
    FileName := '';
    DefaultExt := 'rc';
    Filter := 'Resource scripts|*.rc';
    //    if Execute then RCFileName.Text := FileName;
  end;
end;


procedure TMainForm.GenerateDfmFile(const filename, frmName: string;
  wnd: HWND; PasList: TStrings);
var
  OutStream: TFileStream;
  b1: TDfmBuilder;
begin
  b1 := TDfmBuilder.Create(PasList);
  OutStream := TFileStream.Create(filename, fmCreate);
  b1.Build(OutStream, frmName, wnd);
  OutStream.Free;
  b1.Free;
end;

procedure TMainForm.GeneratePasFile(const filename, frmname: string; PasList: TStrings);
var
  title: string;
  fpas: TTextWriter;
  i: integer;
begin
  fpas := TTextWriter.CreateFile(filename);
  try
    title := ChangeFileExt(ExtractFileName(filename), '');
    with fpas do
    begin
      WriteLn('unit ' + title + ';');
      WriteLn('');
      WriteLn('interface');
      WriteLn('');
      WriteLn('uses');
      WriteLn('  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,');
      WriteLn('  StdCtrls;');
      WriteLn('');
      WriteLn('type');
      WriteLn('  T' + frmname + ' = class(TForm)');
      for i := 0 to PasList.Count - 1 do
        WriteLn('    ' + PasList[i]);
      WriteLn('  private');
      WriteLn('    { Private declarations }');
      WriteLn('  public');
      WriteLn('    { Public declarations }');
      WriteLn('  end;');
      WriteLn('');
      WriteLn('var');
      WriteLn('  ' + frmname + ': T' + frmname + ';');
      WriteLn('');
      WriteLn('implementation');
      WriteLn('');
      WriteLn('{$R *.DFM}');
      WriteLn('');
      WriteLn('end.');
    end;
  finally
    fpas.Free;
  end;
end;

procedure TMainForm.SavePasClick(Sender: TObject);
var
  dfmName: string;
  wnd: HWND;
  s: TStringList;
begin
  Application.Minimize;
  Sleep(DelayTime.Value);

  wnd := GetForegroundWindow;
  Application.Restore;

  dfmName := ChangeFileExt(PasFileName.Text, '.dfm');
  s := TStringList.Create;
  GenerateDfmFile(dfmName, FormName.Text, wnd, s);
  GeneratePasFile(PasFileName.Text, FormName.Text, s);
  s.Free;
end;


procedure TMainForm.EnableSavePas;
var
  UnitName: string;
begin
  UnitName := ChangeFileExt(ExtractFileName(PasFileName.Text), '');
  SavePas.Enabled := IsValidIdent(FormName.Text) and IsValidIdent(UnitName) and
    (CompareText(UnitName, FormName.Text) <> 0);
end;

procedure TMainForm.PasEditChange(Sender: TObject);
begin
  EnableSavePas;
end;

procedure TMainForm.EnableWndInput;
begin
  WndValue.Enabled := optManual.Checked;
  lblWndValue.Enabled := optManual.Checked;
end;

procedure TMainForm.optManualClick(Sender: TObject);
begin
  EnableWndInput;
end;

procedure TMainForm.optAutomaticClick(Sender: TObject);
begin
  EnableWndInput;
end;

procedure TMainForm.RCFileNameChange(Sender: TObject);
begin
  SaveRC.Enabled := False;
end;

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.SaveRCClick(Sender: TObject);
begin

end;

end.
