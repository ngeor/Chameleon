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
    procedure GetWinInfo(wnd: HWND; ParentNode: TTreeNode);
    procedure EnableSavePas;
    procedure EnableWndInput;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses frmResults, Writers, DfmEngine, StrConsts, about1;

{$R *.lfm}

procedure TMainForm.GetWinInfo(wnd: HWND; ParentNode: TTreeNode);
var
  len, i, cbcount: integer;
  Text: PChar;
  class_name: array [0..100] of char;
  R1, R2: TRect;
  node1, node2: TTreeNode;
  childlist: TList;
  EnumParams: TEnumParams;
  style, exstyle: integer;
  parentWnd: HWND;
  itemtext: array [0..300] of char;
begin
  if not IsWindow(wnd) then
  begin
    Results.TreeView1.Items.AddChild(ParentNode, 'Handle = (INVALID HANDLE)');
    Exit;
  end;
  childlist := TList.Create;
  len := GetWindowTextLength(wnd) + 1;
  GetMem(Text, len);
  GetWindowText(wnd, Text, len);
  GetWindowRect(wnd, R1);
  Windows.GetClientRect(wnd, R2);
  GetClassName(wnd, class_name, 100);
  style := GetWindowLong(wnd, GWL_STYLE);
  exstyle := GetWindowLong(wnd, GWL_EXSTYLE);
  parentWnd := GetParent(wnd);
  EnumParams.List := childlist;
  EnumParams.ParentWnd := wnd;
  EnumChildWindows(wnd, @EnumChildrenProc, integer(@EnumParams));

  with Results.TreeView1.Items do
  begin
    AddChild(ParentNode, 'Handle = ' + IntToStr(wnd));
    AddChild(ParentNode, 'Caption = ' + Text);
    AddChild(ParentNode, 'Class name = ' + class_name);
    AddChild(ParentNode, 'Parent Handle = ' + IntToStr(parentWnd));
    node1 := AddChild(ParentNode, 'Style');
    AddChild(node1, 'Value = ' + IntToStr(style));
    for i := Low(WindowStyle) to High(WindowStyle) do
      if ((style and WindowStyle[i]) = WindowStyle[i]) then
        AddChild(node1, WindowStyleName[i]);

    node1 := AddChild(ParentNode, 'Extended Style');
    AddChild(node1, 'Value = ' + IntToStr(exstyle));
    for i := Low(WindowStyle) to High(WindowStyle) do
      if ((style and ExtendedWindowStyle[i]) = ExtendedWindowStyle[i]) then
        AddChild(node1, ExtendedWindowStyleName[i]);
    node1 := AddChild(ParentNode, 'Placement');
    AddChild(node1, 'Left = ' + IntToStr(R1.Left));
    AddChild(node1, 'Top = ' + IntToStr(R1.Top));
    AddChild(node1, 'Width = ' + IntToStr(R1.Right - R1.Left));
    AddChild(node1, 'Height = ' + IntToStr(R1.Bottom - R1.Top));
    AddChild(node1, 'ClientWidth = ' + IntToStr(R2.Right));
    AddChild(node1, 'ClientHeight = ' + IntToStr(R2.Bottom));
    if (CompareText(class_name, 'COMBOBOX') = 0) then
    begin
      node1 := AddChild(ParentNode, '[List Data]');
      cbcount := SendMessage(wnd, CB_GETCOUNT, 0, 0);
      for i := 1 to cbcount do
      begin
        SendMessage(wnd, CB_GETLBTEXT, i - 1, longint(@itemtext));
        node2 := AddChild(node1, 'Item #' + IntToStr(i));
        AddChild(node2, 'Text = ' + itemtext);
        AddChild(node2, 'Data = ' +
          IntToStr(SendMessage(wnd, CB_GETITEMDATA, i - 1, 0)));
      end;
    end;
    if childlist.Count > 0 then
    begin
      node1 := AddChild(ParentNode, 'Children information');
      for i := 1 to childlist.Count do
      begin
        node2 := AddChild(node1, 'Child #' + IntToStr(i));
        GetWinInfo(integer(childlist[i - 1]), node2);
      end;
    end;
  end;
  FreeMem(Text);
  childlist.Free;
end;

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
  GetWinInfo(wnd, nil);
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
