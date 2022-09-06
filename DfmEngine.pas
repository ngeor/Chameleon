unit DfmEngine;

{$MODE Delphi}

interface

uses Windows, Messages, SysUtils, Classes, Forms, Graphics,
  Writers;

type
  PEnumParams = ^TEnumParams;

  TEnumParams = record
    List: TList;
    ParentWnd: HWND;
  end;

  TKnownControl = (
    kcUnknown, kcButton, kcCheckBox, kcComboBox, kcEdit,
    kcGroupBox, kcImage, kcLabel, kcListBox, kcListView,
    kcPanel, kcRadioButton, kcStatusBar, kcTreeView
    );
  TKnownControls = set of TKnownControl;
  TKnownControlHandlerProc = procedure(wnd: HWND; style: integer) of object;

  TDfmBuilder = class
  private
    PasList: TStrings;
    dfm1: TDfmWriter;
    Counts: array[TKnownControl] of integer;
    CtlType: TKnownControl;
    procedure DispatchHandle(wnd: HWND; style: integer);
    procedure HandleUnknown(wnd: HWND; style: integer);
    procedure HandleButton(wnd: HWND; style: integer);
    procedure HandleCheckBox(wnd: HWND; style: integer);
    procedure HandleComboBox(wnd: HWND; style: integer);
    procedure HandleEdit(wnd: HWND; style: integer);
    procedure HandleGroupBox(wnd: HWND; style: integer);
    procedure HandleImage(wnd: HWND; style: integer);
    procedure HandleLabel(wnd: HWND; style: integer);
    procedure HandleListBox(wnd: HWND; style: integer);
    procedure HandleListView(wnd: HWND; style: integer);
    procedure HandlePanel(wnd: HWND; style: integer);
    procedure HandleRadioButton(wnd: HWND; style: integer);
    procedure HandleStatusBar(wnd: HWND; style: integer);
    procedure HandleTreeView(wnd: HWND; style: integer);
    procedure PreHandleCtl(wnd: HWND);
    procedure WriteDecl;
  public
    constructor Create(APasList: TStrings);
    procedure Build(OutStream: TStream; const frmName: string; wnd: HWND);
  end;

const
  KCNames: array[TKnownControl] of string = (
    'Unknown', 'Button', 'CheckBox', 'ComboBox', 'Edit',
    'GroupBox', 'Image', 'Label', 'ListBox', 'ListView',
    'Panel', 'RadioButton', 'StatusBar', 'TreeView'
    );

  KCClassNames: array[TKnownControl] of string = (
    'TPanel', 'TButton', 'TCheckBox', 'TComboBox', 'TEdit',
    'TGroupBox', 'TImage', 'TLabel', 'TListBox', 'TListView',
    'TPanel', 'TRadioButton', 'TStatusBar', 'TTreeView'
    );

function BitTest(Value, Mask: integer): boolean;
function GetWndText(wnd: HWND): string;
function EnumChildrenProc(wnd: HWND; lp: LPARAM): BOOL; stdcall;
procedure WriteBitmapData(dfm1: TDfmWriter; bmp: HBITMAP;
  BelongsToPicture: boolean; const Name: string);
procedure WriteIconData(dfm1: TDfmWriter; icon: HICON; BelongsToPicture: boolean;
  const Name: string);

implementation

function BitTest(Value, Mask: integer): boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function GetWndText(wnd: HWND): string;
var
  len: integer;
begin
  len := GetWindowTextLength(wnd);
  SetLength(Result, len);
  GetWindowText(wnd, PChar(Result), len + 1);
end;

function EnumChildrenProc(wnd: HWND; lp: LPARAM): BOOL; stdcall;
var
  p: PEnumParams;
begin
  p := PEnumParams(lp);
  if GetParent(wnd) = p^.ParentWnd then
    p^.List.Add(Pointer(wnd));
  Result := True;
end;

function GetBorderIconsStr(ABorderIcons: TBorderIcons): string;
const
  biStr: array [TBorderIcon] of string = (
    'biSystemMenu', 'biMinimize', 'biMaximize', 'biHelp');
var
  k: TBorderIcon;
begin
  if ABorderIcons = [] then
  begin
    Result := '[]';
    Exit;
  end;
  Result := '[';
  for k := biSystemMenu to biHelp do
    if k in ABorderIcons then
      Result := Result + biStr[k] + ',';
  Result[Length(Result)] := ']';
end;


constructor TDfmBuilder.Create(APasList: TStrings);
begin
  inherited Create;
  PasList := APasList;
end;

procedure TDfmBuilder.Build(OutStream: TStream; const frmName: string; wnd: HWND);
var
  InStream: TMemoryStream;
  childlist: TList;
  i: integer;
  style, exstyle: longint;
  wndX: HWND;
  EnumParams: TEnumParams;

  procedure WriteBorderIcons;
  var
    bi: TBorderIcons;
    s: string;
  begin
    bi := [];
    if BitTest(style, WS_SYSMENU) then
      Include(bi, biSystemMenu);
    if BitTest(style, WS_MINIMIZEBOX) then
      Include(bi, biMinimize);
    if BitTest(style, WS_MAXIMIZEBOX) then
      Include(bi, biMaximize);
    if BitTest(exstyle, WS_EX_CONTEXTHELP) then
      Include(bi, biHelp);
    s := GetBorderIconsStr(bi);
    dfm1.WriteCustomProp('BorderIcons', s);
  end;

begin
  // Initialize
  FillChar(Counts, sizeof(Counts), #0);
  InStream := TMemoryStream.Create;
  Dfm1 := TDfmWriter.Create(InStream);
  childlist := TList.Create;

  dfm1.WriteLn('object ' + frmName + ': T' + frmName);
  style := GetWindowLong(wnd, GWL_EXSTYLE);
  exstyle := GetWindowLong(wnd, GWL_EXSTYLE);
  dfm1.Ident := 2;
  dfm1.WritePlacement(wnd, 0);
  WriteBorderIcons;
  dfm1.WriteStringProp('Caption', GetWndText(wnd));
  dfm1.WriteCustomProp('Font.Charset', 'GREEK_CHARSET');
  dfm1.WriteColorProp('Font.Color', clWindowText);
  dfm1.WriteIntProp('Font.Height', -11);
  dfm1.WriteStringProp('Font.Name', 'MS Sans Serif');
  dfm1.WriteCustomProp('Font.Style', '[]');
  WriteIconData(dfm1, SendMessage(wnd, WM_GETICON, 0, 0), False, 'Icon.Data');
  dfm1.WriteIntProp('PixelsPerInch', 96);
  dfm1.WriteIntProp('TextHeight', 13);

  // write the children
  EnumParams.List := childlist;
  EnumParams.ParentWnd := wnd;
  EnumChildWindows(wnd, @EnumChildrenProc, integer(@EnumParams));
  for i := 0 to childlist.Count - 1 do
  begin
    wndX := HWND(childlist[i]);
    PreHandleCtl(wndX);
  end;
  dfm1.Ident := 0;
  dfm1.WriteLn('end');
  InStream.Position := 0;

  ObjectTextToResource(InStream, OutStream);

  // finalization
  childlist.Free;
  DFM1.Free;
end;

procedure TDfmBuilder.HandleUnknown(wnd: HWND; style: integer);
var
  class_name: array [0..100] of char;
  childlist: TList;
  EnumParams: TEnumParams;
  i: integer;
begin
  // since we don't know what window we're after write the class name
  GetClassName(wnd, class_name, 100);
  dfm1.WriteStringProp('Caption', class_name);
  childlist := TList.Create;
  EnumParams.List := childlist;
  EnumParams.ParentWnd := wnd;
  EnumChildWindows(wnd, @EnumChildrenProc, integer(@EnumParams));
  for i := 0 to childlist.Count - 1 do
  begin
    PreHandleCtl(HWND(childlist[i]));
  end;
  childlist.Free;
end;

procedure TDfmBuilder.HandleButton(wnd: HWND; style: integer);
begin
  dfm1.WriteStringProp('Caption', GetWndText(wnd));
  dfm1.WriteBoolProp('Default', BitTest(style, BS_DEFPUSHBUTTON));
end;

procedure TDfmBuilder.HandleCheckBox(wnd: HWND; style: integer);
const
  CheckedStr: array [0..2] of string = ('cbUnchecked', 'cbChecked', 'cbGrayed');
var
  state: integer;
  allowgrayed: boolean;
begin
  allowgrayed := BitTest(style, BS_3STATE) or BitTest(style, BS_AUTO3STATE);
  state := SendMessage(wnd, BM_GETCHECK, 0, 0);
  dfm1.WriteBoolProp('AllowGrayed', allowgrayed);
  dfm1.WriteStringProp('Caption', GetWndText(wnd));
  dfm1.WriteCustomProp('State', CheckedStr[state]);
end;

procedure TDfmBuilder.HandleComboBox(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandleEdit(wnd: HWND; style: integer);
begin
  dfm1.WriteStringProp('Text', GetWndText(wnd));
end;

procedure TDfmBuilder.HandleGroupBox(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandleImage(wnd: HWND; style: integer);
var
  h: THANDLE;
  OnlyIcon: boolean;
begin
  OnlyIcon := BitTest(style, SS_ICON);
  if OnlyIcon then
  begin
    h := SendMessage(wnd, STM_GETICON, 0, 0);
    WriteIconData(dfm1, h, True, 'Picture.Data');
  end
  else
  begin
    h := SendMessage(wnd, STM_GETIMAGE, IMAGE_BITMAP, 0);
    if h <> 0 then
      WriteBitmapData(dfm1, h, True, 'Picture.Data')
    else
    begin
      h := SendMessage(wnd, STM_GETIMAGE, IMAGE_ICON, 0);
      WriteIconData(dfm1, h, True, 'Picture.Data');
    end;
  end;
end;

procedure TDfmBuilder.HandleLabel(wnd: HWND; style: integer);
begin
  dfm1.WriteBoolProp('AutoSize', False);
  dfm1.WriteStringProp('Caption', GetWndText(wnd));
  dfm1.WriteBoolProp('WordWrap', True);
end;

procedure TDfmBuilder.HandleListBox(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandleListView(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandlePanel(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandleRadioButton(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandleStatusBar(wnd: HWND; style: integer);
begin
end;

procedure TDfmBuilder.HandleTreeView(wnd: HWND; style: integer);
begin
end;

procedure WriteBitmapData(dfm1: TDfmWriter; bmp: HBITMAP;
  BelongsToPicture: boolean; const Name: string);
var
  b: TBitmap;
  Memory: TTextWriter;
  Size, Offset: integer;
begin
  b := TBitmap.Create;
  Memory := TTextWriter.Create(TMemoryStream.Create);
  try
    b.Handle := CopyImage(bmp, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG);

    if BelongsToPicture then
      Memory.WriteString('TBitmap');
    size := 0;
    Offset := Memory.Stream.Position;
    Memory.Stream.Write(size, SizeOf(size));
    b.SaveToStream(Memory.Stream);
    Memory.Stream.Position := Offset;
    size := Memory.Stream.Size - sizeof(size) - Offset;
    Memory.Stream.Write(size, SizeOf(size));
    Memory.Stream.Position := 0;

    dfm1.Write(Name + ' = {');
    dfm1.Ident := dfm1.Ident + 2;
    dfm1.WriteBinaryAsText(Memory.Stream);
    dfm1.Ident := dfm1.Ident - 2;
    dfm1.WriteLn('}');
  finally
    b.Handle := 0;
    b.Free;
    Memory.Free;
  end;
end;

procedure WriteIconData(dfm1: TDfmWriter; icon: HICON; BelongsToPicture: boolean;
  const Name: string);
var
  i: TIcon;
  k: TTextWriter;
begin
  if icon = 0 then
    Exit;
  i := TIcon.Create;
  k := TTextWriter.Create(TMemoryStream.Create);
  try
    i.Handle := CopyIcon(icon);
    if BelongsToPicture then
      k.WriteString('TIcon');
    i.SaveToStream(k.Stream);
    k.Stream.Position := 0;

    dfm1.Write(Name + ' = {');
    dfm1.Ident := dfm1.Ident + 2;
    dfm1.WriteBinaryAsText(k.Stream);
    dfm1.WriteLn('}');
    dfm1.Ident := dfm1.Ident - 2;

  finally
    i.Free;
    k.Free;
  end;
end;

procedure TDfmBuilder.WriteDecl;
var
  s: string;
begin
  Inc(Counts[CtlType]);
  s := KCNames[CtlType] + IntToStr(Counts[CtlType]) + ': ' + KCClassNames[CtlType];
  PasList.Add(s + ';');
  dfm1.WriteLn('object ' + s);
  dfm1.Ident := dfm1.Ident + 2;
end;

procedure TDfmBuilder.DispatchHandle(wnd: HWND; style: integer);
begin
  case CtlType of
    kcUnknown: HandleUnknown(wnd, style);
    kcButton: HandleButton(wnd, style);
    kcCheckBox: HandleCheckBox(wnd, style);
    kcComboBox: HandleComboBox(wnd, style);
    kcEdit: HandleEdit(wnd, style);
    kcGroupBox: HandleGroupBox(wnd, style);
    kcImage: HandleImage(wnd, style);
    kcLabel: HandleLabel(wnd, style);
    kcListBox: HandleListBox(wnd, style);
    kcListView: HandleListView(wnd, style);
    kcPanel: HandlePanel(wnd, style);
    kcRadioButton: HandleRadioButton(wnd, style);
    kcStatusBar: HandleStatusBar(wnd, style);
    kcTreeView: HandleTreeView(wnd, style);
  end;
end;

procedure TDfmBuilder.PreHandleCtl(wnd: HWND);
var
  class_name: array [0..100] of char;
  style: integer;
begin
  GetClassName(wnd, class_name, 100);
  style := GetWindowLong(wnd, GWL_STYLE);
  if StrIComp(class_name, 'Static') = 0 then
  begin
    if BitTest(style, SS_BITMAP) or BitTest(style, SS_ICON) then
      CtlType := kcImage
    else
      CtlType := kcLabel;
  end
  else if StrIComp(class_name, 'Button') = 0 then
  begin
    if BitTest(style, BS_GROUPBOX) then
      CtlType := kcGroupBox
    else if BitTest(style, BS_RADIOBUTTON) or BitTest(style, BS_AUTORADIOBUTTON) then
      CtlType := kcRadioButton
    else if BitTest(style, BS_CHECKBOX) or BitTest(style, BS_AUTOCHECKBOX) or
      BitTest(style, BS_3STATE) or BitTest(style, BS_AUTO3STATE) then
      CtlType := kcCheckBox
    else
      CtlType := kcButton;
  end
  else if StrIComp(class_name, 'ComboBox') = 0 then
    CtlType := kcComboBox
  else if StrIComp(class_name, 'ListBox') = 0 then
    CtlType := kcListBox
  else if StrIComp(class_name, 'Edit') = 0 then
    CtlType := kcEdit
  else if StrIComp(class_name, 'msctls_statusbar32') = 0 then
    CtlType := kcStatusBar
  else
    CtlType := kcUnknown;

  WriteDecl;
  dfm1.WritePlacement(wnd, GetParent(wnd));
  DispatchHandle(wnd, style);
  dfm1.Ident := dfm1.Ident - 2;
  dfm1.WriteLn('end');
end;

end.
