unit frmResults;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus;

type
  TResults = class(TForm)
    popListData: TPopupMenu;
    Savelistdata1: TMenuItem;
    TreeView1: TTreeView;
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Savelistdata1Click(Sender: TObject);
  private
    procedure GetWinInfoStyle(wnd: HWND; ParentNode: TTreeNode);
    procedure GetWinInfoExtendedStyle(wnd: HWND; ParentNode: TTreeNode);
  public
    procedure GetWinInfo(wnd: HWND; ParentNode: TTreeNode);
  end;

var
  Results: TResults;

implementation

uses DfmEngine, StyleNames;

{$R *.lfm}

procedure TResults.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if (Assigned(Node) and (Node.Text = '[List Data]')) then
    TreeView1.PopupMenu := popListData
  else
    TreeView1.PopupMenu := nil;
end;

procedure TResults.Savelistdata1Click(Sender: TObject);
var
  f: TextFile;
  n: TTreeNode;
  i: integer;

  function RemoveTag(const s: string): string;
  var
    pos1: integer;
  begin
    pos1 := Pos('=', s);
    Result := Copy(s, pos1 + 2, Length(s) - pos1 - 1);
  end;

begin
  AssignFile(f, 'c:\listdata.csv');
  Rewrite(f);
  n := TreeView1.Selected;
  for i := 0 to n.Count - 1 do
  begin
    WriteLn(f, i, ';', RemoveTag(n.Items[i].Items[0].Text), ';',
      RemoveTag(n.Items[i].Items[1].Text));
  end;
  CloseFile(f);
end;

procedure TResults.GetWinInfo(wnd: HWND; ParentNode: TTreeNode);
var
  len, i, cbcount: integer;
  Text: PChar;
  class_name: array [0..100] of char;
  R1, R2: TRect;
  node1, node2: TTreeNode;
  childlist: TList;
  EnumParams: TEnumParams;
  parentWnd: HWND;
  itemtext: array [0..300] of char;
begin
  if not IsWindow(wnd) then
  begin
    TreeView1.Items.AddChild(ParentNode, 'Handle = (INVALID HANDLE)');
    Exit;
  end;
  childlist := TList.Create;
  len := GetWindowTextLength(wnd) + 1;
  GetMem(Text, len);
  GetWindowText(wnd, Text, len);
  GetWindowRect(wnd, R1);
  Windows.GetClientRect(wnd, R2);
  GetClassName(wnd, class_name, 100);
  parentWnd := GetParent(wnd);
  EnumParams.List := childlist;
  EnumParams.ParentWnd := wnd;
  EnumChildWindows(wnd, @EnumChildrenProc, integer(@EnumParams));

  with TreeView1.Items do
  begin
    AddChild(ParentNode, 'Handle = ' + IntToStr(wnd));
    AddChild(ParentNode, 'Caption = ' + Text);
    AddChild(ParentNode, 'Class name = ' + class_name);
    AddChild(ParentNode, 'Parent Handle = ' + IntToStr(parentWnd));
    GetWinInfoStyle(wnd, ParentNode);
    GetWinInfoExtendedStyle(wnd, ParentNode);
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

procedure TResults.GetWinInfoStyle(wnd: HWND; ParentNode: TTreeNode);
var
  style: Integer;
  list: TStringList;
  node1: TTreeNode;
  i: Integer;
begin
  style := GetWindowLong(wnd, GWL_STYLE);
  list := GetWindowStyleNames(style);

  with TreeView1.Items do
  begin
    node1 := AddChild(ParentNode, 'Style');
    AddChild(node1, 'Value = ' + IntToStr(style));

    for i := 0 to list.Count - 1 do
      AddChild(node1, list[i]);
  end;

  list.Free();
end;

procedure TResults.GetWinInfoExtendedStyle(wnd: HWND; ParentNode: TTreeNode);
var
  style: Integer;
  list: TStringList;
  node1: TTreeNode;
  i: Integer;
begin
  style := GetWindowLong(wnd, GWL_EXSTYLE);
  list := GetExtendedWindowStyleNames(style);

  with TreeView1.Items do
  begin
    node1 := AddChild(ParentNode, 'Extended Style');
    AddChild(node1, 'Value = ' + IntToStr(style));

    for i := 0 to list.Count - 1 do
      AddChild(node1, list[i]);
  end;

  list.Free();
end;

end.
