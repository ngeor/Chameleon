unit Writers;

{$MODE Delphi}

interface

uses Windows, SysUtils, Classes, Graphics;

type
  TTextWriter = class
  private
    FStream: TStream;
    FIdent: Cardinal;
    FWriteIdent: Boolean;
    procedure WriteIdent;
  public
    constructor Create(AStream: TStream);
    constructor CreateFile(const FileName: String);
    destructor Destroy; override;
    procedure NewLine;
    procedure Write(const str: String);
    procedure WriteLn(const str: String);
    procedure WriteString(const str: String);
    property Ident: Cardinal read FIdent write FIdent;
    property Stream: TStream read FStream;
  end;

  TDfmWriter = class(TTextWriter)
    procedure WriteBinaryAsText(Input: TStream);
    procedure WriteBoolProp(const Name: String; Value: Boolean);
    procedure WriteColorProp(const Name: String; Value: TColor);
    procedure WriteCustomProp(const Name, Value: String);
    procedure WriteIntProp(const Name: String; Value: Integer);
    procedure WriteStringProp(const Name, Value: String);
    procedure WritePlacement(wnd, parent: HWND);
  end;

implementation

constructor TTextWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FIdent := 0;
  FWriteIdent := True;
end;

constructor TTextWriter.CreateFile(const FileName: String);
begin
  inherited Create;
  FStream := TFileStream.Create(FileName, fmCreate);
  FIdent := 0;
  FWriteIdent := True;
end;

destructor TTextWriter.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TTextWriter.WriteIdent;
var
  s: String;
  i: Integer;
begin
  if FWriteIdent then
  begin
    if FIdent > 0 then
    begin
      s := '';
      for i := 1 to FIdent do
        s := s + ' ';
      FStream.Write(s[1], FIdent);
    end;
    FWriteIdent := False;
  end;
end;

procedure TTextWriter.Write(const str: String);
begin
  WriteIdent;
  FStream.Write(str[1], Length(str));
end;

procedure TTextWriter.WriteLn(const str: String);
begin
  Write(str);
  NewLine;
end;

procedure TTextWriter.WriteString(const str: String);
begin
  Write(Chr(Length(str)) + str);
end;

procedure TTextWriter.NewLine;
const
  crlf: array [0..1] of Char = #13#10;
begin
  FStream.Write(crlf[0], 2);
  FWriteIdent := True;
end;

procedure TDfmWriter.WriteBoolProp(const Name: String; Value: Boolean);
const
  s: array [False..True] of String = ('False', 'True');
begin
  WriteCustomProp(Name, s[Value]);
end;

procedure TDfmWriter.WriteColorProp(const Name: String; Value: TColor);
begin
  WriteCustomProp(Name, ColorToString(Value));
end;

procedure TDfmWriter.WriteCustomProp(const Name, Value: String);
begin
  WriteLn(Name + ' = ' + Value);
end;

procedure TDfmWriter.WriteIntProp(const Name: String; Value: Integer);
begin
  WriteCustomProp(Name, IntToStr(Value));
end;

procedure TDfmWriter.WriteStringProp(const Name, Value: String);
begin
  WriteCustomProp(Name, '''' + Value + '''');
end;

procedure BinToHex(Binary, Text: PChar; Count: Integer);
const
  HexChars: array[0..15] of Char = '0123456789ABCDEF';
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Text^ := HexChars[(Byte(Binary[I]) and $F0) shr 4];
    Inc(Text);
    Text^ := HexChars[(Byte(Binary[I]) and $0F)];
    Inc(Text);
  end;
end;

procedure TDfmWriter.WriteBinaryAsText(Input: TStream);
const
  BytesPerLine = 32;
var
  MultiLine: Boolean;
  I: Integer;
  Count: Longint;
  Buffer: array[0..BytesPerLine - 1] of Char;
  Text: array[0..BytesPerLine * 2 - 1] of Char;
begin
  Count := Input.Size;
  MultiLine := Count > BytesPerLine;
  while Count > 0 do
  begin
    if MultiLine then
      NewLine;
    if Count >= BytesPerLine then
      I := BytesPerLine
    else
      I := Count;
    Input.Read(Buffer, I);
    BinToHex(Buffer, Text, I);
    Write(Text);
    Dec(Count, I);
  end;
end;

procedure TDfmWriter.WritePlacement(wnd, parent: HWND);
var
  R: TRect;
begin
  GetWindowRect(wnd, R);
  if IsWindow(parent) then
  begin
    Windows.ScreenToClient(parent, R.TopLeft);
    Windows.ScreenToClient(parent, R.BottomRight);
  end;
  WriteIntProp('Left', R.Left);
  WriteIntProp('Top', R.Top);
  WriteIntProp('Width', R.Right - R.Left);
  WriteIntProp('Height', R.Bottom - R.Top);
end;

end.
