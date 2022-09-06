unit WindowEnumerator;

{$mode Delphi}

interface

uses
  Windows;

type
  TWndConsumer = procedure (wnd: HWND) of object;

procedure EnumerateChildWindows(wnd: HWND; consumer: TWndConsumer);

implementation

type
  TWindowEnumerator = class
  private
    wnd: HWND;
    consumer: TWndConsumer;
    class function MyEnumChildrenProc(wnd: HWND; lp: LPARAM): BOOL; static; stdcall;
    procedure Add(wnd: HWND);
  public
    constructor Create(wnd: HWND; consumer: TWndConsumer);
    procedure Run;
  end;

procedure EnumerateChildWindows(wnd: HWND; consumer: TWndConsumer);
var
  enumerator: TWindowEnumerator;
begin
  enumerator := TWindowEnumerator.Create(wnd, consumer);
  try
    enumerator.Run;
  finally
    enumerator.Free;
  end;
end;

constructor TWindowEnumerator.Create(wnd: HWND; consumer: TWndConsumer);
begin
  Self.wnd := wnd;
  Self.consumer := consumer;
end;

procedure TWindowEnumerator.Run;
begin
  EnumChildWindows(wnd, @TWindowEnumerator.MyEnumChildrenProc, LPARAM(Self));
end;

class function TWindowEnumerator.MyEnumChildrenProc(wnd: HWND; lp: LPARAM): BOOL; static; stdcall;
var
  me: TWindowEnumerator;
begin
  me := TWindowEnumerator(lp);
  me.Add(wnd);
  Result := True;
end;

procedure TWindowEnumerator.Add(wnd: HWND);
begin
  consumer(wnd);
end;

end.
