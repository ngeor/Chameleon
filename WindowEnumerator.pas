unit WindowEnumerator;

{$mode Delphi}

interface

uses
  Windows;

type
  TWndConsumer = procedure (Wnd: HWND) of object;

procedure EnumerateChildWindows(Wnd: HWND; Consumer: TWndConsumer);

implementation

type
  TWindowEnumerator = class
  private
    FWnd: HWND;
    FConsumer: TWndConsumer;
    class function MyEnumChildrenProc(Wnd: HWND; Lp: LPARAM): BOOL; static; stdcall;
    procedure Add(Wnd: HWND);
  public
    constructor Create(Wnd: HWND; Consumer: TWndConsumer);
    procedure Run;
  end;

procedure EnumerateChildWindows(Wnd: HWND; Consumer: TWndConsumer);
var
  Enumerator: TWindowEnumerator;
begin
  Enumerator := TWindowEnumerator.Create(Wnd, Consumer);
  try
    Enumerator.Run;
  finally
    Enumerator.Free;
  end;
end;

constructor TWindowEnumerator.Create(Wnd: HWND; Consumer: TWndConsumer);
begin
  FWnd := Wnd;
  FConsumer := Consumer;
end;

procedure TWindowEnumerator.Run;
begin
  EnumChildWindows(FWnd, @TWindowEnumerator.MyEnumChildrenProc, LPARAM(Self));
end;

class function TWindowEnumerator.MyEnumChildrenProc(Wnd: HWND; Lp: LPARAM): BOOL; static; stdcall;
var
  Me: TWindowEnumerator;
begin
  Me := TWindowEnumerator(Lp);
  Me.Add(Wnd);
  Result := True;
end;

procedure TWindowEnumerator.Add(Wnd: HWND);
begin
  FConsumer(wnd);
end;

end.
