unit WindowEnumerator;

{$mode Delphi}

interface

uses
  Windows;

type
  { An object method that accepts a HWND }
  TWndConsumer = procedure(Wnd: HWND) of object;

{ Calls the given consumer for every child window of the given window }
procedure EnumerateChildWindows(Wnd: HWND; Consumer: TWndConsumer);

implementation

type
  { Wrapper structure to hold the object procedure and pass it as a pointer }
  PWrapper = ^TWrapper;

  TWrapper = record
    Consumer: TWndConsumer;
  end;

{ Callback for Windows' EnumChildWindows }
function MyEnumChildrenProc(Wnd: HWND; Lp: LPARAM): BOOL; stdcall;
var
  Wrapper: PWrapper;
begin
  Wrapper := PWrapper(Lp);
  Wrapper^.Consumer(Wnd);
  Result := True;
end;

procedure EnumerateChildWindows(Wnd: HWND; Consumer: TWndConsumer);
var
  Wrapper: TWrapper;
begin
  Wrapper.Consumer := Consumer;
  EnumChildWindows(Wnd, @MyEnumChildrenProc, LPARAM(@Wrapper));
end;

end.
