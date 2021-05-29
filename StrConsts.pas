unit StrConsts;

{$MODE Delphi}

interface

uses Windows;

const
  WindowStyle: array [0..21] of DWORD = (
    WS_BORDER,
    WS_CAPTION,
    WS_CHILD,
    WS_CLIPCHILDREN,
    WS_CLIPSIBLINGS,
    WS_DISABLED,
    WS_DLGFRAME,
    WS_GROUP,
    WS_HSCROLL,
    WS_MAXIMIZE,
    WS_MAXIMIZEBOX,
    WS_MINIMIZE,
    WS_MINIMIZEBOX,
    WS_OVERLAPPED,
    WS_OVERLAPPEDWINDOW,
    WS_POPUP,
    WS_POPUPWINDOW,
    WS_SYSMENU,
    WS_TABSTOP,
    WS_THICKFRAME,
    WS_VISIBLE,
    WS_VSCROLL);

  WindowStyleName: array [0..21] of string = (
    'Border',
    'Caption',
    'Child',
    'Clip children',
    'Clip siblings',
    'Disabled',
    'Dialog frame',
    'Group',
    'Horizontal scroll bar',
    'Maximized',
    'Maximize button',
    'Minimized',
    'Minimize button',
    'Overlapped',
    'Overlapped window',
    'Pop-up',
    'Pop-up window',
    'System menu',
    'Tab stop',
    'Sizing frame',
    'Visible',
    'Vertical scroll bar');

  ExtendedWindowStyle: array [0..20] of integer = (
    WS_EX_ACCEPTFILES,
    WS_EX_APPWINDOW,
    WS_EX_CLIENTEDGE,
    WS_EX_CONTEXTHELP,
    WS_EX_CONTROLPARENT,
    WS_EX_DLGMODALFRAME,
    WS_EX_LEFT,
    WS_EX_LEFTSCROLLBAR,
    WS_EX_LTRREADING,
    WS_EX_MDICHILD,
    WS_EX_NOPARENTNOTIFY,
    WS_EX_OVERLAPPEDWINDOW,
    WS_EX_PALETTEWINDOW,
    WS_EX_RIGHT,
    WS_EX_RIGHTSCROLLBAR,
    WS_EX_RTLREADING,
    WS_EX_STATICEDGE,
    WS_EX_TOOLWINDOW,
    WS_EX_TOPMOST,
    WS_EX_TRANSPARENT,
    WS_EX_WINDOWEDGE);

  ExtendedWindowStyleName: array [0..20] of string = (
    'Drag drop recepient',
    'Minimize on taskbar',
    'Sunken edge border',
    'Context help',
    'TAB key navigation',
    'Double border',
    'Left aligned (default)',
    'Left vertical scrollbar',
    'Left to Right Text (default)',
    'MDI Child',
    'No parent notify',
    'Client & Window edge',
    'Window & ToolWindow & TopMost edge',
    'Right aligned',
    'Right vertical scrollbar (default)',
    'Right to Left text',
    'Static edge',
    'Tool window',
    'Topmost',
    'Transparent',
    'Raised edge');

implementation

end.
