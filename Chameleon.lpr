program Chameleon;

{$MODE Delphi}

uses
  Forms,
  Interfaces,
  frmMain in 'frmMain.pas' {MainForm},
  frmResults in 'frmResults.pas' {Results},
  Writers in 'Writers.pas',
  DfmEngine in 'DfmEngine.pas',
  StyleNames in 'StyleNames.pas',
  about1 in 'about1.pas' {AboutBox};


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TResults, Results);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
