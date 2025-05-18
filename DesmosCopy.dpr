program DesmosCopy;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  OpenGLControl in 'OpenGLControl.pas',
  Parser in 'Parser.pas',
  GLFSWindow in 'GLFSWindow.pas' {GLFSWindowForm},
  MathUtils in 'MathUtils.pas',
  DrawUtils in 'DrawUtils.pas',
  EditListBox in 'EditListBox.pas',
  EditListBoxItem in 'EditListBoxItem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TGLFSWindowForm, GLFSWindowForm);
  Application.Run;
end.
