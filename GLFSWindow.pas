unit GLFSWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, OpenGLControl;

type
  TGLFSWindowForm = class(TForm)
    Panel1: TPanel;
  published
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
  public
    function GetOpenGLControl: TOpenGLControl;

  end;

var
  GLFSWindowForm: TGLFSWindowForm;

implementation

{$R *.dfm}

uses Unit1;

function TGLFSWindowForm.GetOpenGLControl: TOpenGLControl;
begin
  if (Panel1.ControlCount > 0) and (Panel1.Controls[0] is TOpenGLControl) then
    Result := TOpenGLControl(Panel1.Controls[0])
  else
    Result := nil;
end;

procedure TGLFSWindowForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Form1.HandleMouseWheelZoom(WheelDelta, MousePos);
  Handled := True;
end;

end.
