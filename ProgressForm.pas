unit ProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
  private
  public
    procedure SetProgress(Step: Integer; const Msg: string = '');
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}



procedure TForm2.SetProgress(Step: Integer; const Msg: string);
begin
  ProgressBar1.Position := Step;
  if Msg <> '' then
    Label1.Caption := Msg;
  Application.ProcessMessages;
end;

end.
