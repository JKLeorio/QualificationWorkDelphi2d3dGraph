unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, OpenGLControl, GLFSWindow, Parser,
  Generics.Collections, StrUtils, DrawUtils, EditListBox, Vcl.ComCtrls;



type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    Panel1: TPanel;
    Panel3: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControlPaint(Sender : TObject);
    procedure FormResize(Sender: TObject);
    procedure AddEditListBoxItem(Sender: TObject);
    procedure CalculateAll(sender : Tobject);
    procedure PaintLine(points : array of TPoint2d);
  private
    ChildWindow : TForm;
  public

  end;


var
  Form1: TForm1;
  FOpenGLControl: TOpenGLControl;
  GLSFWindow : TGLFSWindowForm;
  MathExpressionCalc : TMathExpressionCalc;
  EditListBox : TEditListBox;

procedure CallArrange;

implementation

uses
  OpenGL, EditListBoxItem, MathUtils;

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  MathExpressionCalc := TMathExpressionCalc.Create();
  FOpenGLControl := TOpenGLControl.Create(nil);
  FOpenGLControl.Parent := Panel1;
  FOpenGLControl.Align := alClient;
  FOpenGLControl.Visible := True;
  FOpenGLControl.OnPaint := OpenGLControlPaint;
  EditListBox := TEditListBox.Create(nil);
  EditListBox.Parent := Panel2;
  EditListBox.Align := alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FOpenGLControl.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  Aspect: Single;
begin
  glViewPort(0, 0, FOpenGLControl.Width, FOpenGLControl.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  Aspect := Real(FOpenGLControl.Width) / Real(FOpenGLControl.Height);
  glOrtho(-Aspect, Aspect, -1.0, 1.0, -1.0, 1.0);
end;


procedure TForm1.OpenGLControlPaint(sender : Tobject);
begin
  glViewport(0, 0, FOpenGLControl.Width, FOpenGLControl.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(-50, 50, -50, 50);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;



procedure TForm1.AddEditListBoxItem(Sender: TObject);
begin
 EditListBox.AddEdit;
end;

procedure TForm1.PaintLine(points : array of TPoint2d);
  var
    point : TPoint2d;
  begin
      for point in points do
      begin
        glBegin(GL_LINES);

        glEnd;
      end;

  end;



procedure TForm1.CalculateAll(sender : Tobject);
  var
    I, I2, I3, rstart, rend : Integer;
    items : TObjectList<TEditListBoxItem>;
    variables : TObjectList<TVariablePair>;
    points : TPoints2d;
    math_expression : string;
    variables_dict : TDictionary<string, float64>;
    value : Float64;
    ranges : Tlist<TRangePair>;
    range : TRangePair;
    parsed_data : TQueue<string>;
    parsed_data_copy : array of string;
    elem : string;
    symbol, buff : string;
    
  begin
    variables_dict := TDictionary<string, float64>.Create;
    items := EditListBox.ListBoxItems;
    for I := 0 to items.Count-1 do
    begin
    items[i].GetVariables(variables);
    items[i].GetMainEditData(math_expression);
    items[i].GetRanges(ranges);
    if Assigned(variables_dict) then variables_dict.Free;
    variables_dict := TDictionary<string, float64>.Create;
    buff := ranges[0].NameEdit.Text;
    symbol := 'x';
    if buff = 'y' 
    then
      symbol := 'x'
    else if True then
      symbol := 'y';
    
    math_expression := SolveExpressionSymPy(math_expression, symbol);
    if MathExpressionCalc.Parse(math_expression, parsed_data) then
      begin
      if parsed_data.Count <> 0 then
          begin
            SetLength(parsed_data_copy, parsed_data.Count);
            for I3 := 0 to parsed_data.Count-1 do                                         
            begin
              parsed_data_copy[I] := parsed_data.Extract;
            end;
          end;
        for I2 := 0 to variables.Count-1 do
          begin
            if TryStrToFloat(variables[I2].ValueEdit.Text, value) then
              variables_dict.AddOrSetValue(variables[I2].NameEdit.Text, value);
          end;
        for range in ranges do
          begin
            if TryDecimalStrToInt(range.StartEdit.Text, rstart) and TryDecimalStrToInt(range.EndEdit.Text, rend) then
            begin
              if rstart > rend then raise Exception.Create('range must be positive');
              for elem in parsed_data_copy do
              begin
                parsed_data.Enqueue(elem);
              end;
              if MathExpressionCalc.CalcPointsByRange(rstart, rend, variables_dict, symbol, parsed_data, points) then
                begin
                  glClearColor(1, 1, 1, 1);
                  glClear(GL_COLOR_BUFFER_BIT);
                  DrawCoordinate;
                  glColor3f(0, 0, 0);
                  DrawGraph(points);
                  SwapBuffers(wglGetCurrentDC);
                end;
            end
            else
              raise Exception.Create('range error');
          end;
        end;
      end;
  end;

procedure CallArrange;
  begin
    EditListBox.ArrangeEdits;
  end;
end.

