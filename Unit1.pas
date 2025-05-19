unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, OpenGLControl, GLFSWindow, Parser,
  Generics.Collections, StrUtils, DrawUtils, EditListBox, Vcl.ComCtrls, EditListBoxItem;



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
    Panel4: TPanel;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControlPaint(Sender : TObject);
    procedure FormResize(Sender: TObject);
    procedure AddEditListBoxItem2d(Sender: TObject);
    procedure AddEditListBoxItem3d(Sender: TObject);
    procedure CalculateAll(sender : Tobject);
    procedure OnClickBoxItemHide(sender : TObject);
  private
    ChildWindow : TForm;
  public
    items_points : TDictionary<TEditlistBoxitem, TArray<TGraphPoint>>;
    procedure DrawGraphs2d;
    procedure OnClickBoxItemCalc(sender : TObject);

  end;



var
  Form1: TForm1;
  FOpenGLControl: TOpenGLControl;
  FOpenGLControl3D : TOpenGLControl;
  GLSFWindow : TGLFSWindowForm;
  MathExpressionCalc : TMathExpressionCalc;
  EditListBox2d : TEditListBox;
  EditlistBox3d : TEditListBox;

procedure CallListBoxArrange2d;
procedure CallListBoxArrange3d;

implementation

uses
  OpenGL, MathUtils;

{$R *.dfm}


procedure TForm1.OnClickBoxItemCalc(sender : TObject);
  var
    item : TEditListBoxItem;
    btn : TButton;
  begin
    if sender is TButton then
    begin
      btn := TButton(sender);
      if btn.parent is TEditListBoxItem then
      begin
        item := TEditListBoxItem(btn.Parent);

      end;
    end;
  end;

procedure TForm1.OnClickBoxItemHide(sender : TObject);
  var
    item : TEditListBoxItem;
    btn : TButton;
  begin
    if sender is TButton then
    begin
      btn := TButton(sender);
      if btn.parent is TEditListBoxItem then
      begin
        item := TEditListBoxItem(btn.Parent);
        item.IsGraphHidden := not item.IsGraphHidden;
      end;
    end;
  end;

procedure TForm1.DrawGraphs2d;
  var
  I : Integer;
  item : TEditListBoxItem;
  items : TObjectList<TEditListBoxItem>;
  points : TArray<TGraphPoint>;
  begin
    items := EditListBox2d.ListBoxItems;
    for I := 0 to items.Count-1 do
    begin
      item := items[i];
      if not item.IsGraphHidden then
        if items_points.ContainsKey(item) then
        begin
          points := items_points.Items[item];
          DrawGraph(points);
        end;
    end;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MathExpressionCalc := TMathExpressionCalc.Create();

  items_points := TDictionary<TEditlistBoxitem, TArray<TGraphPoint>>.Create;

  FOpenGLControl := TOpenGLControl.Create(nil);
  FOpenGLControl.Parent := Panel1;
  FOpenGLControl.Align := alClient;
  FOpenGLControl.Visible := True;
  FOpenGLControl.OnPaint := OpenGLControlPaint;

  FOpenGLControl3d := TOpenGLControl.Create(nil);
  FOpenGLControl3d.Parent := Panel3;
  FOpenGLControl3d.Align := alClient;
  FOpenGLControl3d.Visible := True;
  FOpenGLControl3d.OnPaint := OpenGLControlPaint;

  EditListBox2d := TEditListBox.Create(nil);
  EditListBox2d.Parent := Panel2;
  EditListBox2d.Align := alClient;

  EditlistBox3d := TEditListBox.Create(nil);
  EditlistBox3d.Parent := Panel4;
  EditlistBox3d.Align := alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FOpenGLControl.Free;
  FOpenGLControl3D.Free;
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
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(0, 0, 0);
  DrawCoordinate;
  SwapBuffers(wglGetCurrentDC);
end;



procedure TForm1.AddEditListBoxItem2d(Sender: TObject);
begin
 EditListBox2d.AddEdit;
end;

procedure TForm1.AddEditListBoxItem3d(Sender: TObject);
  begin
   EditListBox3d.AddEdit;
  end;


procedure TForm1.CalculateAll(sender : Tobject);
  var
    I, I2, I3, I4, I5 , rstart, rend : Integer;
    items : TObjectList<TEditListBoxItem>;
    variables : TObjectList<TVariablePair>;
    points : TArray<TGraphPoint>;
    points_result : TArray<TGraphPoint>;
    pointsList : Tlist<TGraphPoint>;
    math_expression : string;
    variables_dict : TDictionary<string, float64>;
    value : Float64;
    ranges : Tlist<TRangePair>;
    range : TRangePair;
    parsed_data : TQueue<string>;
    parsed_data_copy : array of string;
    elem : string;
    symbol, buff : string;
    is_x, is_y, is_z, edited : Boolean;
    dependentVar : string;
  begin
    variables_dict := TDictionary<string, float64>.Create;
    items := EditListBox2d.ListBoxItems;
    pointsList := TList<TGraphPoint>.Create;
    for I := 0 to items.Count-1 do
    begin

      if not items[i].IfChangedGetMainEditData(math_expression) then
        Continue;

      pointsList.Clear;
      items[i].GetVariables(variables);
      items[i].GetRanges(ranges);
      if Assigned(variables_dict) then variables_dict.Free;
      variables_dict := TDictionary<string, float64>.Create;
      dependentVar := items[i].DependentVarEdit.Text;

      is_x := True;
      is_y := True;
      is_z := True;



      for range in ranges do
      begin
        variables_dict.AddOrSetValue('x', 1);
        variables_dict.AddOrSetValue('y', 1);
        variables_dict.AddOrSetValue('z', 1);

        if range.NameEdit.Modified or range.StartEdit.Modified or range.EndEdit.Modified then
          edited := True;

        buff := range.NameEdit.Text;
        if dependentVar = buff then
          raise Exception.Create('«ависима€ переменна€ не может быть диапазоном')
        else if buff = 'x' then
          is_x := False
        else if buff = 'y' then
          is_x := False
        else if buff = 'z' then
          is_z := False;
      end;

      if is_x and is_y and is_z then
        raise Exception.Create('“ребуетс€ диапазон');

      if not is_x and not is_y and not is_z then
        raise Exception.Create('«ависима€ переменна€ не может быть диапазоном');


        for I2 := 0 to variables.Count-1 do
            begin
              if variables[i2].NameEdit.Modified or variables[i].ValueEdit.Modified and TryStrToFloat(variables[I2].ValueEdit.Text, value) then
                variables_dict.AddOrSetValue(variables[I2].NameEdit.Text, value);
                edited := True;
            end;

      math_expression := SolveExpressionSymPy(math_expression, dependentVar);
      ShowMessage(math_expression);
      glClearColor(1, 1, 1, 1);
      if MathExpressionCalc.Parse(math_expression, parsed_data) then
        begin
        if parsed_data.Count <> 0 then
            begin
              SetLength(parsed_data_copy, parsed_data.Count);
              for I3 := 0 to parsed_data.Count-1 do                                         
              begin
                parsed_data_copy[I3] := parsed_data.Extract;
              end;
            end;
          for range in ranges do
            begin
              if TryDecimalStrToInt(range.StartEdit.Text, rstart) and TryDecimalStrToInt(range.EndEdit.Text, rend) then
              begin
                if rstart > rend then raise Exception.Create('range must be positive');
                for I4:=0 to Length(parsed_data_copy)-1 do
                begin
                  parsed_data.Enqueue(parsed_data_copy[I4]);
                end;
                if MathExpressionCalc.CalcPointsByRange(rstart, rend, variables_dict, dependentVar, range.NameEdit.Text, parsed_data, points, parsed_data_copy) then
                  begin
                  for I5 := 0 to High(points)-1 do
                    pointsList.Add(points[I5]);
                  Self.items_points.AddOrSetValue(items[i], pointsList.ToArray);
                  pointsList.Clear;
//                    DrawCoordinate;
//                    SetGLColor(clRed);
//                    DrawGraph(points);
                  end;
              end
              else
                raise Exception.Create('range error');
            end;
        end;
    end;
//    SwapBuffers(wglGetCurrentDC);
  end;

procedure CallListBoxArrange2d;
  begin
    EditListBox2d.ArrangeEdits;
  end;


procedure CallListBoxArrange3d;
  begin
    EditListBox3d.ArrangeEdits;
  end;


end.


