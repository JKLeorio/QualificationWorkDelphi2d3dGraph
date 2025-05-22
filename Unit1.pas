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
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControlPaint(Sender : TObject);
    procedure FormResize(Sender: TObject);
    procedure AddEditListBoxItem2d(Sender: TObject);
    procedure AddEditListBoxItem3d(Sender: TObject);
    procedure CalculateAll(sender : Tobject);
    procedure OnClickBoxItemHide(sender : TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pagecontrolchange(Sender: TObject);
    procedure OpenGLControlPaint3d(Sender : TObject);
    procedure Panel3dMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel3dMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel3dMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BtnViewXYClick(Sender: TObject);
    procedure BtnViewXZClick(Sender: TObject);
    procedure BtnViewYZClick(Sender: TObject);

  private
    ChildWindow : TForm;
    FirstCameraSetupDone: Boolean;
  public

    items_points : TDictionary<TEditlistBoxitem, TArray<TArray<TGraphPoint>>>;
    procedure DrawGraphs;
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
  FontReadyA, FontReadyB: Boolean;
  BaseFontA, BaseFontB: Cardinal;
  Zoom: Float32 = 1.0;
  ZoomLevel: Single = 1.0;
  RotationX, RotationY: Single;
  LastMouseX, LastMouseY: Integer;
  IsRotating: Boolean = False;
  FontInitialized: Boolean = False;
const
  MinZoom = 0.02;
  MaxZoom = 5.0;
  MaxZoomLevel = 2.0;
  MinZoomLevel = 0.1;


procedure CallListBoxArrange;

implementation

uses
  OpenGL, MathUtils;

{$R *.dfm}


procedure InitFont(hdc: HDC; out BaseFont: GLuint);
var
  font: HFONT;
begin
  BaseFont := glGenLists(256);

  font := CreateFont(
    -12, 0, 0, 0, FW_NORMAL, 0, 0, 0,
    ANSI_CHARSET, OUT_DEFAULT_PRECIS,
    CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
    FF_DONTCARE or DEFAULT_PITCH,
    'Courier New'
  );

  SelectObject(hdc, font);
  wglUseFontBitmaps(hdc, 0, 256, BaseFont);
  DeleteObject(font);
end;


procedure SetZoomlevel(Value: Float32);
begin
  if Value < MinZoomLevel then
    ZoomLevel := MinZoomLevel
  else if Value > MaxZoomLevel then
    ZoomLevel := MaxZoomLevel
  else
    ZoomLevel := Value;

  FOpenGLControl3d.Invalidate;
end;

procedure TForm1.BtnViewXYClick(Sender: TObject);
begin
  RotationX := 0;
  RotationY := 0;
  FOpenGLControl3d.Invalidate;
end;

procedure TForm1.BtnViewXZClick(Sender: TObject);
begin
  RotationX := 90;
  RotationY := 0;
  FOpenGLControl3d.Invalidate;
end;

procedure TForm1.BtnViewYZClick(Sender: TObject);
begin
  RotationX := 0;
  RotationY := 90;
  FOpenGLControl3d.Invalidate;
end;



procedure TForm1.Panel3dMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    IsRotating := True;
    LastMouseX := X;
    LastMouseY := Y;
  end;
end;

procedure TForm1.Panel3dMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  IsRotating := False;
end;

procedure TForm1.Panel3dMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if IsRotating then
  begin
    RotationX := RotationX + (Y - LastMouseY);
    RotationY := RotationY + (X - LastMouseX);
    LastMouseX := X;
    LastMouseY := Y;
    FOpenGLControl3d.Invalidate;
  end;
end;


procedure TForm1.DrawGraphs;
  var
  I : Integer;
  item : TEditListBoxItem;
  items : TObjectList<TEditListBoxItem>;
  editlistbox : TEditListBox;
  points : TArray<TArray<TGraphPoint>>;
  proc : Tproc<TArray<TArray<TGraphPoint>>>;
  begin
    if PageControl1.ActivePage = TabSheet1 then
    begin
      editlistbox := EditListBox2d;
      proc := DrawGraph;
    end
    else if PageControl1.ActivePage = TabSheet2 then
    begin
      editlistbox := EditlistBox3d;
      proc := DrawGraph3d;
    end;
    items := editlistbox.ListBoxItems;
    for I := 0 to items.Count-1 do
    begin
      item := items[i];
      if not item.IsGraphHidden then
        if items_points.ContainsKey(item) then
        begin
          points := items_points.Items[item];
          SetGLColor(item.SelectedColor);
            proc(points);
        end;
    end;
  end;

procedure TForm1.OpenGLControlPaint3d(Sender: TObject);
var
  w, h: Integer;
begin
  w := FOpenGLControl3D.Width;
  h := FOpenGLControl3D.Height;

  if not FontReadyA then
  begin
    InitFont(wglGetCurrentDC, BaseFontA);
    FontReadyA := True;
  end;
  glViewport(0, 0, w, h);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, w / h, 0.1, 1000.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  if not FirstCameraSetupDone then
  begin
    RotationX := 30;
    RotationY := 45;
    FirstCameraSetupDone := True;
  end;

  glTranslatef(0.0, 0.0, -300 / ZoomLevel);
  glRotatef(RotationX, 1, 0, 0);
  glRotatef(RotationY, 0, 1, 0);

  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  DrawCoordinate3d(BaseFontA, ZoomLevel);
  DrawGraphs;

  SwapBuffers(wglGetCurrentDC);
end;



procedure TForm1.pagecontrolchange(sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    FOpenGLControl.MakeCurrent;
    if FOpenGLControl.IsCurrent then
      ShowMessage('2d');
    if FontReadyA then
    begin
      glDeleteLists(BaseFontA, 256);
      FontReadyA := False;
    end;
    FOpenGLControl.Repaint;
  end
  else if PageControl1.ActivePage = TabSheet2 then
  begin
    FOpenGLControl3D.MakeCurrent;
    if FOpenGLControl3d.IsCurrent then
      ShowMessage('3d');
    if FontReadyB then
    begin
      glDeleteLists(BaseFontB, 256);
      FontReadyB := False;
    end;
    FOpenGLControl3D.Repaint;
  end;
end;

procedure SetZoom(Value: Float32);
begin
  if Value < MinZoom then
    Zoom := MinZoom
  else if Value > MaxZoom then
    Zoom := MaxZoom
  else
    Zoom := Value;

  FOpenGLControl.Invalidate;
end;



procedure TForm1.ZoomInBtnClick(Sender: TObject);
const
  ZoomFactor = 0.9;
begin
  SetZoom(Zoom * ZoomFactor);
  FOpenGLControl.Invalidate;
end;

procedure TForm1.ZoomOutBtnClick(Sender: TObject);
const
  ZoomFactor = 1.1;
begin
  SetZoom(Zoom * ZoomFactor);
  FOpenGLControl.Invalidate;
end;






procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  var
    pt: TPoint;
  const
    ZoomFactor = 1.1;
  begin
  if PageControl1.ActivePage = TabSheet1 then
    begin
      pt := FOpenGLControl.ScreenToClient(MousePos);

    if (pt.X >= 0) and (pt.X < FOpenGLControl.Width) and (pt.Y >= 0) and (pt.Y < FOpenGLControl.Height) then
       begin

        if WheelDelta > 0 then
          SetZoom(Zoom * ZoomFactor)
        else
          SetZoom(Zoom / ZoomFactor);
        FOpenGLControl.Invalidate;
        Handled := True;
      end
    end
    else if PageControl1.ActivePage = TabSheet2 then
      begin
        pt := FOpenGLControl3D.ScreenToClient(MousePos);
        if  (pt.X >= 0) and (pt.X < FOpenGLControl3D.Width) and (pt.Y >= 0) and (pt.Y < FOpenGLControl3D.Height) then
        begin
          if WheelDelta > 0 then
          SetZoomlevel(ZoomLevel * 1.1)
          else
          SetZoomlevel(ZoomLevel / 1.1);
          FOpenGLControl3d.Invalidate;
          Handled := True;
        end;
      end;

  end;



procedure TForm1.FormCreate(Sender: TObject);
begin

  FirstCameraSetupDone := False;

  Self.OnMouseWheel := FormMouseWheel;

  MathExpressionCalc := TMathExpressionCalc.Create();

  items_points := TDictionary<TEditlistBoxitem, TArray<TArray<TGraphPoint>>>.Create;

  FOpenGLControl3d := TOpenGLControl.Create(nil);
  FOpenGLControl3d.Parent := Panel3;
  FOpenGLControl3d.Align := alClient;
  FOpenGLControl3d.Visible := True;
  FOpenGLControl3d.OnPaint := OpenGLControlPaint3d;
  FOpenGLControl3D.OnMouseDown := Panel3dMouseDown;
  FOpenGLControl3D.OnMouseMove := Panel3dMouseMove;
  FOpenGLControl3D.OnMouseUp := Panel3dMouseUp;

  FOpenGLControl := TOpenGLControl.Create(nil);
  FOpenGLControl.Parent := Panel1;
  FOpenGLControl.Align := alClient;
  FOpenGLControl.Visible := True;
  FOpenGLControl.OnPaint := OpenGLControlPaint;



  EditListBox2d := TEditListBox.Create(nil);
  EditListBox2d.Parent := Panel2;
  EditListBox2d.Align := alClient;
  EditListBox2d.onBoxItemCalcMethod := OnClickBoxItemCalc;
  EditlistBox2d.onBoxItemHideMethod := OnClickBoxItemHide;

  EditlistBox3d := TEditListBox.Create(nil);
  EditlistBox3d.Parent := Panel4;
  EditlistBox3d.Align := alClient;
  EditListBox3d.onBoxItemCalcMethod := OnClickBoxItemCalc;
  EditlistBox3d.onBoxItemHideMethod := OnClickBoxItemHide;

  glClearColor(1, 1, 1, 1);

end;



procedure TForm1.OnClickBoxItemCalc(sender : TObject);
  var
    item : TEditListBoxItem;
    btn : TButton;
    I, I2, I3, I4, I5, I6, rstart, rend : Integer;
    variables : TObjectList<TVariablePair>;
    pointsList : TList<TArray<TGraphPoint>>;
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

  procedure CalcRanges(ranges : Tlist<TRangePair>; count : Integer; variables_dict : TDictionary<string, float64>);
  var
    I4, I5, rstart, rend : Integer;
    range : TRangePair;
    points : TArray<TGraphPoint>;
  begin
    if count = ranges.Count-1 then
      begin
      range := ranges[count];
        if TryDecimalStrToInt(range.StartEdit.Text, rstart) and TryDecimalStrToInt(range.EndEdit.Text, rend) then
          begin
            if rstart > rend then raise Exception.Create('range must be positive');
            for I4:=0 to Length(parsed_data_copy)-1 do
            begin
              parsed_data.Enqueue(parsed_data_copy[I4]);
            end;
            if MathExpressionCalc.CalcPointsByRange(rstart, rend, variables_dict, dependentVar, range.NameEdit.Text, points, parsed_data_copy) then
              begin
                pointsList.Add(points);
              end;
          end
      end
    else
      begin
        range := ranges[count];
        if TryDecimalStrToInt(range.StartEdit.Text, rstart) and TryDecimalStrToInt(range.EndEdit.Text, rend) then
          begin
            if rstart > rend then raise Exception.Create('range must be positive');
            for I4:=0 to Length(parsed_data_copy)-1 do
            begin
              parsed_data.Enqueue(parsed_data_copy[I4]);
            end;
            for I5 := 0 to Abs(rstart) + Abs(rend) do
            begin
              variables_dict.AddOrSetValue(ranges[count].NameEdit.Text, I5+rstart);
              CalcRanges(ranges, count+1, variables_dict)
            end;
          end
      end;
  end;

  begin
    if sender is TButton then
    begin
      btn := TButton(sender);
      if btn.parent is TEditListBoxItem then
      begin
        pointsList := TList<TArray<TGraphPoint>>.Create;
        item := TEditListBoxItem(btn.Parent);
        if not item.IfChangedGetMainEditData(math_expression) and items_points.ContainsKey(item) and (High(items_points.Items[item]) > 0) then
          Exit;
        item.GetVariables(variables);
        item.GetRanges(ranges);
        variables_dict := TDictionary<string, float64>.Create;
        dependentVar := item.DependentVarEdit.Text;

        is_x := True;
        is_y := True;
        is_z := True;
        edited := True;

        variables_dict.AddOrSetValue('x', 1);
        variables_dict.AddOrSetValue('y', 1);
        variables_dict.AddOrSetValue('z', 1);



        for range in ranges do
        begin
          if range.NameEdit.Modified or range.StartEdit.Modified or range.EndEdit.Modified then
            edited := True;

          buff := range.NameEdit.Text;
          variables_dict.AddOrSetValue(buff, 0);
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
                if (variables[i2].NameEdit.Modified or variables[i].ValueEdit.Modified) and TryStrToFloat(variables[I2].ValueEdit.Text, value) then
                begin
                  variables_dict.AddOrSetValue(variables[I2].NameEdit.Text, value);
                  edited := True;
                end;
              end;

        if not edited then Exit;

        math_expression := SolveExpressionSymPy(math_expression, dependentVar);
        ShowMessage(math_expression);

        if MathExpressionCalc.Parse(math_expression, parsed_data, variables_dict) then
        begin
        if parsed_data.Count <> 0 then
            begin
              SetLength(parsed_data_copy, parsed_data.Count);
              for I3 := 0 to parsed_data.Count-1 do
              begin
                parsed_data_copy[I3] := parsed_data.Extract;
              end;
                CalcRanges(ranges, 0, variables_dict);
                Self.items_points.AddOrSetValue(item, pointsList.ToArray);
                pointsList.Clear;
            end;
        end;
        FOpenGLControl.Repaint;
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
        FOpenGLControl.Repaint;
      end;
    end;
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


procedure TForm1.OpenGLControlPaint(Sender: TObject);
var
  w, h: Integer;
begin
  w := FOpenGLControl.Width;
  h := FOpenGLControl.Height;

  if not FontReadyB then
  begin
    InitFont(wglGetCurrentDC, BaseFontB);
    FontReadyB := True;
  end;

  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  gluOrtho2D(-50 * Zoom, 50 * Zoom, -50 * Zoom, 50 * Zoom);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glColor3f(0, 0, 0);

  DrawCoordinate(BaseFontB, Zoom);
  DrawGraphs;

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
    pointsList : TList<TArray<TGraphPoint>>;
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
    editlistbox : TEditListBox;
  begin
    if PageControl1.ActivePage = TabSheet1 then
    begin
      editlistbox := EditListBox2d
    end
    else if PageControl1.ActivePage = TabSheet2 then
    begin
      editlistbox := EditlistBox3d;
    end;
    variables_dict := TDictionary<string, float64>.Create;
    items := editlistbox.ListBoxItems;
    pointsList := TList<TArray<TGraphPoint>>.Create;
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

      variables_dict.AddOrSetValue('x', 1);
      variables_dict.AddOrSetValue('y', 1);
      variables_dict.AddOrSetValue('z', 1);


      for range in ranges do
      begin
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
              begin
                variables_dict.AddOrSetValue(variables[I2].NameEdit.Text, value);
                edited := True;
              end;
            end;

      if not edited then Continue;

      math_expression := SolveExpressionSymPy(math_expression, dependentVar);
      if MathExpressionCalc.Parse(math_expression, parsed_data, variables_dict) then
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
                if MathExpressionCalc.CalcPointsByRange(rstart, rend, variables_dict, dependentVar, range.NameEdit.Text, points, parsed_data_copy) then
                  begin
                  pointsList.Add(points);
                  Self.items_points.AddOrSetValue(items[i], pointsList.ToArray);
                  pointsList.Clear;
                  end;
              end
              else
                raise Exception.Create('range error');
            end;
        end;
        FOpenGLControl.Repaint;
    end;
  end;

procedure CallListBoxArrange;
  begin
    EditListBox2d.ArrangeEdits;
    EditlistBox3d.ArrangeEdits;
  end;



end.


