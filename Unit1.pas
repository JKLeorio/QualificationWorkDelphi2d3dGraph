unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, OpenGLControl, GLFSWindow, Parser,
  Generics.Collections, StrUtils, DrawUtils, EditListBox, Vcl.ComCtrls, EditListBoxItem, ProgressForm,
  Vcl.Menus;



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
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel10: TPanel;
    Panel9: TPanel;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
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
    procedure pagecontrolchange(Sender: TObject);
    procedure OpenGLControlPaint3d(Sender : TObject);
    procedure Panel3dMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel3dMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel3dMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BtnViewXYClick(Sender: TObject);
    procedure BtnViewXZClick(Sender: TObject);
    procedure BtnViewYZClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure HelpMenuItemClick(Sender: TObject);
    procedure LoadMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure FullScreen(Sender: Tobject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private
    ChildWindow : TForm;
    FirstCameraSetupDone: Boolean;
  public
    items_points : TDictionary<TEditlistBoxitem, TArray<TArray<TGraphPoint>>>;
    procedure DrawGraphs;
    procedure OnClickBoxItemCalc(sender : TObject);
    procedure HandleMouseWheelZoom(WheelDelta: Integer; MousePos: TPoint);

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
  form_page_factor, page_opengl_control : float32;
  Caption : string;
  FProgressForm : Tform2;
const
  MinZoom = 0.02;
  MaxZoom = 5.0;
  MaxZoomLevel = 4.0;
  MinZoomLevel = 0.35;



procedure CallListBoxArrange;

implementation

uses
  OpenGL, MathUtils;

{$R *.dfm}


procedure TForm1.SaveMenuItemClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  editlistbox: TEditListBox;
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    editlistbox := EditListBox2d
  end
  else if PageControl1.ActivePage = TabSheet2 then
  begin
    editlistbox := EditlistBox3d;
  end;
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'JSON файлы (*.json)|*.json|Все файлы (*.*)|*.*';
    OpenDialog.Title := 'Выберите файл для загрузки';
    if OpenDialog.Execute then
    begin
      editlistbox.SaveEditListBoxToFile(OpenDialog.FileName);
    end;
  finally
    OpenDialog.Free;
  end;
end;


procedure TForm1.LoadMenuItemClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  editlistbox: TEditListBox;
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    editlistbox := EditListBox2d
  end
  else if PageControl1.ActivePage = TabSheet2 then
  begin
    editlistbox := EditlistBox3d;
  end;
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'JSON файлы (*.json)|*.json|Все файлы (*.*)|*.*';
    OpenDialog.Title := 'Выберите файл для загрузки';
    if OpenDialog.Execute then
    begin
      editlistbox.LoadEditListBoxFromFile(OpenDialog.FileName, False);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TForm1.HelpMenuItemClick(Sender: TObject);
begin
  ShowMessage(
  'Для вычисления требуется график' + #13#10 +
  'Для добавления графика нажимаете на кнопку "Добавить график"' +#13#10+
  'Кнопка "Вычислить всё" — Вычисляет все графики и отображает их на холсте'+
  #13#10+
  'Для вычисления графика требуется добавить его формулу' + #13#10+
  #13#10 + 'Для вычисления графика обязательно требуется добавить диапазон' +
  #13#10 + 'для этого нажмите на кнопку "+диапазон" внизу графика'+#13#10+
  'Вы также можете добавить более одного диапазона' + #13#10 +
  'Так же вы можете добавить переменную нажав на кнопку "+переменная" внизу графика');
end;

procedure TForm1.AboutMenuItemClick(Sender: TObject);
begin
  ShowMessage(
  'Квалификационный проект'
   + #13#10 +
   'Приложение: Графический калькулятор 2D/3D — Автор: Козубаев Жуманазар');
end;



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


procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  HandleMouseWheelZoom(WheelDelta, MousePos);
  Handled := True;
end;

procedure TForm1.Fullscreen(Sender: TObject);
var
  OwnerPanel1, OwnerPanel2: TWinControl;
  OwnerAlign1, OwnerAlign2: TAlign;
  ScreenWidth, ScreenHeight: Integer;
  NewWidth, NewHeight: Integer;
begin
  GLFSWindowForm := TGLFSWindowForm.Create(nil);
  try
    ScreenWidth := Screen.Width;
    ScreenHeight := Screen.Height;

    NewWidth := Round(ScreenWidth * 0.9);
    NewHeight := Round(ScreenHeight * 0.9);

    GLFSWindowForm.BorderStyle := bsSizeable;
    GLFSWindowForm.Position := poDesigned;
    GLFSWindowForm.SetBounds(
      (ScreenWidth - NewWidth) div 2,
      (ScreenHeight - NewHeight) div 2,
      NewWidth,
      NewHeight
    );


    if FontReadyA then
    begin
      glDeleteLists(BaseFontA, 256);
      FontReadyA := False;
    end;

    if FOpenGLControl.IsCurrent and (PageControl1.ActivePage = TabSheet1) then
    begin
      OwnerPanel1 := FOpenGLControl.Parent;
      OwnerAlign1 := FOpenGLControl.Align;
      FOpenGLControl.MakeCurrent;
      if FontReadyA then
      begin
        glDeleteLists(BaseFontA, 256);
        FontReadyA := False;
      end;
      FOpenGLControl.Parent := GLFSWindowForm.Panel1;
      FOpenGLControl.Align := alClient;
      InitFont(wglGetCurrentDC, BaseFontA);
      FontReadyA := True;
    end;

    if FontReadyB then
    begin
      glDeleteLists(BaseFontB, 256);
      FontReadyB := False;
    end;

    if FOpenGLControl3D.IsCurrent and (PageControl1.ActivePage = TabSheet2) then
    begin
      OwnerPanel2 := FOpenGLControl3D.Parent;
      OwnerAlign2 := FOpenGLControl3D.Align;
      FOpenGLControl3D.MakeCurrent;
      if FontReadyB then
      begin
        glDeleteLists(BaseFontB, 256);
        FontReadyB := False;
      end;
      FOpenGLControl3D.Parent := GLFSWindowForm.Panel1;
      FOpenGLControl3D.Align := alClient;
      InitFont(wglGetCurrentDC, BaseFontB);
      FontReadyB := True;
    end;



    GLFSWindowForm.ShowModal;

  finally

    if FontReadyA then
    begin
      glDeleteLists(BaseFontA, 256);
      FontReadyA := False;
    end;

    if FOpenGLControl.IsCurrent then
    begin
      FOpenGLControl.Parent := OwnerPanel1;
      FOpenGLControl.Align := OwnerAlign1;
    end;

    if FOpenGLControl3D.IsCurrent then
    begin
      FOpenGLControl3D.Parent := OwnerPanel2;
      FOpenGLControl3D.Align := OwnerAlign2;
    end;

    if FontReadyB then
    begin
      glDeleteLists(BaseFontB, 256);
      FontReadyB := False;
    end;

    GLFSWindowForm.Free;
    FOpenGLControl.Repaint;
    FOpenGLControl3D.Repaint;
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


procedure TForm1.FormResize(Sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin

  end;
end;


procedure TForm1.pagecontrolchange(sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    FOpenGLControl.MakeCurrent;
    if FontReadyB then
    begin
      FOpenGLControl3D.MakeCurrent;
      glDeleteLists(BaseFontB, 256);
      FontReadyB := False;
    end;

    FOpenGLControl.MakeCurrent;
    if not FontReadyA then
      InitFont(wglGetCurrentDC, BaseFontA);
    FontReadyA := True;
    FOpenGLControl.Repaint;
  end
  else if PageControl1.ActivePage = TabSheet2 then
  begin
    FOpenGLControl3D.MakeCurrent;
    if FontReadyA then
    begin
      FOpenGLControl.MakeCurrent;
      glDeleteLists(BaseFontA, 256);
      FontReadyA := False;
    end;

    FOpenGLControl3D.MakeCurrent;
    if not FontReadyB then
      InitFont(wglGetCurrentDC, BaseFontB);
    FontReadyB := True;
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


procedure TForm1.HandleMouseWheelZoom(WheelDelta: Integer; MousePos: TPoint);
const
  ZoomFactor = 1.1;
var
  pt: TPoint;
  TargetControl: TOpenGLControl;
begin
  TargetControl := nil;

  if Assigned(GLFSWindowForm) and GLFSWindowForm.Visible then
  begin
    TargetControl := GLFSWindowForm.GetOpenGLControl;
  end
  else
  begin

    if (PageControl1.ActivePage = TabSheet1) then
      TargetControl := FOpenGLControl
    else if (PageControl1.ActivePage = TabSheet2) then
      TargetControl := FOpenGLControl3D;
  end;

  if not Assigned(TargetControl) then Exit;

  pt := TargetControl.ScreenToClient(MousePos);

  if (pt.X >= 0) and (pt.X < TargetControl.Width) and (pt.Y >= 0) and (pt.Y < TargetControl.Height) then
  begin
    if WheelDelta > 0 then
      if TargetControl = FOpenGLControl then
        SetZoom(Zoom * ZoomFactor)
      else
        SetZoomLevel(ZoomLevel * ZoomFactor)
    else
      if TargetControl = FOpenGLControl then
        SetZoom(Zoom / ZoomFactor)
      else
        SetZoomLevel(ZoomLevel / ZoomFactor);

    TargetControl.Invalidate;
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
    ProgressForm : TForm2;

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
        if not btn.Enabled then Exit;
        btn.Enabled := False;
        ProgressForm := TForm2.Create(nil);
        try
          Application.ProcessMessages;
          ProgressForm.Show;
          ProgressForm.Update;
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
              raise Exception.Create('Зависимая переменная не может быть диапазоном')
            else if buff = 'x' then
              is_x := False
            else if buff = 'y' then
              is_x := False
            else if buff = 'z' then
              is_z := False;
          end;

          if is_x and is_y and is_z then
            raise Exception.Create('Требуется диапазон');

          if not is_x and not is_y and not is_z then
            raise Exception.Create('Зависимая переменная не может быть диапазоном');


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
      finally
          ProgressForm.Close;
          ProgressForm.Free;
          btn.Enabled := True;
        end;
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
        if FOpenGLControl.IsCurrent then
          FOpenGLControl.Repaint
        else if FOpenGLControl3D.IsCurrent then
             FOpenGLControl3D.Repaint;
      end;
    end;
  end;





procedure TForm1.FormDestroy(Sender: TObject);
begin
  FOpenGLControl.Free;
  FOpenGLControl3D.Free;
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
var
  buff : TEditListBoxItem;
begin
 Editlistbox2d.AddEdit(buff);
end;

procedure TForm1.AddEditListBoxItem3d(Sender: TObject);
var
  buff : TEditListBoxItem;
begin
 EditListBox3d.AddEdit(buff);
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
    parsed_data : TQueue<string>;
    parsed_data_copy : array of string;
    elem : string;
    range : TRangePair;
    symbol, buff : string;
    is_x, is_y, is_z, edited : Boolean;
    dependentVar : string;
    editlistbox : TEditListBox;
    ProgressForm : TForm2;
    btn : TButton;

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
    btn := sender as TButton;
    if not btn.Enabled then Exit;
    btn.Enabled := False;
    ProgressForm := TForm2.Create(nil);
    try
      Application.ProcessMessages;
      ProgressForm.Show;
      ProgressForm.Update;
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
          FProgressForm := TForm2.Create(self);
            for I := 0 to items.Count-1 do
            begin
              if not items[i].IfChangedGetMainEditData(math_expression) then
                Continue;
              pointsList.Clear;
              items[i].GetVariables(variables);
              items[i].GetRanges(ranges);
              variables_dict := TDictionary<string, float64>.Create;
              dependentVar := items[i].DependentVarEdit.Text;

              is_x := True;
              is_y := True;
              is_z := True;

              variables_dict.AddOrSetValue('x', 1);
              variables_dict.AddOrSetValue('y', 1);
              variables_dict.AddOrSetValue('z', 1);

              edited := True;


              for range in ranges do
              begin
                if range.NameEdit.Modified or range.StartEdit.Modified or range.EndEdit.Modified then
                  edited := True;

                buff := range.NameEdit.Text;
                variables_dict.AddOrSetValue(buff, 0);
                if dependentVar = buff then
                  raise Exception.Create('Зависимая переменная не может быть диапазоном')
                else if buff = 'x' then
                  is_x := False
                else if buff = 'y' then
                  is_x := False
                else if buff = 'z' then
                  is_z := False;
              end;

              if is_x and is_y and is_z then
                raise Exception.Create('Требуется диапазон');

              if not is_x and not is_y and not is_z then
                raise Exception.Create('Зависимая переменная не может быть диапазоном');


                for I2 := 0 to variables.Count-1 do
                    begin
                      if (variables[i2].NameEdit.Modified or variables[i].ValueEdit.Modified) and TryStrToFloat(variables[I2].ValueEdit.Text, value) then
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
                      CalcRanges(ranges, 0, variables_dict);
                      Self.items_points.AddOrSetValue(items[i], pointsList.ToArray);
                      pointsList.Clear;
                    end;
                end;
            end;
            if FOpenGLControl3d.IsCurrent then
              FOpenGLControl3d.Repaint
            else
              FOpenGLControl.Repaint;
    finally
      ProgressForm.Close;
      ProgressForm.Free;
      btn.Enabled := True;
    end;
  end;

procedure CallListBoxArrange;
  begin
    EditListBox2d.ArrangeEdits;
    EditlistBox3d.ArrangeEdits;
  end;



end.


