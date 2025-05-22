unit DrawUtils;

interface

uses OpenGL, Parser, System.SysUtils, Vcl.Dialogs, Vcl.Graphics, Windows, System.Math;

procedure DrawOs(alfa: Float32; zoom: Float32);

procedure DrawCoordinate(basefont: Cardinal; zoom: Float32);

procedure DrawGraph(points: TArray<TArray<TGraphPoint>>);

procedure SetGLColor(Color: TColor);

procedure glPrint(x, y: Float32; text: string; basefont: Cardinal);

procedure DrawCoordinate3D(BaseFont: Cardinal; Zoom: Float32);

procedure DrawGraph3d(points: TArray<TArray<TGraphPoint>>);

procedure glPrint3D(x, y, z: Float32; const text: string; BaseFont: Cardinal);

implementation

procedure DrawOs(alfa, zoom: Float32);
var
  d, l: Float32;
begin
  d := 0.05;
  l := 50 * zoom;

  glPushMatrix;
  glRotate(alfa, 0, 0, 1);

  glBegin(GL_LINES);
    glVertex2d(-l, 0);
    glVertex2d( l, 0);
    glVertex2d(l, 0);
    glVertex2d(l - d * l, d * l);
    glVertex2d(l, 0);
    glVertex2d(l - d * l, -d * l);
  glEnd;

  glPopMatrix;
end;


procedure DrawGraph(points: TArray<TArray<TGraphPoint>>);
var
  I, I2: Integer;
begin
  if Length(points) < 1 then raise Exception.Create('3');
  glLineWidth(2);
  for I := 0 to High(points) do
  begin
  glBegin(GL_LINE_STRIP);

    for I2 := 0 to High(points[I]) do
      glVertex2f(points[i][i2].x, points[i][i2].y);

    glEnd;
  end;
end;

procedure DrawGraph3d(points: TArray<TArray<TGraphPoint>>);
var
  I, I2: Integer;
begin
  if Length(points) < 1 then raise Exception.Create('3');
  glLineWidth(2);
  for I := 0 to High(points) do
  begin
    glBegin(GL_LINE_STRIP);

    for I2 := 0 to High(points[I]) do
      glVertex3f(points[i][i2].x, points[i][i2].y, points[i][i2].z);

    glEnd;
  end;
end;


procedure SetGLColor(Color: TColor);
var
  R, G, B: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  glColor3f(R / 255, G / 255, B / 255);
end;



procedure DrawCoordinate(basefont: Cardinal; zoom : float32);
var
  i: Integer;
  step, range: Integer;
  l: Float32;
begin
  l := 1;

  glLineWidth(2);
  glColor3b(30, 50, 90);
  DrawOs(0, zoom);
  glColor3b(90, 50, 30);
  DrawOs(90, zoom);

  step := 10;

  range := Trunc(50 * Zoom);

  for i := -range to range do
    if (i mod step = 0) and (i <> 0) then
      glPrint(i, -0.5, IntToStr(i), basefont);

  for i := -range to range do
    if (i mod step = 0) and (i <> 0) then
      glPrint(0.5 , i, IntToStr(i), basefont);
end;



procedure glPrint(x, y: Float32; text: string; basefont : Cardinal);
begin
  glColor3f(1.0, 1.0, 1.0);
  glRasterPos2f(x, y);
  glPushAttrib(GL_LIST_BIT);
  glListBase(basefont);
  glCallLists(Length(text), GL_UNSIGNED_BYTE, PAnsiChar(AnsiString(text)));
  glPopAttrib;
end;


procedure DrawArrow(const BaseFont: Cardinal);
const
  ArrowSize = 3.0;
begin
  glBegin(GL_TRIANGLES);
    glVertex3f(0, 0, 0);
    glVertex3f(-ArrowSize, ArrowSize/2, 0);
    glVertex3f(-ArrowSize, -ArrowSize/2, 0);
  glEnd;
end;

procedure DrawCoordinate3D(BaseFont: Cardinal; Zoom: Float32);
const
  AxisLength = 100.0;
var
  i, step: Integer;
  scaledLength: Float32;
begin
  scaledLength := AxisLength;

  glLineWidth(2.0);
  glBegin(GL_LINES);

    glColor3f(1, 0, 0);
    glVertex3f(-scaledLength, 0, 0);
    glVertex3f(scaledLength, 0, 0);

    glColor3f(0, 1, 0);
    glVertex3f(0, -scaledLength, 0);
    glVertex3f(0, scaledLength, 0);

    glColor3f(0, 0, 1);
    glVertex3f(0, 0, -scaledLength);
    glVertex3f(0, 0, scaledLength);
  glEnd;


  glColor3f(1, 0, 0);
  glPushMatrix;
    glTranslatef(scaledLength, 0, 0);
    glRotatef(90, 0, 1, 0);
    DrawArrow(BaseFont);
  glPopMatrix;

  glColor3f(0, 1, 0);
  glPushMatrix;
    glTranslatef(0, scaledLength, 0);
    glRotatef(-90, 1, 0, 0);
    DrawArrow(BaseFont);
  glPopMatrix;

  glColor3f(0, 0, 1);
  glPushMatrix;
    glTranslatef(0, 0, scaledLength);
    DrawArrow(BaseFont);
  glPopMatrix;

  step := 10;
  glColor3f(1, 1, 1);

  for i := -Round(AxisLength) to Round(AxisLength) do
    if (i mod step = 0) and (i <> 0) then
      glPrint3D(i, 0, 0, IntToStr(i), BaseFont);

  for i := -Round(AxisLength) to Round(AxisLength) do
    if (i mod step = 0) and (i <> 0) then
      glPrint3D(0, i, 0, IntToStr(i), BaseFont);

  for i := -Round(AxisLength) to Round(AxisLength) do
    if (i mod step = 0) and (i <> 0) then
      glPrint3D(0, 0, i, IntToStr(i), BaseFont);

  glColor3f(1, 0, 0);
  glPrint3D(scaledLength + 5, 0, 0, 'X', BaseFont);

  glColor3f(0, 1, 0);
  glPrint3D(0, scaledLength + 5, 0, 'Y', BaseFont);

  glColor3f(0, 0, 1);
  glPrint3D(0, 0, scaledLength + 5, 'Z', BaseFont);
end;


procedure glPrint3D(x, y, z: Float32; const text: string; BaseFont: Cardinal);
begin
  glPushMatrix;
  glTranslatef(x, y, z);
  glRasterPos3f(0, 0, 0);

  glPushAttrib(GL_LIST_BIT);
  glListBase(BaseFont);
  glCallLists(Length(text), GL_UNSIGNED_BYTE, PAnsiChar(AnsiString(text)));
  glPopAttrib;

  glPopMatrix;
end;


end.


