unit DrawUtils;

interface

uses OpenGL, Parser, System.SysUtils, Vcl.Dialogs, Vcl.Graphics, Windows;

procedure DrawOs(alfa : Float32);

procedure DrawGraph(points : TArray<TPoint>);

procedure DrawCoordinate;

procedure SetGLColor(Color: TColor);

implementation

procedure DrawOs(alfa : Float32);
  var
    d : Float32;
    l : Float32;
  begin
    d:= 0.05;
    l:= 50;
    glPushMatrix();
    glRotate(alfa, 0, 0, 1);
    glBegin(GL_LINES);
      glVertex2d(-1 * l, 0);
      glVertex2d(1 * l, 0);
      glVertex2d(1 * l, 0);
      glVertex2d(1 * l - d * l, 0 + d * l);
      glVertex2d(1 * l, 0);
      glVertex2d(1 * l - d * l, 0 - d * l);
    glEnd;
    glPopMatrix;
  end;

procedure DrawGraph(points : TArray<TPoint>);
  var
    I : Integer;
  begin
    if Length(points) < 1 then raise Exception.Create('3');
    glLineWidth(2);
    glBegin(GL_LINE_STRIP);
    for I := 0 to High(points)-1 do
    begin
        glVertex2f(points[i].x, points[i].y);
    end;
    glEnd;
    
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

procedure DrawCoordinate;
begin
  glLineWidth(2);
  glColor3b(30,50,90);
  DrawOs(0);
  glColor3b(90,50,30);
  DrawOs(90);
  glColor3b(100,90,80);
end;

end.


