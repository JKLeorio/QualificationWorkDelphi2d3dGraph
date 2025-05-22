unit OpenGLControl;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Messages;

type
  TOpenGLControl = class(TCustomControl)
  private
    FDC: HDC;
    FRC: HGLRC;
    FOnPaint: TNotifyEvent;
    class var SharedFontCreated: Boolean;
    class var SharedFontList: Cardinal;
    class procedure InitSharedFont(DC: HDC);
    class procedure ReleaseSharedFont;
  protected
    procedure SetupPixelFormat;
    procedure GLInit;
    procedure GLRelease;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeCurrent;
    function IsCurrent : Boolean;
    class function GetSharedFont: Cardinal;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

uses
  OpenGL;

{ TOpenGLControl }

procedure TOpenGLControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TOpenGLControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := Params.WindowClass.style and
    not (CS_HREDRAW or CS_VREDRAW);
end;

constructor TOpenGLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TOpenGLControl.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenGLControl.CreateHandle;
begin
  inherited;
  GLInit;
  if not SharedFontCreated then
    InitSharedFont(FDC);
end;

procedure TOpenGLControl.DestroyHandle;
begin
  GLRelease;
  inherited;
end;

procedure TOpenGLControl.SetupPixelFormat;
var
  PixelFormatDescriptor: TPixelFormatDescriptor;
  pfIndex: Integer;
begin
  with PixelFormatDescriptor do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 32;
    cDepthBits := 16;
    iLayerType := PFD_MAIN_PLANE;
  end;

  pfIndex := ChoosePixelFormat(FDC, @PixelFormatDescriptor);
  if pfIndex = 0 then Exit;
  if not SetPixelFormat(FDC, pfIndex, @PixelFormatDescriptor) then
    raise Exception.Create('Unable to set pixel format.');
end;

procedure TOpenGLControl.GLInit;
begin
  FDC := GetDC(Handle);
  if FDC = 0 then Exit;
  SetupPixelFormat;
  FRC := wglCreateContext(FDC);
  if FRC = 0 then Exit;
  if not wglMakeCurrent(FDC, FRC) then
    raise Exception.Create('Unable to initialize OpenGL.');
end;

procedure TOpenGLControl.GLRelease;
begin
  wglMakeCurrent(FDC, 0);
  if FRC <> 0 then wglDeleteContext(FRC);
  if FDC <> 0 then ReleaseDC(Handle, FDC);
  FRC := 0;
  FDC := 0;
end;

procedure TOpenGLControl.Paint;
begin
  inherited;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TOpenGLControl.MakeCurrent;
begin
  if (FDC <> 0) and (FRC <> 0) then
    wglMakeCurrent(FDC, FRC);
end;

function TOpenGLControl.IsCurrent: Boolean;
begin
  Result := wglGetCurrentContext = FRC;
end;

class function TOpenGLControl.GetSharedFont: Cardinal;
begin
  Result := SharedFontList;
end;

class procedure TOpenGLControl.InitSharedFont(DC: HDC);
var
  font: HFONT;
begin
  SharedFontList := glGenLists(256);
  font := CreateFont(-12, 0, 0, 0, FW_NORMAL, 0, 0, 0,
    ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
    FF_DONTCARE or DEFAULT_PITCH, 'Courier New');

  SelectObject(DC, font);
  wglUseFontBitmaps(DC, 0, 256, SharedFontList);
  SharedFontCreated := True;
end;

class procedure TOpenGLControl.ReleaseSharedFont;
begin
  if SharedFontCreated then
  begin
    glDeleteLists(SharedFontList, 256);
    SharedFontCreated := False;
  end;
end;

end.

