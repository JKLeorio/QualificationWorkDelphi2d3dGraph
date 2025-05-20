unit OpenGLControl;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls;

type
  TOpenGLControl = class(TCustomControl)
  private
    FDC: HDC;
    FRC: HGLRC;
    FOnPaint: TNotifyEvent;
  protected
    procedure SetupPixelFormat;
    procedure GLInit;
    procedure GLRelease;

    procedure CreateHandle; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

implementation

uses
  OpenGL;


constructor TOpenGLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TOpenGLControl.Destroy;
begin
  GLRelease;
  inherited Destroy;
end;

procedure TOpenGLControl.CreateHandle;
begin
  inherited;
  GLInit;
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
    cRedBits := 0;
    cRedShift := 0;
    cGreenBits := 0;
    cGreenShift := 0;
    cBlueBits := 0;
    cBlueShift := 0;
    cAlphaBits := 0;
    cAlphaShift := 0;
    cAccumBits := 0;
    cAccumRedBits := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits := 0;
    cAccumAlphaBits := 0;
    cDepthBits := 16;
    cStencilBits := 0;
    cAuxBuffers := 0;
    iLayerType := PFD_MAIN_PLANE;
    bReserved := 0;
    dwLayerMask := 0;
    dwVisibleMask := 0;
    dwDamageMask := 0;
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
    raise Exception.Create('Unable to initialize.');
end;

procedure TOpenGLControl.GLRelease;
begin
  wglMakeCurrent(FDC, 0);
  wglDeleteContext(FRC);
  ReleaseDC(Handle, FDC);
end;

procedure TOpenGLControl.Paint;
begin
  inherited;
  if Assigned(FOnPaint) then
  begin
    FOnPaint(Self);
  end;
end;

end.
