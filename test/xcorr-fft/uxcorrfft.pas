unit uxcorrfft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, FileUtil, Forms, Controls, Graphics, Dialogs,
  sndfile, fftw_s, LCLType, StdCtrls, ExtCtrls, ComCtrls, Dos, FFTReal;

const
  buffer_size = 1024;

type

  { TSimpleWaveForm }

  TSimpleWaveForm = class(TCustomControl)
  private
    FData: PSingle;
    FDataSize: Integer;
    FChannelCount: Integer;
    FFrames: Integer;
    FZoom: single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    property Data: PSingle read FData write FData;
    property Zoom: single read FZoom write FZoom;
    property DataSize: Integer read FDataSize write FDataSize;
    property Frames: Integer read FFrames write FFrames;
    property ChannelCount: Integer read FChannelCount write FChannelCount;
  protected
  published
  end;

  { TSpectrum }

  TSpectrum = class(TCustomControl)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  public
    spectrum_array: array[0..1023] of single;
    maximum: single;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private
    { private declarations }
    Wave: TSimpleWaveForm;
    FDataSize: Integer;
    FChannelCount: Integer;
    FFrames: Integer;
    FSampleRate: Integer;
    FReadCount: Integer;
    x, f: pflt_array;
    FSpectrum: TSpectrum;
    function LoadSample(AFileName: PChar): Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{ TSpectrum }

constructor TSpectrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TSpectrum.Destroy;
begin

  inherited Destroy;
end;

procedure TSpectrum.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TSpectrum.Paint;
var
  bmp: TBitmap;
  screenloop: integer;
  zeroline: integer;
  scale: single;
  y_scale: single;
begin
  bmp := TBitmap.Create;
  try
    bmp.Height := Height;
    bmp.Width := Width;
    zeroline := Height div 2;

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(0, 0, Width, Height);

    scale := 1024 div 2 / bmp.width;
    if maximum = 0 then maximum:=Height;
    y_scale := Height / maximum;
//    y_scale := maximum/Height;

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Line(0, zeroline, Width, zeroline);

    bmp.Canvas.Pen.Color := clBlue;
    bmp.Canvas.MoveTo(0, zeroline);

    for ScreenLoop := 0 to Pred(bmp.Width) do
      bmp.Canvas.LineTo(ScreenLoop, Height - Round(spectrum_array[Round(ScreenLoop * scale)] * y_scale));

    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  inherited Paint;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  lFFT: TFFTReal;
  i: Integer;
  areal, img  : double;
  f_abs       : double;
  fft: TFFTReal;
begin
  lFFT := TFFTReal.Create(buffer_size);
  try
    // Compute FFT and IFFT
    lFFT.do_fft(f, x);
    lFFT.do_ifft(f, x);
    lFFT.rescale(x);

    for i := 0 to buffer_size div 2 do
     begin
       areal := f^[i];
       if (i > 0) and (i < buffer_size div 2) then
         img := f^[i + buffer_size div 2]
       else
         img := 0;

       f_abs := Sqrt(areal * areal + img * img);
       FSpectrum.spectrum_array[i] := f_abs;
//       WriteLn(Format('%5d: %12.6f %12.6f (%12.6f)', [i, areal, img, f_abs]));
     end;

    FSpectrum.maximum:=0;
     for i := 0 to buffer_size div 2 do
      begin
           if FSpectrum.spectrum_array[i] > FSpectrum.maximum then
             FSpectrum.maximum:=FSpectrum.spectrum_array[i];
      end;
  finally
    lFFT.Free;
  end;

  Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i:integer;
  PI, dt, t:single;
  omega:single;
  frequency:single;
begin
  frequency := 1000;
  PI := ArcTan(1) * 4;
  omega := 2 * pi * frequency;
  dt:=1/44100;
  t:=0;
  for i := 0 to buffer_size-1 do
  begin
    x^[i] := sin(omega*t);
    t := t + dt;
  end;

  Button1Click(nil);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i:integer;
begin
  for i := 0 to Pred(buffer_size) do
  begin
    x^[i] := Wave.Data[i * 2];
  end;

  Button1Click(nil);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  i:integer;
  PI:single;
begin
  for i := 0 to buffer_size-1 do
  begin
    x^[i] :=  frac(i / TrackBar1.Position);
  end;

  Button1Click(nil);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
var
  i:integer;
  PI, dt, t:single;
  omega:single;
  frequency:single;
begin
  frequency := TrackBar2.Position;
  PI := ArcTan(1) * 4;
  omega := 2 * pi * frequency;
  dt:=1/44100;
  t:=0;
  for i := 0 to buffer_size-1 do
  begin
    x^[i] := sin(omega*t);
    t := t + dt;
  end;

  Button1Click(nil);
end;

function TForm1.LoadSample(AFileName: PChar): Boolean;
var
  lChannelIndex: Integer;
  lChannelSize: Integer;
  lChannelItems: Integer;
  lBufferIndex: Integer;
  lBuffer: PSingle;
  lSampleHandle: PSndFile;
  lSampleInfo: SF_INFO;
begin
  Result := False;

  lSampleHandle := sf_open(AFilename, SFM_READ, lSampleInfo);
  try
    if not Assigned(lSampleHandle) then
    begin
      ShowMessage(sf_strerror(lSampleHandle));
      Result := False;
    end
    else
    begin
      Wave.DataSize := lSampleInfo.frames * lSampleInfo.channels * SizeOf(Single);
      FFrames := lSampleInfo.frames;
      FSampleRate := lSampleInfo.samplerate;
      FChannelCount := lSampleInfo.channels;
      Wave.ChannelCount:=lSampleInfo.channels;
      Wave.Frames:=lSampleInfo.frames;

      if Assigned(Wave.Data) then
      begin
        Freemem(Wave.Data);
      end;

      Wave.Data := GetMem(Wave.DataSize * 4 + 1000);
      try
        FReadCount := sf_read_float(lSampleHandle, Wave.Data, Wave.DataSize);

      except
        on e: exception do
        begin
          Result := False;

          raise;
        end;
      end;
    end;
  finally
    sf_close(lSampleHandle);
  end;
end;

constructor TForm1.Create(AOwner: TComponent);
var
  i:integer;
begin
  inherited Create(AOwner);

  GetMem(x, buffer_size * sizeof_flt);
  GetMem(f, buffer_size * sizeof_flt);

  for i := 0 to Pred(buffer_size) do
  begin
    x^[i] := 0;
  end;

  Wave := TSimpleWaveForm.Create(nil);
  wave.parent := Panel2;
  wave.Left:=0;
  wave.Top:=0;
  wave.Height:=200;
  wave.Align := alTop;

  FSpectrum := TSpectrum.Create(nil);
  FSpectrum.Parent := panel2;
  FSpectrum.Left:=0;
  FSpectrum.Height:=400;
  FSpectrum.Align:=alTop;

  LoadSample('nogood.wav');
end;

destructor TForm1.Destroy;
begin
  Wave.Free;

  FreeMem(x);
  FreeMem(f);

  inherited Destroy;
end;

{ TSimpleWaveForm }

constructor Tsimplewaveform.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  FZoom := 4;
end;

destructor Tsimplewaveform.Destroy;
begin
  inherited Destroy;
end;

procedure Tsimplewaveform.Erasebackground(Dc: Hdc);
begin
  //inherited Erasebackground(Dc);
end;

procedure Tsimplewaveform.Paint;
var
  bmp: TBitmap;
  screenloop: integer;
  zeroline: integer;
  scale: single;
begin
  bmp := TBitmap.Create;
  try
    bmp.Height := Height;
    bmp.Width := Width;
    zeroline := Height div 2;

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(0, 0, Width, Height);

    if (FData <> nil) and (FDataSize > 0) and (FChannelCount in [1,2]) then
    begin
      scale := FFrames / bmp.width;

      bmp.Canvas.Pen.Color := clBlack;
      bmp.Canvas.Line(0, zeroline, Width, zeroline);

      bmp.Canvas.Pen.Color := clBlue;
      bmp.Canvas.MoveTo(0, zeroline);

      for ScreenLoop := 0 to Pred(bmp.Width) do
        bmp.Canvas.LineTo(ScreenLoop, Round(FData[Round(ScreenLoop * FChannelCount * scale)] * zeroline) + zeroline);
    end;
    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  inherited Paint;
end;

end.

