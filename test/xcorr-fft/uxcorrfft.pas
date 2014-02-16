unit uxcorrfft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  sndfile, LCLType, StdCtrls, ExtCtrls, ComCtrls, Dos, FFTReal, UComplex,
  uCrossCorrelateFFT, LCLIntf;

const
  buffer_size = 1024 * 2;
  STEREO = 2;

type

  { TSimpleWaveForm }

  TSimpleWaveForm = class(TCustomControl)
  private
    FBufferSize: Integer;
    FData: PSingle;
    FDataSize: Integer;
    FChannelCount: Integer;
    FFrames: Integer;
    FOffset: Integer;
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
    property Offset: Integer read FOffset write FOffset;
    property BufferSize: Integer write FBufferSize;
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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
  private
    { private declarations }
    Wave: TSimpleWaveForm;
    FoundWave: TSimpleWaveForm;
    FDataSize: Integer;
    FChannelCount: Integer;
    FFrames: Integer;
    FSampleRate: Integer;
    FReadCount: Integer;
    x, f: pflt_array;
    FSpectrum: TSpectrum;
    procedure CreateSineAt(AFFT: pflt_array; Frequency: single; AStart,
      ALength: Integer);
    procedure CrossCorrelate;
    function LoadSample(AFileName: PChar; AWaveform: TSimpleWaveForm): Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function Conj(AComplex: Complex): Complex;

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

    scale := buffer_size div 2 / bmp.width;
    if maximum = 0 then maximum := 0.0000000;
    y_scale := Height / maximum;

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

function Conj(AComplex: Complex): Complex;
begin
  Result.re := AComplex.re;
  Result.im := -AComplex.im;
end;

procedure TForm1.CrossCorrelate;
var
  a_input, b_input, a_output, b_output, f_input, f_output: pflt_array;
  a_real, b_real, a_img, b_img: double;
  lFFT: TFFTReal;
  lHalfWave: Integer;
  lScale: double;
  i: Integer;
  a_cmp, b_cmp, f_cmp: complex;
  f_abs: double;
  offset: integer;
begin
  lHalfWave := Wave.Frames div 2;

  // Prepare source buffer
  GetMem(a_input, buffer_size * sizeof_flt * 4);
  for i := 0 to Pred(buffer_size) do
  begin
//    a_input^[i] := Wave.Data[i * STEREO];
    a_input^[i] := 0;
    a_input^[i + buffer_size] := 0;
  end;
  CreateSineAt(a_input, 1000, 0, 200);

  // Prepare target buffer
  GetMem(b_input, buffer_size * sizeof_flt * 4);
  for i := 0 to Pred(buffer_size) do
  begin
    //b_input^[i] := FoundWave.Data[(i + lHalfWave) * STEREO];
    b_input^[i] := 0;
    b_input^[i + buffer_size] := 0;
  end;
  CreateSineAt(b_input, 1000, 100, 200);
  CreateSineAt(b_input, 1010, 500, 200);

  // Prepare .. buffer
  GetMem(a_output, buffer_size * sizeof_flt * 2);
  for i := 0 to Pred(buffer_size * 2) do
  begin
    a_output^[i] := 0;
  end;

  GetMem(b_output, buffer_size * sizeof_flt * 2);
  for i := 0 to Pred(buffer_size * 2) do
  begin
    b_output^[i] := 0;
  end;

  GetMem(f_input, buffer_size * sizeof_flt * 2);
  for i := 0 to Pred(buffer_size * 2) do
  begin
    f_input^[i] := 0;
  end;

  GetMem(f_output, buffer_size * sizeof_flt * 2);
  for i := 0 to Pred(buffer_size * 2) do
  begin
    f_output^[i] := 0;
  end;

  lFFT := TFFTReal.Create(buffer_size * 2 - 1);
  try
    lFFT.do_fft(a_output, a_input);
    lFFT.do_fft(b_output, b_input);

    lScale := 1/(2 * buffer_size -1);
    for i := 0 to Pred(buffer_size div 2) do
    begin
      a_cmp.re := a_output^[i];
      if (i > 0) and (i < buffer_size div 2) then
        a_cmp.im := a_output^[i + buffer_size div 2]
      else
        a_cmp.im := 0;

      b_cmp.re := b_output^[i];
      if (i > 0) and (i < buffer_size div 2) then
        b_cmp.im := b_output^[i + buffer_size div 2]
      else
        b_cmp.im := 0;

      f_cmp := a_cmp * Conj(b_cmp) * lScale;
      f_input^[i] := f_cmp.re;
      f_input^[i + buffer_size div 2] := f_cmp.im;
    end;

    lFFT.do_ifft(f_input, f_output);

    FSpectrum.maximum := 0;
    offset := 0;
    for i := 0 to Pred(buffer_size div 2) do
    begin
      a_real := f_output^[i];
      if (i > 0) and (i < buffer_size div 2) then
        a_img := f_output^[i + buffer_size div 2]
      else
        a_img := 0;

      f_abs := Sqrt(a_real * a_real + a_img * a_img);
      FSpectrum.spectrum_array[i] := f_abs;

      if FSpectrum.spectrum_array[i] > FSpectrum.maximum then
      begin
        FSpectrum.maximum := FSpectrum.spectrum_array[i];
        offset := i;
      end;
    end;
    FoundWave.Offset := offset + lHalfWave;
  finally
    FreeMem(a_input);
    FreeMem(b_input);
    FreeMem(a_output);
    FreeMem(b_output);
    FreeMem(f_input);
    FreeMem(f_output);
    lFFT.Free;
  end;

  {FoundWave.Invalidate;
  Wave.Invalidate;}
  Invalidate;

  showmessage(format('max %f offset %d', [FSpectrum.maximum, offset]));

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  lFFT: TFFTReal;
  i: Integer;
  areal, img  : double;
  f_abs       : double;
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

procedure TForm1.CreateSineAt(AFFT: pflt_array; Frequency: single; AStart, ALength: Integer);
var
  i:integer;
  PI, dt, t:single;
  omega:single;
begin
  PI := ArcTan(1) * 4;
  omega := 2 * pi * frequency;
  dt:=1/44100;
  t:=0;
  for i := 0 to ALength-1 do
  begin
    AFFT^[i + AStart] := sin(omega*t);
    t := t + dt;
  end;
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

procedure TForm1.Button4Click(Sender: TObject);
begin
  CrossCorrelate;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  lXCorr: TCrossCorrelate;
  lOffset: Integer;
  lStartTime, lEndTime: Cardinal;
  lIndex: Integer;
begin
  lStartTime := LclIntf.GetTickCount;
  lXCorr := TCrossCorrelate.Create(44100);
  try
    lXCorr.OverlapLengthMs := 8;
    lXCorr.SeekWindowLengthMs := 40;
    lOffset := lXCorr.Process(Wave.Data, @FoundWave.Data[10000+Random(500)],Wave.ChannelCount);
    Wave.Offset:=lOffset;
    FoundWave.Offset:=lOffset;
    FoundWave.BufferSize:=lXCorr.BufferSize;
    FoundWave.Invalidate;
    Wave.BufferSize:=lXCorr.BufferSize;
    Wave.Invalidate;
  finally
    lXCorr.Free;
  end;
  lEndTime := LclIntf.GetTickCount;
  ShowMessage(Format('Performance %d ms', [lEndTime - lStartTime]));
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

procedure TForm1.TrackBar3Change(Sender: TObject);
var
  i:integer;
begin
  TrackBar3.Max:=wave.DataSize div (STEREO * sizeof(single)) - buffer_size;

  for i := 0 to Pred(buffer_size) do
  begin
    x^[i] := Wave.Data[(i + TrackBar3.Position) * 2];
  end;

  Button1Click(nil);
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
  FoundWave.Zoom:=TrackBar4.Position / 1000;
  Wave.Zoom:=TrackBar4.Position / 1000;
  FoundWave.Invalidate;
  Wave.Invalidate;
end;

function TForm1.LoadSample(AFileName: PChar; AWaveform: TSimpleWaveForm): Boolean;
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
      AWaveform.DataSize := lSampleInfo.frames * lSampleInfo.channels * SizeOf(Single);
      FFrames := lSampleInfo.frames;
      FSampleRate := lSampleInfo.samplerate;
      FChannelCount := lSampleInfo.channels;
      AWaveform.ChannelCount:=lSampleInfo.channels;
      AWaveform.Frames:=lSampleInfo.frames;

      if Assigned(AWaveform.Data) then
      begin
        Freemem(AWaveform.Data);
      end;

      AWaveform.Data := GetMem(AWaveform.DataSize * 4 + 1000);
      try
        FReadCount := sf_read_float(lSampleHandle, AWaveform.Data, AWaveform.DataSize);

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

  FoundWave := TSimpleWaveForm.Create(nil);
  FoundWave.parent := Panel2;
  FoundWave.Left:=0;
  FoundWave.Top:=0;
  FoundWave.Height:=100;
  FoundWave.Align := alTop;

  Wave := TSimpleWaveForm.Create(nil);
  wave.parent := Panel2;
  wave.Left:=0;
  wave.Top:=0;
  wave.Height:=100;
  wave.Align := alTop;

  FSpectrum := TSpectrum.Create(nil);
  FSpectrum.Parent := panel2;
  FSpectrum.Left:=0;
  FSpectrum.Height:=200;
  FSpectrum.Align:=alTop;

  LoadSample('nogood.wav', Wave);
  LoadSample('nogood.wav', FoundWave);
end;

destructor TForm1.Destroy;
begin
  Wave.Free;
  FoundWave.Free;

  FreeMem(x);
  FreeMem(f);

  inherited Destroy;
end;

{ TSimpleWaveForm }

constructor Tsimplewaveform.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  FOffset := 0;
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
      scale := FFrames / bmp.width * FZoom;

      bmp.Canvas.Pen.Color := clBlack;
      bmp.Canvas.Line(0, zeroline, Width, zeroline);

      bmp.Canvas.Pen.Color := clBlue;
      bmp.Canvas.MoveTo(0, zeroline);

      for ScreenLoop := 0 to Pred(bmp.Width) do
      begin
        bmp.Canvas.Pen.Color := clBlue;
        bmp.Canvas.LineTo(ScreenLoop,
        Round(FData[Round(ScreenLoop * FChannelCount * scale)] * zeroline) + zeroline);
      end;
      bmp.Canvas.Pen.Color := clRed;
      bmp.Canvas.Line(Round(FOffset * FChannelCount * scale), 0,
      Round(FOffset * FChannelCount * scale), Height);

    end;
    bmp.Canvas.Pen.Color := clRed;
    bmp.Canvas.TextOut(5, 5, Format('Offset %d, BufferSize %d', [FOffset, FBufferSize]));
    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  inherited Paint;
end;

end.

