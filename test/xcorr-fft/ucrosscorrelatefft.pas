unit uCrossCorrelateFFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FFTReal, UComplex, LCLIntf;

type
  { TCrossCorrelate }

  TCrossCorrelate = class
  private
    FChannelCount: Integer;
    FFFT: TFFTReal;
    a_input, b_input, a_output, b_output, f_input, f_output: pflt_array;
    FPerformance: Cardinal;
    FOverlapLength: Integer;
    FSeekWindowLength: Integer;
    FBufferSize: Integer;
    FHalfBufferSize: Integer;
    FSampleRate: Integer;
    procedure SetOverlapLengthMs(AValue: Integer);
    procedure SetSeekWindowLengthMs(AValue: Integer);
  protected
    procedure CalcParameters;
  public
    constructor Create(ASampleRate: Integer); reintroduce;
    destructor Destroy; override;
    function Process(AOverlapWindow, ASeekWindow: PSingle; AChannelCount: Integer = 2): Integer;

    property OverlapLengthMs: Integer write SetOverlapLengthMs;
    property SeekWindowLengthMs: Integer write SetSeekWindowLengthMs;
    property Performance: Cardinal read FPerformance;
    property BufferSize: Integer read FBufferSize;
  end;

  //function Conj(AComplex: Complex): Complex;

implementation

{function Conj(AComplex: Complex): Complex;
begin
  Result.re := AComplex.re;
  Result.im := -AComplex.im;
end;}

{ TCrossCorrelate }

procedure TCrossCorrelate.SetOverlapLengthMs(AValue: Integer);
begin
  FOverlapLength := Round(FSampleRate / (1000 / AValue));

  CalcParameters;
end;

procedure TCrossCorrelate.SetSeekWindowLengthMs(AValue: Integer);
begin
  FSeekWindowLength := Round(FSampleRate / (1000 / AValue));

  CalcParameters;
end;

procedure TCrossCorrelate.CalcParameters;
begin
  if (FOverlapLength > 0) and (FSeekWindowLength > 0) then
  begin
    FBufferSize := 2;
    while FBufferSize < FSeekWindowLength do
    begin
      FBufferSize := FBufferSize * 2;
    end;

    FHalfBufferSize := FBufferSize;

    // Expand buffer size for padding with zero's
    FBufferSize := FBufferSize * 2;

    if Assigned(FFFT) then
    begin
      FFFT.Free;
    end;
    FFFT := TFFTReal.Create(FBufferSize);

    GetMem(a_input, FBufferSize * sizeof_flt);
    GetMem(b_input, FBufferSize * sizeof_flt);
    GetMem(a_output, FBufferSize * sizeof_flt);
    GetMem(b_output, FBufferSize * sizeof_flt);
    GetMem(f_input, FBufferSize * sizeof_flt);
    GetMem(f_output, FBufferSize * sizeof_flt);
  end;
end;

constructor TCrossCorrelate.Create(ASampleRate: Integer);
begin
  inherited Create;

  FSampleRate := ASampleRate;
end;

destructor TCrossCorrelate.Destroy;
begin
  if Assigned(FFFT) then
  begin
    FFFT.Free;
  end;

  FreeMem(a_input);
  FreeMem(b_input);
  FreeMem(a_output);
  FreeMem(b_output);
  FreeMem(f_input);
  FreeMem(f_output);

  inherited Destroy;
end;

function TCrossCorrelate.Process(
  AOverlapWindow,
  ASeekWindow: PSingle;
  AChannelCount: Integer = 2): Integer;
var
  a_real, a_img: double;
  scale: double;
  i: Integer;
  lOffset: Integer;
  a_cmp, b_cmp, f_cmp: complex;
  f_abs: double;
  lMaximum: double;
  lStartTime, lEndTime: Cardinal;
begin
  lStartTime := LclIntf.GetTickCount;

  // Prepare source buffer
  if AChannelCount = 2 then
  begin
    lOffset := 0;
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FOverlapLength then
        a_input^[i] := AOverlapWindow[lOffset] + AOverlapWindow[lOffset + 1]
      else
        a_input^[i] := 0;
      Inc(lOffset, AChannelCount);
    end;

    // Prepare target buffer
    lOffset := 0;
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FSeekWindowLength then
        b_input^[i] := ASeekWindow[lOffset] + ASeekWindow[lOffset + 1]
      else
        b_input^[i] := 0;
      Inc(lOffset, AChannelCount);
    end;
  end
  else if AChannelCount = 1 then
  begin
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FOverlapLength then
        a_input^[i] := AOverlapWindow[i]
      else
        a_input^[i] := 0;
    end;

    // Prepare target buffer
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FSeekWindowLength then
        b_input^[i] := ASeekWindow[i]
      else
        b_input^[i] := 0;
    end;
  end;

  // Clear buffers
  FillByte(a_output^, FBufferSize * sizeof_flt, 0);
  FillByte(b_output^, FBufferSize * sizeof_flt, 0);
  FillByte(f_input^, FBufferSize * sizeof_flt, 0);
  FillByte(f_output^, FBufferSize * sizeof_flt, 0);

  // FFT overlap and seekwindow buffers
  FFFT.do_fft(a_output, a_input);
  FFFT.do_fft(b_output, b_input);

  scale := 1/(2 * FBufferSize -1);
  for i := 0 to Pred(FHalfBufferSize) do
  begin
    a_cmp.re := a_output^[i];
    if (i > 0) and (i < FHalfBufferSize) then
      a_cmp.im := a_output^[i + FHalfBufferSize]
    else
      a_cmp.im := 0;

    b_cmp.re := b_output^[i];
    if (i > 0) and (i < FHalfBufferSize) then
      b_cmp.im := b_output^[i + FHalfBufferSize]
    else
      b_cmp.im := 0;

    f_cmp := a_cmp * cong(b_cmp) * scale;
    f_input^[i] := f_cmp.re;
    f_input^[i + FHalfBufferSize] := f_cmp.im;
  end;

  FFFT.do_ifft(f_input, f_output);

  lMaximum := 0;
  Result := 0;
  for i := 0 to Pred(FHalfBufferSize) do
  begin
    a_real := f_output^[i];
    if (i > 0) and (i < FHalfBufferSize) then
      a_img := f_output^[i + FHalfBufferSize]
    else
      a_img := 0;

    f_abs := Sqrt(a_real * a_real + a_img * a_img);

    if f_abs > lMaximum then
    begin
      lMaximum := f_abs;
      Result := i;
    end;
  end;

  lEndTime := LclIntf.GetTickCount;
  FPerformance := lEndTime - lStartTime;
end;

end.

