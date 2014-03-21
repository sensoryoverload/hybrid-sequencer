unit determinetransients;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FFTReal, UComplex, LCLIntf, math, fgl;

const
  BLOCKSIZE = 512;
  HALFBLOCKSIZE = BLOCKSIZE div 2;
  THRESHOLD_WINDOW_SIZE = 10;
  DEFAULT_SENSITIVITY = 1.9;

type
  TIntegerList = specialize TFPGList<Integer>;
  TSingleList = specialize TFPGList<Single>;

  { TDetermineTransients }

  TDetermineTransients = class
  private
    FFFT: TFFTReal;
    FA_Input,
    FA_Output,
    FF_Input: pflt_array;
    FPerformance: Cardinal;
    FSampleRate: Integer;
    FSensitivity: Single;
    FTransients: TIntegerList;
    procedure SetSensitivity(AValue: Single);
  public
    constructor Create(ASampleRate: Integer); reintroduce;
    destructor Destroy; override;
    function Process(ABuffer: PSingle; AFrames: Integer; AChannelCount: Integer = 2): Integer;

    property Performance: Cardinal read FPerformance;
    property Transients: TIntegerList read FTransients;
    property Sensitivity: Single read FSensitivity write SetSensitivity;
  end;

implementation

{ TDetermineTransients }

procedure TDetermineTransients.SetSensitivity(AValue: Single);
begin
  if FSensitivity=AValue then Exit;
  FSensitivity:=AValue;
end;

constructor TDetermineTransients.Create(ASampleRate: Integer);
begin
  inherited Create;

  FSensitivity := DEFAULT_SENSITIVITY;

  FTransients := TIntegerList.Create;

  FSampleRate := ASampleRate;

  FFFT := TFFTReal.Create(BLOCKSIZE);

  Getmem(FA_Input, BLOCKSIZE * sizeof_flt);
  Getmem(FA_Output, BLOCKSIZE * sizeof_flt);
  Getmem(FF_Input, BLOCKSIZE * sizeof_flt);
end;

destructor TDetermineTransients.Destroy;
begin
  FFFT.Free;

  FreeMem(FA_Input);
  FreeMem(FA_Output);
  FreeMem(FF_Input);

  FTransients.Free;

  inherited Destroy;
end;

function TDetermineTransients.Process(
  ABuffer: PSingle; AFrames: Integer;
  AChannelCount: Integer = 2): Integer;
var
  a_real, a_img: double;
  scale: double;
  i, j: Integer;
  lOffset: Integer;
  a_cmp, b_cmp, f_cmp: complex;
  f_abs: double;
  lMaximum: double;
  lStartTime, lEndTime: Cardinal;
  lBlockIndex: Integer;
  lBlockOffset: Integer;
  lFlux: Single;
  lValue: Single;
  lStart: Integer;
  lEnd: Integer;
  lMean: Single;
  lSpectrum: Array[0..BLOCKSIZE - 1] of Single;
  lLastSpectrum: Array[0..BLOCKSIZE - 1] of Single;
  lSpectralFlux: TSingleList;
  lPrunnedSpectralFlux: TSingleList;
  lThreshold: TSingleList;
  lPeaks: TSingleList;
begin
  lStartTime := LclIntf.GetTickCount;

  FTransients.Clear;

  lSpectralFlux := TSingleList.Create;
  lPrunnedSpectralFlux := TSingleList.Create;
  lThreshold := TSingleList.Create;
  lPeaks := TSingleList.Create;
  try
    for lBlockIndex := 0 to Pred(AFrames div BLOCKSIZE) - 1 do
    begin
      lBlockOffset := lBlockIndex * BLOCKSIZE * AChannelCount;

      if AChannelCount = 2 then
      begin
        lOffset := 0;
        for i := 0 to Pred(BLOCKSIZE) do
        begin
          FA_Input^[i] :=
            ABuffer[lBlockOffset + lOffset] +
            ABuffer[lBlockOffset + lOffset + 1];

          Inc(lOffset, 2);
        end;
      end
      else if AChannelCount = 1 then
      begin
        for i := 0 to Pred(BLOCKSIZE) do
        begin
          FA_Input^[i] := ABuffer[lBlockOffset + i]
        end;
      end;

      // Clear buffers
      FillByte(FA_Output^, BLOCKSIZE * sizeof_flt, 0);
      FillByte(FF_Input^, BLOCKSIZE * sizeof_flt, 0);

      FFFT.do_fft(FA_Output, FA_Input);

      scale := 1/(2 * BLOCKSIZE -1);
      for i := 0 to Pred(HALFBLOCKSIZE) do
      begin
        a_cmp.re := FA_Output^[i];
        if (i > 0) and (i < HALFBLOCKSIZE) then
          a_cmp.im := FA_Output^[i + HALFBLOCKSIZE]
        else
          a_cmp.im := 0;

        lSpectrum[i] := sqrt(a_cmp.re * a_cmp.re + a_cmp.im * a_cmp.im);
      end;

      lFlux := 0;
      for j := 0 to Pred(BLOCKSIZE) do
      begin
        lValue := lSpectrum[j] - lLastSpectrum[j];
        if lValue > 0 then
        begin
          lFlux += lValue;
        end;
        lLastSpectrum[j] := lSpectrum[j];
      end;
      lSpectralFlux.Add(lFlux);
    end;

    for i := 0 to Pred(lSpectralFlux.Count) do
    begin
      lStart := Math.max(0, i - THRESHOLD_WINDOW_SIZE);
      lEnd := Math.min(lSpectralFlux.Count - 1, i + THRESHOLD_WINDOW_SIZE);
      lMean := 0;
      for j := lStart to lEnd do
      begin
        lMean += lSpectralFlux[j];
      end;
      lMean /= (lEnd - lStart);
      lThreshold.add(lMean * FSensitivity);
    end;

    for i := 0 to Pred(lThreshold.Count) do
    begin
      if lThreshold[i] <= lSpectralFlux[i] then
      begin
        lPrunnedSpectralFlux.Add(lSpectralFlux[i] - lThreshold[i]);
      end
      else
      begin
        lPrunnedSpectralFlux.Add(0);
      end;
    end;

    for i := 0 to lPrunnedSpectralFlux.Count - 2 do
    begin
      if lPrunnedSpectralFlux[i] > lPrunnedSpectralFlux[i + 1] then
      begin
//        lPeaks.Add(lPrunnedSpectralFlux[i]);
        FTransients.Add(Round(i * BLOCKSIZE));
      end
      else
      begin
        //lPeaks.Add(0);
      end;
    end;

    for i := 0 to Pred(lPeaks.Count) do
    begin
      if lPeaks[i] > 0 then
      begin
        FTransients.Add(Round(i * BLOCKSIZE));
      end;
    end;
  finally
    lSpectralFlux.Free;
    lPrunnedSpectralFlux.Free;
    lThreshold.Free;
    lPeaks.Free;
  end;

  lEndTime := LclIntf.GetTickCount;
  FPerformance := lEndTime - lStartTime;
end;

end.

