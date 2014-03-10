unit determinetransients;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FFTReal, UComplex, LCLIntf, math;

const
  BLOCKSIZE = 1024;
  HALFBLOCKSIZE = BLOCKSIZE div 2;
  THRESHOLD_WINDOW_SIZE = 10;
  MULTIPLIER = 1.5;

type
  { TDetermineTransients }

  TDetermineTransients = class
  private
    FFFT: TFFTReal;
    FA_Input,
    FA_Output,
    FF_Input: pflt_array;
    FPerformance: Cardinal;
    FSampleRate: Integer;
    FSpectralFluxList: TStringList;
    FSpectrum: Array[0..BLOCKSIZE - 1] of Single;
    FLastSpectrum: Array[0..BLOCKSIZE - 1] of Single;
  public
    constructor Create(ASampleRate: Integer); reintroduce;
    destructor Destroy; override;
    function Process(ABuffer: PSingle; AFrames: Integer; AChannelCount: Integer = 2): Integer;

    property Performance: Cardinal read FPerformance;
    property PositionList: TStringList read FSpectralFluxList;
  end;

implementation

{ TDetermineTransients }


constructor TDetermineTransients.Create(ASampleRate: Integer);
begin
  inherited Create;

  FSampleRate := ASampleRate;

  FSpectralFluxList := TStringList.Create;
  FSpectralFluxList.Sorted := True;

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

  FSpectralFluxList.Free;

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
  lStart: Single;
  lEnd: Single;
begin
  lStartTime := LclIntf.GetTickCount;

  for lBlockIndex := 0 to Pred(AFrames div BLOCKSIZE) do
  begin
    lBlockOffset := lBlockIndex * BLOCKSIZE;

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

      FSpectrum[i] := sqrt(a_cmp.re * a_cmp.re + a_cmp.im * a_cmp.im);
    end;

    lFlux := 0;
    lValue := 0;
    for j := 0 to Pred(BLOCKSIZE) do
    begin
      lValue := FSpectrum[j] - FLastSpectrum[j];
      lFlux += ifthen(lValue < 0, 0, lValue);
      FLastSpectrum[j] := FSpectrum[j];
      FSpectralFluxList.Add(FloatToStr(lFlux));
    end;

    for i := 0 to Pred(lFlux) do
    begin
             lStart := Math.max(0, i - THRESHOLD_WINDOW_SIZE);
             lEnd := Math.min(spectralFlux.size() - 1, i + THRESHOLD_WINDOW_SIZE);
             float lMean = 0;
             for j := lStart to lEnd do
             begin
                lMean += spectralFlux.get(j);
             end;
             lMean /= (lEnd - lStart);
             threshold.add( lMean * MULTIPLIER);
    end;

    for( int i = 0; i < threshold.size(); i++ )
    {
       if( threshold.get(i) <= spectralFlux.get(i) )
          prunnedSpectralFlux.add( spectralFlux.get(i) - threshold.get(i) );
       else
          prunnedSpectralFlux.add( (float)0 );
    }



for( int i = 0; i < prunnedSpectralFlux.size() - 1; i++ )
{
   if( prunnedSpectralFlux.get(i) > prunnedSpectralFlux.get(i+1) )
      peaks.add( prunnedSpectralFlux.get(i) );
   else
      peaks.add( (float)0 );
}
  end;


  (*
  And that’s it. Any value > 0 in the ArrayList peaks is an onset or beat now.
  To calculate the point in time for each peak in peaks we simply take its index and
  multiply it by the time span that the original sample window takes up.
  Say we used a sample window of 1024 samples at a sampling rate of 44100Hz then
  we have the simple forumula time = index * (1024 / 44100). That’s it.
  Here’s the output:  *)




  lEndTime := LclIntf.GetTickCount;
  FPerformance := lEndTime - lStartTime;
end;

end.

