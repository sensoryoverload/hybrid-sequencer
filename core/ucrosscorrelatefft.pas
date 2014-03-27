{
  Copyright (C) 2014 Robbert Latumahina

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  uCrossCorrelateFFT.pas
}
unit uCrossCorrelateFFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FFTReal, UComplex, LCLIntf;

type
  { TCrossCorrelate }

  TCrossCorrelate = class
  private
    FFFT: TFFTReal;
    FA_Input,
    FB_Input,
    FA_Output,
    FB_Output,
    FF_Input,
    FF_Output: pflt_array;
    FPerformance: Cardinal;
    FOverlapLength: Integer;
    FSeekWindowLength: Integer;
    FBufferSize: Integer;
    FHalfBufferSize: Integer;
    FSampleRate: Integer;
    procedure SetOverlapLength(AValue: Integer);
    procedure SetSampleRate(AValue: Integer);
    procedure SetSeekWindowLength(AValue: Integer);
  protected
    procedure CalcParameters;
  public
    constructor Create(ASampleRate: Integer); reintroduce;
    destructor Destroy; override;
    function Process(AOverlapWindow, ASeekWindow: PSingle; AChannelCount: Integer = 2): Integer;

    property OverlapLength: Integer write SetOverlapLength;
    property SeekWindowLength: Integer write SetSeekWindowLength;
    property Performance: Cardinal read FPerformance;
    property BufferSize: Integer read FBufferSize;
    property SampleRate: Integer read FSampleRate write SetSampleRate;
  end;

implementation

{ TCrossCorrelate }

procedure TCrossCorrelate.SetOverlapLength(AValue: Integer);
begin
  FOverlapLength := AValue;

  CalcParameters;
end;

procedure TCrossCorrelate.SetSampleRate(AValue: Integer);
begin
  FSampleRate:=AValue;

  CalcParameters;
end;

procedure TCrossCorrelate.SetSeekWindowLength(AValue: Integer);
begin
  FSeekWindowLength := AValue;

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

    ReAllocMem(FA_Input, FBufferSize * sizeof_flt);
    ReAllocMem(FB_Input, FBufferSize * sizeof_flt);
    ReAllocMem(FA_Output, FBufferSize * sizeof_flt);
    ReAllocMem(FB_Output, FBufferSize * sizeof_flt);
    ReAllocMem(FF_Input, FBufferSize * sizeof_flt);
    ReAllocMem(FF_Output, FBufferSize * sizeof_flt);
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

  FreeMem(FA_Input);
  FreeMem(FB_Input);
  FreeMem(FA_Output);
  FreeMem(FB_Output);
  FreeMem(FF_Input);
  FreeMem(FF_Output);

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
        FA_Input^[i] := AOverlapWindow[lOffset] + AOverlapWindow[lOffset + 1]
      else
        FA_Input^[i] := 0;
      Inc(lOffset, AChannelCount);
    end;

    // Prepare target buffer
    lOffset := 0;
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FSeekWindowLength then
        FB_Input^[i] := ASeekWindow[lOffset] + ASeekWindow[lOffset + 1]
      else
        FB_Input^[i] := 0;
      Inc(lOffset, AChannelCount);
    end;
  end
  else if AChannelCount = 1 then
  begin
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FOverlapLength then
        FA_Input^[i] := AOverlapWindow[i]
      else
        FA_Input^[i] := 0;
    end;

    // Prepare target buffer
    for i := 0 to Pred(FBufferSize) do
    begin
      if i < FSeekWindowLength then
        FB_Input^[i] := ASeekWindow[i]
      else
        FB_Input^[i] := 0;
    end;
  end;

  // Clear buffers
  FillByte(FA_Output^, FBufferSize * sizeof_flt, 0);
  FillByte(FB_Output^, FBufferSize * sizeof_flt, 0);
  FillByte(FF_Input^, FBufferSize * sizeof_flt, 0);
  FillByte(FF_Output^, FBufferSize * sizeof_flt, 0);

  // FFT overlap and seekwindow buffers
  FFFT.do_fft(FA_Output, FA_Input);
  FFFT.do_fft(FB_Output, FB_Input);

  scale := 1/(2 * FBufferSize -1);
  for i := 0 to Pred(FHalfBufferSize) do
  begin
    a_cmp.re := FA_Output^[i];
    if (i > 0) and (i < FHalfBufferSize) then
      a_cmp.im := FA_Output^[i + FHalfBufferSize]
    else
      a_cmp.im := 0;

    b_cmp.re := FB_Output^[i];
    if (i > 0) and (i < FHalfBufferSize) then
      b_cmp.im := FB_Output^[i + FHalfBufferSize]
    else
      b_cmp.im := 0;

    f_cmp := a_cmp * cong(b_cmp) * scale;
    FF_Input^[i] := f_cmp.re;
    FF_Input^[i + FHalfBufferSize] := f_cmp.im;
  end;

  FFFT.do_ifft(FF_Input, FF_Output);

  lMaximum := 0;
  Result := 0;
  for i := 0 to Pred(FHalfBufferSize) do
  begin
    a_real := FF_Output^[i];
    if (i > 0) and (i < FHalfBufferSize) then
      a_img := FF_Output^[i + FHalfBufferSize]
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

