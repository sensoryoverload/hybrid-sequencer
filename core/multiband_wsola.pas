{
  Copyright (C) 2009 Robbert Latumahina

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

  multiband_wsola.pas
}

unit multiband_wsola;

{$mode objfpc}{$H+}

{$fputype sse2}

interface

uses
  Classes, SysUtils, Math, {uRbjEqFilters, }ContNrs, soundtouch;



type
  TFilterType = (ftLowPass, ftHighPass);

  { TLinkWitzFilter }

  TLinkWitzFilter = class(TObject)
  private
    fc: Integer;
    srate: Integer;
    wc, wc2, wc3, wc4, k, k2, k3, k4: single;
    sqrt2, sq_tmp1, sq_tmp2, a_tmp, inverse_a_tmp: single;
    a0, a1, a2, a3, a4: single;
    b1, b2, b3, b4: single;

    //Low
    lxm1, lxm2, lxm3, lxm4, lym1, lym2, lym3, lym4: single;
    ltempx, ltempy: single;

    //High
    hxm1, hxm2, hxm3, hxm4, hym1, hym2, hym3, hym4: single;
    htempx, htempy: single;

    FFilterType: TFilterType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure process(AInput, AOutput: PSingle; AFrames: Integer);
    property Cutoff: Integer read fc write fc;
    property FilterType: TFilterType read FFilterType write FFilterType;
    property Samplerate: Integer read SRate write SRate;
  end;

  { TMultiWSOLA }

  TMultiWSOLA = class
  private
    FChannels: Integer;
    FBands: Integer;
    FOverlapMS: Integer;
    FSamplerate: Integer;
    FBufferLow1: psingle;
    FBufferHigh2: psingle;
    FBufferLow2: psingle;
    FBufferHigh3: psingle;
    FBufferLow3: psingle;

    FTimeBuffer1: psingle;
    FTimeBuffer2: psingle;
    FTimeBuffer3: psingle;

    FBand1Pitcher: TSoundTouch;
    FBand2Pitcher: TSoundTouch;
    FBand3Pitcher: TSoundTouch;

    FBand1LowPass: TLinkWitzFilter;
    FBand2HighPass: TLinkWitzFilter;
    FBand2LowPass: TLinkWitzFilter;
    FBand3HighPass: TLinkWitzFilter;

    FPitch: single;
    FPitchOld: single;
    FSequenceMS: Integer;
    FSeekWindowMS: Integer;
    FQuickSeek: Boolean;
    FAntiAliasFilter: Boolean;
    FDefaultSeekWindow: Integer;
    FDefaultSequenceWindow: Integer;
    FDefaultOverlapWindow: Integer;
    function GetAntiAliasFilter: Boolean;
    procedure SetAntiAliasFilter(const AValue: Boolean);
    procedure SetBands(AValue: Integer);
    procedure SetChannels(AValue: Integer);
    procedure SetOverlapMS(AValue: Integer);
    procedure SetPitch(const AValue: single);
    procedure SetQuickSeek(const AValue: Boolean);
    procedure SetSamplerate(AValue: Integer);
    procedure SetSeekWindowMS(const AValue: Integer);
    procedure SetSequenceMS(const AValue: Integer);
    function BoolToInt(AExpression: Boolean): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    function GetLatency: Integer;
    procedure Flush;
    procedure Clear;
    procedure Process(AInput, AOutput: PSingle; AFrames: Integer);
    property Channels: Integer read FChannels write SetChannels;
    property Bands: Integer read FBands write SetBands;
    property Pitch: single read FPitch write SetPitch;
    property SequenceMS: Integer read FSequenceMS write SetSequenceMS;
    property SeekWindowMS: Integer read FSeekWindowMS write SetSeekWindowMS;
    property OverlapMS: Integer read FOverlapMS write SetOverlapMS;
    property AntiAliasFilter: Boolean read GetAntiAliasFilter write SetAntiAliasFilter;
    property QuickSeek: Boolean read FQuickSeek write SetQuickSeek;
    property Samplerate: Integer read FSamplerate write SetSamplerate;
  end;

implementation

{ LinkWitzFilter }

constructor TLinkWitzFilter.Create;
begin
  fc := 1000;
  srate := 44100;
  FFilterType := ftLowPass;
end;

destructor TLinkWitzFilter.Destroy;
begin
end;

procedure TLinkWitzFilter.Init;
begin
  //================================================
  // shared for both lp, hp; optimizations here
  //================================================
  wc :=2 * pi * fc;
  wc2 := wc * wc;
  wc3 := wc2 * wc;
  wc4 := wc2 * wc2;
  k := wc / tan(pi * fc / srate);
  k2 := k * k;
  k3 := k2 * k;
  k4 := k2 * k2;
  sqrt2 := sqrt(2);
  sq_tmp1 := sqrt2 * wc3 * k;
  sq_tmp2 := sqrt2 * wc * k3;
  a_tmp := 4 * wc2 * k2 + 2 * sq_tmp1 + k4 + 2 * sq_tmp2 + wc4;
  inverse_a_tmp := 1 / a_tmp;

  b1 := (4*(wc4+sq_tmp1-k4-sq_tmp2))/a_tmp;
  b2 := (6*wc4-8*wc2*k2+6*k4)/a_tmp;
  b3 := (4*(wc4-sq_tmp1+sq_tmp2-k4))/a_tmp;
  b4 := (k4-2*sq_tmp1+wc4-2*sq_tmp2+4*wc2*k2)/a_tmp;
end;

procedure TLinkWitzFilter.process(AInput, AOutput: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  if FFilterType = ftLowPass then
  begin
    // low-pass
    a0 := wc4 * inverse_a_tmp;
    a1 := 4 * wc4 * inverse_a_tmp;
    a2 := 6 * wc4 * inverse_a_tmp;
    a3 := a1;
    a4 := a0;
    for i := 0 to Pred(AFrames) do
    begin
      ltempx := AInput[i];

      ltempy :=
        a0 * ltempx + a1 * lxm1 + a2 * lxm2 + a3 * lxm3 + a4 * lxm4 - b1 * lym1 - b2 * lym2 - b3 * lym3 - b4 * lym4;
      lxm4 := lxm3;
      lxm3 := lxm2;
      lxm2 := lxm1;
      lxm1 := ltempx;
      lym4 := lym3;
      lym3 := lym2;
      lym2 := lym1;
      lym1 := ltempy;

      AOutput[i] := ltempy;
    end;
  end
  else
  begin
    // high-pass
    a0 := k4 * inverse_a_tmp;
    a1 := -4 * k4 * inverse_a_tmp;
    a2 := 6 * k4 * inverse_a_tmp;
    a3 := a1;
    a4 := a0;
    for i := 0 to Pred(AFrames) do
    begin

      htempx := AInput[i];

      htempy := a0 * htempx + a1 * hxm1 + a2 * hxm2 + a3 * hxm3 + a4 * hxm4 - b1 * hym1 - b2 * hym2 - b3 * hym3 - b4 * hym4;
      hxm4 := hxm3;
      hxm3 := hxm2;
      hxm2 := hxm1;
      hxm1 := htempx;
      hym4 := hym3;
      hym3 := hym2;
      hym2 := hym1;
      hym1 := htempy;

      AOutput[i] := htempy;
    end;
  end;
end;

{ MultiWSOLA }

procedure TMultiWSOLA.SetPitch(const AValue: single);
begin
  if FPitch = AValue then Exit;
  FPitch := AValue;

  FBand1Pitcher.setPitch(FPitch);
  FBand2Pitcher.setPitch(FPitch);
  FBand3Pitcher.setPitch(FPitch);
end;

procedure TMultiWSOLA.SetAntiAliasFilter(const AValue: Boolean);
begin
  FAntiAliasFilter := AValue;

  FBand1Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
  FBand2Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
  FBand3Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
end;

procedure TMultiWSOLA.SetBands(AValue: Integer);
begin
  if FBands = AValue then Exit;
  FBands := AValue;
end;

procedure TMultiWSOLA.SetChannels(AValue: Integer);
begin
  if FChannels = AValue then Exit;
  FChannels := AValue;

  FBand1Pitcher.setChannels(AValue);
  FBand2Pitcher.setChannels(AValue);
  FBand3Pitcher.setChannels(AValue);
end;

procedure TMultiWSOLA.SetOverlapMS(AValue: Integer);
begin
  if FOverlapMS = AValue then Exit;
  FOverlapMS := AValue;

  FBand1Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FOverlapMS);
  FBand2Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FOverlapMS);
  FBand3Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FOverlapMS);
end;

function TMultiWSOLA.GetAntiAliasFilter: Boolean;
begin
  Result := FAntiAliasFilter;
end;

procedure TMultiWSOLA.SetQuickSeek(const AValue: Boolean);
begin
  if FQuickSeek = AValue then Exit;
  FQuickSeek := AValue;

  FBand1Pitcher.setSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand2Pitcher.setSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand3Pitcher.setSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
end;

procedure TMultiWSOLA.SetSamplerate(AValue: Integer);
begin
  if FSamplerate = AValue then Exit;
  FSamplerate := AValue;

  FBand1Pitcher.setSampleRate(FSamplerate);
  FBand2Pitcher.setSampleRate(FSamplerate);
  FBand3Pitcher.setSampleRate(FSamplerate);
end;

procedure TMultiWSOLA.SetSeekWindowMS(const AValue: Integer);
begin
  if FSeekWindowMS = AValue then Exit;
  FSeekWindowMS := AValue;

  FBand1Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
  FBand2Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
  FBand3Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
end;

procedure TMultiWSOLA.SetSequenceMS(const AValue: Integer);
begin
  if FSequenceMS = AValue then Exit;
  FSequenceMS := AValue;

  FBand1Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
  FBand2Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
  FBand3Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
end;

function TMultiWSOLA.BoolToInt(AExpression: Boolean): Integer;
begin
  if AExpression then
    Result := 1
  else
    Result := 0;
end;

constructor TMultiWSOLA.Create;
begin
  FBand1LowPass := TLinkWitzFilter.Create;
  FBand1LowPass.FilterType := ftLowPass;
  FBand2HighPass := TLinkWitzFilter.Create;
  FBand2HighPass.FilterType := ftHighPass;
  FBand2LowPass := TLinkWitzFilter.Create;
  FBand2LowPass.FilterType := ftLowPass;
  FBand3HighPass := TLinkWitzFilter.Create;
  FBand3HighPass.FilterType := ftHighPass;

  FBand1Pitcher := TSoundTouch.Create;
  FBand2Pitcher := TSoundTouch.Create;
  FBand3Pitcher := TSoundTouch.Create;

  FBufferLow1 := GetMem(40000);
  FBufferHigh2 := GetMem(40000);
  FBufferLow2 := GetMem(40000);
  FBufferHigh3 := GetMem(40000);

  FTimeBuffer1 := GetMem(40000);
  FTimeBuffer2 := GetMem(40000);
  FTimeBuffer3 := GetMem(40000);
end;

destructor TMultiWSOLA.Destroy;
begin
  FreeMem(FBufferLow1);
  FreeMem(FBufferHigh2);
  FreeMem(FBufferLow2);
  FreeMem(FBufferHigh3);

  FreeMem(FTimeBuffer1);
  FreeMem(FTimeBuffer2);
  FreeMem(FTimeBuffer3);

  FBand1Pitcher.Free;
  FBand2Pitcher.Free;
  FBand3Pitcher.Free;

  FBand1LowPass.Free;
  FBand2HighPass.Free;
  FBand2LowPass.Free;
  FBand3HighPass.Free;

  inherited Destroy;
end;

procedure TMultiWSOLA.Initialize;
begin
(*
  FBand1Pitcher.setSampleRate(FSamplerate);
  FBand1Pitcher.setChannels(FChannels);
  FBand1Pitcher.SetSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand1Pitcher.setSetting(SETTING_USE_AA_FILTER, BoolToInt(FAntiAliasFilter));
  FBand1Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand1Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand1Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);

  FBand2Pitcher.setSampleRate(FSamplerate);
  FBand2Pitcher.setChannels(FChannels);
  FBand2Pitcher.SetSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand2Pitcher.setSetting(SETTING_USE_AA_FILTER, BoolToInt(FAntiAliasFilter));
  FBand2Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand2Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand2Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);

  FBand3Pitcher.setSampleRate(FSamplerate);
  FBand3Pitcher.setChannels(FChannels);
  FBand3Pitcher.SetSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand3Pitcher.setSetting(SETTING_USE_AA_FILTER, BoolToInt(FAntiAliasFilter));
  FBand3Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand3Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand3Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);
  *)
  FBand1LowPass.Cutoff := 300;
  FBand1LowPass.Samplerate := FSamplerate;
  FBand1LowPass.Init;
  FBand2HighPass.Cutoff := 300;
  FBand2HighPass.Samplerate := FSamplerate;
  FBand2HighPass.Init;
  FBand2LowPass.Cutoff := 1500;
  FBand2LowPass.Samplerate := FSamplerate;
  FBand2LowPass.Init;
  FBand3HighPass.Cutoff := 1500;
  FBand3HighPass.Samplerate := FSamplerate;
  FBand3HighPass.Init;
end;

function TMultiWSOLA.GetLatency: Integer;
begin
  Result := FBand1Pitcher.GetSetting(SETTING_SEQUENCE_MS);
end;

procedure TMultiWSOLA.Flush;
begin
  FBand1Pitcher.flush;
  FBand1Pitcher.flush;
  FBand3Pitcher.flush;
end;

procedure TMultiWSOLA.Clear;
begin
  FBand1Pitcher.Clear;
  FBand1Pitcher.Clear;
  FBand3Pitcher.Clear;
end;

procedure TMultiWSOLA.Process(AInput, AOutput: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  // Band 1
  FBand1LowPass.process(AInput, FBufferLow1, AFrames);
  FBand1Pitcher.PutSamples(FBufferLow1, AFrames);
  FBand1Pitcher.ReceiveSamples(FTimeBuffer1, AFrames);

  // Band 2
  FBand2HighPass.process(AInput, FBufferHigh2, AFrames);
  FBand2LowPass.process(FBufferHigh2, FBufferLow2, AFrames);
  FBand2Pitcher.PutSamples(FBufferLow2, AFrames);
  FBand2Pitcher.ReceiveSamples(FTimeBuffer2, AFrames);

  // Band 3
  FBand3HighPass.process(AInput, FBufferHigh3, AFrames);
  FBand3Pitcher.PutSamples(FBufferHigh3, AFrames);
  FBand3Pitcher.ReceiveSamples(FTimeBuffer3, AFrames);

  for i := 0 to Pred(AFrames) do
  begin
    AOutput[i] := FTimeBuffer1[i] + FTimeBuffer2[i] + FTimeBuffer3[i];
  end;
end;

end.

