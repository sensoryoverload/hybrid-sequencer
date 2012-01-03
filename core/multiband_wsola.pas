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

interface

uses
  Classes, SysUtils, Math, uRbjEqFilters, ContNrs, soundtouch;



type
  TFilterType = (ftLowPass, ftHighPass);

  { TLinkWitzFilter }

  TLinkWitzFilter = class(TObject)
  private
    fc: Integer;
    srate: Integer;
    wc, wc2, wc3, wc4, k, k2, k3, k4: single;
    sqrt2, sq_tmp1, sq_tmp2, a_tmp: single;
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

  TMultiWSOLA = class(TObject)
  private
    FChannels: Integer;
    FBands: Integer;
    FSamplerate: Integer;
    FBufferLow1: psingle;
    FBufferHigh2: psingle;
    FBufferLow2: psingle;
    FBufferHigh3: psingle;
    FBufferLow3: psingle;
    FBufferHigh4: psingle;
    FTimeBuffer1: psingle;
    FTimeBuffer2: psingle;
    FTimeBuffer3: psingle;
    FTimeBuffer4: psingle;
    FBand1Pitcher: TSoundTouch;
    FBand2Pitcher: TSoundTouch;
    FBand3Pitcher: TSoundTouch;
    FBand4Pitcher: TSoundTouch;

    FBand1LowPass: TLinkWitzFilter;
    FBand2HighPass: TLinkWitzFilter;
    FBand2LowPass: TLinkWitzFilter;
    FBand3HighPass: TLinkWitzFilter;
    FBand3LowPass: TLinkWitzFilter;
    FBand4HighPass: TLinkWitzFilter;

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
    procedure SetPitch(const AValue: single);
    procedure SetQuickSeek(const AValue: Boolean);
    procedure SetSeekWindowMS(const AValue: Integer);
    procedure SetSequenceMS(const AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    function GetLatency: Integer;
    procedure Process(AInput, AOutput: PSingle; AFrames: Integer);
    property Channels: Integer read FChannels write FChannels;
    property Bands: Integer read FBands write FBands;
    property Pitch: single read FPitch write SetPitch;
    property SequenceMS: Integer read FSequenceMS write SetSequenceMS;
    property SeekWindowMS: Integer read FSeekWindowMS write SetSeekWindowMS;
    property AntiAliasFilter: Boolean read GetAntiAliasFilter write SetAntiAliasFilter;
    property QuickSeek: Boolean read FQuickSeek write SetQuickSeek;
    property Samplerate: Integer read FSamplerate write FSamplerate;
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
    a0 := wc4 / a_tmp;
    a1 := 4 * wc4 / a_tmp;
    a2 := 6 * wc4 / a_tmp;
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
    a0 := k4 / a_tmp;
    a1 := -4 * k4 / a_tmp;
    a2 := 6 * k4 / a_tmp;
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
  if AValue <> FPitchOld then
  begin
    FPitch := AValue;
    FPitchOld := FPitch;
  end;
end;

procedure TMultiWSOLA.SetAntiAliasFilter(const AValue: Boolean);
begin
  FAntiAliasFilter := AValue;

  FBand1Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
  FBand2Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
  FBand3Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
  FBand4Pitcher.setSetting(SETTING_USE_AA_FILTER, Integer(FAntiAliasFilter));
end;

function TMultiWSOLA.GetAntiAliasFilter: Boolean;
begin
  Result := FAntiAliasFilter;
end;

procedure TMultiWSOLA.SetQuickSeek(const AValue: Boolean);
begin
  FQuickSeek := AValue;

  FBand1Pitcher.setSetting(SETTING_USE_QUICKSEEK, Integer(FQuickSeek));
  FBand2Pitcher.setSetting(SETTING_USE_QUICKSEEK, Integer(FQuickSeek));
  FBand3Pitcher.setSetting(SETTING_USE_QUICKSEEK, Integer(FQuickSeek));
  FBand4Pitcher.setSetting(SETTING_USE_QUICKSEEK, Integer(FQuickSeek));
end;

procedure TMultiWSOLA.SetSeekWindowMS(const AValue: Integer);
begin
  if AValue < 4 then
    FSeekWindowMS := 4
  else if AValue > 100 then
    FSeekWindowMS := 100;

  FBand1Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
  FBand2Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
  FBand3Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
  FBand4Pitcher.setSetting(SETTING_SEEKWINDOW_MS, FSeekWindowMS);
  writeln(format('SetSeekWindowMS %d', [FBand1Pitcher.getSetting(SETTING_SEEKWINDOW_MS)]));
end;

procedure TMultiWSOLA.SetSequenceMS(const AValue: Integer);
begin
  if AValue < 20 then
    FSequenceMS := 20
  else if AValue > 400 then
    FSequenceMS := 400;

  FBand1Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
  FBand2Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
  FBand3Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
  FBand4Pitcher.setSetting(SETTING_SEQUENCE_MS, FSequenceMS);
  writeln(format('SetSequenceMS %d', [FBand1Pitcher.getSetting(SETTING_SEQUENCE_MS)]));
end;

constructor TMultiWSOLA.Create;
begin
  FChannels := 2;
  FBands := 4;
  FQuickSeek := True;
  FDefaultSeekWindow := 15;
  FDefaultSequenceWindow := 20;
  FDefaultOverlapWindow := 8;

  FBand1LowPass := TLinkWitzFilter.Create;
  FBand1LowPass.FilterType := ftLowPass;
  FBand2HighPass := TLinkWitzFilter.Create;
  FBand2HighPass.FilterType := ftHighPass;
  FBand2LowPass := TLinkWitzFilter.Create;
  FBand2LowPass.FilterType := ftLowPass;
  FBand3HighPass := TLinkWitzFilter.Create;
  FBand3HighPass.FilterType := ftHighPass;
  FBand3LowPass := TLinkWitzFilter.Create;
  FBand3LowPass.FilterType := ftLowPass;
  FBand4HighPass := TLinkWitzFilter.Create;
  FBand4HighPass.FilterType := ftHighPass;

  FBand1Pitcher := TSoundTouch.Create;
  FBand2Pitcher := TSoundTouch.Create;
  FBand3Pitcher := TSoundTouch.Create;
  FBand4Pitcher := TSoundTouch.Create;

  FBufferLow1 := GetMem(10000);
  FBufferHigh2 := GetMem(10000);
  FBufferLow2 := GetMem(10000);
  FBufferHigh3 := GetMem(10000);
  FBufferLow3 := GetMem(10000);
  FBufferHigh4 := GetMem(10000);

  FTimeBuffer1 := GetMem(10000);
  FTimeBuffer2 := GetMem(10000);
  FTimeBuffer3 := GetMem(10000);
  FTimeBuffer4 := GetMem(10000);
end;

destructor TMultiWSOLA.Destroy;
begin
  FreeMem(FBufferLow1);
  FreeMem(FBufferHigh2);
  FreeMem(FBufferLow2);
  FreeMem(FBufferHigh3);
  FreeMem(FBufferLow3);
  FreeMem(FBufferHigh4);

  FreeMem(FTimeBuffer1);
  FreeMem(FTimeBuffer2);
  FreeMem(FTimeBuffer3);
  FreeMem(FTimeBuffer4);

  FBand1Pitcher.Free;
  FBand2Pitcher.Free;
  FBand3Pitcher.Free;
  FBand4Pitcher.Free;

  FBand1LowPass.Free;
  FBand2HighPass.Free;
  FBand2LowPass.Free;
  FBand3HighPass.Free;
  FBand3LowPass.Free;
  FBand4HighPass.Free;

  inherited Destroy;
end;

procedure TMultiWSOLA.Initialize;
begin
  FBand1Pitcher.setChannels(FChannels);
  FBand1Pitcher.setSampleRate(44100);
  FBand1Pitcher.SetSetting(SETTING_USE_QUICKSEEK, 1);
  FBand1Pitcher.setSetting(SETTING_USE_AA_FILTER, 0);
  FBand1Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand1Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand1Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);

  FBand2Pitcher.setChannels(FChannels);
  FBand2Pitcher.setSampleRate(44100);
  FBand2Pitcher.SetSetting(SETTING_USE_QUICKSEEK, 1);
  FBand2Pitcher.setSetting(SETTING_USE_AA_FILTER, 0);
  FBand2Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand2Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand2Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);

  FBand3Pitcher.setChannels(FChannels);
  FBand3Pitcher.setSampleRate(44100);
  FBand3Pitcher.SetSetting(SETTING_USE_QUICKSEEK, 1);
  FBand3Pitcher.setSetting(SETTING_USE_AA_FILTER, 0);
  FBand3Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand3Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand3Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);

  FBand4Pitcher.setChannels(FChannels);
  FBand4Pitcher.setSampleRate(44100);
  FBand4Pitcher.SetSetting(SETTING_USE_QUICKSEEK, 1);
  FBand4Pitcher.setSetting(SETTING_USE_AA_FILTER, 0);
  FBand4Pitcher.setSetting(SETTING_SEQUENCE_MS, FDefaultSequenceWindow);
  FBand4Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FBand4Pitcher.setSetting(SETTING_OVERLAP_MS, FDefaultOverlapWindow);

  FBand1LowPass.Cutoff := 500;
  FBand1LowPass.Samplerate := 44100;
  FBand1LowPass.Init;
  FBand2HighPass.Cutoff := 500;
  FBand2HighPass.Samplerate := 44100;
  FBand2HighPass.Init;
  FBand2LowPass.Cutoff := 1500;
  FBand2LowPass.Samplerate := 44100;
  FBand2LowPass.Init;
  FBand3HighPass.Cutoff := 1500;
  FBand3HighPass.Samplerate := 44100;
  FBand3HighPass.Init;
  FBand3LowPass.Cutoff := 3000;
  FBand3LowPass.Samplerate := 44100;
  FBand3LowPass.Init;
  FBand4HighPass.Cutoff := 3000;
  FBand4HighPass.Samplerate := 44100;
  FBand4HighPass.Init;
end;

function TMultiWSOLA.GetLatency: Integer;
begin
  Result := FBand1Pitcher.GetSetting(SETTING_SEQUENCE_MS);
end;

procedure TMultiWSOLA.Process(AInput, AOutput: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  if Assigned(AInput) and Assigned(AOutput) then
  begin
    // Band 1
    FBand1LowPass.process(AInput, FBufferLow1, AFrames);
    FBand1Pitcher.setPitch(FPitch);
    FBand1Pitcher.PutSamples(FBufferLow1, AFrames);
    FBand1Pitcher.ReceiveSamples(FTimeBuffer1, AFrames);

    // Band 2
    FBand2HighPass.process(AInput, FBufferHigh2, AFrames);
    FBand2LowPass.process(FBufferHigh2, FBufferLow2, AFrames);
    FBand2Pitcher.setPitch(FPitch);
    FBand2Pitcher.PutSamples(FBufferLow2, AFrames);
    FBand2Pitcher.ReceiveSamples(FTimeBuffer2, AFrames);

    // Band 3
    FBand3HighPass.process(AInput, FBufferHigh3, AFrames);
    FBand3LowPass.process(FBufferHigh3, FBufferLow3, AFrames);
    FBand3Pitcher.setPitch(FPitch);
    FBand3Pitcher.PutSamples(FBufferLow3, AFrames);
    FBand3Pitcher.ReceiveSamples(FTimeBuffer3, AFrames);

    // Band 4
    FBand4HighPass.process(AInput, FBufferHigh4, AFrames);
    FBand4Pitcher.setPitch(FPitch);
    FBand4Pitcher.PutSamples(FBufferHigh4, AFrames);
    FBand4Pitcher.ReceiveSamples(FTimeBuffer4, AFrames);
    //if AFrames > 500 then AFrames := 500;
    for i := 0 to Pred(AFrames) do
    begin
      AOutput[i] := (FTimeBuffer1[i] + FTimeBuffer2[i] + FTimeBuffer3[i] + FTimeBuffer4[i]) * 1.1;
    end;
  end;
end;

end.

