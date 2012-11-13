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
  Classes, SysUtils, Math, ContNrs, soundtouch;

type

  { T3BandFilterbank }

  T3BandFilterbank = class
  private
    // Filter #1 (Low band)

    lf: single; // Frequency
    f1p0: single; // Poles ...
    f1p1: single;
    f1p2: single;
    f1p3: single;

    // Filter #2 (High band)

    hf: single; // Frequency
    f2p0: single; // Poles ...
    f2p1: single;
    f2p2: single;
    f2p3: single;

    // Sample history buffer

    sdm1: single; // Sample data minus 1
    sdm2: single; // 2
    sdm3: single; // 3

    // Gain Controls

    lg: single; // low gain
    mg: single; // mid gain
    hg: single; // high gain

  public
    l,m,h: single; // Low / Mid / High - Sample Values

    constructor Create;
    procedure Initialize(lowfreq: Integer; highfreq: Integer; mixfreq: Integer);
    procedure Process(
      AInput: psingle; ABand1: psingle; ABand2: psingle; ABand3: psingle; anumsamples: Integer);
  end;

const
  vsa = (1.0 / 4294967295.0); // Very small amount (Denormal Fix)
  M_PI = 3.14159265358979323846;

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
    FBufferLow: psingle;
    FBufferMid: psingle;
    FBufferHigh: psingle;

    FTimeBuffer1: psingle;
    FTimeBuffer2: psingle;
    FTimeBuffer3: psingle;

    FBand1Pitcher: TSoundTouch;
    FBand2Pitcher: TSoundTouch;
    FBand3Pitcher: TSoundTouch;

    FThreeBandFilterbank: T3BandFilterbank;

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
  FBand1Pitcher := TSoundTouch.Create;
  FBand2Pitcher := TSoundTouch.Create;
  FBand3Pitcher := TSoundTouch.Create;

  FBufferLow := GetMem(40000);
  FBufferMid := GetMem(40000);
  FBufferHigh := GetMem(40000);

  FTimeBuffer1 := GetMem(40000);
  FTimeBuffer2 := GetMem(40000);
  FTimeBuffer3 := GetMem(40000);

  FThreeBandFilterbank := T3BandFilterbank.Create;
end;

destructor TMultiWSOLA.Destroy;
begin
  FreeMem(FBufferLow);
  FreeMem(FBufferMid);
  FreeMem(FBufferHigh);

  FreeMem(FTimeBuffer1);
  FreeMem(FTimeBuffer2);
  FreeMem(FTimeBuffer3);

  FBand1Pitcher.Free;
  FBand2Pitcher.Free;
  FBand3Pitcher.Free;

  FThreeBandFilterbank.Free;

  inherited Destroy;
end;

procedure TMultiWSOLA.Initialize;
begin

  {FBand1Pitcher.setSampleRate(FSamplerate);
  FBand1Pitcher.setChannels(FChannels);
  FBand1Pitcher.SetSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand1Pitcher.setSetting(SETTING_USE_AA_FILTER, BoolToInt(FAntiAliasFilter));}
  {FBand1Pitcher.setSetting(SETTING_SEQUENCE_MS, 40);
  FBand1Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 40);
  FBand1Pitcher.setSetting(SETTING_OVERLAP_MS, 8);    }

  {FBand2Pitcher.setSampleRate(FSamplerate);
  FBand2Pitcher.setChannels(FChannels);
  FBand2Pitcher.SetSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand2Pitcher.setSetting(SETTING_USE_AA_FILTER, BoolToInt(FAntiAliasFilter));}
  {FBand2Pitcher.setSetting(SETTING_SEQUENCE_MS, 40);
  FBand2Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 40);
  FBand2Pitcher.setSetting(SETTING_OVERLAP_MS, 8);  }

  {FBand3Pitcher.setSampleRate(FSamplerate);
  FBand3Pitcher.setChannels(FChannels);
  FBand3Pitcher.SetSetting(SETTING_USE_QUICKSEEK, BoolToInt(FQuickSeek));
  FBand3Pitcher.setSetting(SETTING_USE_AA_FILTER, BoolToInt(FAntiAliasFilter));}
  {FBand3Pitcher.setSetting(SETTING_SEQUENCE_MS, 40);
  FBand3Pitcher.setSetting(SETTING_SEEKWINDOW_MS, 40);
  FBand3Pitcher.setSetting(SETTING_OVERLAP_MS, 8); }


  FThreeBandFilterbank.Initialize(1000, 3000, FSamplerate);
end;

function TMultiWSOLA.GetLatency: Integer;
begin
  Result := FBand1Pitcher.GetSetting(SETTING_SEQUENCE_MS);
end;

procedure TMultiWSOLA.Flush;
begin
  FBand1Pitcher.flush;
  FBand2Pitcher.flush;
  FBand3Pitcher.flush;
end;

procedure TMultiWSOLA.Clear;
begin
  FBand1Pitcher.Clear;
  FBand2Pitcher.Clear;
  FBand3Pitcher.Clear;
end;

procedure TMultiWSOLA.Process(AInput, AOutput: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  FThreeBandFilterbank.Process(AInput, FTimeBuffer1, FTimeBuffer2, FTimeBuffer3, AFrames);

  // Band 1
  FBand1Pitcher.PutSamples(FTimeBuffer1, AFrames);
  FBand1Pitcher.ReceiveSamples(FBufferLow, AFrames);

  // Band 2
  FBand2Pitcher.PutSamples(FTimeBuffer2, AFrames);
  FBand2Pitcher.ReceiveSamples(FBufferMid, AFrames);

  // Band 3
  FBand3Pitcher.PutSamples(FTimeBuffer3, AFrames);
  FBand3Pitcher.ReceiveSamples(FBufferHigh, AFrames);

  for i := 0 to Pred(AFrames) do
  begin
    AOutput[i] := FBufferLow[i] + FBufferMid[i] + FBufferHigh[i];
  end;
end;

constructor T3BandFilterbank.Create;
begin
  Initialize(300, 3000, 44100);
end;

// Recommended frequencies are ...
//
// lowfreq = 880 Hz
// highfreq = 5000 Hz
//
// Set mixfreq to whatever rate your system is using (eg 48Khz)
procedure T3BandFilterbank.Initialize(lowfreq: Integer; highfreq: Integer; mixfreq: Integer);
begin
  // Set Low/Mid/High gains to unity
  lg := 1.0;
  mg := 1.0;
  hg := 1.0;

  // Calculate filter cutoff frequencies
  lf := 2 * sin(M_PI * (lowfreq / mixfreq));
  hf := 2 * sin(M_PI * (highfreq / mixfreq));
end;

procedure T3BandFilterbank.Process(AInput: psingle; ABand1: psingle; ABand2: psingle; ABand3: psingle; anumsamples: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(anumsamples) do
  begin
    // Filter #1 (lowpass)
    f1p0 += (lf * (AInput[i] - f1p0)) + vsa;
    f1p1 += (lf * (f1p0 - f1p1));
    f1p2 += (lf * (f1p1 - f1p2));
    f1p3 += (lf * (f1p2 - f1p3));

    l := f1p3;

    // Filter #2 (highpass)
    f2p0 += (hf * (AInput[i] - f2p0)) + vsa;
    f2p1 += (hf * (f2p0 - f2p1));
    f2p2 += (hf * (f2p1 - f2p2));
    f2p3 += (hf * (f2p2 - f2p3));

    h := sdm3 - f2p3;

    // Calculate midrange (signal - (low + high))
    m := sdm3 - (h + l);

    // Scale, Combine and store
    {l *= lg;
    m *= mg;
    h *= hg;}

    // Shuffle history buffer
    sdm3 := sdm2;
    sdm2 := sdm1;
    sdm1 := AInput[i];

    // Return result
    ABand1[i] := l;
    ABand2[i] := m;
    ABand3[i] := h;
  end;
end;

end.

