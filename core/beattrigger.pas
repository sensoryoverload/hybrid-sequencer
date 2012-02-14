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

  beattrigger.pas

  Based on code found on musicdsp.org
}

unit beattrigger;

interface

type

  { TBeatDetector }

  TBeatDetector = class
  private
    KBeatFilter,               // Filter coefficient
    Filter1Out,
    Filter2Out,
    BeatRelease,               // Release time coefficient
    PeakEnv:single;            // Peak enveloppe follower
    PrevBeatPulse:Boolean;     // Rising edge memory
    PrevBeatTrigger: Boolean;
    T_Filter,
    Beat_ReleaseTime: single;
    ThresHold: single;
    LowThresHold: Single;
  public
    BeatPulse:Boolean;         // Beat detector output
    ReleasePulse:Boolean;         // Beat detector output
    BeatTrigger: Boolean;      // Schmitt trigger output
    constructor Create;
    procedure setSampleRate(SampleRate: single);
    procedure setFilterCutOff(Freq_Lp_Beat: single);
    procedure setThresHold(AValue: single);
    procedure AudioProcess(input: single);
    property TriggerOn: Boolean read BeatTrigger;
    property Envelope: single read PeakEnv;
  end;

function fabs(value:single):Single;

implementation

// Beat detector constructor
constructor TBeatDetector.create;
begin
  inherited;

  LowThresHold := 0.3;
  ThresHold := 0.5;
  Filter1Out := 0.0;
  Filter2Out := 0.0;
  PeakEnv := 0.0;
  BeatTrigger := false;
  PrevBeatPulse := false;
  PrevBeatTrigger := false;
  Setfiltercutoff(2000);
  setSampleRate(44100);
end;

// Compute all sample frequency related coeffs
procedure TBeatDetector.setSampleRate(sampleRate:single);
begin
  KBeatFilter := 1.0 / (sampleRate * T_Filter);
  BeatRelease := exp(-1.0 / (sampleRate * Beat_ReleaseTime));
end;

procedure Tbeatdetector.Setfiltercutoff(Freq_Lp_Beat: Single);
begin
  T_Filter := 1.0 / (2.0 * PI * Freq_Lp_Beat); // Low Pass filter time constant
  Beat_ReleaseTime := 0.05; // Release time of enveloppe detector in second
end;

procedure Tbeatdetector.Setthreshold(AValue: Single);
begin
  if AValue < LowThresHold then
    Threshold:= LowThresHold
  else
  begin
    if AValue > 1.0 then
      ThresHold := 1
    else
      ThresHold := AValue;
  end;
end;

function fabs(value:single):Single;
begin
  Result := abs(value);
end;

// Process incoming signal
procedure TBeatDetector.AudioProcess(input:single);
var
  EnvIn: Single;
begin
  // Step 1 : 2nd order low pass filter (made of two 1st order RC filter)
  Filter1Out := Filter1Out + (KBeatFilter * (input - Filter1Out));
  Filter2Out := Filter2Out + (KBeatFilter * (Filter1Out - Filter2Out));
  
  // Step 2 : peak detector
  EnvIn := fabs(Filter2Out);
  if EnvIn > PeakEnv then
    PeakEnv := EnvIn  // Attack time = 0
  else
  begin
    PeakEnv := PeakEnv * BeatRelease;
    PeakEnv := PeakEnv + (1.0 - BeatRelease) * EnvIn;
  end;
  
  // Step 3 : Schmitt trigger
  if not BeatTrigger then
  begin
    if PeakEnv > ThresHold then
      BeatTrigger:= true;
  end
  else
  begin
    if PeakEnv < LowThresHold then
    begin
      BeatTrigger := false;
    end;
  end;

  // Step 4 : rising edge detector
  BeatPulse := false;
  if BeatTrigger and (not PrevBeatPulse) then
    BeatPulse := true;

  // Step 5 : declining edge detector
  ReleasePulse := false;
  if (not BeatTrigger) and PrevBeatTrigger then
    ReleasePulse := true;
    
  PrevBeatPulse := BeatTrigger;
  PrevBeatTrigger := BeatTrigger;
end;

end.


