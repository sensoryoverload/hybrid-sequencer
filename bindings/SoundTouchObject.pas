unit SoundTouchObject;

//////////////////////////////////////////////////////////////////////////////
//
// SoundTouch.dll wrapper for accessing SoundTouch routines from Delphi/Pascal
//
//  Module Author : Christian Budde
//  
////////////////////////////////////////////////////////////////////////////////
//
// $Id$
//
////////////////////////////////////////////////////////////////////////////////
//
// License :
//
//  SoundTouch audio processing library
//  Copyright (c) Olli Parviainen
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
////////////////////////////////////////////////////////////////////////////////

interface

uses
  dynlibs;

type
  TSoundTouchHandle = type pointer;

  TSoundTouchTest = procedure ; cdecl;

  // Create a new instance of SoundTouch processor.
  TSoundTouchCreateInstance = function : TSoundTouchHandle; cdecl;

  // Destroys a SoundTouch processor instance.
  TSoundTouchDestroyInstance = procedure (Handle: TSoundTouchHandle); cdecl;

  // Get SoundTouch library version string
  TSoundTouchGetVersionString = function : PChar; cdecl;

  // Get SoundTouch library version Id
  TSoundTouchGetVersionId = function : Cardinal; cdecl;

  // Sets new rate control value. Normal rate = 1.0, smaller values
  // represent slower rate, larger faster rates.
  TSoundTouchSetRate = procedure (Handle: TSoundTouchHandle; newRate: Single); cdecl;

  // Sets new tempo control value. Normal tempo = 1.0, smaller values
  // represent slower tempo, larger faster tempo.
  TSoundTouchSetTempo = procedure (Handle: TSoundTouchHandle; newTempo: Single); cdecl;

  // Sets new rate control value as a difference in percents compared
  // to the original rate (-50 .. +100 %);
  TSoundTouchSetRateChange = procedure (Handle: TSoundTouchHandle; newRate: Single); cdecl;

  // Sets new tempo control value as a difference in percents compared
  // to the original tempo (-50 .. +100 %);
  TSoundTouchSetTempoChange = procedure (Handle: TSoundTouchHandle; newTempo: Single); cdecl;

  // Sets new pitch control value. Original pitch = 1.0, smaller values
  // represent lower pitches, larger values higher pitch.
  TSoundTouchSetPitch = procedure (Handle: TSoundTouchHandle; newPitch: Single); cdecl;

  // Sets pitch change in octaves compared to the original pitch
  // (-1.00 .. +1.00);
  TSoundTouchSetPitchOctaves = procedure (Handle: TSoundTouchHandle; newPitch: Single); cdecl;

  // Sets pitch change in semi-tones compared to the original pitch
  // (-12 .. +12);
  TSoundTouchSetPitchSemiTones = procedure (Handle: TSoundTouchHandle; newPitch: Single); cdecl;

  // Sets the number of channels, 1 = mono, 2 = stereo
  TSoundTouchSetChannels = procedure (Handle: TSoundTouchHandle; numChannels: Cardinal); cdecl;

  // Sets sample rate.
  TSoundTouchSetSampleRate = procedure (Handle: TSoundTouchHandle; SampleRate: Cardinal); cdecl;

  // Flushes the last samples from the processing pipeline to the output.
  // Clears also the internal processing buffers.
  //
  // Note: This function is meant for extracting the last samples of a sound
  // stream. This function may introduce additional blank samples in the end
  // of the sound stream, and thus it
  // in the middle of a sound stream.
  TSoundTouchFlush = procedure (Handle: TSoundTouchHandle); cdecl;

  // Adds 'numSamples' pcs of samples from the 'samples' memory position into
  // the input of the object. Notice that sample rate _has_to_ be set before
  // calling this function, otherwise throws a runtime_error exception.
  TSoundTouchPutSamples = procedure (Handle: TSoundTouchHandle;
                                     const Samples: PSingle; //< Pointer to sample buffer.
                                     NumSamples: Cardinal    //< Number of samples in buffer. Notice
                                                             //< that in case of stereo-sound a single sample
                                                             //< contains data for both channels.
                                    ); cdecl;

  // Clears all the samples in the object's output and internal processing
  // buffers.
  TSoundTouchClear = procedure (Handle: TSoundTouchHandle); cdecl;

  // Changes a setting controlling the processing system behaviour. See the
  // 'SETTING_...' defines for available setting ID's.
  //
  // \return 'TRUE' if the setting was succesfully changed
  TSoundTouchSetSetting = function (Handle: TSoundTouchHandle;
                                 SettingId: Integer;   //< Setting ID number. see SETTING_... defines.
                                 Value: Integer        //< New setting value.
                                ): Boolean; cdecl;

  // Reads a setting controlling the processing system behaviour. See the
  // 'SETTING_...' defines for available setting ID's.
  //
  // \return the setting value.
  TSoundTouchGetSetting = function (Handle: TSoundTouchHandle;
                                 settingId: Integer     //< Setting ID number, see SETTING_... defines.
                                ): Integer; cdecl;


  // Returns number of samples currently unprocessed.
  TSoundTouchNumUnprocessedSamples = function (Handle: TSoundTouchHandle): Cardinal; cdecl;

  // Adjusts book-keeping so that given number of samples are removed from beginning of the
  // sample buffer without copying them anywhere.
  //
  // Used to reduce the number of samples in the buffer when accessing the sample buffer directly
  // with 'ptrBegin' function.
  TSoundTouchReceiveSamples = function (Handle: TSoundTouchHandle;
                                     outBuffer: PSingle;           //< Buffer where to copy output samples.
                                     maxSamples: Integer      //< How many samples to receive at max.
                                    ): Cardinal; cdecl;

  // Returns number of samples currently available.
  TSoundTouchNumSamples = function (Handle: TSoundTouchHandle): Cardinal; cdecl;

  // Returns nonzero if there aren't any samples available for outputting.
  TSoundTouchIsEmpty = function (Handle: TSoundTouchHandle): Integer; cdecl;

var
  SoundTouchCreateInstance        : TSoundTouchCreateInstance;
  SoundTouchDestroyInstance       : TSoundTouchDestroyInstance;
  SoundTouchGetVersionString      : TSoundTouchGetVersionString;
  SoundTouchGetVersionId          : TSoundTouchGetVersionId;
  SoundTouchSetRate               : TSoundTouchSetRate;
  SoundTouchSetTempo              : TSoundTouchSetTempo;
  SoundTouchSetRateChange         : TSoundTouchSetRateChange;
  SoundTouchSetTempoChange        : TSoundTouchSetTempoChange;
  SoundTouchSetPitch              : TSoundTouchSetPitch;
  SoundTouchSetPitchOctaves       : TSoundTouchSetPitchOctaves;
  SoundTouchSetPitchSemiTones     : TSoundTouchSetPitchSemiTones;
  SoundTouchSetChannels           : TSoundTouchSetChannels;
  SoundTouchSetSampleRate         : TSoundTouchSetSampleRate;
  SoundTouchFlush                 : TSoundTouchFlush;
  SoundTouchPutSamples            : TSoundTouchPutSamples;
  SoundTouchClear                 : TSoundTouchClear;
  SoundTouchSetSetting            : TSoundTouchSetSetting;
  SoundTouchGetSetting            : TSoundTouchGetSetting;
  SoundTouchNumUnprocessedSamples : TSoundTouchNumUnprocessedSamples;
  SoundTouchReceiveSamples        : TSoundTouchReceiveSamples;
  SoundTouchNumSamples            : TSoundTouchNumSamples;
  SoundTouchIsEmpty               : TSoundTouchIsEmpty;

const
  /// Enable/disable anti-alias filter in pitch transposer (0 = disable)
  SETTING_USE_AA_FILTER =         0;

  /// Pitch transposer anti-alias filter length (8 .. 128 taps, default = 32)
  SETTING_AA_FILTER_LENGTH =      1;

  /// Enable/disable quick seeking algorithm in tempo changer routine
  /// (enabling quick seeking lowers CPU utilization but causes a minor sound
  ///  quality compromising)
  SETTING_USE_QUICKSEEK =         2;

  /// Time-stretch algorithm single processing sequence length in milliseconds. This determines
  /// to how long sequences the original sound is chopped in the time-stretch algorithm.
  /// See "STTypes.h" or README for more information.
  SETTING_SEQUENCE_MS =           3;

  /// Time-stretch algorithm seeking window length in milliseconds for algorithm that finds the
  /// best possible overlapping location. This determines from how wide window the algorithm
  /// may look for an optimal joining location when mixing the sound sequences back together.
  /// See "STTypes.h" or README for more information.
  SETTING_SEEKWINDOW_MS =         4;

  /// Time-stretch algorithm overlap length in milliseconds. When the chopped sound sequences
  /// are mixed back together, to form a continuous sound stream, this parameter defines over
  /// how long period the two consecutive sequences are let to overlap each other.
  /// See "STTypes.h" or README for more information.
  SETTING_OVERLAP_MS =            5;

type

  { TSoundTouch }

  TSoundTouch = class
  private
    FHandle     : TSoundTouchHandle;
    FRate       : Single;
    FPitch      : Single;
    FTempo      : Single;
    FSampleRate : Single;
    FChannels   : Cardinal;
    function GetNumSamples: Cardinal;
    function GetNumUnprocessedSamples: Cardinal;
    function GetIsEmpty: Integer;
    function GetPitchChange: Single;
    function GetRateChange: Single;
    function GetTempoChange: Single;
    procedure SetRate(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetTempo(const Value: Single);
    procedure SetPitchChange(const Value: Single);
    procedure SetRateChange(const Value: Single);
    procedure SetTempoChange(const Value: Single);
    procedure SetChannels(const Value: Cardinal);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure SamplerateChanged; virtual;
    procedure ChannelsChanged; virtual;
    procedure PitchChanged; virtual;
    procedure TempoChanged; virtual;
    procedure RateChanged; virtual;
  public
    class function GetVersionString: string;
    class function GetVersionId: Cardinal;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Flush; virtual;
    procedure Clear; virtual;

    procedure PutSamples(const Samples: PSingle; const NumSamples: Cardinal);
    function ReceiveSamples(const outBuffer: PSingle; const maxSamples: Integer): Cardinal;

    function SetSetting(const SettingId: Integer; const Value: Integer): Boolean;
    function GetSetting(const settingId: Integer): Integer;

    property Channels: Cardinal read FChannels write SetChannels;
    property Rate: Single read FRate write SetRate;
    property RateChange: Single read GetRateChange write SetRateChange;
    property Tempo: Single read FTempo write SetTempo;
    property TempoChange: Single read GetTempoChange write SetTempoChange;
    property Pitch: Single read FPitch write SetPitch;
    property PitchChange: Single read GetPitchChange write SetPitchChange;
    property SampleRate: Single read FSampleRate write SetSampleRate;

    property NumSamples: Cardinal read GetNumSamples;
    property NumUnprocessedSamples: Cardinal read GetNumUnprocessedSamples;
    property IsEmpty: Integer read GetIsEmpty;
  end;

implementation

uses
  SysUtils;

{ TSoundTouch }

constructor TSoundTouch.Create;
begin
  inherited;
  FHandle := SoundTouchCreateInstance();
  FRate := 1;
  FTempo := 1;
  FPitch := 1;
  FChannels := 1;
  FSampleRate := 44100;
  SamplerateChanged;
  ChannelsChanged;
end;

destructor TSoundTouch.Destroy;
begin
  SoundTouchDestroyInstance(FHandle);
  inherited;
end;

procedure TSoundTouch.Flush;
begin
  SoundTouchFlush(FHandle);
end;

procedure TSoundTouch.Clear;
begin
  SoundTouchClear(FHandle);
end;

function TSoundTouch.GetIsEmpty: Integer;
begin
  result := SoundTouchIsEmpty(FHandle);
end;

function TSoundTouch.GetNumSamples: Cardinal;
begin
  result := SoundTouchNumSamples(FHandle);
end;

function TSoundTouch.GetNumUnprocessedSamples: Cardinal;
begin
  result := SoundTouchNumUnprocessedSamples(FHandle);
end;

function TSoundTouch.GetPitchChange: Single;
begin
  result := 100 * (FPitch - 1.0);
end;

function TSoundTouch.GetRateChange: Single;
begin
  result := 100 * (FRate - 1.0);
end;

function TSoundTouch.GetTempoChange: Single;
begin
  result := 100 * (FTempo - 1.0);
end;

class function TSoundTouch.GetVersionId: Cardinal;
begin
  //result := SoundTouchGetVersionId;
end;

class function TSoundTouch.GetVersionString: string;
begin
  //result := StrPas(SoundTouchGetVersionString);
end;

procedure TSoundTouch.SetChannels(const Value: Cardinal);
begin
  if FChannels <> Value then
  begin
    FChannels := Value;
    ChannelsChanged;
  end;
end;

procedure TSoundTouch.ChannelsChanged;
begin
  //assert(FChannels in [1, 2]);
  SoundTouchSetChannels(FHandle, FChannels);
end;

procedure TSoundTouch.SetPitch(const Value: Single);
begin
  if FPitch <> Value then
  begin
    FPitch := Value;
    PitchChanged;
  end;
end;

procedure TSoundTouch.PitchChanged;
begin
  SoundTouchSetPitch(FHandle, FPitch);
end;

procedure TSoundTouch.putSamples(const Samples: PSingle;
  const NumSamples: Cardinal);
begin
  SoundTouchPutSamples(FHandle, Samples, NumSamples);
end;

procedure TSoundTouch.RateChanged;
begin
  SoundTouchSetRate(FHandle, FRate);
end;

function TSoundTouch.ReceiveSamples(const outBuffer: PSingle;
  const maxSamples: Integer): Cardinal;
begin
  result := SoundTouchReceiveSamples(FHandle, outBuffer, maxSamples);
end;

procedure TSoundTouch.SetPitchChange(const Value: Single);
begin
  Pitch := 1.0 + 0.01 * Value;
end;

procedure TSoundTouch.SetRate(const Value: Single);
begin
  if FRate <> Value then
  begin
    FRate := Value;
    RateChanged;
  end;
end;

procedure TSoundTouch.SetRateChange(const Value: Single);
begin
  Rate := 1.0 + 0.01 * Value;
end;

procedure TSoundTouch.SetSampleRate(const Value: Single);
begin
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    SamplerateChanged;
  end;
end;

procedure TSoundTouch.SamplerateChanged;
begin
  //assert(FSampleRate > 0);
  SoundTouchsetSampleRate(FHandle, round(FSampleRate));
end;

procedure TSoundTouch.SetTempo(const Value: Single);
begin
 if FTempo <> Value then
  begin
    FTempo := Value;
    TempoChanged;
  end;
end;

procedure TSoundTouch.SetTempoChange(const Value: Single);
begin
  Tempo := 1.0 + 0.01 * Value;
end;

function TSoundTouch.GetSetting(const SettingId: Integer): Integer;
begin
  result := SoundTouchGetSetting(FHandle, SettingId);
end;

function TSoundTouch.SetSetting(const SettingId: Integer;
  const Value: Integer): Boolean;
begin
  result := SoundTouchSetSetting(FHandle, SettingId, Value);
end;

procedure TSoundTouch.TempoChanged;
begin
  SoundTouchsetTempo(FHandle, FTempo);
end;

var
  SoundTouchLibHandle: TLibHandle;

procedure InitDLL;
begin
  SoundTouchLibHandle := LoadLibrary('/usr/local/lib/libsoundtouch4c.so');
  if SoundTouchLibHandle <> 0 then
  begin
    try
      SoundTouchCreateInstance         := TSoundTouchCreateInstance(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_construct')));
      SoundTouchDestroyInstance        := TSoundTouchDestroyInstance(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_destruct')));
      SoundTouchSetRate                := TSoundTouchSetRate(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setRate')));
      SoundTouchSetTempo               := TSoundTouchSetTempo(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setTempo')));
      SoundTouchSetRateChange          := TSoundTouchSetRateChange(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setRateChange')));
      SoundTouchSetTempoChange         := TSoundTouchSetTempoChange(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setTempoChange')));
      SoundTouchSetPitch               := TSoundTouchSetPitch(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setPitch')));
      SoundTouchSetPitchOctaves        := TSoundTouchSetPitchOctaves(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setPitchOctaves')));
      SoundTouchSetPitchSemiTones      := TSoundTouchSetPitchSemiTones(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setPitchSemiTones')));
      SoundTouchSetChannels            := TSoundTouchSetChannels(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setChannels')));
      SoundTouchSetSampleRate          := TSoundTouchSetSampleRate(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setSampleRate')));
      SoundTouchFlush                  := TSoundTouchFlush(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_flush')));
      SoundTouchPutSamples             := TSoundTouchPutSamples(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_putSamples')));
      SoundTouchClear                  := TSoundTouchClear(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_clear')));
      SoundTouchSetSetting             := TSoundTouchSetSetting(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_setSetting')));
      SoundTouchGetSetting             := TSoundTouchGetSetting(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_getSetting')));
      SoundTouchNumUnprocessedSamples  := TSoundTouchNumUnprocessedSamples(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_numUnprocessedSamples')));
      SoundTouchReceiveSamples         := TSoundTouchReceiveSamples(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_receiveSamples')));
      SoundTouchNumSamples             := TSoundTouchNumSamples(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_numSamples')));
      SoundTouchIsEmpty                := TSoundTouchIsEmpty(GetProcAddress(SoundTouchLibHandle, PChar('SoundTouch_isEmpty')));

    except
      FreeLibrary(SoundTouchLibHandle);
      SoundTouchLibHandle := 0;
    end;
  end
  else
  begin
    writeln('Library problem!')
  end;
end;

procedure FreeDLL;
begin
  if SoundTouchLibHandle <> 0 then FreeLibrary(SoundTouchLibHandle);
end;

initialization
  InitDLL;

finalization
  FreeDLL;

end.

