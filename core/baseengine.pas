unit baseengine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TBaseEngine - A really simple base class }

  TBaseEngine = class
  private
    FSampleRate: single;
    FFrames: Integer;
    FModifier: PSingle;
    FModAmount: single;
    FInternalModifier: single;
  public
    constructor Create(AFrames: Integer); virtual;
    procedure Initialize; virtual;
    procedure ResetModifier;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property Frames: Integer read FFrames write FFrames;
    property Modifier: PSingle read FModifier write FModifier;
    property ModAmount: single read FModAmount write FModAmount;
  end;

implementation

uses global, utils;

{ TBaseEngine }

constructor TBaseEngine.Create(AFrames: Integer);
begin
  DBLog(Format('Creating class: %s', [Self.ClassName]));

  FFrames := AFrames;

{  // Descendants initalize samplerate at constructor call
  Initialize;}
end;

{
  Retrieve samplerate setting
}
procedure TBaseEngine.Initialize;
begin
  DBLog(Format('Initializing class: %s', [Self.ClassName]));

  FSamplerate := GSettings.SampleRate;
  FFrames := GSettings.Frames;

  // Sets default modvalue, 1
  FInternalModifier := 1;
  FModifier := @FInternalModifier;
end;

procedure TBaseEngine.ResetModifier;
begin
  FModifier := @FInternalModifier;
end;

end.

