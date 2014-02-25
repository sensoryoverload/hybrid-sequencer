unit uTestSoundTouch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, soundtouch,
  LCLType, ExtCtrls, Spin, StdCtrls, sndfile;

type
  { TSimpleWaveForm }

  TSimpleWaveForm = class(TCustomControl)
  private
    FBufferSize: Integer;
    FData: PSingle;
    FDataSize: Integer;
    FChannelCount: Integer;
    FFrames: Integer;
    FOffset: Integer;
    FZoom: single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure Clear;
    property Data: PSingle read FData write FData;
    property Zoom: single read FZoom write FZoom;
    property DataSize: Integer read FDataSize write FDataSize;
    property Frames: Integer read FFrames write FFrames;
    property ChannelCount: Integer read FChannelCount write FChannelCount;
    property Offset: Integer read FOffset write FOffset;
    property BufferSize: Integer write FBufferSize;
  protected
  published
  end;

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    fspnPitch: TFloatSpinEdit;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure fspnPitchChange(Sender: TObject);
  private
    { private declarations }
    FST: TSoundTouch;
    FInputLoop: TSimpleWaveForm;
    FInput: PSingle;
    FInputSize: Integer;
    FOutputLoop: TSimpleWaveForm;
    FOutput: PSingle;
    FOutputSize: Integer;
    FFrames: Integer;
    FSampleRate: Integer;
    FChannelCount: Integer;
    FReadCount: Integer;
    function LoadSample(AFileName: PChar; AWaveform: TSimpleWaveForm): Boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FST := TSoundTouch.Create;

  FInputLoop := TSimpleWaveForm.Create(Self);
  FInputLoop.Parent := Self;
  FInputLoop.Align := alTop;
  FInputLoop.Height := 150;
  FOutputLoop := TSimpleWaveForm.Create(Self);
  FOutputLoop.Parent := Self;
  FOutputLoop.Align := alTop;
  FOutputLoop.Height := 150;

  FInputLoop.Data := FInput;

  LoadSample('nogood.wav', FInputLoop);

  GetMem(FOutputLoop.FData, FFrames * SizeOf(Single) * FChannelCount * 2);
  FOutputLoop.DataSize := FFrames * SizeOf(Single) * FChannelCount * 2;
  FOutputLoop.ChannelCount := FChannelCount;
  FOutputLoop.Frames := FFrames;

  FST.setChannels(FChannelCount);
  FST.setSampleRate(44100);
  FST.setSetting(SETTING_OVERLAP_MS, 8);
  FST.setSetting(SETTING_SEEKWINDOW_MS, 20);
  FST.setSetting(SETTING_SEQUENCE_MS, 80);
  FST.setSetting(SETTING_USE_QUICKSEEK, 0);
  FST.setSetting(SETTING_USE_AA_FILTER, 0);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  fspnPitchChange(self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FST.Free;
end;

procedure TForm1.fspnPitchChange(Sender: TObject);
var
  i: Integer;
  lInputOffset, lOutputOffset: Integer;
  lPart: Integer;
begin
  FOutputLoop.Clear;
  if CheckBox1.Checked then
  begin
    FST.setSetting(SETTING_USE_QUICKSEEK, 1);
  end
  else
  begin
    FST.setSetting(SETTING_USE_QUICKSEEK, 0);
  end;
  FST.setPitch(fspnPitch.Value);
  lOutputOffset := 0;
  lInputOffset := 0;
  {FST.flush;}

  FOutputLoop.Zoom:=0.01;
  lPart:= fframes div 16;
  for i := 0 to lPart div 1024 do
  begin
    lInputOffset := i * 1024;
    FST.putSamples(@FInputLoop.Data[lInputOffset], 1024);
    lOutputOffset := lOutputOffset + FST.receiveSamples(@FOutputLoop.Data[lOutputOffset], 1024);
  end;
  FST.Clear;
{  while FST.numSamples > 0 do
  begin
    lOutputOffset := lOutputOffset + FST.receiveSamples(@FOutputLoop.Data[lOutputOffset], 1024);
  end;   }




{  FOutputLoop.Zoom:=0.1;
  FOutputLoop.DataSize:=8 * 1024;
  FST.putSamples(FInputLoop.Data, 8 * 1024);
  FST.receiveSamples(FOutputLoop.Data, 8 * 1024);
  FST.Clear;}

  FOutputLoop.Invalidate;
end;

{$R *.lfm}

{ TSimpleWaveForm }

constructor Tsimplewaveform.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  FOffset := 0;
  FZoom := 4;
end;

destructor Tsimplewaveform.Destroy;
begin
  inherited Destroy;
end;

procedure Tsimplewaveform.Erasebackground(Dc: Hdc);
begin
  //inherited Erasebackground(Dc);
end;

procedure Tsimplewaveform.Paint;
var
  bmp: TBitmap;
  screenloop: integer;
  zeroline: integer;
  scale: single;
begin
  bmp := TBitmap.Create;
  try
    bmp.Height := Height;
    bmp.Width := Width;
    zeroline := Height div 2;

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(0, 0, Width, Height);

    if (FData <> nil) and (FDataSize > 0) and (FChannelCount in [1,2]) then
    begin
      scale := FFrames / bmp.width * FZoom;

      bmp.Canvas.Pen.Color := clBlack;
      bmp.Canvas.Line(0, zeroline, Width, zeroline);

      bmp.Canvas.Pen.Color := clBlue;
      bmp.Canvas.MoveTo(0, zeroline);

      for ScreenLoop := 0 to Pred(bmp.Width) do
      begin
        bmp.Canvas.Pen.Color := clBlue;
        bmp.Canvas.LineTo(ScreenLoop,
        Round(FData[Round(ScreenLoop * FChannelCount * scale)] * zeroline) + zeroline);
      end;
      bmp.Canvas.Pen.Color := clRed;
      bmp.Canvas.Line(Round(FOffset * FChannelCount * scale), 0,
      Round(FOffset * FChannelCount * scale), Height);

    end;
    bmp.Canvas.Pen.Color := clRed;
    bmp.Canvas.TextOut(5, 5, Format('Offset %d, BufferSize %d', [FOffset, FBufferSize]));
    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  inherited Paint;
end;

procedure TSimpleWaveForm.Clear;
begin
  //FillByte(FData, DataSize, 0);
end;

function TForm1.LoadSample(AFileName: PChar; AWaveform: TSimpleWaveForm): Boolean;
var
  lChannelIndex: Integer;
  lChannelSize: Integer;
  lChannelItems: Integer;
  lBufferIndex: Integer;
  lBuffer: PSingle;
  lSampleHandle: PSndFile;
  lSampleInfo: SF_INFO;
begin
  Result := False;

  lSampleHandle := sf_open(AFilename, SFM_READ, lSampleInfo);
  try
    if not Assigned(lSampleHandle) then
    begin
      ShowMessage(sf_strerror(lSampleHandle));
      Result := False;
    end
    else
    begin
      AWaveform.DataSize := lSampleInfo.frames * lSampleInfo.channels * SizeOf(Single);
      FFrames := lSampleInfo.frames;
      FSampleRate := lSampleInfo.samplerate;
      FChannelCount := lSampleInfo.channels;

      AWaveform.ChannelCount:=lSampleInfo.channels;
      AWaveform.Frames:=lSampleInfo.frames;

      if Assigned(AWaveform.Data) then
      begin
        Freemem(AWaveform.Data);
      end;

      AWaveform.Data := GetMem(AWaveform.DataSize * 4 + 1000);
      try
        FReadCount := sf_read_float(lSampleHandle, AWaveform.Data, AWaveform.DataSize);

      except
        on e: exception do
        begin
          Result := False;

          raise;
        end;
      end;
    end;
  finally
    sf_close(lSampleHandle);
  end;
end;

end.

