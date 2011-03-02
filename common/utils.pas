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

  utils.pas
}

unit utils;

{$mode Objfpc}{$H+}{$INLINE+}

interface

uses
  Classes, Sysutils, jacktypes, Graphics, Process, FileUtil, dbugintf, ringbuffer,
  contnrs, flqueue;

Type
  TTreeFolderData = class
  private
    FPath: string;
    FHasSubFolders: Boolean;
    FOpened: Boolean;
  public
    constructor Create(const Path: string);
    destructor Destroy; override;
    property Path: string read FPath write FPath;
    property HasSubFolders: Boolean read FHasSubFolders write FHasSubFolders;
    property Opened: Boolean read FOpened write FOpened;
  end;

  { TIntegerList }

  TIntegerList = class(TList)
  private
   procedure SetInteger(Index: Integer; Value: LongInt);
   function GetInteger(Index: Integer):LongInt;
  public
   property Items [index: Integer]: LongInt read GetInteger write SetInteger; default;
   procedure Add(Value: LongInt);
   function IndexOf(Value: LongInt): Integer;
   procedure Sort;
  end;

  { TBooleanStack }

  TBooleanStack = class(TObject)
  private
    FStackDepth: Integer;
    function GetValue: Boolean;
  public
    procedure SetFalse;
    procedure SetTrue;
    property Value: Boolean read GetValue;
  end;

  THybridLoggerMessage = class(TObject)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  { THybridLogger }

  THybridLogger = class(TObjectQueue)
  private
  public
    procedure PushMessage(AMessage: string);
    function PopMessage: string;
    function PeekMessage: string;
    procedure ProcessQueue;
  end;

  { TLogMessageThread }

  TLogMessageThread = class(TThread)
  private
    FMessageQueue: TObjectQueue;

    procedure Updater;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
    function PopMessage: string;
    procedure PushMessage(AMessage: string);
  end;



procedure memcpy(dest, source: Pointer; count: Integer);
function memmove(dest, src: Pointer; n: Cardinal): Pointer;
procedure memset(P: Pointer; B: Integer; count: Integer);

function SortHelper(Item1, Item2: Pointer): Integer;

procedure NormalizeInMemorySample(Sample: Pjack_default_audio_sample_t; NumSamples: Longint);
function StripLastDir(ADirectory: string): string;
function StringToPChar(PascalString : string): pchar;
function MMtoDB(Milimeter:Single):Single;
function DBtoMM(db:Single):Single;

function GetByte(Value : TColor; Shift : byte): byte;
procedure ColorToRGB(Color : TColor; var R, G, B : byte);
function RGBToColor(R, G, B : byte): TColor;
function TransparencyColor(BGColor, FRColor : TColor; TranspValue : byte): TColor;

function NoDivByZero(AValue: Single): Single;

procedure DBLog(AMessage: string);
procedure DBLog(AValue: integer);
procedure DBLog(AFormat: string; AMessage: string);
procedure DBLog(AFormat: string; AMessage: single);
procedure DBLog(AFormat: string; AMessage: integer);

function hermite4(frac_pos, xm1, x0, x1, x2: single): single; inline;

var
  FLogging: Boolean;
  GLogger: TLogMessageThread;

implementation

var
  FPushPullBoolean: Integer;

{ THybridLogger }

procedure THybridLogger.PushMessage(AMessage: ansistring);
var
  lMessage: THybridLoggerMessage;
begin
  lMessage := THybridLoggerMessage.Create;
  try
    lMessage.Text := AMessage;

    Push(lMessage);
  except
    lMessage.Free;
  end;
end;

function THybridLogger.PopMessage: string;
var
  lMessage: THybridLoggerMessage;
begin
  // Fetch from buffer
  lMessage := THybridLoggerMessage(Pop);

  // Extract data
  Result := lMessage.Text;

  // Free item
  lMessage.Free;
end;

function THybridLogger.PeekMessage: string;
begin
  Result := THybridLoggerMessage(Peek).Text;
end;

procedure THybridLogger.ProcessQueue;
begin
  while Count > 0 do
  begin
    writeln(PopMessage);
  end;
end;

{ TTreeFolderData }

constructor TTreeFolderData.Create(const Path: string);
begin
  FPath := Path;

  FHasSubFolders := False;
  FOpened := False;
end;

destructor TTreeFolderData.Destroy;
begin
  inherited;
end;

procedure memcpy(dest, source: Pointer; count: Integer); inline;
begin
  Move(source^, dest^, count);
end;

function memmove(dest, src: Pointer; n: Cardinal): Pointer; inline;
begin
  Move(src^, dest^, n);
  Result := dest;
end;

procedure memset(P: Pointer; B: Integer; count: Integer); inline;
begin
  FillChar(P^, count, B);
end;

function SortHelper(Item1, Item2: Pointer): Integer;
begin
   if (LongInt(Item1)<LongInt(Item2)) then result:=-1
       else
           if (LongInt(Item1)>LongInt(Item2)) then result:=1
               else
                   result:=0;
end;

procedure TIntegerList.Sort;
begin
   inherited Sort(@SortHelper);
end;

function TIntegerList.IndexOf(Value: LongInt): Integer;
begin
   result:=inherited IndexOf(Pointer(Value));
end;

procedure TIntegerList.SetInteger(Index: Integer; Value: LongInt);
begin
   inherited Items[Index] := Pointer(Value);
end;

function TIntegerList.GetInteger(Index: Integer): LongInt;
begin
   Result:= LongInt(inherited Items[Index]);
end;

procedure TIntegerList.Add(Value: LongInt);
begin
   inherited Add(Pointer(Value));
end;

procedure Normalizeinmemorysample(
  Sample: Pjack_default_audio_sample_t; Numsamples: Longint);
var
  i: Longint;
  peak: single;
  abs_sample: single;
  normalizer: single;
begin
  peak := 0.0;
  for i := 0 to Numsamples - 1 do
  begin
    abs_sample:= Abs(Sample[i]);
    if abs_sample > peak then
      peak := abs_sample;
  end;
  if peak <> 0.0 then
  begin
    normalizer:= 1 / peak;
    for i := 0 to Numsamples - 1 do
    begin
      Sample[i] *= normalizer;
    end;
  end;
End;

function StripLastDir(ADirectory: string): string;
var
  i: Integer;
  lDirectory: string;
begin
  lDirectory:= ADirectory;
  for i := Length(lDirectory) - 1 downto 1 do
  begin
    if lDirectory[i] = '/' then
    begin
      SetLength(lDirectory, Length(ADirectory) - i);
      Result:= lDirectory;
      break;
    end;
  end;
end;

function StringToPChar(PascalString : string): pchar;
var
  NewString : String;
  Convert_To_PChar : pchar;
begin
  NewString := PascalString + Chr(0) ;
  Convert_To_PChar := @NewString[1] ;
  StringToPChar := StrNew(Convert_To_PChar) ;
end;

function MMtoDB(Milimeter:Single):Single; inline;
var mm: Single;
begin
  mm:=100-Milimeter;
  if mm = 0 then Result:=10
  else if mm < 48 then Result:=10-5/12*mm
  else if mm < 84 then Result:=-10-10/12*(mm-48)
  else if mm < 96 then Result:=-40-20./12*(mm-84)
  else if mm < 100 then Result:=-60-35*(mm-96)
  else Result:=-200.;
end;

function DBtoMM(db:Single):Single; inline;
begin
  if db>=10 then result:=0
  else if db>-10 then result:=-12/5*(db-10)
  else if db>-40 then result:=48-12/10*(db+10)
  else if db>-60 then result:=84-12/20*(db+40)
  else if db>-200 then result:=96-1/35*(db+60)
  else result:=100.;
  Result:=100-Result;
end;

//Hilfsfunktion zum Auslesen eines Bytes aus TColor
//Value gibt die Farbe an
//Shift gibt die Anzahl der zu schiebenden Bits an
//Bsp: um aus Value=$00120000 das Rot-Byte auszulesen, muss Shift den Wert 16 haben
//     ($12 wird dann zurückgegeben
function GetByte(Value : TColor; Shift : byte): byte;
begin
  Result := (Value and ($FF shl Shift)) shr Shift;
  //Byte an entsprechender Stelle maskieren und dann nach Rechts verschieben
end;

//Hilfsprozedur zum Auslesen des Rot-,Grün- und Blauwertes aus TColor
//nutzt GetByte
procedure ColorToRGB(Color : TColor; var R, G, B : byte);
begin
  R := GetByte(Color, 16); //zweites Byte aus Color (v.R.)
  G := GetByte(Color, 8);  //drittes Byte aus Color (v.R.)
  B := GetByte(Color, 0);  //viertes Byte aus Color (v.R.)
end;

//Hilfsfunckion zum Erstellen eines TColor aus Rot-,Grün- und Blauwerten
function RGBToColor(R, G, B : byte): TColor;
begin
  Result := ((R and $FF) shl 16) +
    ((G and $FF) shl 8) + (B and $FF);
end;

//Eigentliche Transparenzfunktion, ermittelt die Transparenzfarbe des transparenten
//Vordergrunds, wenn Hintergrundfarbe=BGColor und Vordergrundfarbe=FRColor ist
//TranspValue gibt den Ganzzahligen Prozentsatz des Transparenzwertes an
function TransparencyColor(BGColor, FRColor : TColor; TranspValue : byte): TColor;
var
  BGR, BGG, BGB, FRR, FRG, FRB, ergR, ergG, ergB : byte;
  TrFact : real;
begin
  //Transparenzfaktor errechnen
  TrFact := TranspValue / 100;

  //Hinter- und Vordergrundfarbe in Rot-,Grün- und Blauwerte splitten
  ColorToRGB(BGColor, BGR, BGG, BGB);
  ColorToRGB(FRColor, FRR, FRG, FRB);

  //Ergebnisfarbwerte errechnen
  ergR := byte(Trunc(BGR * TrFact + FRR * (1 - TrFact)));
  ergG := byte(Trunc(BGG * TrFact + FRG * (1 - TrFact)));
  ergB := byte(Trunc(BGB * TrFact + FRB * (1 - TrFact)));

  //Rot-,Grün- und Blauwert zu TColor und zurückgeben
  Result := RGBToColor(ErgR, ergG, ergB);
end;

function NoDivByZero(AValue: Single): Single; inline;
begin
  if AValue = 0 then
    Result:= 0.000000001
  else
    Result:= AValue;
end;

procedure DBLog(AMessage: string); inline;
begin
  if FLogging then
  begin
    // Send final output to DebugServer
    //SendDebug(AMessage);
    //writeln(AMessage);
    //FLogger.RingbufferWrite(shortstring(AMessage), 256);
    GLogger.PushMessage(AMessage);
  end;
end;

procedure DBLog(AValue: integer); inline;
begin
  if FLogging then
    DBLog(IntToStr(AValue));
end;

procedure DBLog(AFormat: string; AMessage: string);
begin
  if FLogging then
    DBLog(Format(AFormat, [AMessage]));
end;

procedure DBLog(AFormat: string; AMessage: single); inline;
begin
  if FLogging then
    DBLog(Format(AFormat, [AMessage]));
end;

procedure DBLog(AFormat: string; AMessage: integer); inline;
begin
  if FLogging then
    DBLog(Format(AFormat, [AMessage]));
end;

function CreateObjectID: string;
var
  lGUID: TGUID;

begin
  CreateGUID(lGUID);
  Result := GUIDToString(lGUID);
end;

{ TBooleanStack }

function TBooleanStack.GetValue: Boolean;
begin
  Result := (FStackDepth = 0);
end;

procedure TBooleanStack.SetFalse;
begin
  inc(FStackDepth);
end;

procedure TBooleanStack.SetTrue;
begin
  dec(FStackDepth);
end;


// laurent de soras
// Hermite Interpolation
function hermite4(frac_pos, xm1, x0, x1, x2: single): single; inline;
var
  c, v, w, a, b_neg: single;
begin
  c := (x1 - xm1) * 0.5;
  v := x0 - x1;
  w := c + v;
  a := w + v + (x2 - x0) * 0.5;
  b_neg := w + a;

  Result := ((((a * frac_pos) - b_neg) * frac_pos + c) * frac_pos + x0);
end;

{ TLogMessageThread }

procedure TLogMessageThread.Updater;
begin
  while FMessageQueue.Count > 0 do
  begin
    writeln(PopMessage);
  end;
end;

procedure TLogMessageThread.Execute;
begin
  while (not Terminated) do
  begin
    Synchronize(@Updater);

    // Only update at 100 ms
    Sleep(100);
  end;
end;

constructor TLogMessageThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  FMessageQueue := TObjectQueue.Create;
end;

destructor TLogMessageThread.Destroy;
begin
  FMessageQueue.Free;

  inherited Destroy;
end;

procedure TLogMessageThread.PushMessage(AMessage: string);
var
  lMessage: THybridLoggerMessage;
begin
  lMessage := THybridLoggerMessage.Create;
  try
    lMessage.Text := AMessage;

    FMessageQueue.Push(lMessage);
  except
    lMessage.Free;
  end;
end;

function TLogMessageThread.PopMessage: string;
var
  lMessage: THybridLoggerMessage;
begin
  lMessage := THybridLoggerMessage( FMessageQueue.Pop );

  if Assigned(lMessage) then
  begin
    Result := lMessage.Text;

    lMessage.Free;
  end;
end;

initialization
  GLogger := TLogMessageThread.Create(False);

finalization
  GLogger.Free;

end.

