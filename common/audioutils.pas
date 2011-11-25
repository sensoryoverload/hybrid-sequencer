unit audioutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

const
  DB_TABLE_SIZE = 1024;
  DB_MIN = -60.0;
  DB_MAX = 24.0;
  LIN_TABLE_SIZE = 1024;
  LIN_MIN = 0.0000000002;
  LIN_MAX = 9.0;

type

  { TParamSmooth }

  TParamSmooth = class
  private
    a, b, z: single;
  public
    constructor Create;
    function Process(AInput: single): single;
  end;

function lin2db(lin: single): single;
function db2lin(db: single): single;
function fast_log2(val:single):single;
function log_approx(val: single): single; inline;
function log_approx3(val: single): single; inline;
function log_approx4(val: single): single; inline;


implementation

var
  db_data: array[0..DB_TABLE_SIZE] of single;
  lin_data: array[0..LIN_TABLE_SIZE] of single;

procedure db_init;
var
  i: Integer;
begin
	for i := 0 to Pred(LIN_TABLE_SIZE) do
  begin
		lin_data[i] := power(10.0, ((DB_MAX - DB_MIN) * i / LIN_TABLE_SIZE + DB_MIN) / 20.0);
	end;

	for i := 0 to Pred(DB_TABLE_SIZE) do
  begin
		db_data[i] := 20.0 * log10((LIN_MAX - LIN_MIN) * i / DB_TABLE_SIZE + LIN_MIN);
	end;
end;


function db2lin(db: single): single;
var
  scale: single;
  base: integer;
  ofs: single;
begin
	scale := (db - DB_MIN) * LIN_TABLE_SIZE / (DB_MAX - DB_MIN);
	base := Round(scale - 0.5);
	ofs := scale - base;

	if (base < 1) then
		Result := 0.0
  else if (base > LIN_TABLE_SIZE - 3) then
		Result := lin_data[LIN_TABLE_SIZE - 2]
  else
  	Result := (1.0 - ofs) * lin_data[base] + ofs * lin_data[base+1];
end;


function lin2db(lin: single): single;
var
  scale: single;
  base: integer;
  ofs: single;
begin
	scale := (lin - LIN_MIN) * DB_TABLE_SIZE / (LIN_MAX - LIN_MIN);
	base := Round(scale - 0.5);
	ofs := scale - base;

	if (base < 2) then
		Result := db_data[2] * scale * 0.5 - 23.0 * (2.0 - scale)
	else if (base > DB_TABLE_SIZE - 2) then
		Result := db_data[DB_TABLE_SIZE - 1]
	else
  	Result := (1.0 - ofs) * db_data[base] + ofs * db_data[base+1];
end;

function fast_log2(val:single):single;
var
  log2,
  x:longint;
begin
  x := longint((@val)^);
  log2 := ((x shr 23) and 255) - 128;
  x := x and (not(255 shl 23));
  x := x + 127 shl 23;

  result := single((@x)^) + log2;
end;

{
  linear 0..1 to log 0..1 approx. adds denormal killer
}
function log_approx(val: single): single; inline;
var
  lNormVal: single;
begin
  //lNormVal := val + 1E-20;
  Result := val * val;
end;

{
  steep function
}
function log_approx3(val: single): single; inline;
var
  lNormVal: single;
begin
  //lNormVal := val + 1E-20;
  Result := val * val * val;
end;

{
  supersteep function
}
function log_approx4(val: single): single; inline;
var
  lNormVal: single;
begin
  //lNormVal := val + 1E-20;
  Result := val * val * val * val;
end;

{ TParamSmooth }

constructor TParamSmooth.Create;
begin
  inherited Create;

  a := 0.999; // Slow tracking
  b := 1.0 - a;
  z := 0;
end;

function TParamSmooth.Process(AInput: single): single;
begin
  z := (AInput * b) + (z * a);
  Result := z;
end;

initialization
  db_init;

end.

