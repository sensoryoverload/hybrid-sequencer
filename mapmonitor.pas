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

  mapmonitor.pas
}

unit mapmonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, Global, syncobjs, globalconst, contnrs;

type

  { TfmMappingMonitor }

  TfmMappingMonitor = class(TForm)
    StringGrid1: TStringGrid;
  private
    { private declarations }
    FMaps: TObjectList;
    FCriticalSection: TCriticalSection;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateGrid;
    property Maps: TObjectList read FMaps write FMaps;
  end; 

var
  fmMappingMonitor: TfmMappingMonitor;

implementation

{ TfmMappingMonitor }


constructor TfmMappingMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCriticalSection := TCriticalSection.Create;

  Left := TForm(AOwner).Left + TForm(AOwner).Width + 5;
end;

destructor TfmMappingMonitor.Destroy;
begin
  if Assigned(FCriticalSection) then
    FCriticalSection.Free;

  inherited Destroy;
end;

procedure TfmMappingMonitor.UpdateGrid;
var
  lRow: Integer;
  lTempStr: string;
begin
  if Assigned(FMaps) then
  begin
    StringGrid1.RowCount := FMaps.Count;

    for lRow := 0 to Pred(FMaps.Count) do
    begin
      StringGrid1.Cells[1, lRow] := THybridPersistentModel(FMaps[lRow]).ObjectID;
      if Assigned(FMaps[lRow]) then
      begin
        if Assigned(THybridPersistentModel(FMaps[lRow])) then
        begin
          lTempStr := THybridPersistentModel(FMaps[lRow]).ClassName;
          StringGrid1.Cells[2, lRow] := lTempStr;
        end;
      end;
      StringGrid1.ColWidths[1] := 350;
      StringGrid1.ColWidths[2] := 200;
    end;
  end;
end;

initialization
  {$I mapmonitor.lrs}

end.

