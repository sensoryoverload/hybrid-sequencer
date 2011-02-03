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

  global_command.pas
}

unit global_command;

{
  This unit implements the base classes for unlimitied persistent undo/redo
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, global, globalconst, ExtCtrls, utils;

type
  { TCommand }

  TCommand = class(TObject)
  private
    FObjectIdList: TStringList;
    FMemento: TObjectList;
    FObjectOwner: string;
    FObjectID: string;
    FPersist: Boolean;
  protected
    procedure DoExecute; virtual; abstract;
    procedure DoRollback; virtual;
    procedure Initialize; virtual;
  public
    constructor Create(AObjectID: string);
    destructor Destroy; override;

    procedure Execute;
    procedure Rollback; // Reverse effect of Execute

    property ObjectIdList : TStringList read FObjectIdList write FObjectIdList;
    property Memento: TObjectList read FMemento write FMemento;
    property ObjectOwner: string read FObjectOwner write FObjectOwner;
    property ObjectID: string read FObjectID write FObjectID;
    property Persist: Boolean read FPersist write FPersist default True;
  end;

  { TCommandQueue }

  TCommandQueue = class(TObjectQueue)
  private
    FCommandQueue: TObjectQueue;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExecuteCommandQueue;
    procedure PushCommand(AObject: TObject);
    function PopCommand: TObject;
    property CommandQueue: TObjectQueue read FCommandQueue write FCommandQueue;
  end;

  { TMidiDataList }

  TMidiDataList = class(TList)
  private
    FLastIndex: Integer;
    FIndex: Integer;
  public
    destructor Destroy; override;

    function FrameFirstIndex(ALocation: Integer): Integer;
    function FrameLastIndex(ALocation: Integer): Integer;
    function CurrentMidiData: TMidiData;
    procedure Next;
    procedure First;
    function Eof: Boolean;

    procedure IndexList;

    property LastIndex: Integer read FLastIndex write FLastIndex;
    property Index: Integer read FIndex write FIndex;
  protected
  end;

var
  GCommandQueue: TCommandQueue;
  GHistoryQueue: TObjectList;
  GHistoryIndex: Integer;

implementation

uses
  patterngui, trackgui, pattern;

{ TCommand }

procedure TCommand.DoRollback;
begin
  //
end;

procedure TCommand.Initialize;
begin
  // Virtual base method placeholder
end;

constructor TCommand.Create(AObjectID: string);
begin
  FObjectOwner := AObjectID;
  FObjectIdList := TStringList.Create;
  FMemento := TObjectList.Create(True);
  FPersist := True;
end;

destructor TCommand.Destroy;
begin
  FObjectIdList.Free;
  FMemento.Free;

  inherited Destroy;
end;

procedure TCommand.Execute;
begin
  DoExecute;
end;

procedure TCommand.Rollback;
begin
  DoRollback;
end;

{ TCommandQueue }

constructor TCommandQueue.Create;
begin

  FCommandQueue := TObjectQueue.Create;
end;

destructor TCommandQueue.Destroy;
begin
  FCommandQueue.Free;

  inherited Destroy;
end;

procedure TCommandQueue.ExecuteCommandQueue;
var
  lCommand: TCommand;
begin
  while FCommandQueue.Count > 0 do
  begin
    try
      lCommand:= TCommand(FCommandQueue.Pop);
      lCommand.Initialize;
      lCommand.Execute;

      if lCommand.Persist then
      begin
        GHistoryQueue.Add(lCommand);
        Inc(GHistoryIndex);
      end
      else
      begin
        lCommand.Free;
      end;
    except
      on e: exception do
      begin
        //raise;// Exception.CreateFmt('Failed command execute: %s ', [e.message]);
        DBLog(Format('Failed command execute: %s, command class %s', [e.message, lCommand.ClassName]));
        lCommand.Free;
      end;
    end;
  end;
end;

procedure TCommandQueue.PushCommand(AObject: TObject);
begin
  if Assigned(CommandQueue) then
  begin
    CommandQueue.Push(AObject);

    // just testing TODO
    ExecuteCommandQueue;
  end;
end;

function TCommandQueue.PopCommand: TObject;
begin
  if Assigned(CommandQueue) then
  begin
    Result := CommandQueue.Pop;
  end;
end;


{ TMidiDataList }

function SortOnLocation(Item1, Item2: Pointer): Integer;
begin
  if (TMidiData(Item1).Location < TMidiData(Item2).Location) then
  begin
    result := -1
  end
  else
  begin
    if (TMidiData(Item1).Location > TMidiData(Item2).Location) then
      result := 1
    else
      result := 0;
  end;
end;

destructor TMidiDataList.Destroy;
begin

  inherited Destroy;
end;

{
  Return index of first element within window
}
function TMidiDataList.FrameFirstIndex(ALocation: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Pred(Count) do
  begin
    if TMidiData(Items[i]).Location >= ALocation then
    begin
      Result := i;
      break;
    end;
  end;
end;

{
  Return index of last element within window
}
function TMidiDataList.FrameLastIndex(ALocation: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := Pred(Count) downto 0 do
  begin
    if TMidiData(Items[i]).Location < ALocation then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TMidiDataList.CurrentMidiData: TMidiData;
begin
  if FIndex < Count then
  begin
    Result := TMidiData(Items[FIndex]);
  end
  else
    Result := TMidiData(self.Last)
end;

procedure TMidiDataList.Next;
begin
//  if FIndex < Pred(Count) then
  begin
    FIndex := FIndex + 1;
  end;
end;

function TMidiDataList.Eof: Boolean;
begin
  Result := (FIndex >= Count);
end;

{
  This method sorts the list on location starting low ending high
  After that it will also be linked into a linked list
}
procedure TMidiDataList.IndexList;
var
  i: Integer;
begin
  Sort(@SortOnLocation);

  for i := 0 to Count - 2 do
  begin
    TMidiData(Items[i]).Next := TMidiData(Items[i + 1]);
  end;

  // Initialize last one with nil
  if Count > 0 then
  begin
    TMidiData(Items[Count - 1]).Next := nil;
  end;
end;

procedure TMidiDataList.First;
begin
  FIndex := 0;
end;

initialization
  GCommandQueue:= TCommandQueue.Create;

  GHistoryQueue:= TObjectList.Create;
  GHistoryIndex:= -1;

finalization
  GHistoryQueue.Free;
  GCommandQueue.Free;


end.

