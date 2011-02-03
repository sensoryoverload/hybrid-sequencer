unit GUIFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, global, globalconst, pattern, patterngui, track, trackgui,
  ExtCtrls, Controls, ContNrs, global_command, syncobjs;

type
  { TGuiUpdateHandler }


var
  GUpdateHandler: TGuiUpdateHandler;

implementation

{ TGuiUpdateHandler }

procedure TGuiUpdateHandler.HandleUpdate(AUpdateMessage: TUpdateMessage);
var
  lMapping: TMapping;
  lObjectOwnerID: string;
  lObjectOwner: TObject;
  lModelObject: TObject;
  lViewObject: TObject;

  lPatternGUI: TPatternGUI;
  lTrackGUI: TTrack;
  lTrackTotalWidth: Integer;
  lTrackIndex: Integer;
begin
  try
    lMapping := TMapping(GObjectMapper.GetMapping(AUpdateMessage.ObjectID));

    case AUpdateMessage.ActionType of
      atCreateBank:
      begin

      end;
      atDeleteBank:
      begin

      end;
      atCreateMarker:
      begin
        //
      end;
      atDeleteMarker:
      begin
        GObjectMapper.DeleteMapping(AUpdateMessage.ObjectID);
      end;
    end;

  except
    on e: exception do
    begin
      writeln('Internal error: ' + e.Message);
    end;
  end;
end;

initialization
  GUpdateHandler := TGuiUpdateHandler.Create('');

finalization
  GUpdateHandler.Free;

end.

