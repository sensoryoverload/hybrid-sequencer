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

  samplestreamprovider.pas
}

unit samplestreamprovider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, contnrs, global, utils;

type
  TSampleCache = class(TObject)
  private
    FCount: Integer;
    FWave: TWaveFile;
  public
    property Count: Integer read FCount write FCount;
    property Wave: TWaveFile read FWave write FWave;
  end;

  { TSampleStreamProvider }

  TSampleStreamProvider = class(TObject)
  private
    FCachedSampleList: TStringList;
  public
    {
      Load sample or get reference from cache
    }
    function LoadSample(AFilename: string): TWaveFile;
    procedure UnloadSample(AFilename: string);
    constructor Create;
    destructor Destroy; override;
  end;

var
  GSampleStreamProvider: TSampleStreamProvider;

implementation

{ TSampleStreamProvider }

function TSampleStreamProvider.LoadSample(AFilename: string): TWaveFile;
var
  lSampleCacheIndex: Integer;
  lSampleCache: TSampleCache;
  lWave: TWaveFile;
begin
  DBLog('start TSampleStreamProvider.LoadSample');
  if FCachedSampleList.Find(AFilename, lSampleCacheIndex) then
  begin
    // Increase reference counter
    TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Count :=
      TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Count + 1;
    Result := TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Wave;

    DBLog(Format('Loaded cached sample: %s', [Result.FileName]));
  end
  else
  begin
    lSampleCache := TSampleCache.Create;
    lWave := TWaveFile.Create('', NOT_MAPPED);
    try
      lWave.LoadSample(AFilename);
      lSampleCache.Wave := lWave;
      lSampleCache.Count := 1;
      FCachedSampleList.AddObject(AFilename, lSampleCache);

      Result := lWave;

      DBLog(Format('FCachedSampleList %d, ChannelList.Count %d', [FCachedSampleList.Count, lSampleCache.Wave.ChannelCount]));
    except
      on e: exception do
      begin
        lWave.Free;
        lSampleCache.Free;

        DBLog(e.Message);
      end;
    end;
  end;
  DBLog('end TSampleStreamProvider.LoadSample');
end;

procedure TSampleStreamProvider.UnloadSample(AFilename: string);
var
  lSampleCacheIndex: Integer;
begin
  if FCachedSampleList.Find(AFilename, lSampleCacheIndex) then
  begin
    // Decrease reference counter
    if TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Count > 0 then
    begin
      TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Count :=
        TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Count - 1;
    end
    else
    begin
      // Remove from memory as it's not in use anymore
      TSampleCache(FCachedSampleList.Objects[lSampleCacheIndex]).Free;
    end;
  end
  else
  begin
    // Nothing to be done, sample already gone...
  end;
end;

constructor TSampleStreamProvider.Create;
begin
  FCachedSampleList := TStringList.Create;
  FCachedSampleList.Sorted := True;
end;

destructor TSampleStreamProvider.Destroy;
begin
  FCachedSampleList.Free;

  inherited Destroy;
end;

initialization
  GSampleStreamProvider := TSampleStreamProvider.Create;

finalization
  GSampleStreamProvider.Free;

end.

