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

  global_scriptactions.pas
}

unit global_scriptactions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList;

// Define scriptaction base class here, subclassed from TAction
type

  { THybridAction }

  THybridAction = class(TCustomAction)
  private
    FScript: TStringList;
    FGUID: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Script: TStringList read FScript write FScript;
    property GUID: string read FGUID write FGUID;
  end;

implementation

{ THybridAction }

constructor THybridAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScript := TStringList.Create;
end;

destructor THybridAction.Destroy;
begin
  if Assigned(FScript) then
    FScript.Free;

  inherited Destroy;
end;

end.

