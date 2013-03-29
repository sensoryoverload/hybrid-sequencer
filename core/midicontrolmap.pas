unit midicontrolmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, contnrs;

type
  TMidiControlMap = class(THybridPersistentModel)
  public
    Mapping: TObjectList;
  end;

implementation

end.

