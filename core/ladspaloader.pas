unit ladspaloader; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ladspa;

type
  TLadspaPlugin = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate
  end;

implementation

end.

