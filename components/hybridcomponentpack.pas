{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit HybridComponentPack; 

interface

uses
  dialcontrol, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('dialcontrol', @dialcontrol.Register); 
end; 

initialization
  RegisterPackage('HybridComponentPack', @Register); 
end.
