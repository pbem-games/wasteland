unit uGameSubs;

interface

uses
  SysUtils, Classes, Math, DataStructs;

  function FindMan(u: TUnit): TItemData;
  function CalcWeight(u: TUnit): integer;
  function CalcCapacity(u: TUnit): integer;
  function CalcRations(u: TUnit): integer;
  function CalcRadiationDanger(u: TUnit): integer;
  function CalcTemperatureDanger(u: TUnit): integer;
  function IsInside(u: TUnit; s: TStruct): boolean;
  function CalcStructWeight(s: TStruct; r: TRegion): integer;

implementation

function CalcWeight(u: TUnit): integer;
var i, wt: integer;
begin
  Result := -1;
  wt := 0;
  for i := 0 to u.Items.Count-1 do begin
    if not(u.Items[i].Data.Full) then Exit;
    if (u.Items[i].Data.CapacityWalk = 0) then
      wt := wt + u.Items[i].Amount * u.Items[i].Data.Weight;
  end;
  Result := wt;
end;

function CalcStructWeight(s: TStruct; r: TRegion): integer;
var i, j, wt: integer;
    u: TUnit;
begin
  Result := -1;
  wt := 0;
  for j := 0 to r.Units.Count-1 do begin
    u := r.Units[j];
    if (u.Struct = s) then begin
      for i := 0 to u.Items.Count-1 do begin
        if not(u.Items[i].Data.Full) then Exit;
        wt := wt + u.Items[i].Amount * u.Items[i].Data.Weight;
      end;
    end;
  end;
  Result := wt;
end;

function CalcCapacity(u: TUnit): integer;
var i, wt: integer;
begin
  Result := -1;
  wt := 0;
  for i := 0 to u.Items.Count-1 do begin
    if not(u.Items[i].Data.Full) then Exit;
    wt := wt + u.Items[i].Amount * u.Items[i].Data.CapacityWalk;
  end;
  Result := wt;
end;

function CalcRations(u: TUnit): integer;
var i, wt: integer;
begin
  Result := -1;
  wt := 0;
  for i := 0 to u.Items.Count-1 do begin
    if not(u.Items[i].Data.Full) then Exit;
    wt := wt + u.Items[i].Amount * u.Items[i].Data.Rations;
  end;
  Result := wt;
end;

function FindMan(u: TUnit): TItemData;
var i: integer;
begin
  i := 0;
  while (i < u.Items.Count) and not(u.Items[i].Data.IsMan) do Inc(i);
  if (i = u.Items.Count) then Result := nil
  else Result := u.Items[i].Data;
end;

function CalcRadiationDanger(u: TUnit): integer;
var i, wt, mx: integer;
    man, idata: TItemData;
begin
  Result := -1;
  man := FindMan(u);
  if (man = nil) or not(man.Full) then Exit;

  wt := u.Region.Radiation;
  mx := 0;
  for i := 0 to u.Items.Count-1 do begin
    idata := u.Items[i].Data;
    if not(idata.Full) then Exit;
    if (idata.OwnerRadiation < mx) then
      mx := idata.OwnerRadiation
    else if (idata.OwnerRadiation > 0) then
      wt := wt + u.Items[i].Amount * idata.OwnerRadiation;
  end;
  wt := wt + mx;
  if (u.Struct <> nil) and IsInside(u, u.Struct) then begin
    if not(u.Struct.Data.Full) then Exit;
    wt := wt + u.Struct.Data.RadiationInside;
  end;
  if (wt < 0) then
    wt := 0;

  if (wt > man.RadiationTo) then
    Result := wt - man.RadiationTo
  else if (wt < man.RadiationFrom) then
    Result := man.RadiationFrom - wt
  else
    Result := 0;
end;

function CalcTemperatureDanger(u: TUnit): integer;
var i, wt, mx: integer;
    man, idata: TItemData;
begin
  Result := -1;
  man := FindMan(u);
  if (man = nil) or not(man.Full) then Exit;

  wt := u.Region.Temperature;
  mx := 0;
  for i := 0 to u.Items.Count-1 do begin
    idata := u.Items[i].Data;
    if not(idata.Full) then Exit;
    if (idata.OwnerTemperature > mx) then
      mx := idata.OwnerTemperature
    else if (idata.OwnerTemperature < 0) then
      wt := wt + u.Items[i].Amount * idata.OwnerTemperature;
  end;
  wt := wt + mx;
  if (u.Struct <> nil) and IsInside(u, u.Struct) then begin
    if not(u.Struct.Data.Full) then Exit;
    wt := wt + u.Struct.Data.TemperatureInside;
  end;
  if (wt > 0) then
    wt := 0;

  if (wt < 0) then
    Result := 0 - wt
  else
    Result := 0;
end;

function IsInside(u: TUnit; s: TStruct): boolean;
var i, d: integer;
begin
  if (s.Needs.Count > 0) then
    Result := false
  else begin
    i := 0;
    d := s.Data.Defence;
    while (i < u.Region.Units.Count) and (u.Region.Units[i] <> u) and (d > 0) do begin
      if (u.Region.Units[i].Struct = s) then
        Dec(d);
      Inc(i);
    end;
    Result := (d > 0);
  end;
end;

end.
