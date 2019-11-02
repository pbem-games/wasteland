{ Processors for each particular order }

unit uOrderExec;

interface

uses
  SysUtils, Classes, DataStructs, MyStrings, Math, Types, uResources;

type
  EParseError = class(Exception);

  procedure DoGive(AUnit: TUnit; s: string; var Line: integer);
  procedure DoTeam(AUnit: TUnit; s: string; var Line: integer);
  procedure DoEnter(AUnit: TUnit; s: string; var Line: integer);
  procedure DoLeave(AUnit: TUnit; s: string; var Line: integer);
  procedure DoMonth(AUnit: TUnit; s: string; var Line: integer);

  procedure ClearErrorComments(Lines: TStrings);


implementation

procedure SetAmountInc(AUnit: TUnit; AData: TItemData; AValue: integer);
var i: integer;
begin
  i := 0;
  while (i < AUnit.Items.Count) and (AUnit.Items[i].Data <> AData) do Inc(i);
  if i < AUnit.Items.Count then begin
    AValue := AUnit.Items[i].Amount + AValue;
    if AValue <= 0 then begin
      AUnit.Items[i].Free;
      AUnit.Items.Delete(i);
    end
    else AUnit.Items[i].Amount := AValue;
  end
  else if AValue > 0 then begin
    AUnit.Items.Add(TItem.Create);
    with AUnit.Items[AUnit.Items.Count-1] do begin
      Amount := AValue;
      Data := AData;
    end;
  end;
end;

function GetUnit(R: TRegion; var s: string; AllowZero: boolean;
  RaiseException: boolean): TUnit;
var num: integer;
begin
  Result := nil;
  // Get unit's number
  try
    num := StrToInt(GetToken(s));
  except
    on EConvertError do Exit;
  end;
  // Find unit in region (player's troop will be scanned first)
  Result := R.Units.Find(num);
  // Raise exception if unit invalid (faction 1 new 1 may be invisible)
  if RaiseException and (Result = nil)
    and not (AllowZero and (num = 0)) then
    raise EParseError.Create('Bad target');
end;

function GetStruct(R: TRegion; var s: string; RaiseException: boolean): TStruct;
var num: integer;
begin
  Result := nil;
  try
    num := StrToInt(GetToken(s));
  except
    on EConvertError do Exit;
  end;
  Result := R.Structs.Find(num);
  if RaiseException and (Result = nil) then
    raise EParseError.Create('Bad target');
end;

function TestItemName(AItemData: TItemData; s: string): boolean;
begin
  Result := (UpperCase(s) = AItemData.Short)
    or (s = AItemData.Name0)
    or (s = AItemData.Name1)
    or (s = AItemData.Name2);
end;

procedure DoGive(AUnit: TUnit; s: string; var Line: integer);
var i, amount: integer;
    t3, t4: string;
    Target: TUnit;
    not_enough: boolean;

  procedure GiveItem(AUnit, Target: TUnit; i, amount: integer);
  begin
    if Target <> nil then
      SetAmountInc(Target, AUnit.Items[i].Data, +amount);
    SetAmountInc(AUnit, AUnit.Items[i].Data, -amount);
  end;

begin
  not_enough := False;
  Target := GetUnit(AUnit.Region, s, True, True);

  try
    t3 := LowerCase(GetToken(s));
    if t3 = 'all' then amount := -1
    else amount := StrToInt(t3);
  except
    on EConvertError do raise EParseError.Create('Invalid value');
  end;

  // Give items
  t4 := AnsiLowerCase(GetToken(s));
  i := 0;
  while (i < AUnit.Items.Count) and not TestItemName(AUnit.Items[i].Data, t4) do
    Inc(i);
  if (i >= AUnit.Items.Count) then
    raise EParseError.Create('Bad item');
  if amount = -1 then
    amount := AUnit.Items[i].Amount;
  if amount > AUnit.Items[i].Amount then not_enough := True;
  GiveItem(AUnit, Target, i, Min(amount, AUnit.Items[i].Amount));
  if not_enough then raise EParseError.Create('Not enough');
end;


procedure ClearErrorComments(Lines: TStrings);
var i: integer;
begin
  i := 0;
  while i < Lines.Count do begin
    if Pos(';.', Lines[i]) > 0 then
      Lines[i] := Copy(Lines[i], 1, Pos(';.', Lines[i]) - 1);
    Inc(i);
  end;
end;

procedure DoTeam(AUnit: TUnit; s: string; var Line: integer);
var i: integer;
    kick: boolean;
    u: TUnit;
begin
  if (Trim(s) = '') then begin
    AUnit.Leader := nil;
    for i := 0 to AUnit.Region.Units.Count-1 do
      if (AUnit.Region.Units[i].Leader = AUnit) then
        AUnit.Region.Units[i].Leader := nil;
  end
  else begin
    kick := (Pos('kick', TrimLeft(s)) = 1);
    if (kick) then
      GetToken(s);
    u := GetUnit(AUnit.Region, s, false, true);
    if (kick) then begin
      if (u.Leader <> AUnit) then
        raise EParseError.Create('Not in team');
      u.Leader := nil;
    end
    else begin
      if (u.Leader <> nil) then
        u.Leader := nil;
      AUnit.Leader := u;
      for i := 0 to AUnit.Region.Units.Count-1 do
        if (AUnit.Region.Units[i].Leader = AUnit) then
          AUnit.Region.Units[i].Leader := u;
      for i := 0 to AUnit.Region.Units.Count-1 do
        if (AUnit.Region.Units[i].Leader = u) then
          AUnit.Region.Units[i].Struct := u.Struct;
    end;
  end;
end;

procedure DoEnter(AUnit: TUnit; s: string; var Line: integer);
var i: integer;
begin
  AUnit.Struct := GetStruct(AUnit.Region, s, true);
  for i := 0 to AUnit.Region.Units.Count-1 do
    if (AUnit.Region.Units[i].Leader = AUnit) then
      AUnit.Region.Units[i].Struct := AUnit.Struct;
end;

procedure DoLeave(AUnit: TUnit; s: string; var Line: integer);
var i: integer;
begin
  AUnit.Struct := nil;
  for i := 0 to AUnit.Region.Units.Count-1 do
    if (AUnit.Region.Units[i].Leader = AUnit) then
      AUnit.Region.Units[i].Struct := AUnit.Struct;
end;

procedure DoMonth(AUnit: TUnit; s: string; var Line: integer);
var i: integer;
begin
  AUnit.MonthOrder := AUnit.Orders[Line];
  for i := 0 to AUnit.Region.Units.Count-1 do
    if AUnit.Region.Units[i].Leader = AUnit then
      AUnit.Region.Units[i].MonthOrder := AUnit.Orders[Line];
end;

end.
