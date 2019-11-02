unit uRepRead;

interface

uses
  DataStructs, Classes;

  function LoadReport(filename: string): boolean;
  function GetTurnNum(xml: string): integer;
  procedure ReadOrders(Lines: TStrings);

implementation

uses SysUtils, uXml, Forms, Controls;

function IsNumber(s: string): boolean;
var i: integer;
begin
  if Pos('-', s) = 1 then i := 2
  else i := 1;
  while (i <= Length(s)) and (s[i] in ['0'..'9']) do Inc(i);
  Result := (i > Length(s));
end;

function ToInt(s: string): integer;
begin
  if (s <> '') and IsNumber(s) then Result := StrToInt(s)
  else Result := 0;
end;

procedure ReadItemData(el: TXmlElement);
var it: TItemData;
begin
  it := Game.ItemData.Seek(el.GetAttribute('id'));
  it.Name0 := el.GetAttribute('name0');
  it.Name1 := el.GetAttribute('name1');
  it.Name2 := el.GetAttribute('name2');

  if (el.GetAttribute('full') = 'True') then begin
    it.Full := true;
    it.Weight := ToInt(el.GetAttribute('weight'));
    it.CapacityWalk := ToInt(el.GetAttribute('capacity-walk'));
    it.OwnerRadiation := ToInt(el.GetAttribute('owner-radiation'));
    it.OwnerTemperature := ToInt(el.GetAttribute('owner-temperature'));
    it.Rations := ToInt(el.GetAttribute('rations'));
    it.IsMan := (el.GetAttribute('is-man') = 'True');
    it.RadiationFrom := ToInt(el.GetAttribute('radiation-from'));
    it.RadiationTo := ToInt(el.GetAttribute('radiation-to'));
  end;
end;

procedure ReadSkillData(el: TXmlElement);
var st: TSkillData;
begin
  st := Game.SkillData.Seek(el.GetAttribute('id'));
  st.Name := el.GetAttribute('name');
end;

procedure ReadStructData(el: TXmlElement);
var sd: TStructData;
begin
  sd := Game.StructData.Seek(el.GetAttribute('id'));
  sd.Name := el.GetAttribute('name');

  if (el.GetAttribute('full') = 'True') then begin
    sd.Full := true;
    sd.RadiationInside := ToInt(el.GetAttribute('radiation-inside'));
    sd.TemperatureInside := ToInt(el.GetAttribute('temperature-inside'));
    sd.Capacity := ToInt(el.GetAttribute('capacity'));
    sd.Defence := ToInt(el.GetAttribute('defence'));
    sd.Speed := ToInt(el.GetAttribute('speed'));
    sd.IsVehicle := (el.GetAttribute('is-vehicle') = 'True');
  end;
end;

procedure ReadFaction(el: TXmlElement);
var f: TFaction;
begin
  f := Game.Factions.Seek(ToInt(el.GetAttribute('n')));
  f.Name := el.GetAttribute('name');
  if (ToInt(el.GetAttribute('color')) > 0) then
    f.Color := ToInt(el.GetAttribute('color'));

  if (el.GetAttribute('attitude') = 'Hostile') then f.Attitude := 1
  else if (el.GetAttribute('attitude') = 'Unfriendly') then f.Attitude := 2
  else if (el.GetAttribute('attitude') = 'Neutral') then f.Attitude := 3
  else if (el.GetAttribute('attitude') = 'Friendly') then f.Attitude := 4
  else if (el.GetAttribute('attitude') = 'Ally') then f.Attitude := 5;
end;

function ReadItem(el: TXmlElement): TItem;
begin
  Result := TItem.Create;
  Result.Data := Game.ItemData.Seek(el.GetAttribute('id'));
  Result.Amount := ToInt(el.GetAttribute('amt'));
end;

function ReadSkill(el: TXmlElement): TSkill;
begin
  Result := TSkill.Create;
  Result.Data := Game.SkillData.Seek(el.GetAttribute('id'));
  if (el.Attributes.IndexOfName('lv') >= 0) then
    Result.Level := ToInt(el.GetAttribute('lv'))
  else
    Result.Level := -1;
end;

function ReadUnit(el: TXmlElement; R: TRegion; S: TStruct; L: TUnit): TUnit;
var i: integer;
    node: TXmlElement;
begin
  Result := TUnit.Create;
  Result.Region := R;
  R.Units.Add(Result);
  Result.OrigStruct := S;
  Result.OrigLeader := L;
  Result.Faction := Game.Factions.Seek(ToInt(el.GetAttribute('faction')));
  Result.Faction.Units.Add(Result);

  Result.Name := el.GetAttribute('name');
  Result.Num := ToInt(el.GetAttribute('n'));
  Result.Chosen := (el.GetAttribute('chosen') = 'True');
  Result.Insane := (el.GetAttribute('insane') = 'True');
  Result.Avoiding := (el.GetAttribute('avoiding') = 'True');
  Result.Greedy := (el.GetAttribute('greedy') = 'True');
  Result.Patrolling := (el.GetAttribute('patrolling') = 'True');
  Result.HidingFaction := (el.GetAttribute('hiding') = 'faction');

  if (el.GetAttribute('insanity') <> '') then
    Result.Insanity := ToInt(el.GetAttribute('insanity'));
  if (el.GetAttribute('hire-demand') <> '') then
    Result.HireFee := ToInt(el.GetAttribute('hire-demand'));

  for i := 0 to el.ChildNodes.Count-1 do begin
    node := el.ChildNodes[i];
    if (node.Name = 'item') then
      Result.OrigItems.Add(ReadItem(node));
    if (node.Name = 'skill') then
      Result.Skills.Add(ReadSkill(node));
    if (node.Name = 'person') then
      ReadUnit(node, R, S, Result);
    if (node.Name = 'orders') then
      Result.Orders.Text := StringReplace(node.InnerText, '\n', #13#10, [rfReplaceAll]);
  end;
end;

procedure ReadEvent(el: TXmlElement);
var u: TUnit;
begin
  u := Turn.PlayerFaction.Units.Find(ToInt(el.GetAttribute('n')));
  if (u <> nil) then
    u.Events.Add(el.GetAttribute('text'));
end;

function ReadStruct(el: TXmlElement; R: TRegion): TStruct;
var i: integer;
    node: TXmlElement;
begin
  Result := TStruct.Create;
  Result.Data := Game.StructData.Seek(el.GetAttribute('id'));
  Result.Name := el.GetAttribute('name');
  Result.Num := ToInt(el.GetAttribute('n'));

  for i := 0 to el.ChildNodes.Count-1 do begin
    node := el.ChildNodes[i];
    if (node.Name = 'item') then
      Result.Installed.Add(ReadItem(node));
    if (node.Name = 'needs') then
      Result.Needs.Add(ReadItem(node));
    if (node.Name = 'person') then
      ReadUnit(node, R, Result, nil);
  end;
end;

procedure ReadRegion(el: TXmlElement; full: boolean);
var i, x, y: integer;
    R: TRegion;
    node: TXmlElement;
begin
  if (el.GetAttribute('full-data') <> '') then
    full := (el.GetAttribute('full-data') = 'True');

  x := StrToInt(el.GetAttribute('x'));
  y := StrToInt(el.GetAttribute('y'));

  R := Map.SeekRegion(x, y, 0);
  R.Terrain := Game.TerrainData.Find(el.GetAttribute('terrain'));
  R.Land := el.GetAttribute('in');
  if not(R.FullData) then
    R.FullData := full;
  if (el.GetAttribute('visited') <> '') then
    R.Visited := StrToInt(el.GetAttribute('visited'))
  else
    R.Visited := 0;

  if not(full) then Exit;

  R.Radiation := ToInt(el.GetAttribute('radiation'));
  R.Temperature := ToInt(el.GetAttribute('t'));
  R.Weather := el.GetAttribute('weather');

  R.Resources.ClearItems;
  R.Junk.ClearItems;
  R.Structs.ClearItems;
  R.Units.Clear;

  for i := 0 to el.ChildNodes.Count-1 do begin
    node := el.ChildNodes[i];
    if (node.Name = 'res') then
      R.Resources.Add(ReadItem(node));
    if (node.Name = 'junk') then
      R.Junk.Add(ReadItem(node));
    if (node.Name = 'obj') then
      R.Structs.Add(ReadStruct(node, R));
    if (node.Name = 'person') then
      ReadUnit(node, R, nil, nil);
  end;

  // Exits
  for i := 0 to el.ChildNodes.Count-1 do
    if (el.ChildNodes[i].Name = 'exit') then
      ReadRegion(el.ChildNodes[i], false);
end;

function LoadReport(filename: string): boolean;
var doc: TXmlDocument;
    el, el2: TXmlElement;
    lines: TStrings;
    i, j, wid, hei: integer;
    xml: string;
begin
  Screen.Cursor := crHourGlass;

  Result := false;
  doc := TXMLDocument.Create;
  lines := TStringList.Create;
  try
  try
    // Load report and look for xml part
    lines.LoadFromFile(filename);
    xml := lines.Text;
    doc.LoadXml(xml);

    // Factions
    for i := 0 to Turn.Regions.Count-1 do
      Turn.Regions[i].Units.Clear;
    for i := 0 to Game.Factions.Count-1 do
      Game.Factions[i].Units.ClearItems;

    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'factions') then
        for j := 0 to el.ChildNodes.Count-1 do
          ReadFaction(el.ChildNodes[j]);
    end;

    if (doc.DocumentElement.Name = 'report') then begin
      // Load report-specific
      Turn.Num := StrToInt(doc.DocumentElement.GetAttribute('turn'));
      Turn.Season := doc.DocumentElement.GetAttribute('season');
      Turn.Lang := doc.DocumentElement.GetAttribute('language');
      Turn.PlayerFaction := Game.Factions.Find(ToInt(doc.DocumentElement.GetAttribute('for')));
      Turn.Password := doc.DocumentElement.GetAttribute('password');
    end
    else begin
      // Load history-specific
      Game.LastReport := doc.DocumentElement.GetAttribute('last-report');
    end;

    // Terrains
    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'terrains') then begin
        for j := 0 to el.ChildNodes.Count-1 do begin
          el2 := el.ChildNodes[j];
          Game.TerrainData.Seek(el2.Attributes.Values['id']).Name :=
             el2.Attributes.Values['name'];
        end;
      end;
    end;

    // Items
    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'items') then
        for j := 0 to el.ChildNodes.Count-1 do
          ReadItemData(el.ChildNodes[j]);
    end;

    // Skills
    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'skills') then
        for j := 0 to el.ChildNodes.Count-1 do
          ReadSkillData(el.ChildNodes[j]);
    end;

    // Structs
    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'objects') then
        for j := 0 to el.ChildNodes.Count-1 do
          ReadStructData(el.ChildNodes[j]);
    end;

    // Regions
    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'map') then begin
        wid := StrToInt(el.GetAttribute('width'));
        hei := StrToInt(el.GetAttribute('height'));
        if (wid <> Map.Levels[0].Width) or (hei <> Map.Levels[0].Height) then begin
          Map.Levels[0].Free;
          Map.Levels[0] := TMapLevel.Create('surface', wid, hei);
        end;
        for j := 0 to el.ChildNodes.Count-1 do
          ReadRegion(el.ChildNodes[j], true);
      end;
    end;

    // Events
    for i := 0 to doc.DocumentElement.ChildNodes.Count-1 do begin
      el := doc.DocumentElement.ChildNodes[i];
      if (el.Name = 'events') then
        for j := 0 to el.ChildNodes.Count-1 do
          ReadEvent(el.ChildNodes[j]);
    end;

    Result := true;
  finally
    doc.Free;
    lines.Free;
  end;
  except
    on E: Exception do;
  end;
  Screen.Cursor := crDefault;
end;

function GetTurnNum(xml: string): integer;
var doc: TXmlDocument;
begin
  doc := TXmlDocument.Create;
  Result := -1;
  try
  try
    doc.LoadXml(xml);
    Result := StrToInt(doc.DocumentElement.GetAttribute('turn'));
  except
    on E: Exception do;
  end;
  finally
    doc.Free;
  end;
end;

procedure ReadOrders(Lines: TStrings);
var i, j: integer;
    s, num: string;
    u: TUnit;
begin
  u := nil;
  for i := 0 to Lines.Count-1 do begin
    if (Pos('person ', Lines[i]) = 1) then begin
      num := '';
      j := Length('person ') + 1;
      s := Lines[i];
      while (j <= Length(s)) and (s[j] >= '0')
        and (s[j] <= '9') do begin
        num := num + s[j];
        Inc(j);
      end;
      u := Turn.PlayerFaction.Units.Find(ToInt(num));
      if (u <> nil) then
        u.Orders.Clear;
    end
    else if (Pos('#end', Lines[i]) = 1) then
      Break
    else if (u <> nil) then
      u.Orders.Add(Lines[i]);
  end;
end;

end.
