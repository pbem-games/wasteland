unit uHistory;

interface

uses
  Classes, SysUtils, DataStructs, uXml;

  procedure WriteGameHistory(filename: string);

implementation

var
  doc: TXmlDocument;

procedure WriteItems;
var i: integer;
    t: TItemData;
    list, el: TXmlElement;
begin
  list := doc.CreateElement('items');
  doc.DocumentElement.AppendChild(list);

  for i := 0 to Game.ItemData.Count-1 do begin
    t := Game.ItemData[i];
    el := doc.CreateElement('item');
    list.AppendChild(el);
    el.SetAttribute('id', t.Short);
    el.SetAttribute('name0', t.Name0);
    el.SetAttribute('name1', t.Name1);
    el.SetAttribute('name2', t.Name2);

    if (t.Full) then begin
      el.SetAttribute('full', 'True');
      el.SetAttribute('weight', IntToStr(t.Weight));
      el.SetAttribute('capacity-walk', IntToStr(t.CapacityWalk));
      el.SetAttribute('owner-radiation', IntToStr(t.OwnerRadiation));
      el.SetAttribute('owner-temperature', IntToStr(t.OwnerTemperature));
      el.SetAttribute('rations', IntToStr(t.Rations));
      if (t.IsMan) then
        el.SetAttribute('is-man', 'True');
      el.SetAttribute('radiation-from', IntToStr(t.RadiationFrom));
      el.SetAttribute('radiation-to', IntToStr(t.RadiationTo));
    end;
  end;
end;

procedure WriteSkills;
var i: integer;
    t: TSkillData;
    list, el: TXmlElement;
begin
  list := doc.CreateElement('skills');
  doc.DocumentElement.AppendChild(list);

  for i := 0 to Game.SkillData.Count-1 do begin
    t := Game.SkillData[i];
    el := doc.CreateElement('skill');
    list.AppendChild(el);
    el.SetAttribute('id', t.Short);
    el.SetAttribute('name', t.Name);
  end;
end;

procedure WriteStructs;
var i: integer;
    t: TStructData;
    list, el: TXmlElement;
begin
  list := doc.CreateElement('objects');
  doc.DocumentElement.AppendChild(list);

  for i := 0 to Game.StructData.Count-1 do begin
    t := Game.StructData[i];
    el := doc.CreateElement('obj');
    list.AppendChild(el);
    el.SetAttribute('id', t.Short);
    el.SetAttribute('name', t.Name);

    if (t.Full) then begin
      el.SetAttribute('full', 'True');
      el.SetAttribute('radiation-inside', IntToStr(t.RadiationInside));
      el.SetAttribute('temperature-inside', IntToStr(t.TemperatureInside));
      el.SetAttribute('capacity', IntToStr(t.Capacity));
      el.SetAttribute('defence', IntToStr(t.Defence));
      el.SetAttribute('speed', IntToStr(t.Speed));
      if (t.IsVehicle) then
        el.SetAttribute('is-vehicle', 'True');
    end;
  end;
end;

procedure WriteFactions;
var i: integer;
    f: TFaction;
    list, el: TXmlElement;
begin
  list := doc.CreateElement('factions');
  doc.DocumentElement.AppendChild(list);

  for i := 0 to Game.Factions.Count-1 do begin
    f := Game.Factions[i];
    el := doc.CreateElement('faction');
    list.AppendChild(el);
    el.SetAttribute('n', IntToStr(f.Num));
    el.SetAttribute('name', f.Name);
    if (f.Color > 0) then
      el.SetAttribute('color', IntToStr(f.Color));
  end;
end;

procedure WriteTerrains;
var i: integer;
    t: TTerrainData;
    list, el: TXmlElement;
begin
  list := doc.CreateElement('terrains');
  doc.DocumentElement.AppendChild(list);

  for i := 0 to Game.TerrainData.Count-1 do begin
    t := Game.TerrainData[i];
    el := doc.CreateElement('terrain');
    list.AppendChild(el);
    el.SetAttribute('id', t.Short);
    el.SetAttribute('name', t.Name);
  end;
end;

procedure WriteItem(parent: TXmlElement; itm: TItem; name: string);
var el: TXmlElement;
begin
  el := doc.CreateElement(name);
  parent.AppendChild(el);
  el.SetAttribute('id', itm.Data.Short);
  el.SetAttribute('amt', IntToStr(itm.Amount));
end;

procedure WriteRegions;
var i, x, y: integer;
    R: TRegion;
    list, el: TXmlElement;
begin
  list := doc.CreateElement('map');
  doc.DocumentElement.AppendChild(list);
  list.SetAttribute('width', IntToStr(Map.Levels[Map.Level].Width));
  list.SetAttribute('height', IntToStr(Map.Levels[Map.Level].Height));

  for x := Map.Levels[0].Bounds.Left to Map.Levels[0].Bounds.Right do
    for y := Map.Levels[0].Bounds.Top to Map.Levels[0].Bounds.Bottom do begin
      R := Map.Region(x, y);
      if (R = nil) then continue;

      el := doc.CreateElement('region');
      list.AppendChild(el);
      el.SetAttribute('x', IntToStr(R.x));
      el.SetAttribute('y', IntToStr(R.y));
      el.SetAttribute('terrain', R.Terrain.Short);
      el.SetAttribute('in', R.Land);
      if (R.Visited = 0) then
        el.SetAttribute('visited', IntToStr(Turn.Num))
      else
        el.SetAttribute('visited', IntToStr(R.Visited));

      if (R.FullData) then begin
        el.SetAttribute('full-data', 'True');
        el.SetAttribute('radiation', IntToStr(R.Radiation));

        for i := 0 to R.Resources.Count-1 do
          WriteItem(el, R.Resources[i], 'res');
      end
      else
        el.SetAttribute('full-data', 'False');
    end;
end;

procedure WriteGameHistory(filename: string);
var Lines: TStrings;
begin
  doc := TXmlDocument.Create;
  doc.DocumentElement := doc.CreateElement('history');
  doc.DocumentElement.SetAttribute('last-report', Game.LastReport);

  WriteFactions;
  WriteItems;
  WriteSkills;
  WriteStructs;
  WriteTerrains;
  WriteRegions;

  Lines := TStringList.Create;
  Lines.Text := doc.OuterXml;
  Lines.SaveToFile(filename);
  Lines.Free;
  doc.Free;
end;

end.
