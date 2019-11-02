unit uHexMap;

interface

uses
  SysUtils, Classes, Windows, Graphics, DataStructs, uResources, Math, ImgList,
  HexMap;

const
  fogNone = 0;
  fogNonVisited = 1;
  fogNonVisible = 2;

  hs50 = 24;
  hs43 = 21;

  procedure HexMapSetup;
  procedure DrawHex(ACanvas: TCanvas; CX, CY: integer; Region: TRegion);
  procedure DrawHexExtra(ACanvas: TCanvas; cx, cy: integer; Region: TRegion);
  procedure CalcHexMapCoords(mapX, mapY: integer; var HX, HY: integer);
  procedure CalcMapCoords(HX, HY: integer; var mapX, mapY: integer);


implementation

uses
  Main;

procedure CalcHexMapCoords(mapX, mapY: integer; var HX, HY: integer);
var R: TRect;
begin
  R := Map.Levels[Map.Level].BoundsWithMargin;
  HX := Max(mapX - R.Left, 0);
  HY := Max(mapY - R.Top, 0);
end;

procedure CalcMapCoords(HX, HY: integer; var mapX, mapY: integer);
var R: TRect;
begin
  R := Map.Levels[Map.Level].BoundsWithMargin;
  mapX := HX + R.Left;
  mapY := HY + R.Top;
end;

procedure DrawHex(ACanvas: TCanvas; CX, CY: integer; Region: TRegion);
var terr: integer;
begin
  // Fog region
  frmResources.Terrains.DrawingStyle := ImgList.dsNormal;
  case Config.ReadInteger('Map', 'FogType', fogNone) of
    fogNonVisited:
      if not Region.FullData then
        frmResources.Terrains.DrawingStyle := ImgList.dsSelected;
    fogNonVisible:
      if not Region.FullData or (Region.Visited <> 0) then
        frmResources.Terrains.DrawingStyle := ImgList.dsSelected;
    fogNone:
      ;
  end;

  // Draw terrain
  if Region.Terrain <> nil then begin
    terr := Region.Terrain.BmpIndex;
  end
  else terr := 0;
  frmResources.Terrains.Draw(ACanvas, CX - hs50, CY - hs43, terr);
end;


procedure DrawHexExtra(ACanvas: TCanvas; cx, cy: integer; Region: TRegion);
var fright: integer;

  procedure DrawFlags;
  var i, x, y, f: integer;
      factions: TFactionList;
  begin
    factions := TFactionList.Create;
    for i := 0 to Region.Units.Count-1 do
      if (factions.IndexOf(Region.Units[i].Faction) < 0) then
        factions.Add(Region.Units[i].Faction);

    x := cx - hs50 div 2 - 4;
    f := 0;
    // Self troop comes first
    if (factions.IndexOf(Turn.PlayerFaction) >= 0) then begin
      DrawCExtra(extFlag, Turn.PlayerFaction, ACanvas, x, cy - hs43 + 5);
      x := x + 4;
      Inc(f);
    end;
    // Other flags
    i := 0;
    while (i < factions.Count) and (x < fright - 6) do begin
      if (factions[i] <> Turn.PlayerFaction) then begin
        y := cy - hs43 + 2;
        if not Odd(f) then y := y + 3;
        if (factions[i] = nil) then
          continue;
        DrawCExtra(extFlag, factions[i], ACanvas, x, y);
        x := x + 4;
        Inc(f);
      end;
      Inc(i);
    end;
    factions.Free;
  end;

begin
  fright := cx + hs50 div 2 - 1;
  if Config.ReadBool('Map', 'Flags', True) then
    DrawFlags;
end;

procedure HexMapSetup;
var hx, hy: integer;
    Bounds: TRect;
begin
  Bounds := Map.Levels[Map.Level].BoundsWithMargin;
  frmMain.HexMap.MapState := frmMain.HexMap.MapState + [msNoPaint];
  //Map.Level := GameConfig.ReadInteger('Map', 'Level', 0);
  {while (Map.Level < 0) or (Map.Level >= Map.Levels.Count)
    or (Map.Levels[Map.Level].Empty) do
      if Map.Level < Map.Levels.Count then Inc(Map.Level)
      else Map.Level := 0;}
  frmMain.HexMap.FirstOdd := Odd(Bounds.Left) xor Odd(Bounds.Top);
  frmMain.HexMap.ColCount := Bounds.Right - Bounds.Left + 1;
  frmMain.HexMap.RowCount := Bounds.Bottom - Bounds.Top + 1;
  CalcHexMapCoords(Config.ReadInteger('Map', 'SelX_' + Map.Levels[Map.Level].Name, 0),
    Config.ReadInteger('Map', 'SelY_' + Map.Levels[Map.Level].Name, 0), hx, hy);
  if (frmMain.HexMap.FirstOdd) and (hx = 0) and (hy = 0) then
    Inc(hx);   
  frmMain.HexMap.Selected := Point(hx, hy);
  frmMain.HexMap.Center(frmMain.HexMap.Selected.X, frmMain.HexMap.Selected.Y);
  frmMain.HexMap.MapState := frmMain.HexMap.MapState - [msNoPaint];
  frmMain.HexMap.Redraw;
end;


end.
