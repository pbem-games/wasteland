unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uRepRead, HexMap, DataStructs, uHexMap, Math, Menus, ActnList,
  uHistory, ToolWin, ComCtrls, ImgList, StdCtrls, ExtCtrls, Clipbrd,
  uLocalize, IniFiles, Grids, PowerGrid, uInterface, uMemo, uOrders, uGameSubs,
  IntEdit, uOrderProcessor, uAdjustGive;

const
  Version = '1.2';

  colObjName = 2;
  colCount = 11;

  NameIndent = 12;

type
  TAdvisorState = set of (asFirstActivate, asProcessing, asCloseQuery,
    asReadingGame, asFirstProcessing);

  TfrmMain = class(TForm)
    MainMenu: TMainMenu;
    itmFile: TMenuItem;
    LoadReport1: TMenuItem;
    PasteReport1: TMenuItem;
    ActionList: TActionList;
    actLoadReport: TAction;
    actPasteReport: TAction;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Bevel1: TBevel;
    Panel2: TPanel;
    actOptions: TAction;
    N1: TMenuItem;
    Options1: TMenuItem;
    itmMap: TMenuItem;
    itmFogType: TMenuItem;
    itmFogVisible: TMenuItem;
    itmFogVisited: TMenuItem;
    itmFogDisabled: TMenuItem;
    actNewGame: TAction;
    CreateGame1: TMenuItem;
    itmOpenGame: TMenuItem;
    actOpenGame: TAction;
    actSaveGame: TAction;
    N2: TMenuItem;
    SaveGame1: TMenuItem;
    lCoords: TLabel;
    Panel5: TPanel;
    PageControl: TPageControl;
    tsRegion: TTabSheet;
    pRegionGrids: TPanel;
    lResources: TLabel;
    lJunk: TLabel;
    gJunk: TPowerGrid;
    gResources: TPowerGrid;
    Panel4: TPanel;
    lForecast: TLabel;
    lLForecast: TLabel;
    lLTurn: TLabel;
    lTurn: TLabel;
    lLHex: TLabel;
    lHex: TLabel;
    lLand: TLabel;
    tsPerson: TTabSheet;
    pPerson: TPanel;
    gItems: TPowerGrid;
    gSkills: TPowerGrid;
    pcPerson: TPageControl;
    tsOrders: TTabSheet;
    mOrders: TMemo;
    tsEvents: TTabSheet;
    gEvents: TPowerGrid;
    Panel3: TPanel;
    lUnitNum: TLabel;
    lUnitName: TLabel;
    HexMap: THexMap;
    gUnits: TPowerGrid;
    Splitter1: TSplitter;
    tsObject: TTabSheet;
    Panel6: TPanel;
    Panel7: TPanel;
    lStructName: TLabel;
    lStructNum: TLabel;
    gStructItems: TPowerGrid;
    lLStructType: TLabel;
    lStructType: TLabel;
    actViewOrders: TAction;
    actCopyOrders: TAction;
    actSaveOrdersAs: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    N3: TMenuItem;
    ViewOrders1: TMenuItem;
    SaveGameAs1: TMenuItem;
    CopyOrderstoClipboard1: TMenuItem;
    actSaveOrders: TAction;
    SaveOrders1: TMenuItem;
    actFactions: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    itmTools: TMenuItem;
    Factions1: TMenuItem;
    itmFlags: TMenuItem;
    pGive: TPanel;
    eGiveAmt: TIntEdit;
    lGive: TLabel;
    imgPerson: TImage;
    ToolBar2: TToolBar;
    btnCheck: TToolButton;
    btnClear: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HexMapDrawHex(Sender: TObject; HX, HY: Integer;
      ACanvas: TCanvas; CX, CY: Integer; AState: THexMapDrawState);
    procedure HexMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure actLoadReportExecute(Sender: TObject);
    procedure actPasteReportExecute(Sender: TObject);
    procedure HexMapSelectHex(Sender: TObject; HX, HY: Integer);
    procedure gItemsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure pRegionGridsResize(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure itmFogTypeClick(Sender: TObject);
    procedure actNewGameExecute(Sender: TObject);
    procedure actOpenGameExecute(Sender: TObject);
    procedure actSaveGameExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pPersonResize(Sender: TObject);
    procedure gUnitsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure gUnitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure gSkillsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure mOrdersExit(Sender: TObject);
    procedure actViewOrdersExecute(Sender: TObject);
    procedure actCopyOrdersExecute(Sender: TObject);
    procedure actSaveOrdersAsExecute(Sender: TObject);
    procedure actSaveOrdersExecute(Sender: TObject);
    procedure actFactionsExecute(Sender: TObject);
    procedure HexMapDrawExtra(Sender: TObject; HX, HY: Integer;
      ACanvas: TCanvas; CX, CY: Integer; AState: THexMapDrawState);
    procedure itmFlagsClick(Sender: TObject);
    procedure gItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure gItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure gUnitsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure gItemsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure HexMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HexMapDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure imgPersonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgPersonEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure btnClearClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
  private
    OrdersChanged: boolean;
    RegionSelected: boolean;
    State: TAdvisorState;
    procedure OpenReport(filename: string);
    procedure FillRegionInfo;
    procedure FillObjectInfo(obj: TObject);
    procedure ApplyConfig;
    procedure LocalizeForm;
    procedure RefreshForm;
    procedure StoreMemosInfo;
    procedure ProcessAllOrders;
    procedure ProcessOrders(ARegion: TRegion);
    procedure ProcessorThreadTerminate(Sender: TObject);
    procedure GlobalEnable(Value: boolean);
    function NoDraw: boolean;
  public
    CurrRegion: TRegion;
    CurrObject: TObject;
  end;

var
  frmMain: TfrmMain;

implementation

uses uResources, uOptions, uFactions;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frmResources := TfrmResources.Create(Self);
  ApplyConfig;
  actOpenGameExecute(nil);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var res: word;
begin
  if (Config.ReadString('Main', 'Game', '') = '') then begin
    res := MessageDlg(Lang.ReadString('Messages', 'SaveGame', 'Current game is not saved. Save it?'),
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if (res = mrCancel) then CanClose := false;
    if (res = mrYes) then actSaveGame.Execute;
  end;

  StoreMemosInfo;
  if OrdersChanged then begin
    res := MessageDlg(Lang.ReadString('Messages', 'OrdersChanged', 'Orders was changed. Save order file?'), mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if res = mrYes then actSaveOrders.Execute
    else if res = mrCancel then begin
      CanClose := False;
      Exit;
    end;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if (Config.ReadString('Main', 'Game', '') <> '') then
    WriteGameHistory(Config.ReadString('Main', 'Game', ''));
  gUnits.SaveColumns(Config);
  Config.WriteInteger('Main', 'gUnitsHeight', gUnits.Height);
  frmResources.Free;
end;

procedure TfrmMain.ApplyConfig;
begin
  // Fog
  case Config.ReadInteger('Map', 'FogType', fogNone) of
    fogNone:
      itmFogDisabled.Checked := true;
    fogNonVisited:
      itmFogVisited.Checked := true;
    fogNonVisible:
      itmFogVisible.Checked := true;
  end;

  itmFlags.Checked := Config.ReadBool('Map', 'Flags', True);

  gUnits.Height := Config.ReadInteger('Main', 'gUnitsHeight', gUnits.Height);
  gUnits.LoadColumns(Config);
  if (Config.ReadBool('Main', 'PersonImages', True)) then
    gUnits.DefaultRowHeight := BattleImgDim + 1
  else
    gUnits.DefaultRowHeight := 17;
  pPersonResize(nil);
  pRegionGridsResize(nil);
  imgPerson.Cursor := crHand;
  LocalizeForm;
end;

procedure TfrmMain.HexMapDrawHex(Sender: TObject; HX, HY: Integer;
  ACanvas: TCanvas; CX, CY: Integer; AState: THexMapDrawState);
var mapX, mapY: integer;
    Region: TRegion;
begin
  if NoDraw then Exit;
  CalcMapCoords(HX, HY, mapX, mapY);
  Region := Map.Region(mapX, mapY);
  if Region <> nil then
    DrawHex(ACanvas, CX, CY, Region);
end;

procedure TfrmMain.HexMapDrawExtra(Sender: TObject; HX, HY: Integer;
  ACanvas: TCanvas; CX, CY: Integer; AState: THexMapDrawState);
var mapX, mapY: integer;
    Region: TRegion;
begin
  if NoDraw then Exit;
  CalcMapCoords(HX, HY, mapX, mapY);
  Region := Map.Region(mapX, mapY);
  if Region <> nil then
    DrawHexExtra(ACanvas, CX, CY, Region);
end;

procedure TfrmMain.HexMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var Hex: TPoint;
begin
  Hex := HexMap.MouseToHex(X, Y);
  if (Hex.X >= 0) and (Hex.Y >= 0) and (Hex.X < HexMap.ColCount) and
    (Hex.Y < HexMap.RowCount) then begin
    CalcMapCoords(Hex.X, Hex.Y, Hex.X, Hex.Y);
    lCoords.Caption := IntToStr(Hex.X)+', '+IntToStr(Hex.Y);
  end;
end;

procedure TfrmMain.HexMapDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (Source <> gItems) and (Source <> imgPerson) then
    Accept := False;
end;

procedure TfrmMain.OpenReport(filename: string);
var Lines: TStrings;
    orders: string;
begin
  if not(LoadReport(filename)) then begin
    MessageDlg(Lang.ReadString('Messages', 'IncorrectReport', 'Incorrect report.'),
      mtWarning, [mbOk], 0);
  end
  else begin
    orders := ExtractFileDir(filename) + '\turn' + IntToStr(Turn.Num) + '.ord';
    if (FileExists(orders)) then begin
      Lines := TStringList.Create;
      Lines.LoadFromFile(orders);
      ReadOrders(Lines);
      Lines.Free;
    end;

    ProcessAllOrders;
    frmResources.LoadTerrainImages;
    Game.LastReport := filename;
    RefreshForm;
  end;
end;

procedure TfrmMain.HexMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RegionSelected := true;
end;

procedure TfrmMain.HexMapSelectHex(Sender: TObject; HX, HY: Integer);
var mapX, mapY: integer;
begin
  CalcMapCoords(HX, HY, mapX, mapY);

  // Setup CurrRegion variable
  CurrRegion := Map.Region(mapX, mapY);
  // Write config
  Config.WriteInteger('Map', 'SelX_' + Map.Levels[Map.Level].Name, mapX);
  Config.WriteInteger('Map', 'SelY_' + Map.Levels[Map.Level].Name, mapY);
  // Fill panels for selected region
  FillRegionInfo;
  RegionSelected := false;
end;

procedure TfrmMain.FillRegionInfo;
var i: integer;

  procedure AddUnits(s: TStruct; leader: TUnit);
  var i, row, weight, capacity, amt: integer;
      u: TUnit;
      flags: string;
  begin
    for i := 0 to CurrRegion.Units.Count-1 do begin
      u := CurrRegion.Units[i];
      if (u.Struct <> s) or (u.Leader <> leader) then Continue;
      row := gUnits.RowCount;

      gUnits.Cells[0, row] := u.Faction.Name;
      gUnits.Cells[1, row] := IntToStr(u.Faction.Num);
      gUnits.Cells[colObjName, row] := u.Name;
      gUnits.Cells[3, row] := IntToStr(u.Num);

      flags := '';
      if (u.Chosen) then flags := flags + 'C';
      if (u.Avoiding) then flags := flags + 'A';
      if (u.Greedy) then flags := flags + 'G';
      if (u.Patrolling) then flags := flags + 'P';
      if (u.Insane) then flags := flags + 'I';
      if (u.HidingFaction) then flags := flags + 'H';
      gUnits.Cells[4, row] := flags;

      weight := CalcWeight(u);
      if (weight >= 0) then begin
        capacity := CalcCapacity(u);
        if (capacity >= 0) then
          gUnits.Cells[5, row] := IntToStr(weight) + '/' + IntToStr(capacity);
      end;

      amt := CalcRations(u);
      if (amt >= 0) then
        gUnits.Cells[6, row] := IntToStr(amt);

      amt := CalcRadiationDanger(u);
      if (amt >= 0) then
        gUnits.Cells[7, row] := IntToStr(amt);

      amt := CalcTemperatureDanger(u);
      if (amt >= 0) then
        gUnits.Cells[8, row] := IntToStr(amt);

      if (u.Insanity >= 0) then
        gUnits.Cells[9, row] := IntToStr(u.Insanity);
      if (u.HireFee >= 0) then
        gUnits.Cells[10, row] := IntToStr(u.HireFee);

      gUnits.Cells[11, row] := u.MonthOrder;

      gUnits.Rows[row].Color := u.Faction.Color;
      gUnits.Rows[row].Data := u;

      AddUnits(s, u);
    end;
  end;

  procedure AddStructAndUnits(s: TStruct);
  var weight, row: integer;
  begin
    if (s <> nil) then begin
      row := gUnits.RowCount;
      gUnits.Cells[colObjName, row] := s.Name;
      gUnits.Cells[3, row] := IntToStr(s.Num);

      if (s.Data.Capacity > 0) then begin
        weight := CalcStructWeight(s, CurrRegion);
        if (weight >= 0) then
          gUnits.Cells[5, row] := IntToStr(weight) + '/' + IntToStr(s.Data.Capacity);
      end;

      if (s.Needs.Count > 0) then
        gUnits.Rows[row].Color := clLtGray
      else
        gUnits.Rows[row].Color := clWhite;
      gUnits.Rows[row].Data := s;
    end;
    AddUnits(s, nil);
  end;

begin
  // ClearRegionInfo
  lLand.Caption := '';
  lForecast.Caption := '';
  lTurn.Caption := '';
  gResources.RowCount := 0;
  gResources.Fixup;
  gJunk.RowCount := 0;
  gJunk.Fixup;
  gUnits.RowCount := gUnits.FixedRows;
  if (RegionSelected) and Config.ReadBool('Main', 'AutoTabs', true) then
    tsRegion.Show;

  FillObjectInfo(nil);

  if (CurrRegion = nil) then begin
    gUnits.Fixup;
    Exit;
  end;

  // Main data
  lLand.Caption := CurrRegion.Land;
  lHex.Caption := CurrRegion.Terrain.Name + ' (' +
    IntToStr(CurrRegion.x) + ',' + IntToStr(CurrRegion.y) + ')';

  if not(CurrRegion.FullData) then begin
    gUnits.Fixup;
    Exit;
  end;

  // Full data
  if (CurrRegion.Visited = 0) then begin
    lTurn.Caption := Lang.ReadString('Messages', 'CurrentTurn', 'current');
    lForecast.Caption := Format(Lang.ReadString('Messages', 'Forecast', '%s, %d'#0176'C, %d mR/h'),
      [CurrRegion.Weather, CurrRegion.Temperature, CurrRegion.Radiation]);
  end
  else begin
    lTurn.Caption := IntToStr(CurrRegion.Visited);
    lForecast.Caption := Format(Lang.ReadString('Messages', 'ShortForecast', '%d mR/h'),
      [CurrRegion.Radiation]);
  end;

  // Grids
  FillItemGrid(gResources, CurrRegion.Resources);
  FillItemGrid(gJunk, CurrRegion.Junk);

  // Units
  gUnits.RowCount := gUnits.FixedRows;
  AddStructAndUnits(nil);
  for i := 0 to CurrRegion.Structs.Count-1 do
    AddStructAndUnits(CurrRegion.Structs[i]);
  gUnits.Fixup;
end;

procedure TfrmMain.FillObjectInfo(obj: TObject);
var i, j, row: integer;
    u: TUnit;
    s: TStruct;
begin
  if (obj <> nil) and not(RegionSelected) and Config.ReadBool('Main', 'AutoTabs', true) then
    if (TObject(obj).ClassType = TUnit) then
      tsPerson.Show
    else
      tsObject.Show;

  if (CurrObject = obj) then
    Exit;
  CurrObject := obj;

  // Clear person and object tabs
  lUnitNum.Caption := '';
  lUnitName.Caption := '';
  gItems.RowCount := 0;
  gItems.Fixup;
  gSkills.RowCount := 0;
  gSkills.Fixup;
  gStructItems.RowCount := 0;
  gStructItems.Fixup;
  mOrders.Lines.Text := '';
  gEvents.RowCount := 0;
  gEvents.Fixup;
  lStructName.Caption := '';
  lStructNum.Caption := '';
  lStructType.Caption := '';

  if (obj = nil) then
    Exit;
  if (TObject(obj).ClassType = TUnit) then begin

    // Unit
    u := TUnit(obj);
    lUnitName.Caption := u.Name;
    lUnitNum.Caption := IntToStr(u.Num);
    FillItemGrid(gItems, u.Items);
    FillSkillGrid(gSkills, u.Skills);
    mOrders.Lines.Assign(u.Orders);
    mOrders.Modified := false;

    for i := 0 to u.Events.Count-1 do begin
      gEvents.Cells[1, i] := u.Events[i];
      gEvents.Rows[i].ImageIndex := 3;
    end;
    gEvents.Fixup;

  end
  else if (TObject(obj).ClassType = TStruct) then begin

    // Struct
    s := TStruct(obj);
    lStructName.Caption := s.Name;
    lStructNum.Caption := IntToStr(s.Num);
    lStructType.Caption := s.Data.Name;

    for i := 0 to s.Installed.Count-1 do begin
      row := gStructItems.RowCount;
      gStructItems.Cells[0, row] := IntToStr(s.Installed[i].Amount);
      gStructItems.Cells[1, row] := s.Installed[i].Name;
      gStructItems.Rows[row].Data := s.Installed[i];
    end;

    for i := 0 to s.Needs.Count-1 do begin
      j := s.Installed.Count-1;
      while (j >= 0) and (s.Installed[j].Data <> s.Needs[i].Data) do Dec(j);
      if (j >= 0) then begin
        row := j;
        gStructItems.Cells[0, row] := IntToStr(s.Installed[j].Amount) + '/' +
          IntToStr(s.Needs[i].Amount + s.Installed[j].Amount);
      end
      else begin
        row := gStructItems.RowCount;
        gStructItems.Cells[0, row] := '0/' + IntToStr(s.Needs[i].Amount);
        gStructItems.Cells[1, row] := s.Needs[i].Name;
        gStructItems.Rows[row].Data := s.Needs[i];
      end;
      gStructItems.Rows[row].Color := clDkGray;
    end;

    gStructItems.Fixup;

  end;
end;

procedure TfrmMain.gItemsDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
begin
  if NoDraw then Exit;
  ItemGridDrawCell(Sender, ACol, ARow, TxtRect, 1);
end;

procedure TfrmMain.pRegionGridsResize(Sender: TObject);
var half: integer;
begin
  half := pRegionGrids.Height div 2;
  lResources.Top := 0;
  gResources.Top := lResources.Top + lResources.Height;
  gResources.Height := half - lResources.Height;
  lJunk.Top := half + 2;
  gJunk.Top := lJunk.Top + lJunk.Height;
  gJunk.Height := half - lJunk.Height;
end;

procedure TfrmMain.pPersonResize(Sender: TObject);
var third: integer;
begin
  third := pPerson.Height div 3;
  gItems.Top := 1;
  gItems.Height := third - 2;
  pGive.Top := third + 1;
  gSkills.Top := pGive.Top + pGive.Height;
  gSkills.Height := third - 2 - pGive.Height;
  pcPerson.Top := third * 2 + 1;
  pcPerson.Height := third - 2;
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
  frmOptions := TfrmOptions.Create(Self);
  frmOptions.ShowModal;
  frmOptions.Free;
  ApplyConfig;
  FillRegionInfo;
end;

procedure TfrmMain.itmFogTypeClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := true;
  if (itmFogDisabled.Checked) then
    Config.WriteInteger('Map', 'FogType', fogNone);
  if (itmFogVisited.Checked) then
    Config.WriteInteger('Map', 'FogType', fogNonVisited);
  if (itmFogVisible.Checked) then
    Config.WriteInteger('Map', 'FogType', fogNonVisible);
  HexMap.Redraw;
end;

procedure TfrmMain.LocalizeForm;
begin
  Localize(Self);
  RefreshForm;
end;

procedure TfrmMain.RefreshForm;
var gamename: string;
begin
  gamename := ExtractFileName(Config.ReadString('Main', 'Game', ''));
  gamename := Copy(gamename, 1, Length(gamename) - Length(ExtractFileExt(gamename)));
  if (gamename = '') then
    gamename := 'untitled';
  Caption := 'Wasteland Advisor v' + Version + ' - ' + gamename;
  if (Turn.Num > 0) then
    Caption := Caption + ', ' + Format(Lang.ReadString('Messages', 'TurnTitle', 'turn %d, %s'),
       [Turn.Num, Turn.Season]);

  HexMapSetup;
end;

procedure TfrmMain.actNewGameExecute(Sender: TObject);
begin
  // Recreate game
  CurrRegion := nil;
  if (Game <> nil) then Game.Free;
  Game := TGame.Create;
  Config.WriteString('Main', 'Game', '');
  RefreshForm;
end;

procedure TfrmMain.actOpenGameExecute(Sender: TObject);
var dlg: TOpenDialog;
begin
  if (Sender <> nil) then begin
    dlg := TOpenDialog.Create(Self);
    dlg.Filter := 'Game files (*.game)|*.game|All files|*.*';
    if not(dlg.Execute) then Exit;
    Config.WriteString('Main', 'Game', dlg.FileName);
  end;

  // Recreate game
  CurrRegion := nil;
  if (Game <> nil) then Game.Free;
  Game := TGame.Create;

  // Open history
  if (FileExists(GameFile)) then
    LoadReport(GameFile);

  // Open last loaded report
  if (Game.LastReport <> '') then
    OpenReport(Game.LastReport);

  RefreshForm;
end;

procedure TfrmMain.actSaveGameExecute(Sender: TObject);
var dlg: TSaveDialog;
begin
  if (Sender <> nil) then begin
    dlg := TSaveDialog.Create(Self);
    dlg.Filter := 'Game files (*.game)|*.game|All files|*.*';
    if (Config.ReadString('Main', 'Game', '') <> '') then
      dlg.FileName := Config.ReadString('Main', 'Game', '')
    else
      dlg.FileName := 'untitled.game';
    dlg.DefaultExt := '.game';
    if not(dlg.Execute) then Exit;
    Config.WriteString('Main', 'Game', dlg.FileName);
  end;
  WriteGameHistory(GameFile);
  RefreshForm;
end;

procedure TfrmMain.actLoadReportExecute(Sender: TObject);
var dlg: TOpenDialog;
    i: integer;
    Lines: TStringList;
begin
  dlg := TOpenDialog.Create(Self);
  dlg.Filter := 'Report files (report.*, *.rep)|report.*;*.rep|All files|*.*';
  dlg.Options := dlg.Options + [ofAllowMultiSelect];
  if (dlg.Execute) then begin
    Lines := TStringList.Create;
    Lines.Assign(dlg.Files);
    Lines.Sort;
    for i := 0 to Lines.Count-1 do
      OpenReport(Lines[i]);
    Lines.Free;
  end;
  dlg.Free;
end;

procedure TfrmMain.actPasteReportExecute(Sender: TObject);
var Lines: TStrings;
    filename, basename: string;
    num, i: integer;
begin
  Lines := TStringList.Create;
  Lines.Text := Clipboard.AsText;

  num := GetTurnNum(Lines.Text);
  if (num >= 0) then
    basename := ExtractFileDir(GameFile) + '\turn' + IntToStr(num)
  else
    basename := 'unknown';
  filename := basename;
  i := 1;
  while (FileExists(filename + '.rep')) do begin
    filename := basename + ' (' + IntToStr(i) + ')';
    Inc(i);
  end;
  filename := filename + '.rep';

  Lines.SaveToFile(filename);
  Lines.Free;

  OpenReport(filename);
end;

procedure TfrmMain.gUnitsDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var u: TUnit;
    i, col: integer;
begin
  if NoDraw then Exit;
  if (ARow = 0) then
    Exit;
  col := 0;
  for i := 0 to colCount-1 do
    if (gUnits.Cols[i].ImgCol = ACol) then
      col := i;

  if (col = colObjName) then begin
    if (gUnits.ImgRows[ARow].Data <> nil) and
      (TObject(gUnits.ImgRows[ARow].Data).ClassType = TUnit) then begin
      u := TUnit(gUnits.ImgRows[ARow].Data);
      if (u.Struct <> nil) then
        TxtRect.Left := TxtRect.Left + NameIndent;
      if (u.Leader <> nil) then
        TxtRect.Left := TxtRect.Left + NameIndent;

      if (Config.ReadBool('Main', 'PersonImages', true)) then begin
        DrawBattleImage(gUnits.Canvas, TxtRect.Left, TxtRect.Top,
          GetBattleImage(u, 'Images'), u.Faction.Color, False);
        TxtRect.Left := TxtRect.Left + BattleImgDim;
      end;

      if (u.Struct <> nil) and (IsInside(u, u.Struct)) then begin
        frmResources.Extras.Draw(gUnits.Canvas, TxtRect.Right - 9, TxtRect.Top + 1,
          extBuilding);
        TxtRect.Right := TxtRect.Right - 8;
      end;

    end;
    if (gUnits.ImgRows[ARow].Data <> nil) and
      (TObject(gUnits.ImgRows[ARow].Data).ClassType = TStruct) then begin
      gUnits.Canvas.Font.Style := [fsBold];

      if (Config.ReadBool('Main', 'PersonImages', true)) then begin
        DrawBattleImage(gUnits.Canvas, TxtRect.Left, TxtRect.Top,
          TStruct(gUnits.ImgRows[ARow].Data).Data.Short + '.bmp',
          clWhite, False);
        TxtRect.Left := TxtRect.Left + 18;
      end;
    end;
  end;
end;

procedure TfrmMain.gUnitsSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  FillObjectInfo(gUnits.ImgRows[ARow].Data);
end;

procedure TfrmMain.gSkillsDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
begin
  SkillGridDrawCell(Sender, ACol, ARow, TxtRect, 0);
end;

procedure TfrmMain.mOrdersExit(Sender: TObject);
begin
  if not(mOrders.Modified) or (CurrObject.ClassType <> TUnit) then Exit;
  TUnit(CurrObject).Orders.Text := mOrders.Text;
  mOrders.Modified := false;
  OrdersChanged := true;
  ProcessOrders(CurrRegion);
end;

procedure TfrmMain.StoreMemosInfo;
begin
  if (mOrders.Focused) then mOrdersExit(mOrders);
end;

procedure TfrmMain.actViewOrdersExecute(Sender: TObject);
var RepLines: TStrings;
begin
  StoreMemosInfo; // exit from other controls
  with TfrmMemo.Create(Self) do begin
    Caption := 'Orders';
    CompileOrder(Memo.Lines, False);
    Memo.Modified := False;
    ShowModal;
    if Memo.Modified then begin
      RepLines := TStringList.Create;
      RepLines.Text := Memo.Lines.Text;
      ReadOrders(RepLines);
      RepLines.Free;
      FillObjectInfo(nil);
    end;
    Free;
  end;
end;

procedure TfrmMain.actCopyOrdersExecute(Sender: TObject);
var Lines: TStringList;
begin
  StoreMemosInfo;
  Lines := TStringList.Create;
  CompileOrder(Lines, True);
  Clipboard.SetTextBuf(PChar(Lines.Text));
  Lines.Free;
end;

procedure TfrmMain.actSaveOrdersAsExecute(Sender: TObject);
var Lines: TStringList;
    SaveDialog: TSaveDialog;
begin
  StoreMemosInfo;
  SaveDialog := TSaveDialog.Create(Self);
  SaveDialog.DefaultExt := '*.ord';
  SaveDialog.Filter := 'Orders (*.ord)|*.ord|All files|*.*';
  SaveDialog.InitialDir := ExtractFileDir(GameFile);
  SaveDialog.FileName := 'turn' + IntToStr(Turn.Num) + '.ord';
  if SaveDialog.Execute then begin
    Lines := TStringList.Create;
    CompileOrder(Lines, True);
    Lines.SaveToFile(SaveDialog.FileName);
    Lines.Free;
  end;
  SaveDialog.Free;
end;

procedure TfrmMain.actSaveOrdersExecute(Sender: TObject);
var Lines: TStringList;
begin
  StoreMemosInfo;
  Lines := TStringList.Create;
  CompileOrder(Lines, True);
  Lines.SaveToFile(ExtractFileDir(GameFile) + '\turn' + IntToStr(Turn.Num) + '.ord');
  Lines.Free;
  OrdersChanged := false;
end;

procedure TfrmMain.actFactionsExecute(Sender: TObject);
var i: integer;
begin
  frmFactions := TfrmFactions.Create(Self);
  frmFactions.ShowModal;

  i := Turn.PlayerFaction.Units.Count-1;
  while (i > 0) and not(Turn.PlayerFaction.Units[i].Chosen) do Dec(i);
  Turn.PlayerFaction.Units[i].Orders.Text := Turn.PlayerFaction.Units[i].Orders.Text
    + frmFactions.GetOrders();

  frmFactions.Free;
  HexMap.Redraw;
  FillRegionInfo;
end;

procedure TfrmMain.itmFlagsClick(Sender: TObject);
begin
  itmFlags.Checked := not(itmFlags.Checked);
  Config.WriteBool('Map', 'Flags', itmFlags.Checked);
  HexMap.Redraw;
end;

procedure TfrmMain.gItemsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (CurrObject <> nil) and (CurrObject.ClassType = TUnit)
    and (Button = mbLeft) and (gItems.RowCount > 0)
    and (TUnit(CurrObject).Faction = Turn.PlayerFaction)
    and  not (ssDouble in Shift)
    and (gItems.ImgRows[gItems.Row].Data <> nil) then begin
    gItems.BeginDrag(False);
  end;
end;

procedure TfrmMain.gItemsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Sender = Source then begin
    if PageControl.ActivePage = tsPerson then eGiveAmt.SetFocus;
  end
end;

procedure TfrmMain.gItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
var i: integer;
    Item: TItem;

  procedure DoGive(Item: TItem; amt: integer);
  var P: TGridCoord;
      Giver, AUnit: TUnit;
      give_amt: string;
  begin
    if not(CurrObject.ClassType = TUnit) then Exit;
    Giver := TUnit(CurrObject);

    if Target = gUnits then begin
      P := gUnits.MouseCell;
      if P.Y >= 1 then begin
        AUnit := TUnit(gUnits.ImgRows[P.Y].Data);
        if AUnit = CurrObject then Exit;
        // Adjust amount to allow unit move
        if AltPressed then give_amt := 'all'
        else give_amt := IntToStr(amt);
        AddGiveOrder(Giver, 'give ' + IntToStr(AUnit.Num) + ' ' + give_amt +
          ' ' + Item.Data.Short + '; ' + AUnit.Name);
        OrdersChanged := true;
        ProcessOrders(CurrRegion);
      end;
    end
    else if (Target = HexMap)  then begin
      if AltPressed then give_amt := 'all'
      else give_amt := IntToStr(amt);
      AddGiveOrder(Giver, 'give 0 ' + give_amt +
        ' ' + Item.Data.Short);
      OrdersChanged := true;
      ProcessOrders(CurrRegion);
    end;
  end;

begin
  with Sender as TPowerGrid do begin
    if SelectedRows = 1 then
      DoGive(TItem(ImgRows[Row].Data), eGiveAmt.Value)
    else begin
      for i := 0 to RowCount-1 do
        if Rows[i].Selected then begin
          Item := TItem(Rows[i].Data);
          if (Item = nil) then Continue;
          DoGive(Item, Item.Amount);
        end;
    end;
  end;
end;

procedure TfrmMain.gUnitsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (Source <> gItems) and (Source <> imgPerson) then
    Accept := False;
end;

procedure TfrmMain.gItemsSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var Item: TItem;
begin
  if (ARow <> TPowerGrid(Sender).Row) or (eGiveAmt.Value = 0) then begin
    Item := TItem(TPowerGrid(Sender).ImgRows[ARow].Data);
    if Item <> nil then eGiveAmt.Value := Item.Amount
    else eGiveAmt.Value := 0;
  end;
end;

procedure TfrmMain.ProcessAllOrders;
begin
  ProcessOrders(nil);
end;

procedure TfrmMain.ProcessOrders(ARegion: TRegion);
begin
  State := State + [asProcessing];
  OrdersChanged := True;
  // Disable all
  GlobalEnable(False);
  //ItemGridTop := ItemGrid.TopRow;
  //StopProcessorAction.Enabled := True;
  // Start order processor thread
  ThreadTerminate := ProcessorThreadTerminate;
  DoProcessOrders(ARegion);
end;

// End processor
procedure TfrmMain.ProcessorThreadTerminate(Sender: TObject);
begin
  State := State - [asProcessing, asFirstProcessing];
  // Enable all
  GlobalEnable(True);
  // Redraw all
  HexMap.Selected := HexMap.Selected;
  HexMap.Redraw;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.GlobalEnable(Value: boolean);
var i: integer;
begin
  HexMap.Enabled := Value;
  gUnits.Enabled := Value;
  for i := 0 to MainMenu.Items.Count-1 do
    MainMenu.Items[i].Enabled := Value;

  gUnits.NoRepaint := not Value;
  gItems.NoRepaint := not Value;
  gSkills.NoRepaint := not Value;
  gJunk.NoRepaint := not Value;
  gResources.NoRepaint := not Value;
  gStructItems.NoRepaint := not Value;
  gEvents.NoRepaint := not Value;
end;

function TfrmMain.NoDraw: boolean;
begin
  Result := (asProcessing in State);
end;

procedure TfrmMain.imgPersonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (CurrObject <> nil) and (CurrObject.ClassType = TUnit)
    and (Button = mbLeft)
    and (TUnit(CurrObject).Faction = Turn.PlayerFaction)
    and  not (ssDouble in Shift) then begin
    imgPerson.BeginDrag(False);
  end;
end;

procedure TfrmMain.imgPersonEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var u, AUnit: TUnit;
    AStruct: TStruct;
    P: TGridCoord;
begin
  if not(CurrObject.ClassType = TUnit) then Exit;
  u := TUnit(CurrObject);

  if Target = gUnits then begin
    P := gUnits.MouseCell;
    if P.Y >= 1 then begin
      if (TObject(gUnits.ImgRows[P.Y].Data).ClassType = TUnit) then begin
        AUnit := TUnit(gUnits.ImgRows[P.Y].Data);
        if AUnit = CurrObject then Exit;
        u.Orders.Add('team ' + IntToStr(AUnit.Num) + '; ' + AUnit.Name);
      end
      else if (TObject(gUnits.ImgRows[P.Y].Data).ClassType = TStruct) then begin
        AStruct := TStruct(gUnits.ImgRows[P.Y].Data);
        u.Orders.Add('enter ' + IntToStr(AStruct.Num) + '; ' + AStruct.Name);
      end;
      mOrders.Text := u.Orders.Text;
      OrdersChanged := true;
      ProcessOrders(CurrRegion);
    end;
  end
  else if (Target = HexMap)  then begin
    if (u.Leader <> nil) then
      u.Orders.Add('team')
    else if (u.Struct <> nil) then
      u.Orders.Add('leave');
    mOrders.Text := u.Orders.Text;
    OrdersChanged := true;
    ProcessOrders(CurrRegion);
  end;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  mOrders.Clear;
  mOrders.Modified := true;
  mOrdersExit(nil);
end;

procedure TfrmMain.btnCheckClick(Sender: TObject);
begin
  mOrdersExit(nil);
end;

end.
