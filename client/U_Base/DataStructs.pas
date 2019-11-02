unit DataStructs;

interface

uses
  Classes, SysUtils, Windows, Math, Graphics;

const
  ordGive = 0;
  ordTeam = 1;
  ordEnter = 2;
  ordLeave = 3;
  ordScavenge = 4;
  ordProduce = 5;
  ordMove = 6;
  ordDrive = 7;
  ordPatrol = 8;
  ordBuild = 9;
  ordCure = 10;
  ordInstall = 11;
  ordUninstall = 12;
  ordCount = 13;

  OrderNames: array[0..ordCount-1] of string =
    ('give', 'team', 'enter', 'leave', 'scavenge', 'produce', 'move', 'drive',
     'patrol', 'build', 'cure', 'install', 'uninstall');


type
  TCoords = record
    x, y, z: integer;
  end;
  TCoordArray = array of TCoords;
  TPointArray = array of TPoint;
  TIntArray = array of integer;

  TFactionList = class;
  TUnitList = class;
  TRegionList = class;
  TMapLevelList = class;
  TTerrainDataList = class;
  TItemList = class;
  TItemDataList = class;
  TSkillList = class;
  TSkillDataList = class;
  TStructList = class;
  TStructDataList = class;

  TRegion = class;
  TFaction = class;

  TTerrainData = class
    Name: string;
    Short: string;
    Flags: DWord;
    BmpIndex: integer;
    MoveCost: integer;
    Color: TColor;
    constructor Create(AShort: string);
  end;

  TItemData = class
    Short: string;
    Name0: string;
    Name1: string;
    Name2: string;
    Full: boolean;
    IsMan: boolean;
    Weight: integer;
    CapacityWalk: integer;
    OwnerRadiation: integer;
    OwnerTemperature: integer;
    Rations: integer;
    RadiationFrom, RadiationTo: integer;
    constructor Create(AShort: string);
  end;

  TItem = class
    Data: TItemData;
    Amount: integer;
    function Name: string;
  end;

  TSkillData = class
    Name, Short, Description: string;
    BasedOn: TSkillData;
    constructor Create(Short: string);
  end;

  TSkill = class
    Data: TSkillData;
    Level: integer;
    function Name: string;
  end;

  TStructData = class
    Short: string;
    Name: string;
    Full: boolean;
    RadiationInside: integer;
    TemperatureInside: integer;
    Defence: integer;
    Capacity: integer;
    IsVehicle: boolean;
    Speed: integer;
    constructor Create(Short: string);
  end;

  TStruct = class
    Data: TStructData;
    Name: string;
    Num: integer;
    Installed: TItemList;
    Needs: TItemList;
    constructor Create;
    destructor Destroy; override;
  end;

  TUnit = class
    Name: string;
    Num: integer;
    Region: TRegion;
    Faction: TFaction;
    Chosen, Insane, Patrolling, Avoiding, HidingFaction, Greedy: boolean;
    Struct, OrigStruct: TStruct;
    Leader, OrigLeader: TUnit;
    Items, OrigItems: TItemList;
    Skills: TSkillList;
    Events, Orders: TStrings;
    Insanity, HireFee: integer;
    OrdersPresent: array[0..ordCount-1] of boolean;
    MonthOrder: string;
    constructor Create;
    destructor Destroy; override;
    function Order(Index: integer): string;
    procedure ResetVirtual;
  private
    procedure OrdersChanged(Sender: TObject);
  end;

  TFaction = class
    Name: string;
    Num: integer;
    Color: integer;
    Attitude: integer;
    Units: TUnitList;
    constructor Create;
    destructor Destroy; override;
  end;

  TRegion = class
    x, y, z: integer;
    FullData: boolean;
    Visited: integer;
    HasExit: array[1..6] of boolean;
    IsVirtual: boolean;

    Terrain: TTerrainData;
    Land: string;
    Radiation: integer;
    Temperature: integer;
    Weather: string;

    Resources: TItemList;
    Junk: TItemList;
    Structs: TStructList;
    Units: TUnitList;
  private
    function GetCoords: TCoords;
    procedure SetCoords(const Value: TCoords);
  public
    constructor Create(X, Y, Z, TurnNum: integer);
    destructor Destroy; override;
    property Coords: TCoords read GetCoords write SetCoords;
  end;

  TMapLevel = class
    Name: string;
    Empty: boolean;
    Width, Height: integer;
    Bounds: TRect;
    Regions: TRegionList;
    function BoundsWithMargin: TRect;
    constructor Create(Name: string; Width, Height: integer);
    destructor Destroy; override;
  end;

  TMap = class
    Level: integer;
    Levels: TMapLevelList;
    function Region(X, Y: integer): TRegion; overload;
    function Region(X, Y, Z: integer): TRegion; overload;
    function Region(C: TCoords): TRegion; overload;
    function SeekRegion(X, Y, Z: integer): TRegion;
    constructor Create;
    destructor Destroy; override;
  end;

  TTurn = class
    Num: integer;
    Season: string;
    Regions: TRegionList;
    Lang: string;
    PlayerFaction: TFaction;
    Password: string;
    constructor Create(ANum: integer);
    destructor Destroy; override;
  end;

  TGame = class
    LastReport: string;
    TerrainData: TTerrainDataList;
    ItemData: TItemDataList;
    SkillData: TSkillDataList;
    StructData: TStructDataList;
    Factions: TFactionList;
    constructor Create;
    destructor Destroy; override;
  end;


  { Type lists }

  TFactionList = class(TList)
  protected
    function Get(Index: Integer): TFaction;
    procedure Put(Index: Integer; Item: TFaction);
  public
    property Items[Index: Integer]: TFaction read Get write Put; default;
    function Seek(Num: integer): TFaction;
    function Find(Num: integer): TFaction;
    procedure ClearAndFree;
  end;

  TUnitList = class(TList)
  protected
    function Get(Index: Integer): TUnit;
    procedure Put(Index: Integer; Item: TUnit);
  public
    property Items[Index: Integer]: TUnit read Get write Put; default;
    function Find(ANum: integer): TUnit;
    procedure ClearAndFree;
    procedure ClearItems;
  end;

  TMapLevelList = class(TList)
  protected
    function Get(Index: Integer): TMapLevel;
    procedure Put(Index: Integer; Item: TMapLevel);
  public
    property Items[Index: Integer]: TMapLevel read Get write Put; default;
    function NumOf(AName: string): integer;
    procedure ClearAndFree;
  end;

  TRegionList = class(TList)
  protected
    function Get(Index: Integer): TRegion;
    procedure Put(Index: Integer; Item: TRegion);
  public
    property Items[Index: Integer]: TRegion read Get write Put; default;
    procedure ClearAndFree;
  end;

  TTerrainDataList = class(TList)
  protected
    function Get(Index: Integer): TTerrainData;
    procedure Put(Index: Integer; Item: TTerrainData);
  public
    property Items[Index: Integer]: TTerrainData read Get write Put; default;
    function Find(Short: string): TTerrainData;
    function Seek(Short: string): TTerrainData;
    procedure ClearAndFree;
  end;

  TItemList = class(TList)
  protected
    function Get(Index: Integer): TItem;
    procedure Put(Index: Integer; Item: TItem);
  public
    property Items[Index: Integer]: TItem read Get write Put; default;
    procedure ClearAndFree;
    procedure ClearItems;
    function Find(Short: string): TItem;
    function Seek(Short: string): TItem;
  end;

  TItemDataList = class(TList)
  protected
    function Get(Index: Integer): TItemData;
    procedure Put(Index: Integer; Item: TItemData);
  public
    property Items[Index: Integer]: TItemData read Get write Put; default;
    function Find(Short: string): TItemData;
    function Seek(Short: string): TItemData;
    function FindByName(AName: string): TItemData;
    procedure ClearAndFree;
  end;

  TSkillList = class(TList)
  protected
    function Get(Index: Integer): TSkill;
    procedure Put(Index: Integer; Item: TSkill);
  public
    function Find(Short: string): TSkill;
    function Seek(Short: string): TSkill;
    property Items[Index: Integer]: TSkill read Get write Put; default;
    procedure ClearAndFree;
    procedure ClearItems;
  end;

  TSkillDataList = class(TList)
  protected
    function Get(Index: Integer): TSkillData;
    procedure Put(Index: Integer; Item: TSkillData);
  public
    property Items[Index: Integer]: TSkillData read Get write Put; default;
    function Find(Short: string): TSkillData;
    function Seek(Short: string): TSkillData;
    procedure ClearAndFree;
  end;

  TStructList = class(TList)
  protected
    function Get(Index: Integer): TStruct;
    procedure Put(Index: Integer; Item: TStruct);
  public
    property Items[Index: Integer]: TStruct read Get write Put; default;
    procedure ClearAndFree;
    procedure ClearItems;
    function Find(Num: integer): TStruct;
  end;

  TStructDataList = class(TList)
  protected
    function Get(Index: Integer): TStructData;
    procedure Put(Index: Integer; Item: TStructData);
  public
    property Items[Index: Integer]: TStructData read Get write Put; default;
    function Find(Short: string): TStructData;
    function Seek(Short: string): TStructData;
    procedure ClearAndFree;
  end;

var
  Map: TMap;
  Turn: TTurn;
  VTurn: TTurn;
  Game: TGame;

  // Flags
  function Test(A, B: DWord): boolean;
  procedure SetFlag(var Flags: DWord; Flag: DWord; Value: boolean); overload;
  procedure SetFlag(var Flags: DWord; Flag: DWord); overload;
  // Coords
  function Coords(X, Y, Z: integer): TCoords;
  function EqualPoints(A, B: TPoint): boolean;
  function EqualCoords(C1, C2: TCoords): boolean;
  function StrToCoords(s: string): TCoords;
  function CoordsToStr(C: TCoords): string;
  procedure AddCoords(var A: TCoordArray; C: TCoords);
  procedure AddIndexedCoords(var A: TCoordArray; C: TCoords);
  procedure DelCoords(var A: TCoordArray; C: TCoords);
  function CoordsInList(var A: TCoordArray; C: TCoords): boolean;
  procedure AddInt(var A: TIntArray; I: integer);


implementation

function EqualPoints(A, B: TPoint): boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function Coords(X, Y, Z: integer): TCoords;
begin
  Result.x := X;
  Result.y := Y;
  Result.z := Z;
end;

procedure AddCoords(var A: TCoordArray; C: TCoords);
begin
  SetLength(A, Length(A) + 1);
  A[Length(A) - 1] := C;
end;

procedure AddIndexedCoords(var A: TCoordArray; C: TCoords);
var i, j: integer;
begin
  SetLength(A, Length(A) + 1);
  i := 0;
  while (i < Length(A)-1) and (
    (A[i].z < C.z)
    or ((A[i].z = C.z) and (A[i].x < C.x))
    or ((A[i].z = C.z) and (A[i].x = C.x) and (A[i].y < C.y)) ) do Inc(i);
  for j := Length(A)-1 downto i+1 do
    A[j] := A[j-1];
  A[i] := C;
end;

procedure DelCoords(var A: TCoordArray; C: TCoords);
var i, j: integer;
begin
  i := Length(A)-1;
  while (i >= 0) and not EqualCoords(A[i], C) do Dec(i);
  if (i < 0) then Exit;
  for j := i+1 to Length(A)-1 do
    A[j-1] := A[j];
  SetLength(A, Length(A) - 1);
end;

function CoordsInList(var A: TCoordArray; C: TCoords): boolean;
var i: integer;
begin
  i := Length(A)-1;
  while (i >= 0) and not EqualCoords(A[i], C) do Dec(i);
  Result := (i >= 0);
end;

procedure AddInt(var A: TIntArray; I: integer);
begin
  SetLength(A, Length(A) + 1);
  A[Length(A) - 1] := I;
end;

function Test(A, B: DWord): boolean;
begin
  Result := (A and B <> 0);
end;

procedure SetFlag(var Flags: DWord; Flag: DWord; Value: boolean);
begin
  if Value then Flags := Flags or Flag
  else Flags := Flags and not Flag;
end;

procedure SetFlag(var Flags: DWord; Flag: DWord);
begin
  SetFlag(Flags, Flag, True);
end;

function EqualCoords(C1, C2: TCoords): boolean;
begin
  Result := (C1.x = C2.x) and (C1.y = C2.y) and (C1.z = C2.z);
end;

function StrToCoords(s: string): TCoords;
var i: integer;
begin
  if s = '' then begin
    Result.x := 0;
    Result.y := 0;
    Result.z := -1;
  end
  else begin
    i := Pos(' ', s);
    Result.x := StrToInt(Copy(s, 1, i-1));
    s := Copy(s, i+1, Length(s));
    i := Pos(' ', s);
    Result.y := StrToInt(Copy(s, 1, i-1));
    s := Copy(s, i+1, Length(s));
    Result.z := StrToInt(Copy(s, 1, Length(s)));
  end;
end;

function CoordsToStr(C: TCoords): string;
begin
  Result := IntToStr(C.x) + ' ' + IntToStr(C.y) + ' ' + IntToStr(C.z);
end;

{ TGame }

constructor TGame.Create;
begin
  Turn := TTurn.Create(0);
  Map := TMap.Create();
  Map.Levels.Add(TMapLevel.Create('surface', 1, 1));

  TerrainData := TTerrainDataList.Create;
  ItemData := TItemDataList.Create;
  SkillData := TSkillDataList.Create;
  StructData := TStructDataList.Create;
  Factions := TFactionList.Create;
end;

destructor TGame.Destroy;
begin
  Turn.Free;
  Map.Free;

  TerrainData.ClearAndFree;
  ItemData.ClearAndFree;
  SkillData.ClearAndFree;
  StructData.ClearAndFree;
  Factions.ClearAndFree;
end;

{ TTurn }

constructor TTurn.Create(ANum: integer);
begin
  Regions := TRegionList.Create();
end;

destructor TTurn.Destroy;
begin
  Regions.Free;
end;

 { TRegion Methods }

constructor TRegion.Create(X, Y, Z, TurnNum: integer);
begin
  Self.x := X;
  Self.y := Y;
  Self.z := Z;
  Self.Visited := TurnNum;
  Resources := TItemList.Create;
  Junk := TItemList.Create;
  Structs := TStructList.Create;
  Units := TUnitList.Create;
end;

destructor TRegion.Destroy;
begin
  Resources.ClearAndFree;
  Junk.ClearAndFree;
  Structs.ClearAndFree;
  Units.Free;
end;

function TRegion.GetCoords: TCoords;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

procedure TRegion.SetCoords(const Value: TCoords);
begin
  x := Value.x;
  y := Value.y;
  z := Value.z;
end;

{ TMap }

constructor TMap.Create;
begin
  Levels := TMapLevelList.Create;
end;

destructor TMap.Destroy;
begin
  Levels.ClearAndFree;
end;

function TMap.Region(X, Y: integer): TRegion;
begin
  Result := Region(X, Y, Level);
end;

function TMap.Region(C: TCoords): TRegion;
begin
  Result := Region(C.x, C.y, C.z);
end;

function TMap.Region(X, Y, Z: integer): TRegion;
begin
  if (Z < 0) or (Z >= Levels.Count) or (X < 0) or (X >= Levels[Z].Width)
    or (Y < 0) or (Y >= Levels[Z].Height) then
    Result := nil
  else
    Result := Levels[Z].Regions[Y * (Levels[Z].Width div 2) + X div 2];
end;

function TMap.SeekRegion(X, Y, Z: integer): TRegion;
var i: integer;
    lv: TMapLevel;
begin
  if (Z < 0) or (Z >= Levels.Count) or (X < 0) or (X >= Levels[Z].Width)
    or (Y < 0) or (Y >= Levels[Z].Height) then
    Result := nil
  else begin
    lv := Levels[Z];
    i := Y * (lv.Width div 2) + X div 2;
    if (lv.Regions[i] = nil) then begin
      lv.Regions[i] := TRegion.Create(X, Y, Z, Turn.Num);
      Turn.Regions.Add(lv.Regions[i]);
      if (lv.Empty) then
        lv.Bounds := Rect(X, Y, X, Y)
      else begin
        if X < lv.Bounds.Left then lv.Bounds.Left := X;
        if X > lv.Bounds.Right then lv.Bounds.Right := X;
        if Y < lv.Bounds.Top then lv.Bounds.Top := Y;
        if Y > lv.Bounds.Bottom then lv.Bounds.Bottom := Y;
      end;
      lv.Empty := False;
    end;
    Result := lv.Regions[i];
  end;
end;

{ TMapLevelList }

function TMapLevelList.Get(Index: integer): TMapLevel;
begin
  Result := TMapLevel(inherited Get(Index));
end;

procedure TMapLevelList.Put(Index: integer; Item: TMapLevel);
begin
  inherited Put(Index, Item);
end;

function TMapLevelList.NumOf(AName: string): integer;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Name <> AName) do Inc(i);
  if i < Count then Result := i
  else Result := -1;
end;

procedure TMapLevelList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

{ TMapLevel }

function TMapLevel.BoundsWithMargin: TRect;
begin
  Result.Left := Max(Bounds.Left - 2, 0);
  Result.Top := Max(Bounds.Top - 4, 0);
  Result.Right := Max(Bounds.Right, Min(Bounds.Right + 2, Width - 1));
  Result.Bottom := Max(Bounds.Bottom, Min(Bounds.Bottom + 4, Height - 1));
end;

constructor TMapLevel.Create(Name: string; Width, Height: integer);
var i: integer;
begin
  Empty := True;
  Self.Name := Name;
  Self.Width := Width;
  Self.Height := Height;
  Regions := TRegionList.Create();
  for i := 0 to Width * Height div 2 do
    Regions.Add(nil);
end;

destructor TMapLevel.Destroy;
begin
  Regions.ClearAndFree;
end;

{ TFaction }

constructor TFaction.Create;
begin
  Units := TUnitList.Create;
  Color := clWhite;
  Attitude := 0;
end;

destructor TFaction.Destroy;
begin
  Units.ClearAndFree;
end;

{ TUnit }

constructor TUnit.Create;
begin
  Items := TItemList.Create;
  OrigItems := TItemList.Create;
  Skills := TSkillList.Create;
  Events := TStringList.Create;
  Orders := TStringList.Create;
  TStringList(Orders).OnChange := OrdersChanged;
  Insanity := -1;
  HireFee := -1;
end;

destructor TUnit.Destroy;
begin
  Items.ClearAndFree;
  OrigItems.ClearAndFree;
  Skills.ClearAndFree;
  Events.Free;
  Orders.Free;
end;

function TUnit.Order(Index: integer): string;
begin
  if Index >= Orders.Count then Result := ''
  else begin
    Result := LowerCase(TrimLeft(Orders[Index]));
    if Pos('@', Result) = 1 then Result := Copy(Result, 2, Length(Result));
    if Pos(';', Result) > 0 then Result := Copy(Result, 1, Pos(';', Result)-1);
    if Pos(' ', Result) > 0 then Result := Copy(Result, 1, Pos(' ', Result)-1);
  end;
end;

procedure TUnit.OrdersChanged(Sender: TObject);
var j, k: integer;
begin
  // Compact orders
  TStringList(Orders).OnChange := nil;
  j := Orders.Count-1;
  while (j >= 0) do begin
    if (Trim(Orders[j]) = '') then
      Orders.Delete(j);
    Dec(j);
  end;
  TStringList(Orders).OnChange := OrdersChanged;

  // Scan orders and remember found ones in OrdersPresent
  for j := 0 to ordCount-1 do begin
    k := 0;
    while k < Orders.Count do begin
      {// Skip TURN construction
      while Order(k) = 'turn' do begin
        repeat
          Inc(k);
        until (Order(k) = 'endturn') or (k >= Orders.Count);
        Inc(k);
      end;}
      // Check if we found given order
      if Order(k) = OrderNames[j] then begin
        OrdersPresent[j] := True;
        break;
      end;
      Inc(k);
    end;
  end;
end;

procedure TUnit.ResetVirtual;
var i: integer;
    itm: TItem;
begin
  Leader := OrigLeader;
  Struct := OrigStruct;

  Items.ClearItems;
  for i := 0 to OrigItems.Count-1 do begin
    itm := TItem.Create;
    itm.Data := OrigItems[i].Data;
    itm.Amount := OrigItems[i].Amount;
    Items.Add(itm);
  end;
  MonthOrder := '';
end;

{ TRegionList }

procedure TRegionList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].Free;
  Clear;
end;

function TRegionList.Get(Index: Integer): TRegion;
begin
  Result := TRegion(inherited Get(Index));
end;

procedure TRegionList.Put(Index: Integer; Item: TRegion);
begin
  inherited Put(Index, Item);
end;

{ TStruct }

constructor TStruct.Create;
begin
  Installed := TItemList.Create;
  Needs := TItemList.Create;
end;

destructor TStruct.Destroy;
begin
  Installed.Free;
  Needs.Free;
end;

{ TTerrainData }

constructor TTerrainData.Create(AShort: string);
begin
  Short := AShort;
end;

{ TItemData }

constructor TItemData.Create(AShort: string);
begin
  Short := AShort;
end;

{ TItem }

function TItem.Name: string;
begin
  if (Turn.Lang = 'ru') then begin
    if (Amount mod 10 >= 5) or (Amount mod 10 = 0) or
      ((Amount >= 10) and (Amount < 20)) then Result := Data.Name0
    else if (Amount mod 10 = 1) then Result := Data.Name1
    else Result := Data.Name2;
  end
  else begin
    if (Amount = 1) then Result := Data.Name1
    else Result := Data.Name2;
  end;
  if (Result = '') then
    Result := Data.Short;
end;

{ TSkillData }

constructor TSkillData.Create(Short: string);
begin
  Self.Short := Short;
end;

{ TSkill }

function TSkill.Name: string;
begin
  Result := Data.Name;
  if (Result = '') then
    Result := Data.Short;
end;

{ TStructData }

constructor TStructData.Create(Short: string);
begin
  Self.Short := Short;
end;


procedure TTerrainDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TTerrainDataList.Get(Index: integer): TTerrainData;
begin
  Result := TTerrainData(inherited Get(Index));
end;

procedure TTerrainDataList.Put(Index: integer; Item: TTerrainData);
begin
  inherited Put(Index, Item);
end;

function TTerrainDataList.Find(Short: string): TTerrainData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Short <> Short) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TTerrainDataList.Seek(Short: string): TTerrainData;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TTerrainData.Create(Short);
    Add(Result);
  end;
end;

procedure TItemList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

procedure TItemList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

function TItemList.Find(Short: string): TItem;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Data.Short <> Short) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TItemList.Seek(Short: string): TItem;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TItem.Create;
    Result.Data := Game.ItemData.Seek(Short);
    Add(Result);
  end;
end;

function TItemList.Get(Index: integer): TItem;
begin
  Result := TItem(inherited Get(Index));
end;

procedure TItemList.Put(Index: integer; Item: TItem);
begin
  inherited Put(Index, Item);
end;

procedure TItemDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TItemDataList.Get(Index: integer): TItemData;
begin
  Result := TItemData(inherited Get(Index));
end;

procedure TItemDataList.Put(Index: integer; Item: TItemData);
begin
  inherited Put(Index, Item);
end;

function TItemDataList.FindByName(AName: string): TItemData;
begin
{  i := 0;
  while (i < Count) and (Items[i].SingleName <> LowerCase(AName)) and
    (Items[i].MultiName <> LowerCase(AName)) and (Items[i].Short <>
    UpperCase(AName)) and (Items[i].SingleName + 's' <> LowerCase(AName))
    and (Items[i].MultiName <> LowerCase(AName) + 's') do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;}
  Result := nil;
end;

function TItemDataList.Find(Short: string): TItemData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Short <> Short) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TItemDataList.Seek(Short: string): TItemData;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TItemData.Create(Short);
    Add(Result);
  end;
end;

procedure TSkillList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

procedure TSkillList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

function TSkillList.Find(Short: string): TSkill;
var i: integer;
begin
  i := Count-1;
  while (i >= 0) and (Items[i].Data.Short <> Short) do Dec(i);
  if i >= 0 then Result := Items[i]
  else Result := nil;
end;

function TSkillList.Seek(Short: string): TSkill;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TSkill.Create;
    Result.Data := Game.SkillData.Seek(Short);
    Add(Result);
  end;
end;

function TSkillList.Get(Index: integer): TSkill;
begin
  Result := TSkill(inherited Get(Index));
end;

procedure TSkillList.Put(Index: integer; Item: TSkill);
begin
  inherited Put(Index, Item);
end;

function TSkillDataList.Get(Index: integer): TSkillData;
begin
  Result := TSkillData(inherited Get(Index));
end;

procedure TSkillDataList.Put(Index: integer; Item: TSkillData);
begin
  inherited Put(Index, Item);
end;

function TSkillDataList.Find(Short: string): TSkillData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Short <> Short) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TSkillDataList.Seek(Short: string): TSkillData;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TSkillData.Create(Short);
    Add(Result);
  end;
end;

procedure TSkillDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

procedure TStructList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

procedure TStructList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

function TStructList.Find(Num: integer): TStruct;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Num <> Num) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TStructList.Get(Index: integer): TStruct;
begin
  Result := TStruct(inherited Get(Index));
end;

procedure TStructList.Put(Index: integer; Item: TStruct);
begin
  inherited Put(Index, Item);
end;

procedure TStructDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TStructDataList.Get(Index: integer): TStructData;
begin
  Result := TStructData(inherited Get(Index));
end;

procedure TStructDataList.Put(Index: integer; Item: TStructData);
begin
  inherited Put(Index, Item);
end;

function TStructDataList.Find(Short: string): TStructData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Short <> Short) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TStructDataList.Seek(Short: string): TStructData;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TStructData.Create(Short);
    Add(Result);
  end;
end;

procedure TFactionList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TFactionList.Get(Index: integer): TFaction;
begin
  Result := TFaction(inherited Get(Index));
end;

procedure TFactionList.Put(Index: integer; Item: TFaction);
begin
  inherited Put(Index, Item);
end;

function TFactionList.Find(Num: integer): TFaction;
var i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if Items[i].Num = Num then begin
      Result := Items[i];
      break;
    end;
end;

function TFactionList.Seek(Num: integer): TFaction;
begin
  Result := Find(Num);
  if Result = nil then begin
    Result := TFaction.Create;
    Result.Num := Num;
    Add(Result);
  end;
end;

function TUnitList.Find(ANum: integer): TUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Num <> ANum) do Inc(i);
  if i < Count then Result := Items[i];
end;

procedure TUnitList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

procedure TUnitList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

function TUnitList.Get(Index: integer): TUnit;
begin
  Result := TUnit(inherited Get(Index));
end;

procedure TUnitList.Put(Index: integer; Item: TUnit);
begin
  inherited Put(Index, Item);
end;



initialization
  Game := TGame.Create;

finalization
  Game.Free;

end.

