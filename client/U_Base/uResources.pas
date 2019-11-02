unit uResources;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, ImgList, DataStructs, StdCtrls, Registry, Math, MyStrings;

const
  IconsFolder = 'Images\Icons\';
  GamesFolder = 'Games\';
  TerrainFolder = 'Images\Terrain\';
  BattleFolder = 'Images\Persons\';

  clNether = $00000008;
  extFlag = 0;
  extBuilding = 2;
  BattleImgDim = 20;

type
  TfrmResources = class(TForm)
    Terrains: TImageList;
    Label1: TLabel;
    BtnImages: TImageList;
    Label2: TLabel;
    IconList: TImageList;
    Label3: TLabel;
    Extras: TImageList;
    Label4: TLabel;
    SmallImages: TImageList;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    procedure LoadTerrainImages;
  end;

var
  frmResources: TfrmResources;
  BaseDir: string;
  Config, Lang, BattleIni: TMemIniFile;
  Icons: TStrings;
  BattleImgCache: TStringList;

  function GameFile: string;
  procedure DrawItemIcon(Canvas: TCanvas; X, Y: integer; AItemData: TItemData);
  procedure DrawTranspText(ACanvas: TCanvas; x, y: integer; s: string;
    MaxWidth: integer; Shadow: boolean);
  procedure DrawCExtra(Index: integer; AFaction: TFaction; ACanvas: TCanvas;
    x, y: integer);
  procedure DrawBattleImage(Cnv: TCanvas; X, Y: integer; Filename: string;
    Color: TColor; Flip: boolean);
  function GetBattleImage(u: TUnit; Section: string): string;
  function ShiftPressed: boolean;
  function AltPressed: boolean;
  function CtrlPressed: boolean;


implementation

{$R *.dfm}

function ShiftPressed: boolean;
begin
  Result := (GetKeyState(VK_SHIFT) and $80 <> 0);
end;

function AltPressed: boolean;
begin
  Result := (GetKeyState(VK_MENU) and $80 <> 0);
end;

function CtrlPressed: boolean;
begin
  Result := (GetKeyState(VK_CONTROL) and $80 <> 0);
end;

function GameFile: string;
begin
  Result := Config.ReadString('Main', 'Game', '');
  if (Copy(Result, 2, 1) <> ':') then
    Result := StringReplace(BaseDir + Result, '\\', '\', []);
end;

function ItemIconIndex(it: TItemData): integer;
begin
  Result := 0;
end;

procedure DrawExternalImage(Filename: string; ACanvas: TCanvas;
  X, Y: integer; clTransp: TColor);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    if (FileExists(BaseDir + Filename)) then
      Bmp.LoadFromFile(BaseDir + Filename);
  except
    on EFOpenError do FreeAndNil(Bmp);
  end;
  if Bmp <> nil then begin
    Bmp.Transparent := True;
    if clTransp <> -1 then Bmp.TransparentColor := clTransp;
    ACanvas.Draw(X, Y, Bmp);
    Bmp.Free;
  end;
end;

function FindIcon(Short: string): boolean;
var i: integer;
begin
  i := 0;
  while (i < Icons.Count) and (LowerCase(Icons[i]) <> LowerCase(Short)) do
    Inc(i);
  Result := (i < Icons.Count);
end;

procedure DrawItemIcon(Canvas: TCanvas; X, Y: integer; AItemData: TItemData);
begin
  if FindIcon(AItemData.Short) then
    DrawExternalImage(IconsFolder + AItemData.Short + '.bmp', Canvas, X, Y, -1)
  else
    frmResources.IconList.Draw(Canvas, X, Y, ItemIconIndex(AItemData));
end;

function GetMaskedBmp(List: TImageList; Index: integer; Color: TColor): TBitmap;
var Mask, Image: TBitmap;
    clTransp: TColor;
begin
  if Color = clBlack then Color := clBlack + 8;
  if Color <> clFuchsia then clTransp := clFuchsia
  else clTransp := clOlive;
  // Draw color mask over extra image
  Image := TBitmap.Create;
  List.BkColor := clTransp;
  List.GetBitmap(Index*2, Image);
  Mask := TBitmap.Create;
  List.BkColor := Color;
  List.GetBitmap(Index*2 + 1, Mask);
  Mask.Transparent := TRUE;
  Image.Canvas.Draw(0, 0, Mask);
  Image.Transparent := TRUE;
  // Drop image onto canvas
  Mask.Free;
  List.BkColor := clNone;
  Result := Image;
end;

procedure DrawCExtra(Index: integer; AFaction: TFaction; ACanvas: TCanvas;
  x, y: integer);
var bmp: TBitmap;
begin
  bmp := GetMaskedBmp(frmResources.Extras, Index, AFaction.Color);
  ACanvas.Draw(x, y, bmp);
end;

procedure DrawFlipped(Cnv: TCanvas; X, Y: integer; Bmp: TBitmap; Flip: boolean);
var Bmp1: TBitmap;
    i: integer;
begin
  if Flip then begin
    Bmp1 := TBitmap.Create;
    Bmp1.Width := Bmp.Width;
    Bmp1.Height := Bmp.Height;
    for i := 0 to Bmp.Width-1 do
      Bmp1.Canvas.CopyRect(Rect(i, 0, i+1, Bmp1.Height), Bmp.Canvas,
        Rect(Bmp.Width-i - 1, 0, Bmp.Width-i, Bmp.Height));
    Bmp1.Transparent := True;
    Cnv.Draw(x, y, Bmp1);
    Bmp1.Free;
  end
  else begin
    Bmp.Transparent := TRUE;
    Cnv.Draw(X, Y, Bmp);
  end;
end;

function GetBattleImage(u: TUnit; Section: string): string;
var Trace: TTrace;
    Lines: TStrings;
    i, j: integer;
    t: string;
begin
  Result := '';
  Lines := TStringList.Create;
  // Get all names from INI file
  BattleIni.ReadSection(Section, Lines);
  i := 0;
  while (i < Lines.Count) and (Result = '') do begin
    j := u.Items.Count-1;
    while (j >= 0) and (u.Items[j].Data.Short <> Lines[i]) do Dec(j);
    if (j >= 0) then
      Result := BattleIni.ReadString(Section, Lines[i], '');
    Inc(i);
  end;
  if Result = '' then
    Result := BattleIni.ReadString(Section, 'default', '');
  Lines.Free;

  // Resolve .styles in Result
  Trace := TTrace.Create(Result);
  Result := '';
  while not Trace.Ends do begin
    t := Trace.Before(' ');
    if Pos('.', t) = 1 then t := GetBattleImage(u, t);
    Result := Trim(Result + ' ' + t);
  end;
  Trace.Free;
end;

procedure DrawBattleImage(Cnv: TCanvas; X, Y: integer; Filename: string;
  Color: TColor; Flip: boolean);
var Bmp, MaskBmp: TBitmap;
    s, img, mask: string;
    Trace: TTrace;
    idx: integer;

  function CreateUnitBmp(Fill: TColor): TBitmap;
  begin
    Result := TBitmap.Create;
    Result.Width := BattleImgDim;
    Result.Height := BattleImgDim;
    Result.Canvas.Brush.Color := Fill;
    Result.Canvas.FillRect(Result.Canvas.ClipRect);
  end;

begin
  // Load custom graphics
  idx := BattleImgCache.IndexOf(Filename);
  if (idx >= 0) then
    Bmp := TBitmap(BattleImgCache.Objects[idx])
  else begin
    Bmp := CreateUnitBmp(clNether);
    Trace := TTrace.Create(Filename);
    while not Trace.Ends do begin
      s := Trace.Before(' ');
      if s <> '' then begin
        if Pos('+', s) > 0 then begin
          mask := Trim(Copy(s, Pos('+', s)+1, Length(s)));
          img := Trim(Copy(s, 1, Pos('+', s)-1));
          DrawExternalImage(BattleFolder + img, Bmp.Canvas, 0, 0, -1);
          MaskBmp := CreateUnitBmp(Color);
          DrawExternalImage(BattleFolder + mask, MaskBmp.Canvas, 0, 0, clWhite);
          MaskBmp.Transparent := True;
          Bmp.Canvas.Draw(0, 0, MaskBmp);
          MaskBmp.Free;
        end
        else
          DrawExternalImage(BattleFolder + s, Bmp.Canvas, 0, 0, -1);
      end;
    end;
    Trace.Free;

    BattleImgCache.AddObject(Filename, Bmp);
  end;
  DrawFlipped(Cnv, X, Y, Bmp, Flip);
end;

procedure DrawTranspText(ACanvas: TCanvas; x, y: integer; s: string;
  MaxWidth: integer; Shadow: boolean);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.Canvas.Font.Assign(ACanvas.Font);
  // Add ...
  if Bmp.Canvas.TextExtent(s).cx > MaxWidth then begin
    while (s <> '') and (Bmp.Canvas.TextExtent(s + '...').cx > MaxWidth) do
      s := Copy(s, 1, Length(s) - 1);
    s := s + '...';
  end;
  // Draw
  Bmp.Width := Min(Bmp.Canvas.TextExtent(s).cx, MaxWidth) + 3;
  Bmp.Height := Bmp.Canvas.TextExtent(s).cy + 3;
  if Shadow then begin
    Bmp.Canvas.Font.Color := clBlack;
    DrawTranspText(Bmp.Canvas, 1, 0, s, MaxWidth, False);
    DrawTranspText(Bmp.Canvas, 1, 2, s, MaxWidth, False);
    DrawTranspText(Bmp.Canvas, 0, 1, s, MaxWidth, False);
    DrawTranspText(Bmp.Canvas, 2, 1, s, MaxWidth, False);
    Bmp.Canvas.Font.Color := ACanvas.Font.Color;
    DrawTranspText(Bmp.Canvas, 1, 1, s, MaxWidth, False);
  end
  else Bmp.Canvas.TextOut(1, 1, s);
  Bmp.Transparent := TRUE;
  ACanvas.Draw(x, y, Bmp);
  Bmp.Free;
end;


procedure TfrmResources.FormCreate(Sender: TObject);
var langname: string;
    lbuf: pointer;
    sr: TSearchRec;
begin
  BaseDir := ExtractFilePath(Application.ExeName);
  Config := TMemIniFile.Create(BaseDir + 'config.ini');
  BattleIni := TMemIniFile.Create(BaseDir + BattleFolder + 'battle.ini');
  BattleImgCache := TStringList.Create;
  Icons := TStringList.Create;

  // Icons
  if FindFirst(BaseDir + IconsFolder + '*.bmp', faAnyFile, sr) = 0 then begin
    Icons.Add(UpperCase(Copy(sr.Name, 1, Pos('.', sr.Name)-1)));
    while FindNext(sr) = 0 do
      Icons.Add(UpperCase(Copy(sr.Name, 1, Pos('.', sr.Name)-1)));
  end;
  FindClose(sr);

  // Languages
  if Config.ReadString('Main', 'Language', '') = '' then begin
    // Get user language on first start
    GetMem(lbuf, 100);
    langname := '';
    if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SENGLANGUAGE, lbuf, 1000)
      <> 0 then langname := PChar(lbuf);
    FreeMem(lbuf);
    Config.WriteString('Main', 'Language', langname);
  end;
  Lang := TMemIniFile.Create(BaseDir + 'Language\' +
    Config.ReadString('Main', 'Language', '') + '.lng');
end;

procedure TfrmResources.FormDestroy(Sender: TObject);
var i: integer;
begin
  Config.UpdateFile;
  Config.Free;
  BattleIni.Free;
  for i := 0 to BattleImgCache.Count-1 do
    TBitmap(BattleImgCache.Objects[i]).Free;
  BattleImgCache.Free;
  Lang.Free;
  Icons.Free;
end;

procedure TfrmResources.LoadTerrainImages;
var Ini: TMemIniFile;
    i: integer;
    s: string;
    DefColor: TColor;

  procedure AddTerrain(Filename: string);
  var Bmp: TBitmap;
  begin
    Bmp := TBitmap.Create;
    Bmp.LoadFromFile(Filename);
    Terrains.AddMasked(Bmp, Bmp.TransparentColor);
    Bmp.Free;
  end;

begin
  Ini := TMemIniFile.Create(BaseDir + TerrainFolder + 'terrain.ini');
  for i := 0 to Game.TerrainData.Count-1 do begin
    // Main terrain
    s := Ini.ReadString('Images', Game.TerrainData[i].Short, '');
    if s <> '' then begin
      AddTerrain(BaseDir + TerrainFolder + s);
      Game.TerrainData[i].BmpIndex := Terrains.Count-1;
    end;
    // Terrain color for minimap
    DefColor := Config.ReadInteger('TerrainColors', 'unknown', clWhite);
    Game.TerrainData[i].Color := Config.ReadInteger('TerrainColors',
      Game.TerrainData[i].Name, DefColor);
  end;
  Ini.Free;
end;


end.
