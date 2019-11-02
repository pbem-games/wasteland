unit uOrders;

interface

uses
  SysUtils, Classes, DataStructs, uResources;

  procedure CompileOrder(Lines: TStrings; Compact: boolean);

implementation

type
  TDelims = set of char;

function StrQuotes(s: string; var i: integer; Fail: boolean;
  Delims: TDelims): string;
var delim: char;
begin
  if i > Length(s) then Result := ''
  else begin
    Result := s[i];
    Inc(i);
    if s[i-1] in Delims then begin
      delim := s[i-1];
      while (i <= Length(s)) and (s[i] <> delim) do begin
        Result := Result + s[i];//StrQuotes(s, i, Fail, Delims);
        Inc(i);
      end;
      if i <= Length(s) then begin
        Result := Result + delim;
        Inc(i);
      end
      else if Fail then
        raise EConvertError.Create('Closing quote expected');
    end;
  end;
end;

function Uncomment(s: string): string;
var i: integer;
begin
  Result := '';
  i := 1;
  while (i <= Length(s)) and (s[i] <> ';') do
    Result := Result + StrQuotes(s, i, False, ['"']);
end;

function MakeRegionName(C: TCoords): string;
var Region: TRegion;
begin
  Result := IntToStr(C.X) + ',' + IntToStr(C.Y);
  Region := Map.Region(C);
  if Region <> nil then
    Result := Region.Terrain.Name + ' (' + Result + '), ' + Region.Land
end;

procedure CompileOrder(Lines: TStrings; Compact: boolean);
var i, k, n: integer;
    s: string;
    U: TUnit;
    R: TRegion;
    RegComment: boolean;

  function CompactOrder(Order, s: string): string;
  const CnvItems: array[0..9] of string = ('give', 'produce', 'buy', 'sell',
          'steal', 'claim', 'exchange', 'armor', 'weapon', '@;needs');
        CnvSkills: array[0..3] of string = ('study', 'cast', 'combat', 'forget');
  begin
    // Strip comments
    s := TrimLeft(s);
    if Pos('@;', s) = 1 then s := '@;' + Uncomment(Copy(s, 3, Length(s)))
    else s := Uncomment(s);

    Result := Trim(s);
  end;

begin
  Lines.Add('; turn ' + IntToStr(Turn.Num));
  Lines.Add('#orders ' + IntToStr(Turn.PlayerFaction.Num) + ' "' + Turn.Password + '"');
  Lines.Add('');
  for n := 0 to Turn.Regions.Count-1 do begin
    R := Turn.Regions[n];

    RegComment := true;
    for i := 0 to R.Units.Count-1 do begin
      U := R.Units[i];
      if (U.Faction <> Turn.PlayerFaction) then Continue;

      // Region comment
      if RegComment and not Compact then begin
        Lines.Add('');
        s := '; ' + MakeRegionName(R.Coords);
        Lines.Add(s);
        Lines.Add('');
        RegComment := false;
      end;

      // Unit
      s := 'person ' + IntToStr(U.Num);
      if not Compact then s := s + '; ' + U.Name;
      Lines.Add(s);
      for k := 0 to U.Orders.Count-1 do begin
        if Compact then s := CompactOrder(U.Orders[k], U.Orders[k])
        else s := U.Orders[k];
        if s <> '' then Lines.Add(TrimLeft(s));
      end;
      Lines.Add('');
    end;
  end;
  Lines.Add('#end');
end;

end.
