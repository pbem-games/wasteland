unit uAdjustGive;

interface

uses SysUtils, Windows, Classes, DataStructs, MyStrings, Math;

  procedure AddGiveOrder(u: TUnit; Order: string);

implementation

function ParseGiveOrder(Order: string; var Target: string; var Amount: integer;
  var Token: string): boolean;
var t3: string;
begin
  Result := False;
  Target := '';
  try
    GetToken(Order); // give
    Target := LowerCase(GetToken(Order));
    if Target = 'new' then Target := Target + ' ' + GetToken(Order);
    t3 := LowerCase(GetToken(Order));
    if (t3 = 'unit') then Exit;
    if (t3 = 'all') then Amount := -1
    else Amount := StrToInt(t3);
    Token := GetToken(Order);
    Result := True;
  except
  end;
end;

function ModifyGiveOrder(Order: string; Target: string; Amount: integer): string;
var Trace: TTrace;
    amt: string;
begin
  Result := '';
  if Amount = 0 then Exit;
  Trace := TTrace.Create(Order);
  Result := Trace.Before(Target) + Target;
  Trace.Text := TrimLeft(Trace.Text);
  Trace.Before(' ');
  if Amount >= 0 then amt := IntToStr(Amount)
  else amt := 'all';
  Result := Result + ' ' + amt + ' ' + Trace.Text;
  Trace.Free;
end;

// Compares HORS (item) and MOUNT (item-type) objects for give operations
function SameGiveTokens(s1, s2: string; var Exact: boolean): boolean;
var Data1, Data2: TItemData;
begin
  Exact := False;
  if s1 = s2 then begin
    Result := True;
    Exact := True;
  end
  else begin
    Data1 := Game.ItemData.FindByName(s1);
    Data2 := Game.ItemData.FindByName(s2);
    if (Data1 <> nil) and (Data2 <> nil) and (Data1.Short = Data2.Short) then begin
      Result := True;
      Exact := True;
    end
    else
      Result := false;
  end;
end;

function ItemAmount(list: TItemList; Name: string): integer;
var Item: TItem;
    IData: TItemData;
begin
  Result := 0;
  IData := Game.ItemData.FindByName(Name);
  if IData <> nil then begin
    Item := list.Find(IData.Short);
    if Item <> nil then Result := Item.Amount;
  end;
end;

function ActualAmount(AUnit: TUnit; Name: string): integer;
begin
  Result := ItemAmount(AUnit.Items, Name);
end;

function RealAmount(AUnit: TUnit; Name: string): integer;
begin
  Result := ItemAmount(AUnit.OrigItems, Name);
end;

// Redirect gives from other units to this order's target; modify order
procedure AdjustItemGive(Giver: TUnit; var Order: string);
var i, j, amt, amt1: integer;
    s, target, target1, token, token1: string;
    u: TUnit;
    order_giveall, was_giveall, exact: boolean;
begin
  if not ParseGiveOrder(Order, target, amt, token) then Exit;
  // If order is GIVE ALL, get actual value for calculations
  order_giveall := (amt = -1);
  if order_giveall then amt := ActualAmount(Giver, token);
  // Scan other units for GIVE orders to current unit
  i := 0;
  while (i < Giver.Region.Units.Count) and (amt > 0) do begin
    u := Giver.Region.Units[i];
    if (u <> Giver) and (u.Faction = Turn.PlayerFaction) then begin
      j := 0;
      while (j < u.Orders.Count) and (amt > 0) do begin
        if (u.Order(j) = 'give') then begin
          // Parse Give order
          if ParseGiveOrder(u.Orders[j], target1, amt1, token1) then begin
            if (target1 = IntToStr(Giver.Num))
              and SameGiveTokens(token1, token, exact) then begin
              was_giveall := (amt1 = -1);
              if was_giveall then amt1 := RealAmount(u, token);
              // Give items to Giver's order target
              if target <> IntToStr(u.Num) then begin
                s := ModifyGiveOrder(Order, target, Min(amt1, amt));
                u.Orders.Insert(j, s);
                Inc(j);
              end;
              // Do not give them to Giver
              if not was_giveall then begin
                if amt >= amt1 then begin
                  u.Orders.Delete(j);
                  Dec(j);
                end
                else begin
                  u.Orders[j] := ModifyGiveOrder(u.Orders[j],
                    target1, amt1 - amt);
                end;
              end;
              if amt1 <> -1 then amt := Max(0, amt - amt1)
              else amt := -1;
            end;
          end;
        end;
        Inc(j);
      end;
    end;
    Inc(i);
  end;
  if amt = 0 then Order := ''
  else if not order_giveall then Order := ModifyGiveOrder(Order, target, amt);
end;

procedure AddGiveOrder(u: TUnit; Order: string);
begin
  AdjustItemGive(u, Order);
  if (Order <> '') then
    u.Orders.Add(Order);
end;

end.
