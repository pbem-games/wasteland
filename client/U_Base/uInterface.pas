unit uInterface;

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, DataStructs,
  uResources, PowerGrid, Graphics, Forms;

  procedure FillItemGrid(Grid: TPowerGrid; ItemList: TItemList);
  procedure ItemGridDrawCell(Sender: TObject; ACol, ARow: Integer;
    var TxtRect: TRect; NameCol: integer);
  procedure FillSkillGrid(Grid: TPowerGrid; SkillList: TSkillList);
  procedure SkillGridDrawCell(Sender: TObject; ACol, ARow: Integer;
    var TxtRect: TRect; NameCol: integer);

implementation


procedure AddItemGridItem(Grid: TPowerGrid; Item: TItem; Color: TColor);
var i: integer;
begin
  i := Grid.RowCount;
  if Item.Amount >= 0 then
    Grid.Cells[0, i] := IntToStr(Item.Amount);
  Grid.Cells[1, i] := Item.Name;
  Grid.SortKeys[1, i] := IntToStr(Game.ItemData.IndexOf(Item.Data));
  Grid.Rows[i].Data := Item;
  Grid.Rows[i].Color := Color;
end;

procedure FillItemGrid(Grid: TPowerGrid; ItemList: TItemList);
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to ItemList.Count-1 do
    AddItemGridItem(Grid, ItemList[i], clWindowText);
  Grid.Fixup;
end;

procedure ItemGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; NameCol: integer);
var Item: TItem;
begin
  with Sender as TPowerGrid do begin
    Item := TItem(ImgRows[ARow].Data);
    if (ACol = NameCol) and (ARow >= FixedRows) then begin
      if Item <> nil then
        DrawItemIcon(Canvas, TxtRect.Left+1, TxtRect.Top, Item.Data);
      TxtRect.Left := TxtRect.Left + 18;
    end;
  end;
end;

procedure AddSkillGridItem(Grid: TPowerGrid; Skill: TSkill; Color: TColor);
var i: integer;
begin
  i := Grid.RowCount;
  if (Skill.Level >= 0) then
    Grid.Cells[1, i] := IntToStr(Skill.Level)
  else
    Grid.Cells[1, i] := '?';
  Grid.Cells[0, i] := Skill.Name;
  Grid.Rows[i].Data := Skill;
  Grid.Rows[i].Color := Color;
end;

procedure FillSkillGrid(Grid: TPowerGrid; SkillList: TSkillList);
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to SkillList.Count-1 do
    AddSkillGridItem(Grid, SkillList[i], clWindowText);
  Grid.Fixup;
end;

procedure SkillGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; NameCol: integer);
var Skill: TSkill;
begin
  with Sender as TPowerGrid do begin
    Skill := TSkill(ImgRows[ARow].Data);
    if (ACol = NameCol) and (ARow >= FixedRows) then begin
      if Skill <> nil then
        frmResources.IconList.Draw(Canvas, TxtRect.Left+1, TxtRect.Top, 0);
      TxtRect.Left := TxtRect.Left + 18;
    end;
  end;
end;

end.
