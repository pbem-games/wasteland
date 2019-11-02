unit uFactions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ImgList, DataStructs, StdCtrls, ComCtrls, ExtCtrls, PowerGrid,
  Buttons, ColorBtn, uResources, uLocalize;

const
  GridCols = 3;
  GridHeaders: array[0..GridCols-1] of string = ('Faction', 'Num', 'Attitude');
  GridFormats: array[0..GridCols-1] of TColFormat = (cfString, cfNumber, cfString);

type
  TfrmFactions = class(TForm)
    GroupBox1: TGroupBox;
    lLAttitude: TLabel;
    cmAttitude: TComboBox;
    lLColor: TLabel;
    Button2: TButton;
    FactionGrid: TPowerGrid;
    ColorDialog: TColorDialog;
    ColorBtn: TColorBtn;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FactionGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cmAttitudeChange(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure FactionGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
  private
    FOrders: string;
    procedure FillFactions;
    procedure DeclareAttitude(Target: string; Attitude: string);
  public
    function GetOrders: string;
  end;

var
  frmFactions: TfrmFactions;


implementation

{$R *.DFM}


procedure TfrmFactions.FormCreate(Sender: TObject);
var i: integer;
begin
  // Setup grid
  for i := 0 to GridCols-1 do begin
    FactionGrid.Cells[i, 0] := GridHeaders[i];
    FactionGrid.Cols[i].Format := GridFormats[i];
  end;
  Localize(Self);
  FillFactions;
end;

procedure TfrmFactions.FillFactions;
var row, i: integer;
    f: TFaction;
begin
  FactionGrid.RowCount := 0;
  row := 1;
  for i := 0 to Game.Factions.Count-1 do begin
    f := Game.Factions[i];
    FactionGrid.Cells[0, row] := f.Name;
    FactionGrid.Cells[1, row] := IntToStr(f.Num);
    if (f.Attitude > 0) then
      FactionGrid.Cells[2, row] := cmAttitude.Items[f.Attitude-1]
    else
      FactionGrid.Cells[2, row] := cmAttitude.Items[Turn.PlayerFaction.Attitude-1];
    FactionGrid.Rows[row].Data := f;
    FactionGrid.Rows[row].Color := f.Color;
    Inc(row);
  end;
  FactionGrid.Fixup;
end;

function TfrmFactions.GetOrders: string;
begin
  Result := FOrders;
end;

procedure TfrmFactions.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmFactions.FactionGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var f: TFaction;
begin
  f := TFaction(FactionGrid.ImgRows[ARow].Data);
  if f.Attitude > 0 then cmAttitude.ItemIndex := f.Attitude - 1
  else cmAttitude.ItemIndex := Turn.PlayerFaction.Attitude - 1;
  if (f.Num = Turn.PlayerFaction.Num) or (f.Num = 0) then begin
    lLAttitude.Caption := Lang.ReadString('Messages', 'DefaultAttitude', 'Default Attitude:');
  end
  else begin
    lLAttitude.Caption := Lang.ReadString('Messages', 'Attitude', 'Attitude:');
  end;
end;

procedure TfrmFactions.cmAttitudeChange(Sender: TObject);
var f: TFaction;
begin
  f := TFaction(FactionGrid.ImgRows[FactionGrid.Row].Data);
  f.Attitude := cmAttitude.ItemIndex + 1;
  if (f = Turn.PlayerFaction) then
    DeclareAttitude('default', cmAttitude.Text)
  else
    DeclareAttitude(IntToStr(f.Num), cmAttitude.Text);
  FillFactions;
end;

procedure TfrmFactions.DeclareAttitude(Target: string; Attitude: string);
begin
  FOrders := FOrders + 'declare ' + Target + ' ' + Attitude + #13#10;
end;

procedure TfrmFactions.ColorBtnClick(Sender: TObject);
var Fac: TFaction;
begin
  if (FactionGrid.RowCount = FactionGrid.FixedRows) then
    Exit;
  Fac := TFaction(FactionGrid.ImgRows[FactionGrid.Row].Data);
  Fac.Color := ColorBtn.Color;
  FactionGrid.ImgRows[FactionGrid.Row].Color := Fac.Color;
end;

procedure TfrmFactions.FactionGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
{  with TPowerGrid(Sender) do
    if (ACol = 3) and (ARow > 0) then begin
      FData := TFaction(ImgRows[ARow].Data).Data;
      if (FData.TurnNum < Turn.Num) and (FData.EMail <> '') then begin
        ResForm.Extras.Draw(Canvas, TxtRect.Right - 8, TxtRect.Top + 1,
           bmp_extUnknown);
        Dec(TxtRect.Right, 10);
      end;
    end;}
end;

end.
