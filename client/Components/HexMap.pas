
 //  Hexagonal DrawGrid component
 //  (C) Wredosoft Software LTD

unit HexMap;

interface

uses Windows, Messages, Classes, Graphics, Forms, Controls, ExtCtrls, Math;

const
  KeyStep = 10;
  crHand = 1;

  dirN = 1;
  dirNE = 2;
  dirSE = 3;
  dirS = 4;
  dirSW = 5;
  dirNW = 6;

type
  THexMapOptions = set of (hmGrid, hmShowSelection, hmDblClickCenter,
    hmRightBtnCenter, hmRightBtnDrag);
  THexMapState = set of (msDblClick, msRedraw, msNoPaint);
  THexMapDrawState = set of (dsSelected);
  THexParts = array[1..6] of boolean;

  TIterationHandler = procedure(HX, HY: integer) of object;

  TBeforePaintEvent = procedure (Sender: TObject; ACanvas: TCanvas) of object;
  TDrawHexEvent = procedure (Sender: TObject; HX, HY: integer; ACanvas: TCanvas;
    CX, CY: integer; AState: THexMapDrawState) of object;
  TDrawExtraEvent = procedure (Sender: TObject; HX, HY: integer; ACanvas: TCanvas;
    CX, CY: integer; AState: THexMapDrawState) of object;
  TMoveMapEvent = procedure (Sender: TObject; X, Y: integer) of object;
  TSelectHexEvent = procedure (Sender: TObject; HX, HY: integer) of object;

  { THexMap }

  { THexMap is a hexagonal grid that can be used to implement grid-like
    behaviour onto hexagonal cells. It handles scrolling, drawing and all
    the base stuff and lets user or derived class fill the hexagons.

    Terms used:
      Hex - the hexagon, as it appears on the screen.
      Extra - any extra drawind on the hex, can even pour outside its limits.
      Cell - rectangle containing hex and all of its extras.
      Map - the whole logical picture, containing all hexes.

    Hex coords are calculated in following manner (depending on FirstOdd):

    FALSE ____        ____
        /    \      /    \
       /(0,0) \____/(2,0) \____/
       \      /    \      /    \
        \____/(1,1) \____/(3,1) \_
        /    \      /    \      /
       /(0,2) \____/(2,2) \____/

    TRUE    ____        ____
           /    \      /    \
      ____/(1,0) \____/(3,0) \____/
     /    \      /    \      /    \
    /(0,1) \____/(2,1) \____/(4,1) \
    \      /    \      /    \
     \____/(1,2) \____/

      CalcOffCoords
        Calculates coords of hex center onto offscreen bitmap.
      CalcHexCoords
        Same, onto component canvas.
      CalcHexPos
        Same, onto map.
      CalcRelHex
        Converts point onto map to relative hex coords. That is, point on the
        center of hex (0,0) will be (0,0), point slightly moved SE - (0.1,0.1).
      CellRect
        Returns rectangle occupied by given cell onto offscreen bitmap.
      CellVisible
        Returns true if any part of cell is visible onto screen.
      CellWidth, CellHeight
        Cell dimensions. Cannot be less than hex dimensions.
      Center
        Centers view around given hex.
      ColCount, RowCount
        Number of hexes.
      FirstOdd
        Indicates, whether the first column of map has an odd or even number.
      GridWidth, GridHeight
        Returns map dimensions.
      Hex
        Draws hexagon onto specified canvas.
      HexSize
        Width of the hex left to right. Setting this property also changes
        consts hs, hs25, hs50, hs75, hs43, where hs25 is 0.25*hs and so on.
      MouseToHex
        Calculates hex coords from control's canvas coords.
      MoveMap
        Moves map view to specified position.
      MapRect
        Very important property, contains rectangle in map coords which is
        currently shown onto screen. When MapRect changes, visible cells
        and drawing offset are recalculated. DrawOffset are needed to center
        small map onto big control canvas.
      Margin
        Indicates the width of map margin.
      Options
        hmGrid:            Draw grid lines
        hmShowSelection:   Draw selection hex
        hmDblClickCenter:  Center map on double click
        hmRightBtnDrag:    Drag map with right button
        hmRightBtnCenter:  Center on right-click. Removes two preceding.
      PartialHex
        Same as Hex, but can draw only specified lines. Params are [S,SE,NE]
        and so on.
      Redraw
        Completely Redraws map.
      RedrawCell
        Redraws one cell.
      Selected
        Coords of selected cell.
      VisCells
        Rectangle of cells currently visible onto screen.
      VisHexes
        Same for hexes.
  }

  THexMap = class(TCustomControl)
  private
    FOnBeforePaint: TBeforePaintEvent;
    FOnDrawHex: TDrawHexEvent;
    FOnDrawExtra: TDrawExtraEvent;
    FOnMoveMap: TMoveMapEvent;
    FOnSelectHex: TSelectHexEvent;

    FCellWidth: integer;
    FCellHeight: integer;
    FExtraSpan: TPoint; // Neibor hexes affected by cell extras
    FFirstOdd: boolean;
    FGridColor: TColor;
    FMapRect: TRect; // Visible rectangle on the global map image
    FMargin: integer;
    FOptions: THexMapOptions;
    FRowCount, FColCount: integer;
    FSelected: TPoint;
    FSelectColor: TColor;
    FVisCells: TRect; // Cells currently visible (partially, maybe)
    FVisHexes: TRect; // Hexes currently visible
    DrawOffset: TPoint;
    DrawSelect: boolean;
    LastX, LastY: integer;
    FMapState: THexMapState;
    OffScreen: TBitmap;
    OldHexes: TRect;
    StoredCursor: TCursor;
    function GetCursor: TCursor;
    procedure IterateHexes(HexRect: TRect; Handler: TIterationHandler);
    procedure IHDrawHex(HX, HY: integer);
    procedure IHDrawNewHex(HX, HY: integer);
    procedure IHDrawNewCell(HX, HY: integer);
    procedure RecalcExtraSpan;
    procedure RecalcMapRect;
    procedure SetCellWidth(Value: integer);
    procedure SetCellHeight(Value: integer);
    procedure SetColCount(Value: integer);
    procedure SetCursor(Value: TCursor);
    procedure SetFirstOdd(Value: boolean);
    procedure SetGridColor(Value: TColor);
    procedure SetHexSize(Value: integer);
    procedure SetMapRect(Value: TRect);
    procedure SetMargin(Value: integer);
    procedure SetOptions(Value: THexMapOptions);
    procedure SetRowCount(Value: integer);
    procedure SetSelected(Value: TPoint);
    procedure SetSelectColor(Value: TColor);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure BeforePaint; dynamic;
    function DoKeyDown(var Message: TWMKey): Boolean;
    procedure DrawHex(HX, HY: integer); dynamic;
    procedure DrawExtra(HX, HY: integer); dynamic;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure SelectHex(HX, HY: integer); dynamic;
  public
    hs, hs25, hs50, hs75, hs43: integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcOffCoords(HX,HY: integer; var CX,CY: integer);
    procedure CalcHexCoords(HX,HY: integer; var CX,CY: integer);
    procedure CalcHexPos(HX,HY: integer; var CX,CY: integer);
    procedure CalcRelHex(X, Y: integer; var HX, HY: real);
    function CellRect(HX,HY: integer): TRect;
    function CellVisible(HX, HY: integer): boolean;
    procedure Center(HX,HY: integer);
    property Cursor read GetCursor write SetCursor;
    function GridWidth: integer;
    function GridHeight: integer;
    procedure Hex(Canvas: TCanvas; CX,CY: integer);
    procedure MoveMap(X,Y: integer); dynamic;
    function MouseToHex(X, Y: integer): TPoint;
    property MapRect: TRect read FMapRect write SetMapRect;
    procedure PartialHex(Canvas: TCanvas; CX,CY: integer; Parts: THexParts);
    procedure Redraw;
    procedure RedrawCell(HX, HY: integer);
    property Selected: TPoint read FSelected write SetSelected;
    property MapState: THexMapState read FMapState write FMapState;
    property VisCells: TRect read FVisCells;
    property VisHexes: TRect read FVisHexes;
  published
    property Align;
    property Color;
    property Constraints;
    property ColCount: integer read FColCount write SetColCount default 12;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FirstOdd: boolean read FFirstOdd write SetFirstOdd default FALSE;
    property GridColor: TColor read FGridColor write SetGridColor default clBtnShadow;
    property Height;
    property HexSize: integer read hs write SetHexSize;
    property CellWidth: integer read FCellWidth write SetCellWidth;
    property CellHeight: integer read FCellHeight write SetCellHeight;
    property Margin: integer read FMargin write SetMargin;
    property Options: THexMapOptions read FOptions write SetOptions
      default [hmGrid, hmShowSelection, hmDblClickCenter, hmRightBtnDrag];
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount: integer read FRowCount write SetRowCount default 12;
    property SelectColor: TColor read FSelectColor write SetSelectColor default clRed;
    property ShowHint;
    property TabStop;
    property Visible;
    property Width;

    property OnBeforePaint: TBeforePaintEvent read FOnBeforePaint write FOnBeforePaint;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawHex: TDrawHexEvent read FOnDrawHex write FOnDrawHex;
    property OnDrawExtra: TDrawExtraEvent read FOnDrawExtra write FOnDrawExtra;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectHex: TSelectHexEvent read FOnSelectHex write FOnSelectHex;
    property OnStartDrag;
    property OnMoveMap: TMoveMapEvent read FOnMoveMap write FOnMoveMap;
  end;

 procedure Register;

implementation

{$R cursors.res}

procedure Register;
begin
  RegisterComponents('Samples', [THexMap]);
end;

  { THexMap }

constructor THexMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed];
  MapState := MapState + [msRedraw];
  Screen.Cursors[crHand] := LoadCursor(HInstance, 'HAND');
  OffScreen := TBitmap.Create;
  FRowCount := 12;
  FColCount := 12;
  FGridColor := clBtnShadow;
  FOptions := [hmGrid, hmShowSelection, hmDblClickCenter, hmRightBtnDrag];
  FSelectColor := clRed;
  FMargin := 15;
  StoredCursor := Cursor;
  HexSize := 48;
  FCellWidth := hs;
  FCellHeight := 2*hs43;
  SetBounds(0, 0, 300, 150);
end;

destructor THexMap.Destroy;
begin
  OffScreen.Free;
  inherited Destroy;
end;

procedure THexMap.BeforePaint;
begin
  if Assigned(FOnBeforePaint) then FOnBeforePaint(Self, OffScreen.Canvas);
end;

function THexMap.DoKeyDown(var Message: TWMKey): Boolean;
begin
  inherited DoKeyDown(Message);
  Result := FALSE;
end;

{ Calculate coords of hex center on the offscreen bitmap }
procedure THexMap.CalcOffCoords(HX,HY: integer; var CX,CY: integer);
begin
  CalcHexPos(HX,HY,CX,CY);
  CX := CX - FMapRect.Left + DrawOffset.X + hs;
  CY := CY - FMapRect.Top + DrawOffset.Y + hs43*2;
end;

{ Calculate coords of hex center on the control's canvas }
procedure THexMap.CalcHexCoords(HX,HY: integer; var CX,CY: integer);
begin
  CalcHexPos(HX,HY,CX,CY);
  CX := CX - FMapRect.Left + DrawOffset.X;
  CY := CY - FMapRect.Top + DrawOffset.Y;
end;

{ Calculate position of hex center on the map }
procedure THexMap.CalcHexPos(HX,HY: integer; var CX,CY: integer);
begin
  CX := HX * hs75 + hs50 + FMargin;
  CY := HY * hs43 + hs43 + FMargin;
end;

{ Calculate hex-relative map position }
procedure THexMap.CalcRelHex(X, Y: integer; var HX, HY: real);
begin
  HX := (X - hs50 - FMargin) / hs75;
  HY := (Y - hs43 - FMargin) / hs43;
end;

{ Calculate cell rect on offscreen bmp from hex coords }
function THexMap.CellRect(HX,HY: integer): TRect;
var cx,cy: integer;
begin
  CalcOffCoords(HX, HY, cx, cy);
  Result := Rect(cx - CellWidth div 2, cy - CellHeight div 2,
    cx + CellWidth div 2, cy + CellHeight div 2);
end;

function THexMap.CellVisible(HX, HY: integer): boolean;
begin
  if (HX >= VisCells.Left) and (HX <= VisCells.Right) and
    (HY >= VisCells.Top) and (HY <= VisCells.Bottom) then
    Result := TRUE
  else Result := FALSE;
end;

procedure THexMap.Center(HX,HY: integer);
var cx,cy: integer;
begin
  CalcHexPos(HX, HY, cx, cy);
  MoveMap(cx - (FMapRect.Right - FMapRect.Left) div 2,
    cy - (FMapRect.Bottom - FMapRect.Top) div 2);
end;

procedure THexMap.DblClick;
begin
  MapState := MapState + [msDblClick];
  inherited DblClick;
  if hmDblClickCenter in FOptions then Center(FSelected.X, FSelected.Y);
end;

procedure THexMap.DrawHex(HX, HY: integer);
var cx,cy: integer;
    State: THexMapDrawState;
begin
  CalcOffCoords(HX,HY,cx,cy);
  if (HX = FSelected.X) and (HY = FSelected.Y) then State := [dsSelected]
  else State := [];
  if not (hmGrid in Options) then begin
    OffScreen.Canvas.Pen.Color := Color;
    Hex(OffScreen.Canvas, cx,cy);
  end;
  if Assigned(FOnDrawHex) then FOnDrawHex(Self, HX, HY, OffScreen.Canvas,
    cx, cy, State);
  if (hmShowSelection in Options) and (dsSelected in State) then begin
    OffScreen.Canvas.Pen.Color := SelectColor;
    Hex(OffScreen.Canvas, cx,cy);
  end
  else begin
    if hmGrid in Options then begin
      OffScreen.Canvas.Pen.Color := GridColor;
      Hex(OffScreen.Canvas, cx,cy);
    end;
  end;
end;

procedure THexMap.DrawExtra(HX, HY: integer);
var cx,cy: integer;
    State: THexMapDrawState;
begin
  if Assigned(FOnDrawExtra) then begin
    CalcOffCoords(HX,HY,cx,cy);
    if (HX = FSelected.X) and (HY = FSelected.Y) then State := [dsSelected]
    else State := [];
    FOnDrawExtra(Self, HX, HY, OffScreen.Canvas, cx, cy, State);
  end;
end;

function THexMap.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

function THexMap.GridWidth: integer;
begin
  Result := ColCount*hs75 + hs25 + FMargin*2;
end;

function THexMap.GridHeight: integer;
begin
  Result := (RowCount+1)*hs43 + FMargin*2;
end;

procedure THexMap.Hex(Canvas: TCanvas; CX,CY: Integer);
begin
  with Canvas do begin
    Polyline([Point(CX - hs50, CY),
      Point(CX - hs25, CY + hs43),
      Point(CX + hs25, CY + hs43),
      Point(CX + hs50, CY),
      Point(CX + hs25, CY - hs43),
      Point(CX - hs25, CY - hs43),
      Point(CX - hs50, CY)]);
  end;
end;

procedure THexMap.IterateHexes(HexRect: TRect; Handler: TIterationHandler);
var x,y: integer;
begin
  y := HexRect.Top;
  while y <= HexRect.Bottom do begin
    x := HexRect.Left;
    if (Odd(y) xor Odd(HexRect.Left)) xor FirstOdd then Inc(x);
    while x <= HexRect.Right do begin
      Handler(x, y);
      x := x + 2;
    end;
    y := y + 1;
  end;
end;

procedure THexMap.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
  VK_UP:
    MoveMap(FMapRect.Left, FMapRect.Top - KeyStep);
  VK_DOWN:
    MoveMap(FMapRect.Left, FMapRect.Top + KeyStep);
  VK_LEFT:
    MoveMap(FMapRect.Left - KeyStep, FMapRect.Top);
  VK_RIGHT:
    MoveMap(FMapRect.Left + KeyStep, FMapRect.Top);
  VK_HOME:
    Center(FSelected.X, FSelected.Y);
  end;
end;

procedure THexMap.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var MousePoint: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  if ssLeft in Shift then begin
    if not (msDblClick in MapState) then begin
      MousePoint := MouseToHex(X, Y);
      if (MousePoint.X >= 0) and (MousePoint.Y >= 0) and (MousePoint.X < ColCount)
        and (MousePoint.Y < RowCount) then SelectHex(MousePoint.X, MousePoint.Y);
    end
    else MapState := MapState - [msDblClick];
  end;
  if ssRight in Shift then begin
    if hmRightBtnDrag in Options then begin
      LastX := X; LastY := Y;
      inherited Cursor := crHand;
    end;
    if hmRightBtnCenter in Options then begin
      MousePoint := MouseToHex(X, Y);
      Center(MousePoint.X, MousePoint.Y);
    end;
  end;
end;

procedure THexMap.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (ssRight in Shift) and (hmRightBtnDrag in Options) then begin
    MoveMap(FMapRect.Left + LastX-X, FMapRect.Top + LastY-Y);
    LastX := X; LastY := Y;
  end
  else inherited Cursor := StoredCursor;
end;

procedure THexMap.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  inherited Cursor := StoredCursor;
end;

function THexMap.MouseToHex(X, Y: integer): TPoint;
var PosX, PosY, CellX, CellY: integer;
    Tangent, Disp: real;
begin
  X := X + FMapRect.Left - FMargin - DrawOffset.X;
  Y := Y + FMapRect.Top - FMargin - DrawOffset.Y;
  CellX := X div hs75;  PosX := X mod hs75;
  CellY := Y div hs43;  PosY := Y mod hs43;
  if Odd(CellX + CellY) xor FirstOdd then begin
    Tangent := hs25 / hs43;
    Disp := 0;
  end
  else begin
    Tangent := - hs25 / hs43;
    Disp := hs25;
  end;
  if PosX < PosY * Tangent + Disp then Result.X := CellX - 1
  else Result.X := CellX;
  if Odd(Result.X) xor FirstOdd then begin
    if Odd(CellY) then Result.Y := CellY else Result.Y := CellY - 1;
  end
  else begin
    if Odd(CellY) then Result.Y := CellY - 1 else Result.Y := CellY;
  end;
end;

procedure THexMap.MoveMap(X,Y: integer);
begin
  X := Min(X, GridWidth - (FMapRect.Right - FMapRect.Left));
  X := Max(X, 0);
  Y := Min(Y, GridHeight - (FMapRect.Bottom - FMapRect.Top));
  Y := Max(Y,0);
  MapRect := Rect(X, Y, X + FMapRect.Right - FMapRect.Left,
    Y + FMapRect.Bottom - FMapRect.Top);
  if Assigned(FOnMoveMap) then FOnMoveMap(Self,X,Y);
end;

procedure THexMap.Paint;
begin
  if not (msNoPaint in MapState) then begin
    BeforePaint;
    if msRedraw in MapState then Redraw
    else Canvas.CopyRect(Rect(0, 0, Width, Height),
      OffScreen.Canvas, Rect(hs, hs43*2, hs+Width, hs43*2+Height));
  end;
end;

procedure THexMap.PartialHex(Canvas: TCanvas; CX,CY: Integer; Parts: THexParts);
begin
  with Canvas do begin
    if Parts[dirS] then
      begin MoveTo(CX - hs25, CY + hs43); LineTo(CX + hs25, CY + hs43); end;
    if Parts[dirSW] then
      begin MoveTo(CX - hs50, CY); LineTo(CX - hs25, CY + hs43); end;
    if Parts[dirSE] then
      begin MoveTo(CX + hs25, CY + hs43); LineTo(CX + hs50, CY); end;
    if Parts[dirN] then
      begin MoveTo(CX + hs25, CY - hs43); LineTo(CX - hs25, CY - hs43); end;
    if Parts[dirNW] then
      begin MoveTo(CX - hs25, CY - hs43); LineTo(CX - hs50, CY); end;
    if Parts[dirNE] then
      begin MoveTo(CX + hs50, CY); LineTo(CX + hs25, CY - hs43); end;
  end;
end;

procedure THexMap.RecalcExtraSpan;
begin
  FExtraSpan.X := ((FCellWidth div 2) + hs50) div hs75;
  FExtraSpan.Y := (FCellHeight div 2) div hs43 + 1;
end;

procedure THexMap.RecalcMapRect;
var hx,hy: real;
begin
  CalcRelHex(FMapRect.Left - hs50, FMapRect.Top - hs43, hx, hy);
  FVisHexes.TopLeft := Point(Ceil(Max(hx,0)), Ceil(Max(hy,0)));
  CalcRelHex(FMapRect.Right + hs50, FMapRect.Bottom + hs43, hx, hy);
  FVisHexes.BottomRight := Point(Floor(Min(hx,FColCount-1)),
    Floor(Min(hy,FRowCount-1)));

  FVisCells := Rect(FVisHexes.Left - FExtraSpan.X, FVisHexes.Top - FExtraSpan.Y,
    FVisHexes.Right + FExtraSpan.X, FVisHexes.Bottom + FExtraSpan.Y);

  if (FMapRect.Right - FMapRect.Left) <= GridWidth then DrawOffset.X := 0
  else DrawOffset.X := ((FMapRect.Right - FMapRect.Left) - GridWidth) div 2;
  if (FMapRect.Bottom - FMapRect.Top) <= GridHeight then DrawOffset.Y := 0
  else DrawOffset.Y := ((FMapRect.Bottom - FMapRect.Top) - GridHeight) div 2;

  OffScreen.Width := (FMapRect.Right - FMapRect.Left) + hs*2 + 1;
  OffScreen.Height := (FMapRect.Bottom - FMapRect.Top) + hs43*4 + 1;
end;

procedure THexMap.IHDrawHex(HX, HY: integer);
begin
  if (HX = FSelected.X) and (HY = FSelected.Y) then DrawSelect := TRUE
  else DrawHex(HX, HY);
end;

procedure THexMap.Redraw;
begin
  MapState := MapState - [msRedraw];
 { Draw hexes on off-screen bitmap }
  OffScreen.Canvas.Brush.Color := Color;
  OffScreen.Canvas.FillRect(OffScreen.Canvas.ClipRect);
  DrawSelect := FALSE;
  IterateHexes(FVisHexes, IHDrawHex);
  if DrawSelect then DrawHex(FSelected.X, FSelected.Y);
 { Draw extras }
  IterateHexes(FVisCells, DrawExtra);
  Paint;
end;

procedure THexMap.RedrawCell(HX, HY: integer);
var Extras: TRect;
begin
  if CellVisible(HX,HY) then begin
   { Draw hex background }
    DrawHex(HX,HY);
   { Restore neibors' extras removed by drawn hex }
    Extras := Rect(Max(HX-FExtraSpan.X, 0), Max(HY-FExtraSpan.Y, 0),
      Min(HX+FExtraSpan.X, ColCount-1), Min(HY+FExtraSpan.Y, RowCount-1));
    IterateHexes(Extras, DrawExtra);
  end;
  Paint;
end;

procedure THexMap.SelectHex(HX, HY: integer);
var OldSelect: TPoint;
begin
  OldSelect := FSelected;
  FSelected := Point(HX, HY);
  RedrawCell(OldSelect.X, OldSelect.Y);
  RedrawCell(FSelected.X, FSelected.Y);
  if Assigned(FOnSelectHex) then FOnSelectHex(Self, HX, HY);
end;

procedure THexMap.SetCellWidth(Value: integer);
begin
  if Value >= hs then FCellWidth := Value;
  RecalcExtraSpan;
end;

procedure THexMap.SetCellHeight(Value: integer);
begin
  if Value >= hs43*2 then FCellHeight := Value;
  RecalcExtraSpan;
end;

procedure THexMap.SetColCount(Value: integer);
begin
  if Value > 0 then FColCount := Value;
  if Value = 1 then FirstOdd := FALSE;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.SetCursor(Value: TCursor);
begin
  inherited Cursor := Value;
  StoredCursor := Value;
end;

procedure THexMap.SetFirstOdd(Value: boolean);
begin
  FFirstOdd := Value;
  if Odd(Selected.X) xor Odd(Selected.Y) xor FFirstOdd then begin
    if Selected.Y < RowCount then Inc(FSelected.Y)
    else if Selected.X < ColCount then Inc(FSelected.X);
  end;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.SetGridColor(Value: TColor);
begin
  FGridColor := Value;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.SetHexSize(Value: integer);
begin
  hs := Value;
  hs25 := Value div 4;
  hs50 := Value div 2;
  hs75 := hs50 + hs25;
  hs43 := Round(Value * 0.433);
  RecalcExtraSpan;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.IHDrawNewHex(HX, HY: integer);
begin
  if not ((HX >= OldHexes.Left) and (HX <= OldHexes.Right) and
    (HY >= OldHexes.Top) and (HY <= OldHexes.Bottom)) then IHDrawHex(HX, HY);
end;

procedure THexMap.IHDrawNewCell(HX, HY: integer);
begin
  if not ((HX >= OldHexes.Left + FExtraSpan.X) and (HX <= OldHexes.Right -
    FExtraSpan.X) and (HY >= OldHexes.Top + FExtraSpan.Y) and
    (HY <= OldHexes.Bottom - FExtraSpan.Y)) then DrawExtra(HX, HY);
end;

procedure THexMap.SetMapRect(Value: TRect);
var TempScreen: TBitmap;
    OX, OY: integer;
begin
  OldHexes := FVisHexes;
  OX := FMapRect.Left; OY := FMapRect.Top;
  FMapRect := Value;
  RecalcMapRect;
 { Move image on the offscreen bmp to specified position }
  TempScreen := TBitmap.Create;
  with TempScreen do begin
    Width := OffScreen.Width;
    Height := OffScreen.Height;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Draw(OX-Value.Left, OY-Value.Top, OffScreen);
    OffScreen.Assign(TempScreen);
    TempScreen.Free;
  end;
 { Redraw hexes positioned on the newly opened margins }
  IterateHexes(FVisHexes, IHDrawNewHex);
  RedrawCell(FSelected.X, FSelected.Y);
 { Redraw extras }
  IterateHexes(FVisCells, IHDrawNewCell);
  Paint;
end;

procedure THexMap.SetMargin(Value: integer);
begin
  if Value > 0 then FMargin := Value;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.SetOptions(Value: THexMapOptions);
begin
  if hmRightBtnCenter in Value then
    Value := Value - [hmRightBtnDrag, hmDblClickCenter];
  FOptions := Value;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.SetRowCount(Value: integer);
begin
  if Value > 0 then FRowCount := Value;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.SetSelected(Value: TPoint);
begin
  SelectHex(Value.X, Value.Y);
end;

procedure THexMap.SetSelectColor(Value: TColor);
begin
  FSelectColor := Value;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure THexMap.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure THexMap.WMSize(var Message: TWMSize);
begin
  MapState := MapState + [msNoPaint];
  inherited;
  FMapRect.Right := FMapRect.Left + Width;
  FMapRect.Bottom := FMapRect.Top + Height;
  MoveMap(FMapRect.Left, FMapRect.Top);
  MapState := MapState + [msRedraw] - [msNoPaint];
end;

end.
