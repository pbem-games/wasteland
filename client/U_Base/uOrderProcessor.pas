unit uOrderProcessor;

interface

uses
  SysUtils, DataStructs, MyStrings, Math, Windows, Classes,
  uOrderExec, uResources;

var
  ThreadTerminate: TNotifyEvent;
  ParseErrors: TStrings;

  // Process orders for region; if region = nil, process all orders
  procedure DoProcessOrders(ARegion: TRegion);
  procedure StopProcessOrders;


implementation

type
  TProcessor = procedure (AUnit: TUnit; s: string; var Line: integer);

  TProcessThread = class(TThread)
  private
    procedure DoOrder(AUnit: TUnit; Order: string; var Line: integer;
      Processor: TProcessor);
    procedure DoOrders(R: TRegion; OrderNr: integer; Processor: TProcessor);
    procedure ProcessRegion(R: TRegion; RecreateRegion: boolean);
    procedure ProcessAllRegions;
  protected
    Region: TRegion;
    Script: integer;
    ScriptArgs: string;
    Startup: boolean;
    UnitNum: integer;
    procedure Execute; override;
  public
    function IsTerminated: boolean;
  end;

  TTerminator = class
    procedure FreeThread(Sender: TObject);
  end;


var
  ProcessThread: TProcessThread;
  Terminator: TTerminator;

procedure TProcessThread.DoOrder(AUnit: TUnit; Order: string; var Line:
  integer; Processor: TProcessor);
var args, msg: string;
begin
  Order := Trim(Order);
  if Pos(' ', Order) = 0 then args := ''
  else args := Copy(Order, Pos(' ', Order) + 1, Length(Order));
  try
    Processor(AUnit, args, Line);
  except
    on E: EParseError do begin
      msg := ';. ' + E.Message;
      if (Line >= 0) and (AUnit.Orders.Count > Line) and (Order = AUnit.Orders[Line]) then
        // Add comment to order
        AUnit.Orders[Line] := Uncomment(AUnit.Orders[Line]) + msg
      else
        // Add comment as first line
        if AUnit.Orders.IndexOf(msg) < 0 then begin
          AUnit.Orders.Insert(0, msg);
          Inc(Line);
        end;
      ParseErrors.AddObject('!E4 ' + AUnit.Name + ' (' +
        IntToStr(AUnit.Num) + '): ' + E.Message, AUnit);
    end;
  end;
end;

// Process one type of orders for all units in region
procedure TProcessThread.DoOrders(R: TRegion; OrderNr: integer; Processor: TProcessor);
var i, k: integer;
    AUnit: TUnit;
    Order: string;
begin
  Order := OrderNames[OrderNr];
  i := 0;
  while (i < R.Units.Count) and not Terminated do begin
    AUnit := R.Units[i];
    if (AUnit.Faction = Turn.PlayerFaction) then begin
      // Find orders
      k := 0;
      if (AUnit.OrdersPresent[OrderNr]) then begin
        while k < AUnit.Orders.Count do begin
          {// Skip TURN construction
          while AUnit.Order(k) = 'turn' do begin
            repeat
              Inc(k);
            until (AUnit.Order(k) = 'endturn') or (k >= AUnit.Orders.Count);
            Inc(k);
          end;}
          // Check if we found given order
          if AUnit.Order(k) = Order then
            DoOrder(AUnit, AUnit.Orders[k], k, Processor);
          Inc(k);
        end;
      end;
    end;
    Inc(i);
  end;
end;

// Process orders for region
procedure TProcessThread.ProcessRegion(R: TRegion; RecreateRegion: boolean);
var i: integer;
    U: TUnit;
begin
  // Ensure that the region has player units
  if RecreateRegion then begin
    // Clear parse errors in region units
      for i := 0 to R.Units.Count-1 do begin
        U := R.Units[i];
        if (U.Faction <> Turn.PlayerFaction) then
          continue;
        ClearErrorComments(U.Orders);
        while ParseErrors.IndexOfObject(U) >= 0 do
          ParseErrors.Delete(ParseErrors.IndexOfObject(U));
        U.ResetVirtual;
      end;
    while ParseErrors.IndexOfObject(R) >= 0 do
      ParseErrors.Delete(ParseErrors.IndexOfObject(R));
    while ParseErrors.IndexOfObject(nil) >= 0 do
      ParseErrors.Delete(ParseErrors.IndexOfObject(nil));
  end;

  DoOrders(R, ordLeave,   DoLeave);
  DoOrders(R, ordEnter,   DoEnter);
  DoOrders(R, ordTeam,    DoTeam);
  DoOrders(R, ordGive,    DoGive);

  DoOrders(R, ordScavenge,DoMonth);
  DoOrders(R, ordProduce, DoMonth);
  DoOrders(R, ordMove,    DoMonth);
  DoOrders(R, ordDrive,   DoMonth);
  DoOrders(R, ordPatrol,  DoMonth);
  DoOrders(R, ordBuild,   DoMonth);
  DoOrders(R, ordCure,    DoMonth);
  DoOrders(R, ordInstall, DoMonth);
  DoOrders(R, ordUninstall,DoMonth);
end;

procedure TProcessThread.ProcessAllRegions;
var i, j: integer;
    u: TUnit;
begin
  for j := 0 to Turn.Regions.Count-1 do
    for i := 0 to Turn.Regions[j].Units.Count-1 do begin
      u := Turn.Regions[j].Units[i];
      u.ResetVirtual;
      ClearErrorComments(u.Orders);
    end;
  ParseErrors.Clear;
  if Terminated then Exit;
  for i := 0 to Turn.Regions.Count-1 do
    ProcessRegion(Turn.Regions[i], False);
end;

procedure TProcessThread.Execute;
begin
  if Region <> nil then ProcessRegion(Region, True)
  else ProcessAllRegions;
end;

// We need it to set ProcessThread to nil; thread will actually free by itself
procedure TTerminator.FreeThread(Sender: TObject);
begin
  ProcessThread := nil;
  if Assigned(ThreadTerminate) then ThreadTerminate(Sender);
end;

// Start order-processing thread
procedure DoProcessOrders(ARegion: TRegion);
begin
  if ProcessThread = nil then begin
    ProcessThread := TProcessThread.Create(True);
    with ProcessThread do begin
      Region := ARegion;
      FreeOnTerminate := True;
      OnTerminate := Terminator.FreeThread;
      Resume;
    end;
  end;
end;

procedure StopProcessOrders;
begin
  if ProcessThread <> nil then ProcessThread.Terminate;
end;

function TProcessThread.IsTerminated: boolean;
begin
  Result := Terminated;
end;


initialization
  ParseErrors := TStringList.Create;
  Terminator := TTerminator.Create;

finalization
  ParseErrors.Free;
  Terminator.Free;

end.

