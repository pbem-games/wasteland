program wadvisor;

uses
  Forms,
  Main in 'U_Base\Main.pas' {frmMain},
  uRepRead in 'U_Base\uRepRead.pas',
  DataStructs in 'U_Base\DataStructs.pas',
  uXml in 'U_Base\uXml.pas',
  uResources in 'U_Base\uResources.pas' {frmResources},
  uHexMap in 'U_Base\uHexMap.pas',
  uHistory in 'U_Base\uHistory.pas',
  uLocalize in 'U_Base\uLocalize.pas',
  uInterface in 'U_Base\uInterface.pas',
  uOptions in 'U_Base\uOptions.pas' {frmOptions},
  uMemo in 'U_Base\uMemo.pas' {MemoForm},
  uOrders in 'U_Base\uOrders.pas',
  uFactions in 'U_Base\uFactions.pas' {frmFactions},
  uGameSubs in 'U_Base\uGameSubs.pas',
  MyStrings in 'U_Base\MyStrings.pas',
  uOrderProcessor in 'U_Base\uOrderProcessor.pas',
  uOrderExec in 'U_Base\uOrderExec.pas',
  uAdjustGive in 'U_Base\uAdjustGive.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Wasteland Advisor';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
