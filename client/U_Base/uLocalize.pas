unit uLocalize;

interface

uses
  SysUtils, Windows, Classes, Forms, IniFiles, Controls, ComCtrls, StdCtrls,
  Menus, ActnList, uResources, PowerGrid;

  procedure Localize(Form: TForm);

implementation

procedure Localize(Form: TForm);
var Section: string;
    i, j: integer;
begin
  Section := Form.Name;
  with Form do begin
    Caption := Lang.ReadString(Section, 'Caption', Caption);
    for i := 0 to ComponentCount-1 do begin
      if Components[i].ClassType = TAction then
        with TAction(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
          Hint := Lang.ReadString(Section, Name+'Hint', Hint);
        end
      else if Components[i].ClassType = TLabel then
        with TLabel(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
        end
      else if Components[i].ClassType = TMenuItem then
        with TMenuItem(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
        end
      else if Components[i].ClassType = TCheckBox then
        with TCheckBox(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
        end
      else if Components[i].ClassType = TButton then
        with TButton(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
        end
      else if Components[i].ClassType = TRadioButton then
        with TRadioButton(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
        end
      else if Components[i].ClassType = TGroupBox then
        with TGroupBox(Components[i]) do begin
          Caption := Lang.ReadString(Section, Name, Caption);
        end
      else if Components[i].ClassType = TPowerGrid then
        with TPowerGrid(Components[i]) do begin
          for j := 0 to ColCount-1 do
            Cells[j, 0] := Lang.ReadString(Section, Name+IntToStr(j), Cells[j, 0]);
          Fixup;
        end
      else if Components[i].ClassType = TTreeView then
        with TTreeView(Components[i]) do begin
          for j := 0 to Items.Count-1 do
            Items[j].Text := Lang.ReadString(Section, Name+IntToStr(j), Items[j].Text);
        end
      else if Components[i].ClassType = TPageControl then
        with TPageControl(Components[i]) do begin
          for j := 0 to PageCount-1 do
            Pages[j].Caption := Lang.ReadString(Section, Name+IntToStr(j), Pages[j].Caption);
        end;
    end;
  end;
end;

end.
