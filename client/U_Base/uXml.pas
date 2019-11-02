unit uXml;

// Fuck that standard libraries, that much effort for that simple task, bleh

interface

uses SysUtils, Classes;

type
  TXmlException = class(Exception);

  TXmlElementList = class;

  TXmlElement = class
    Name: string;
    InnerText: string;
    Attributes: TStringList;
    ChildNodes: TXmlElementList;
    function GetAttribute(name: string): string;
    procedure SetAttribute(name, value: string);
    procedure AppendChild(el: TXmlElement);
    function OuterXml: string;
    constructor Create();
    destructor Destroy; override;
  end;

  TXmlDocument = class
  private
    function ReadUntil(var s: string; token: string): string;
    function ReadWhile(var s: string; token: string): string;
    function ReadChar(var s: string): string;
    procedure Parse(el: TXmlElement; var s: string);
  public
    DocumentElement: TXmlElement;
    procedure LoadXml(s: string);
    function OuterXml: string;
    function CreateElement(name: string): TXmlElement;
    constructor Create;
    destructor Destroy; override;
  end;

  TXmlElementList = class(TList)
  protected
    function Get(Index: Integer): TXmlElement;
    procedure Put(Index: Integer; Item: TXmlElement);
  public
    property Items[Index: Integer]: TXmlElement read Get write Put; default;
  end;

implementation

{ TXmlDocument }

constructor TXmlDocument.Create;
begin
  DocumentElement := TXmlElement.Create();
end;

destructor TXmlDocument.Destroy;
begin
  DocumentElement.Free;
end;

procedure TXmlDocument.LoadXml(s: string);
var i: integer;
begin
  s := StringReplace(s, #13#10, ' ', [rfReplaceAll]);
  i := Pos('<?xml', s);
  if (i = 0) then Exit;
  s := Copy(s, i, Length(s));

  // <?xml blah blah ?>
  ReadUntil(s, '<');
  if (Length(s) > 1) and (s[2] = '?') then
    ReadUntil(s, '>');
  ReadChar(s);

  Parse(DocumentElement, s);
end;

procedure TXmlDocument.Parse(el: TXmlElement; var s: string);
var attr_name, attr_value, el_name: string;
    child: TXmlElement;
begin
  ReadUntil(s, '<');
  ReadChar(s);
  el.Name := ReadUntil(s, '>/ '#13#10);
  while (Length(s) > 0) do begin
    ReadWhile(s, ' '#13#10);

    // End of element
    if (s[1] = '/') then begin
      ReadChar(s);
      if (ReadChar(s) <> '>') then
        raise TXmlException.Create('End of element expected');
      Exit;
    end

    // End of starting tag
    else if (s[1] = '>') then begin
      ReadChar(s);
      el.InnerText := el.InnerText + ReadUntil(s, '<');
    end

    // Next item
    else if (s[1] = '<') then begin
      if (Length(s) > 1) and (s[2] <> '/') then begin
        child := TXmlElement.Create;
        el.ChildNodes.Add(child);
        Parse(child, s);
      end
      else if (Length(s) > 1) and (s[2] = '/') then begin
        ReadChar(s);
        ReadChar(s);
        el_name := ReadUntil(s, '> '#13#10);
        if (el_name <> el.Name) then
          raise TXmlException.Create('Unexpected closing element');
        ReadUntil(s, '>');
        ReadChar(s);
        Exit;
      end;
    end

    // Attribute
    else begin
      attr_name := ReadUntil(s, '=');
      ReadChar(s);
      if (ReadChar(s) <> '"') then
        raise TXmlException.Create('Attribute expected');
      attr_value := ReadUntil(s, '"');
      ReadChar(s);
      el.Attributes.Add(attr_name + '=' + attr_value);
    end;
  end;

  raise TXmlException.Create('Content after closing document element');
end;

function TXmlDocument.ReadUntil(var s: string; token: string): string;
begin
  Result := '';
  while (Length(s) > 0) and (Pos(s[1], token) = 0) do
    Result := Result + ReadChar(s);
  if (Length(s) = 0) then
    raise TXmlException.Create('Unexpected end of input');
end;

function TXmlDocument.ReadWhile(var s: string; token: string): string;
begin
  Result := '';
  while (Length(s) > 0) and (Pos(s[1], token) > 0) do
    Result := ReadChar(s);
  if (Length(s) = 0) then
    raise TXmlException.Create('Unexpected end of input');
end;

function TXmlDocument.ReadChar(var s: string): string;
begin
  Result := s[1];
  s := Copy(s, 2, Length(s));
end;

function TXmlDocument.OuterXml: string;
begin
  Result := '<?xml version="1.0" encoding="windows-1251"?>' +
    DocumentElement.OuterXml;
end;

function TXmlDocument.CreateElement(name: string): TXmlElement;
begin
  Result := TXmlElement.Create;
  Result.Name := name;
end;

{ TXmlElement }

constructor TXmlElement.Create;
begin
  Attributes := TStringList.Create;
  ChildNodes := TXmlElementList.Create;
end;

destructor TXmlElement.Destroy;
var i: integer;
begin
  Attributes.Free;
  for i := 0 to ChildNodes.Count-1 do
    TXmlElement(ChildNodes[i]).Free;
  ChildNodes.Free;
end;

function TXmlElement.GetAttribute(name: string): string;
begin
  Result := Attributes.Values[name];
end;

procedure TXmlElement.SetAttribute(name, value: string);
begin
  Attributes.Values[name] := value;
end;

procedure TXmlElement.AppendChild(el: TXmlElement);
begin
  ChildNodes.Add(el);
end;

function TXmlElement.OuterXml: string;
var i: integer;
begin
  Result := '<' + Name;

  for i := 0 to Attributes.Count-1 do
    Result := Result + Format(' %s="%s"', [Attributes.Names[i],
      Attributes.Values[Attributes.Names[i]]]);

  if (ChildNodes.Count > 0) then begin
    Result := Result + '>';
    for i := 0 to ChildNodes.Count-1 do
      Result := Result + ChildNodes[i].OuterXml;
    Result := Result + '</' + Name + '>';
  end
  else
    Result := Result + '/>';
end;


function TXmlElementList.Get(Index: Integer): TXmlElement;
begin
  Result := TXmlElement(inherited Get(Index));
end;

procedure TXmlElementList.Put(Index: Integer; Item: TXmlElement);
begin
  inherited Put(Index, Item);
end;


end.
