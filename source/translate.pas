unit translate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles;

{ TTranslate }
type

  TTranslate = class
  private
    FLanguage : string;
    FFilename : string;
    FLanguages : TStringList;
    slCode,slTranslation : TStringList;
    procedure SetLanguage(value : string);
  public
    constructor Create;
    destructor Destroy;
    function Text(s : string) : string;
    procedure ReadLanguages;
    procedure ReadTranslations;
  published
    property Language : string read FLanguage write SetLanguage;
    property Filename : string read FFilename write FFilename;
    property Languages : TStringList read FLanguages;
end;

var
  Trans : TTranslate;


implementation

{ TTranslate }

procedure TTranslate.SetLanguage(value: string);
var
  i : integer;
begin
  for i:=0 to FLanguages.Count-1 do
    if value=FLanguages[i] then
      begin
        //Sprache gefunden, nur dann ist eine Änderung möglich
        FLanguage:=value;
      end;
end;

constructor TTranslate.Create;
begin
  inherited Create;
  FLanguage:='DE';
  FFilename:='';

  FLanguages:=TStringList.Create;
  slCode:=TStringList.Create;
  slTranslation:=TStringList.Create;
end;

destructor TTranslate.Destroy;
begin
  slCode.Free;
  slTranslation.Free;
  FLanguages.Free;

  inherited Destroy;
end;

function TTranslate.Text(s: string): string;
var i : integer;
begin
  result:='???';
  for i:=0 to slCode.Count-1 do
    if s=slCode[i] then
      begin
        result:=slTranslation[i];
        break;
      end;
end;

procedure TTranslate.ReadLanguages;
var
  Ini : TInifile;
  i : integer;
  s : string;
begin
  if FileExists(FFilename)=false then exit;

  try
    Ini:=TInifile.Create(FFilename);

    //Mögliche Sprachen einlesen
    FLanguages.Clear;
    Ini.ReadSections(FLanguages);
  finally
    Ini.Free;
  end;

end;


procedure TTranslate.ReadTranslations;
var
  Ini : TInifile;
  i : integer;
  s : string;
begin
  if FileExists(FFilename)=false then exit;
  slCode.Clear;
  slTranslation.Clear;

  try
    Ini:=TInifile.Create(FFilename);

    //DE ist die Basis und Standardwert
    Ini.ReadSection('DE',slCode);

    //Übersetzungen laden
    for i:=0 to slCode.Count-1 do
      begin
        s:=Ini.ReadString(FLanguage,slCode[i],'');

        //Wenn nicht vorhanden, dann DE
        if s='' then s:=Ini.ReadString('DE',slCode[i],'');
        slTranslation.Add(s);
      end;
  finally
    Ini.Free;
  end;

end;


end.

initialization
  //Trans:=TTranslate.Create;


finalization
  //Trans.Free;

