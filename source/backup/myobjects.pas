unit myobjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Graphics, GraphType, DateUtils,
  FileUtil, Dialogs, Controls, LazUTF8, LCLIntf, LCLType, LMessages,
  {$IFDEF WINDOWS}
  Win32Proc,
  ShellApi,
  {$ENDIF}
  Inifiles, translate;

const
  {$IFDEF UNIX}
  BASSLIB_FILE='libbass.so';
  {$ENDIF}
  {$IFDEF WINDOWS}
  BASSLIB_FILE='bass.dll';
  {$ENDIF}


  FILE_HEADER='WEBRADIO';

  //Menü
  PAGE_PLAYER = 1;
  PAGE_SELECT = 2;
  PAGE_SELECT_RADIO = 21;
  PAGE_SELECT_MP3 = 22;
  PAGE_SETUP = 23;
  PAGE_ONOFF = 24;
  PAGE_LINEIN = 3;
  PAGE_SCREENSAVER = 0;


  //Main-Notebook
  MAIN_PAGE_PLAYER = 0;
  MAIN_PAGE_SELECT = 1;
  MAIN_PAGE_LINEIN = 2;
  MAIN_PAGE_SCREENSAVER = 3;

  SELECT_PAGE_RADIO = 0;
  SELECT_PAGE_MP3 = 1;
  SELECT_PAGE_SETUP = 2;
  SELECT_PAGE_ONOFF = 3;

  //Befehle
  COMMAND_LAST = 1;
  COMMAND_PLAY = 2;
  COMMAND_PAUSE = 3;
  COMMAND_NEXT = 4;
  COMMAND_SELECT_MENU = 5;
  COMMAND_SELECT_MENU_RADIO = 6;
  COMMAND_SELECT_MENU_MP3 = 7;
  COMMAND_SELECT_LINEIN = 8;
  COMMAND_SELECT_SETUP = 9;
  COMMAND_SELECT_ONOFF = 10;
  COMMAND_PLAYER = 11;

  COMMAND_ENCODER_LEFT = 12;
  COMMAND_ENCODER_RIGHT = 13;
  COMMAND_ENCODER_KEY = 14;
  COMMAND_SENSOR_MOTION = 15;
  COMMAND_SENSOR_IR = 16;
  COMMAND_LED = 17;


  //Pin-Definitionen
  PIN_ROTARY_TURN1 = 2;
  PIN_ROTARY_TURN2 = 0;
  PIN_ROTARY_CLICK = 3;
  PIN_SENSOR_MOTION = 21;
  PIN_SENSOR_IR = 22;

  PIN_LED_R = 23;
  PIN_LED_G = 24;
  PIN_LED_B = 25;


type

  RGBColorType = record
    r,g,b : longint;
  end;


  TPlayingType = (ptNothing,ptRadio,ptMP3);
  TPlayElement = (peLast,peNext);

  TRadioItemGroup = (rigIcon,rigList,rigPicture);



  { TItemPanel }
  TItemPanel = class(TPanel)
    private
      FTitle : string;
      FUrl : string;
      FInfo : string;
      FTextColor : TColor;
      FBackground : TColor;
      FOnPanelClick : TNotifyEvent;
      FFilename : string;
      FFilenameLarge : string;
      FGroup : TRadioItemGroup;
      FSelected : boolean;
      FBackgroundSelected : TColor;
      FTransparent : boolean;
      FCommand : integer;

      LabelTitle : TLabel;
      LabelInfo : TLabel;
      procedure SetBackgroundSelected(AValue: TColor);
      procedure SetColors;
      procedure SetBackground(value : TColor);
      procedure SetSelected(AValue: boolean);
      procedure SetTextcolor(value: TColor);

      procedure PanelClick(Sender : TObject);
      procedure SetTransparent(AValue: boolean);
    public
      Image : TImage;
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure Assign(RadioItem : TItemPanel);
      procedure SaveToInifile;
      procedure SaveToInifile(Inifile : TInifile);
      procedure LoadFromInifile(Filename : string);
      procedure LoadFromInifile(Inifile : TInifile);
      procedure UpdateView(ReloadPicture : boolean);
      procedure Resize(w, h: integer);
      procedure SetMouseEvents(MouseUpEvent, MouseDownEvent: TMouseEvent;
        MouseMoveEvent: TMouseMoveEvent);
    published
      property Title : string read FTitle write FTitle;
      property Url : string read FUrl write FUrl;
      property Info : string read FInfo write FInfo;
      property TextColor : TColor read FTextColor write SetTextcolor;
      property Background : TColor read FBackground write SetBackground;
      property OnPanelClick : TNotifyEvent read FOnPanelClick write FOnPanelClick;
      property Filename : string read FFilename write FFilename;
      property FilenameLarge : string read FFilenameLarge write FFilenameLarge;
      property Group : TRadioItemGroup read FGroup write FGroup;
      property Selected : boolean read FSelected write SetSelected;
      property BackgroundSelected : TColor read FBackgroundSelected write SetBackgroundSelected;
      property Transparent : boolean read FTransparent write SetTransparent;
      property Command : integer read FCommand write FCommand;
  end;

  { TItemPanelList }
  TItemPanelList = class
    private
      FData : TList;
      FControl : TWinControl;
      FSelected : integer;
      FVisible : boolean;
      procedure SetSelected(AValue: integer);
      procedure SetVisible(AValue: boolean);
      procedure UpdateSelected;
    public
      constructor Create;
      destructor Destory;
      procedure Clear;
      procedure Add(RadioItem : TItemPanel);
      procedure LoadFromDirectory(Path : string);
      function Count : integer;
      procedure SetEvent(NotifyEvent : TNotifyEvent);
      procedure Resize(w, h: integer);
      procedure SetMouseEvents(MouseUpEvent, MouseDownEvent: TMouseEvent;
        MouseMoveEvent: TMouseMoveEvent);
      function GetSelectedTop : integer;
      function GetSelected: TItemPanel;
      function GetSelectedCommand: integer;
      function GetLast(RadioItem : TItemPanel) : TItemPanel;
      function GetNext(RadioItem : TItemPanel) : TItemPanel;
    published
      property Data : TList read FData write FData;
      property BaseControl : TWinControl read FControl write FControl;
      property Selected : integer read FSelected write SetSelected;
      property Visible : boolean read FVisible write SetVisible;
  end;

  { TRangeTimer }

  TRangeTimer = class(TTimer)
    private
      FStep,FSteps : Cardinal;
      procedure SetSteps(AValue: cardinal);
      procedure Start;
    public
      constructor Create(AOwner: TComponent); override;
      procedure DoOnTimer; override;
    published
      property Steps : cardinal read FSteps write SetSteps;
      property Step : cardinal read FSteps;

  end;



var
  TickerIR,TimeoutIR : integer;
  TickerMotion,TimeoutMotion : integer;
  MaxBar : longint;
  IplRadio,IplIconbar,IplPlayer : TItemPanelList;
  CurrentRadioItem : TItemPanel;
  ProgramPath,ConfigPath : string;
  ConfigFile : string;
  RadioFile : string;
  LanguageFile : string;
  MouseCursor : integer;
  MP3Path : string;
  LineInPics : TStringList;
  LineInPic : integer;



function ExtractFilenameWithoutExtension(s : string) : string;
function brighten( col:TColor; value:integer):TColor;
function RGBsplit(Col:TColor):RGBColorType;
function IncColor(Col: TColor; Increments: Integer): TColor;



{$IFDEF UNIX}
function WinCPToUTF8(const s: string): string;
{$ENDIF}

procedure LoadLineInPics(Path : string);


implementation

{$IFDEF UNIX}
function WinCPToUTF8(const s: string): string;
begin
  result:=AnsiToUTF8(s);
end;
{$ENDIF}


function OpenDocument(Document : string) : boolean;
var
  ws    : WideString;
  ans   : AnsiString;
begin
  Result := false;
  {$IFDEF WINDOWS}
  if Document = '' then Exit;
  (*
  if UnicodeEnabledOS then
    begin
      ws:=UTF8Decode(Document);
      Result:=ShellExecuteW(0,'open',PWideChar(ws),nil,nil,0)>32;
    end
  else
    begin
      ans:=Utf8ToAnsi(Document);
      Result:=ShellExecute(0,'open',PAnsiChar(ans),nil,nil,0)> 2;
    end; *)

  ans:=Utf8ToAnsi(Document);
  Result:=ShellExecute(0,'open',PAnsiChar(ans),nil,nil,0)> 2;
    {$ENDIF}
end;

function RGBsplit(Col:TColor):RGBColorType;
var cRGB: RGBColorType;
begin
  cRGB.B := (Col And 16711680) div 65536;
  cRGB.G := (Col And 65280) div 256;
  cRGB.R := Col And 255;
  Result:=cRGB
end;


function IncColor(Col: TColor; Increments: Integer): TColor;
var cRGB: RGBColorType;
begin
     cRGB:=RGBSplit(ColorToRGB(Col));
     if Increments>0 then
       begin
         if not (cRGB.R + Increments>255) then
            Inc(cRGB.R,Increments)
         else cRGB.R:=255;
         if not (cRGB.G + Increments>255) then
            Inc(cRGB.G,Increments)
         else cRGB.G:=255;
         if not (cRGB.B + Increments>255) then
            Inc(cRGB.B,Increments)
         else cRGB.B:=255;
       end
     else
       begin
         if not (cRGB.R + Increments<0) then
            Inc(cRGB.R,Increments)
         else cRGB.R:=0;
         if not (cRGB.G + Increments<0) then
            Inc(cRGB.G,Increments)
         else cRGB.G:=0;
         if not (cRGB.B + Increments<0) then
            Inc(cRGB.B,Increments)
         else cRGB.B:=0;
       end;
     result:=RGB(cRGB.R,cRGB.G,cRGB.B);
end;

function brighten( col:TColor; value:integer):TColor;
var rcol, gcol, bcol : integer;
begin
  col:= ColorToRGB( col);
  rcol:= GetRvalue( col);
  gcol:= GetGvalue( col);
  bcol:= GetBvalue( col);

  if rcol < 255-3 then inc( rcol, 3) else rcol:= 255;
  if gcol < 255-3 then inc( gcol, 3) else gcol:= 255;
  if bcol < 255-3 then inc( bcol, 3) else bcol:= 255;
  result:= RGB( rcol, gcol, bcol);
end;


function ExtractFilenameWithoutExtension(s : string) : string;
var ti : integer;
begin
  result:='';
  s:=ExtractFileName(s);
  for ti:=length(s) downto 1 do
    if copy(s,ti,1)='.' then
      begin
        result:=copy(s,1,ti-1);
        break;
      end;
 end;

{ TRangeTimer }

procedure TRangeTimer.SetSteps(AValue: cardinal);
begin
  if FSteps=AValue then Exit;
  FSteps:=AValue;
end;

constructor TRangeTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Enabled:=false;
  FStep:=0;
end;

procedure TRangeTimer.DoOnTimer;
begin
  FStep:=FStep+1;
  if FStep>=FSteps then Enabled:=false;

  inherited DoOnTimer;
end;


procedure TRangeTimer.Start;
begin
  FStep:=0;
  Enabled:=true;
end;



{ TItemPanelList }

procedure TItemPanelList.UpdateSelected;
var
  i : integer;
  RadioItem : TItemPanel;
begin
  for i:=0 to FData.Count-1 do
    begin
      RadioItem:=TItemPanel(FData[i]);
      if FSelected=i then RadioItem.Selected:=true
      else RadioItem.Selected:=false;
    end;
end;

procedure TItemPanelList.SetSelected(AValue: integer);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
  UpdateSelected;
end;

procedure TItemPanelList.SetVisible(AValue: boolean);
var
  i : integer;
  RadioItem : TItemPanel;
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;

  for i:=0 to FData.Count-1 do
    begin
      RadioItem:=TItemPanel(FData[i]);
      RadioItem.Visible:=FVisible;
      if FVisible=true then RadioItem.BringToFront;
    end;
end;

constructor TItemPanelList.Create;
begin
  inherited Create;
  FControl:=nil;
  FData:=TList.Create;
  FSelected:=-1;
  FVisible:=true;
  Clear;
end;

destructor TItemPanelList.Destory;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TItemPanelList.Clear;
var
  i : integer;
  RadioItem : TItemPanel;
begin
  for i:=0 to FData.Count-1 do
    begin
      RadioItem:=TItemPanel(FData[i]);
      RadioItem.Free;
    end;
  FData.Clear;
end;

procedure TItemPanelList.Add(RadioItem: TItemPanel);
begin
  FData.Add(RadioItem);
end;

procedure TItemPanelList.LoadFromDirectory(Path: string);
var
  i,j,k : integer;
  s : string;
  tex : System.Text;
  RadioItem : TItemPanel;
  Bitmap : TBitmap;
  sr : TSearchRec;
  DosError : integer;
  sl : TStringList;

begin
  if FControl=nil then exit;

  if DirectoryExists(UTF8ToSys(Path))=false then
    begin
      MessageDlg(Trans.Text('OeffnenError'),Path,mtWarning,[mbOK],0);
      exit;
    end;

  if copy(Path,length(Path),1)<>PathDelim then
    Path:=Path+PathDelim;

  sl:=TStringList.Create;

  DosError:=FindFirst(Path+'*.radio',faAnyFile,sr);
  while DosError=0 do
    begin
      if (sr.Attr and faDirectory=faDirectory) then
        begin
        end
       else sl.Add(Path+sr.Name);
      DosError:=FindNext(sr);
    end;
  FindClose(sr);

  sl.Sort;
  for i:=0 to sl.Count-1 do
    begin
      RadioItem:=TItemPanel.Create(nil);
      RadioItem.LoadFromInifile(sl[i]);
      FData.Add(RadioItem);
      FControl.InsertControl(RadioItem);
    end;
  sl.Free;

  Resize(0,0);
end;

function TItemPanelList.Count: integer;
begin
  if FData=nil then result:=0
  else result:=FData.Count;
end;

procedure TItemPanelList.SetEvent(NotifyEvent: TNotifyEvent);
var
  i : integer;
  RadioItem : TItemPanel;
begin
  for i:=0 to FData.Count-1 do
    begin
      RadioItem:=TItemPanel(FData[i]);
      RadioItem.OnPanelClick:=NotifyEvent;
    end;
end;

procedure TItemPanelList.Resize(w, h: integer);
var
  i,x,y,ih : integer;
  RadioItem : TItemPanel;
begin
  y:=0;
  x:=0;
  ih:=Round(h*0.2); //4 Elemente pro Höhe

  for i:=0 to FData.Count-1 do
    begin
      RadioItem:=TItemPanel(FData[i]);
      RadioItem.UpdateView(false);
      RadioItem.Top:=y;
      RadioItem.Left:=x;
      if not ((w=0) and (h=0)) then RadioItem.Resize(w,ih);
      y:=y+Round(1.1*RadioItem.Height);
    end;
end;

procedure TItemPanelList.SetMouseEvents(MouseUpEvent,
  MouseDownEvent: TMouseEvent; MouseMoveEvent: TMouseMoveEvent);
var
  i : integer;
  RadioItem : TItemPanel;
begin
  for i:=0 to FData.Count-1 do
    begin
      RadioItem:=TItemPanel(FData[i]);
      RadioItem.SetMouseEvents(MouseUpEvent, MouseDownEvent, MouseMoveEvent);
    end;
end;

function TItemPanelList.GetSelectedTop: integer;
begin
  result:=-1;
  if FSelected<0 then exit;
  result:=TItemPanel(FData[FSelected]).Top;
end;

function TItemPanelList.GetSelected: TItemPanel;
begin
  result:=nil;
  if FSelected<0 then exit;
  result:=TItemPanel(FData[FSelected]);
end;

function TItemPanelList.GetSelectedCommand : integer;
begin
  result:=-1;
  if FSelected<0 then exit;
  result:=TItemPanel(FData[FSelected]).Command;
end;


function TItemPanelList.GetLast(RadioItem: TItemPanel): TItemPanel;
var
  i : integer;
  Ri : TItemPanel;
begin
  result:=RadioItem;

  for i:=0 to FData.Count-1 do
    begin
      Ri:=TItemPanel(FData[i]);
      if Ri.Title=RadioItem.Title then
        begin
          if i=0 then result:=TItemPanel(FData[FData.Count-1])
          else result:=TItemPanel(FData[i-1]);
        end;
    end;
end;

function TItemPanelList.GetNext(RadioItem: TItemPanel): TItemPanel;
var
  i : integer;
  Ri : TItemPanel;
begin
  result:=RadioItem;
  for i:=0 to FData.Count-1 do
    begin
      Ri:=TItemPanel(FData[i]);
      if Ri.Title=RadioItem.Title then
        begin
          if i=FData.Count-1 then result:=TItemPanel(FData[0])
          else result:=TItemPanel(FData[i+1]);
        end;
    end;
end;


{ TItemPanel }

procedure TItemPanel.PanelClick(Sender: TObject);
begin
  if Assigned(FOnPanelClick) then
    begin
      //CurrentRadioItem:=self; //.Assign(self);
      FOnPanelClick(Self);
    end;
end;

procedure TItemPanel.SetTransparent(AValue: boolean);
begin
  if FTransparent=AValue then Exit;
  FTransparent:=AValue;
  Image.Transparent:=FTransparent;
end;

procedure TItemPanel.SetColors;
begin
  if FSelected=true then self.Color:=FBackgroundSelected
  else self.Color:=FBackground;

  LabelTitle.Font.Color:=FTextColor;
  LabelInfo.Font.Color:=FTextColor;

  Repaint;
end;

procedure TItemPanel.SetBackgroundSelected(AValue: TColor);
begin
  if FBackgroundSelected=AValue then Exit;
  FBackgroundSelected:=AValue;
  SetColors;
end;

procedure TItemPanel.SetBackground(value: TColor);
begin
  if value=FBackground then exit;
  FBackground:=value;
  SetColors;
end;

procedure TItemPanel.SetSelected(AValue: boolean);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
  SetColors;
end;

procedure TItemPanel.SetTextcolor(value: TColor);
begin
  if value=FTextColor then exit;
  FTextColor:=value;
  SetColors;
end;

procedure TItemPanel.Resize(w,h : integer);
begin
  Width:=w;
  Height:=h;

  if FGroup=rigIcon then
    begin
      LabelTitle.Visible:=true;
      LabelInfo.Visible:=true;
      with Image do
       begin
         Enabled:=false;
         Left:=Round(w*0.02);
         Top:=Round(h*0.02);
         Width:=self.Width-self.Left*2;
         Height:=self.Height-Round(self.Height*0.38);
       end;

      with LabelTitle do
       begin
         Alignment:=taCenter;
         Left:=Image.Left;
         Top:=Image.Top+Image.Height;
         Width:=Image.Width;
         Height:=Round(h*0.19);
         Font.Size:=Round(h*0.09);
       end;

      with LabelInfo do
       begin
         Alignment:=taCenter;
         Left:=Image.Left;
         Top:=LabelTitle.Top+LabelTitle.Height;
         Width:=Image.Width;
         Height:=Round(h*0.15);
         Font.Size:=Round(h*0.08);
       end;
    end
  else if FGroup=rigPicture then
    begin
      LabelTitle.Visible:=false;
      LabelInfo.Visible:=false;
      with Image do
       begin
         Enabled:=false;
         Left:=0;
         Top:=0;
         Width:=self.Width;
         Height:=self.Height;
       end;

    end
  else //rigList
    begin
      LabelTitle.Visible:=true;
      LabelInfo.Visible:=true;
      with Image do
       begin
         Enabled:=false;
         Left:=Round(h*0.03);
         Top:=Round(h*0.03);
         Height:=self.Height-2*Top;
         Width:=self.Height-2*Top;
       end;

      with LabelTitle do
       begin
         Alignment:=taLeftJustify;
         Left:=Image.Left+Image.Width+Image.Left*2;
         Top:=Image.Top;
         Width:=w-Left-Image.Left;
         Height:=Round(h*0.5);
         Font.Size:=Round(h*0.2);
       end;

      with LabelInfo do
       begin
         Alignment:=taLeftJustify;
         Left:=LabelTitle.Left;
         Top:=LabelTitle.Top+LabelTitle.Height;
         Width:=LabelTitle.Width;
         Height:=Round(h*0.42);
         Font.Size:=Round(h*0.15);
       end;
    end;

end;

procedure TItemPanel.SetMouseEvents(MouseUpEvent, MouseDownEvent : TMouseEvent;
  MouseMoveEvent: TMouseMoveEvent);
begin
  self.OnMouseUp:=MouseUpEvent;
  self.OnMouseDown:=MouseDownEvent;
  self.OnMouseMove:=MouseMoveEvent;
  LabelTitle.OnMouseUp:=MouseUpEvent;
  LabelTitle.OnMouseDown:=MouseDownEvent;
  LabelTitle.OnMouseMove:=MouseMoveEvent;
  LabelInfo.OnMouseUp:=MouseUpEvent;
  LabelInfo.OnMouseDown:=MouseDownEvent;
  LabelInfo.OnMouseMove:=MouseMoveEvent;
end;

constructor TItemPanel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FGroup:=rigList;
  FTitle:='';
  FInfo:='';
  FUrl:='';
  FCommand:=0;

  Width:=170;
  Height:=Width;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  ParentColor:=false;

  FSelected:=false;
  OnClick:=@PanelClick;
  FTextColor:=clWhite;
  FBackground:=RGBToColor(50,50,50);
  FBackgroundSelected:=clGreen;
  self.Color:=FBackground;

  Image:=TImage.Create(self);
  with Image do
    begin
      Left:=5;
      Top:=5;
      Center:=true;
      Width:=self.Width-Left*2;
      Height:=Width-50;
      Proportional:=true;
      Stretch:=true;
      Parent:=self;
      ParentColor:=true;
      OnClick:=@PanelClick;
    end;

  LabelTitle:=TLabel.Create(self);
  with LabelTitle do
    begin
      Caption:='';
      Autosize:=false;
      Alignment:=taLeftJustify; //taCenter;
      Left:=5;
      Top:=Image.Top+Image.Height;
      Width:=Image.Width;
      Height:=30;
      Font.Size:=16;
      Parent:=self;
      Transparent:=true;
      ParentColor:=true;
      ShowAccelChar:=false;
      OnClick:=@PanelClick;
    end;

  LabelInfo:=TLabel.Create(self);
  with LabelInfo do
    begin
      Caption:='';
      Autosize:=false;
      Alignment:=taLeftJustify; //taCenter;
      Left:=5;
      Top:=LabelTitle.Top+LabelTitle.Height;
      Width:=Image.Width;
      Height:=20;
      Font.Size:=10;
      Parent:=self;
      Transparent:=true;
      ParentColor:=true;
      ShowAccelChar:=false;
      OnClick:=@PanelClick;
    end;

  Resize(Width,Height);



  SetColors;

end;

destructor TItemPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TItemPanel.UpdateView(ReloadPicture : boolean);
var s : string;
begin
  if ReloadPicture=true then
    begin
      s:=ExtractFilePath(RadioFile)+'radio'+PathDelim+FFilename;

      if FileExists(FFilename)=true then Image.Picture.LoadFromFile(FFilename)
      else if FileExists(s)=true then Image.Picture.LoadFromFile(s)
      else Image.Picture:=nil;
    end;

  LabelTitle.Caption:=FTitle;
  LabelInfo.Caption:=FInfo;
end;

procedure TItemPanel.Assign(RadioItem: TItemPanel);
begin
  FTitle:=RadioItem.Title;
  FInfo:=RadioItem.Info;
  FUrl:=RadioItem.Url;
  FFilename:=RadioItem.Filename;
  FFilenameLarge:=RadioItem.FilenameLarge;
  FCommand:=RadioItem.Command;
  Image.Picture.Bitmap.Assign(RadioItem.Image.Picture.Bitmap);
end;

procedure TItemPanel.SaveToInifile;
var
  Ini : TInifile;
  s : string;
begin
  Ini:=TInifile.Create(ConfigPath+'radio'+PathDelim+FTitle+'.radio');

  s:='Setup';
  Ini.WriteString(s,'Title',FTitle);
  Ini.WriteString(s,'URL',FUrl);
  Ini.WriteString(s,'Info',FInfo);

  s:='Icon';
  Ini.WriteString(s,'Small',FFilename);
  Ini.WriteString(s,'Big',FFilenameLarge);

  Ini.Free;
end;

procedure TItemPanel.SaveToInifile(Inifile: TInifile);
begin
  Inifile.WriteString('Aktueller Radiosender','Titel',FTitle);
  Inifile.WriteString('Aktueller Radiosender','URL',FUrl);
  Inifile.WriteString('Aktueller Radiosender','Info',FInfo);
  Inifile.WriteString('Aktueller Radiosender','Dateiname',FFilename);
  Inifile.WriteString('Aktueller Radiosender','Dateiname lang',FFilenameLarge);
end;

procedure TItemPanel.LoadFromInifile(Filename : string);
var
  Ini : TInifile;
  s : string;
begin
  Ini:=TInifile.Create(Filename);

  s:='Setup';
  FTitle:=Ini.ReadString(s,'Title','');
  FUrl:=Ini.ReadString(s,'URL','');
  FInfo:=Ini.ReadString(s,'Info','');

  s:='Icon';
  FFilename:=Ini.ReadString(s,'Small','');
  FFilenameLarge:=Ini.ReadString(s,'Big','');

  UpdateView(true);

  Ini.Free;
end;

procedure TItemPanel.LoadFromInifile(Inifile: TInifile);
var s : string;
begin
  FTitle:=Inifile.ReadString('Aktueller Radiosender','Titel','');
  FUrl:=Inifile.ReadString('Aktueller Radiosender','URL','');
  FInfo:=Inifile.ReadString('Aktueller Radiosender','Info','');
  FFilename:=Inifile.ReadString('Aktueller Radiosender','Dateiname','');
  FFilenameLarge:=Inifile.ReadString('Aktueller Radiosender','Dateiname lang','');

  UpdateView(true);
end;



function Calc(d : double) : integer;
var i : integer;
begin
  try
    i:=Round(d);
    if i<1 then i:=1;
    result:=i;
  except
    result:=0;
  end;
end;


procedure LoadLineInPics(Path : string);
var
  sr : TSearchRec;
  DosError : integer;
  sl : TStringList;
begin
  if copy(Path,length(Path),1)<>PathDelim then
    Path:=Path+PathDelim;

  sl:=TStringList.Create;

  DosError:=FindFirst(Path+'*.*',faAnyFile,sr);
  while DosError=0 do
    begin
      if (sr.Attr and faDirectory=faDirectory) then
        begin
        end
       else sl.Add(Path+sr.Name);
      DosError:=FindNext(sr);
    end;
  FindClose(sr);
  sl.Sort;
  LineInPics.Assign(sl);
  sl.Free;
end;




initialization
  IplRadio:=TItemPanelList.Create;
  IplPlayer:=TItemPanelList.Create;
  IplIconbar:=TItemPanelList.Create;
  MaxBar:=0;

  LineInPics:=TStringList.Create;
  LineInPic:=0;



finalization
  IplRadio.Free;
  IplPlayer.Free;
  IplIconbar.Free;
  LineInPics.Free;





end.

