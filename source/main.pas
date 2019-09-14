unit main;

//{$mode objfpc}{$H+}
{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  FileUtil, ExtCtrls, StdCtrls, Buttons, LCLType, ComCtrls,
  Inifiles, LazUTF8, DateUtils, Process,
  myobjects, translate, scrollpanel,
  gpiosim, lazdynamic_bass, wiringpi;


type
  TISREvent = procedure(x: longint) of object;

  TInputPin = record
    Event : TISREvent;
    Debounce : double;
    Pin : integer;
    Command : integer;
  end;


  { TMainForm }

  TMainForm = class(TForm)
    BitBtnShutdown: TBitBtn;
    BitBtnReboot: TBitBtn;
    BitBtnClose: TBitBtn;
    Button1: TButton;
    CheckBoxLedR: TCheckBox;
    CheckBoxLedG: TCheckBox;
    CheckBoxLedB: TCheckBox;
    CheckBoxLedAnimation: TCheckBox;
    GroupBox1: TGroupBox;
    ImageLineIn: TImage;
    ImagePlayer: TImage;
    Label1: TLabel;
    Notebook1: TNotebook;
    Notebook2: TNotebook;
    PageControl1: TPageControl;
    PageScreensaver: TPage;
    PageOnOff: TPage;
    PageSetup: TPage;
    PageMP3: TPage;
    PageRadio: TPage;
    PagePlayer: TPage;
    PageSelect: TPage;
    PageLineIn: TPage;
    Panel1: TPanel;
    Panel3: TPanel;
    PanelDisplay: TPanel;
    PanelDivider: TPanel;
    PanelIconbar: TPanel;
    Panel2: TPanel;
    PanelImage: TPanel;
    PanelPlayer: TPanel;
    Scrollbox1: TScrollBox;
    TabSheet1: TTabSheet;
    TimerScrolling: TTimer;
    TimerPlayerIcons: TTimer;
    TimerIR: TTimer;
    TimerMotion: TTimer;
    TimerPlaying: TTimer;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure BitBtnRebootClick(Sender: TObject);
    procedure BitBtnShutdownClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImageLineInClick(Sender: TObject);
    procedure ImagePlayerClick(Sender: TObject);
    procedure Scrollbox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Scrollbox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Scrollbox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerIRTimer(Sender: TObject);
    procedure TimerMotionTimer(Sender: TObject);
    procedure TimerPlayerIconsTimer(Sender: TObject);
    procedure TimerPlayingTimer(Sender: TObject);
    procedure TimerScrollingTimer(Sender: TObject);
  private
    FScreenOn : boolean;
    channel : HSTREAM;
    FirstStart : boolean;
    IsPlaying : boolean;
    PlayingType : TPlayingType;
    FBackColor : TColor;
    FTextColor : TColor;
    Display : TScrollPanel;
    TickerPlayerIcons : integer;
    GPIOsimulation : boolean;
    LedR : boolean;
    LedG : boolean;
    LedB : boolean;
    LedAnimation : boolean;
    YstartRadio,VpRadio,RadioDir : integer;

    procedure ChangedSensorIR(x: longint);
    procedure ChangedSensorMotion(x: longint);
    procedure ChangedRotaryClick(x: longint);
    procedure ChangedRotaryTurn(x: longint);
    procedure PanelDisplayClick(Sender: TObject);
    procedure UpdateLed;

    procedure DoEncoderKeyClick;
    procedure DoEncoderLeftClick;
    procedure DoEncoderRightClick;
    procedure RadioItemEvent(Sender : TObject);
    procedure SaveConfig;
    procedure ScreenOn(OnOff: boolean);
    procedure SelectPlay(PlayElement: TPlayElement);
    procedure RadioItemClick(Sender: TObject);
    procedure PlayRadio;
    procedure StartTimerMotion;
    procedure StartTimerIR;
    procedure ShowPlayerIcons(IconsVisible: boolean);
  public
    procedure SetLedB(value: boolean);
    procedure SetLedG(value: boolean);
    procedure SetLedR(value: boolean);

    procedure ShowPage(PageNumber: integer);
    procedure DoCommand(Command: integer);
  end;

var
  MainForm: TMainForm;

implementation


{$R *.lfm}

var
  UseGpios : boolean;

  PinRotaryTurn,PinRotaryClick,PinSensorMotion,PinSensorIR : TInputPin;



function RandomBoolean : boolean;
begin
  if Random(2)=0 then result:=true
  else result:=false;
end;


function BoolToLevel(b : boolean) : TLevel;
begin
  b:=not b; //wegen Pull-Up Widerstand
  if b=true then result:=levHIGH
  else result:=levLOW;
end;

function LevelToBool(l : TLevel) : boolean;
begin
  if l=levHIGH then result:=true
  else result:=false;
end;

procedure UnixCommand(Command : string);
var p : TProcess;
begin
  try
    p:=TProcess.Create(nil);
    p.Options:=[poStderrToOutput];
    p.CommandLine:=Command;
    p.Execute;
  finally
    p.Free;
  end;
end;

//Funktionen für GPIO-INPUT's

procedure CallbackRotaryTurn;
begin
  Application.QueueAsyncCall(PinRotaryTurn.Event,0);
end;

procedure CallbackRotaryClick;
begin
  Application.QueueAsyncCall(PinRotaryClick.Event,0);
end;

procedure CallbackSensorMotion;
begin
  Application.QueueAsyncCall(PinSensorMotion.Event,0);
end;

procedure CallbackSensorIR;
begin
  Application.QueueAsyncCall(PinSensorIR.Event,0);
end;




{ TMainForm }

procedure TMainForm.ChangedRotaryTurn(x: longint);
begin
  {$IFDEF UNIX}
  if MilliSecondsBetween(PinRotaryTurn.Debounce,Now)>200 then
    begin
      if digitalRead(PIN_ROTARY_TURN1)=digitalRead(PIN_ROTARY_TURN2) then
        MainForm.DoCommand(COMMAND_ENCODER_LEFT)
      else MainForm.DoCommand(COMMAND_ENCODER_RIGHT);
      PinRotaryTurn.Debounce:=Now;
    end;
  {$ENDIF}

end;


procedure TMainForm.ChangedRotaryClick(x: longint);
begin
  if MilliSecondsBetween(PinRotaryClick.Debounce,Now)>200 then
    begin
      MainForm.DoCommand(COMMAND_ENCODER_KEY);
      PinRotaryClick.Debounce:=Now;
    end;
end;


procedure TMainForm.ChangedSensorMotion(x: longint);
begin
  if MilliSecondsBetween(PinSensorMotion.Debounce,Now)>1000 then
    begin
      GpioSimForm.SetTickerSignal(PIN_SENSOR_MOTION,LevelToBool(digitalRead(PIN_SENSOR_MOTION)));
      MainForm.DoCommand(COMMAND_SENSOR_MOTION);
      PinSensorMotion.Debounce:=Now;
    end;
end;

procedure TMainForm.ChangedSensorIR(x: longint);
begin
  if MilliSecondsBetween(PinSensorIR.Debounce,Now)>500 then
    begin
      if digitalRead(PIN_SENSOR_IR)=levHIGH then MainForm.DoCommand(COMMAND_SENSOR_IR);
      PinSensorIR.Debounce:=Now;
    end;
end;



//Funktionen für die LED'S

procedure TMainForm.UpdateLed;
begin
  SetLedR(LedR);
  SetLedG(LedG);
  SetLedB(LedB);
end;


procedure TMainForm.SetLedR(value : boolean);
begin
  LedR:=value;
  if UseGpios=true then
    begin
      {$IFDEF UNIX}
      digitalWrite(PIN_LED_R,BoolToLevel(value));
      {$ENDIF}
    end;
end;

procedure TMainForm.SetLedG(value : boolean);
begin
  LedG:=value;
  if UseGpios=true then
    begin
      {$IFDEF UNIX}
      digitalWrite(PIN_LED_G,BoolToLevel(value));
      {$ENDIF}
    end;
end;

procedure TMainForm.SetLedB(value : boolean);
begin
  LedB:=value;
  if UseGpios=true then
    begin
      {$IFDEF UNIX}
      digitalWrite(PIN_LED_B,BoolToLevel(value));
      {$ENDIF}
    end;
end;



procedure TMainForm.FormCreate(Sender: TObject);
var
  Inifile : TInifile;
  i : integer;
  s : string;
  RadioItem : TItemPanel;
begin
  FirstStart:=true;
  IsPlaying:=false;
  PlayingType:=ptNothing;
  Randomize;

  //Pfade
  ProgramPath:=ExtractFilePath(ParamStr(0));
  ConfigPath:=ProgramPath+'config'+PathDelim;
  LanguageFile:=ConfigPath+'language.conf';
  ConfigFile:=ConfigPath+'app.conf';
  RadioFile:=ConfigPath+'radio.conf';


  //Sprache laden
  try
    Inifile:=TInifile.Create(ConfigFile);
    Trans:=TTranslate.Create;
    Trans.Filename:=LanguageFile;
    Trans.ReadLanguages;
    Trans.Language:=Inifile.ReadString('Common','Language','DE');
    Trans.ReadTranslations;

    UseGpios:=Inifile.ReadBool('Common','Use GPIOs',false);

    FBackColor:=Inifile.ReadInteger('View','Backcolor',clBlack);
    FTextColor:=Inifile.ReadInteger('View','Textcolor',clWhite);

    if Inifile.ReadBool('View','Mouse',true)=false then
      Screen.Cursor:=crNone;

    if Inifile.ReadBool('View','Fullscreen',false)=true then
      begin
        WindowState:=wsMaximized;
        BorderIcons:=[];
        BorderStyle:=bsNone;
      end
    else
      begin
        WindowState:=wsNormal;
        BorderIcons:=[biMaximize,biMinimize,biSystemMenu];
        Width:=800;
        Height:=480;
      end;

    TimeoutIR:=Inifile.ReadInteger('Timeout','Infrared',10);
    TimeoutMotion:=Inifile.ReadInteger('Timeout','Motion',15);

    MP3Path:=Inifile.ReadString('MP3','Path','');

    GPIOsimulation:=Inifile.ReadBool('Development','GPIO simulation',false);


  finally
    Inifile.Free;
  end;

  //Farben anpassen
  Color:=FBackColor;
  Font.Color:=FTextColor;

  Display:=TScrollPanel.Create(nil);
  with Display do
    begin
      Align:=alClient;
      Text:='';
      Font.Assign(PanelDisplay.Font);
      Interval:=30;
      Step:=10; //1;
    end;
  PanelDisplay.InsertControl(Display);


  //Texte anpassen
  Caption:=Trans.Text('ApplicationTitle');
  GroupBox1.Caption:=Trans.Text('Augen');
  CheckboxLedR.Caption:=Trans.Text('Rot');
  CheckboxLedG.Caption:=Trans.Text('Grün');
  CheckboxLedB.Caption:=Trans.Text('Blau');
  CheckboxLedAnimation.Caption:=Trans.Text('Farbwechsel');

  Panel1.Font.Color:=FTextColor;

  //Linein-Bilder einlesen
  LoadSmileyPics(ConfigPath+'smiley');


  //BASS-Bibliothek laden
  if FileExists(ConfigPath+BASSLIB_FILE)=false then
    begin
      MessageDlg(Trans.Text('Achtung'),Trans.Text('BassError'),mtError,[mbOk],0);
      Halt;
  end;

  Load_BASSDLL(ConfigPath+BASSLIB_FILE);
  {if (HIWORD(BASS_GetVersion())<>BASSVERSION) then
    begin
      MessageDlg(Trans.Text('Achtung'),Trans.Text('BassError'),mtError,[mbOk],0);
      Halt;
  end;    }

  // Initialize audio - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(-1, 44100, 0, Handle, nil) then
    MessageDlg(Trans.Text('Achtung'),Trans.Text('BassInitError'),mtWarning,[mbOk],0);


  //Notwending fürs Radio
  BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1); // enable playlist processing
  BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0); // minimize automatic pre-buffering, so we can do it (and display it) instead
  BASS_SetConfigPtr(BASS_CONFIG_NET_PROXY, nil);



  IplRadio.BaseControl:=Scrollbox1;
  IplRadio.LoadFromDirectory(ConfigPath+'radio');

  IplRadio.SetEvent(RadioItemClick);

  IplRadio.SetMouseEvents(ScrollBox1MouseUp,ScrollBox1MouseDown,ScrollBox1MouseMove);

end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  i,j : integer;
  RadioItem : TItemPanel;
  Inifile : TInifile;
  LedStripe : string;
begin
  if FirstStart=false then exit;


  FirstStart:=false;

  IplIconbar.SetEvent(RadioItemEvent);
  for i:=6 downto 1 do
    begin
      RadioItem:=TItemPanel.Create(self);
      IplIconbar.Add(RadioItem);
      RadioItem.Height:=10;
      if i<6 then RadioItem.Align:=alTop
      else RadioItem.Align:=alBottom;
      RadioItem.Group:=rigIcon;

      RadioItem.Filename:=ConfigPath+'icons'+PathDelim+'iconbar'+IntToStr(i)+'.bmp';
      if i=6 then RadioItem.Transparent:=false
      else RadioItem.Transparent:=true;

      RadioItem.Title:=Trans.Text('IconBar-Title'+IntToStr(i));
      RadioItem.Command:=StrToInt(Trans.Text('IconBar-Command'+IntToStr(i)));
      RadioItem.BorderSpacing.Around:=10;
      RadioItem.UpdateView(true);
      RadioItem.OnPanelClick:=RadioItemEvent;

      PanelIconbar.InsertControl(RadioItem);
    end;


  IplPlayer.SetEvent(RadioItemEvent);
  for i:=1 to 4 do
    begin
      RadioItem:=TItemPanel.Create(self);
      IplPlayer.Add(RadioItem);

      RadioItem.Group:=rigPicture;

      RadioItem.Filename:=ConfigPath+'icons'+PathDelim+'player'+IntToStr(i)+'.bmp';
      RadioItem.Transparent:=true;

      RadioItem.Title:=Trans.Text('PlayerMenu-Title'+IntToStr(i));
      RadioItem.Command:=StrToInt(Trans.Text('PlayerMenu-Command'+IntToStr(i)));
      RadioItem.UpdateView(true);
      RadioItem.OnPanelClick:=RadioItemEvent;

      //RadioItem.Visible:=false;

      PanelImage.InsertControl(RadioItem);
    end;
  IplPlayer.Selected:=1;



  //Setup in WiringPi-Numbering
  wiringPiSetup();

  //Pin definitions
  PinRotaryTurn.Event:=ChangedRotaryTurn;
  pinmode(PIN_ROTARY_TURN1,pmINPUT);
  wiringPiISR(PIN_ROTARY_TURN1,intBoth,@CallbackRotaryTurn);

  pinmode(PIN_ROTARY_TURN2,pmINPUT);

  PinRotaryClick.Event:=ChangedRotaryClick;
  pinmode(PIN_ROTARY_CLICK,pmINPUT);
  wiringPiISR(PIN_ROTARY_CLICK,intRising,@CallbackRotaryClick);


  PinSensorMotion.Event:=ChangedSensorMotion;
  pinmode(PIN_SENSOR_MOTION,pmINPUT);
  wiringPiISR(PIN_SENSOR_MOTION,intRising,@CallbackSensorMotion);

  PinSensorIR.Event:=ChangedSensorIR;
  pinmode(PIN_SENSOR_IR,pmINPUT);
  wiringPiISR(PIN_SENSOR_IR,intRising,@CallbackSensorIR);

  pinmode(PIN_LED_R,pmOUTPUT);
  pinmode(PIN_LED_G,pmOUTPUT);
  pinmode(PIN_LED_B,pmOUTPUT);


  try
    Inifile:=TInifile.Create(ConfigFile);

    LedR:=Inifile.ReadBool('LED','R',false);
    LedG:=Inifile.ReadBool('LED','G',false);
    LedB:=Inifile.ReadBool('LED','B',false);
    LedAnimation:=Inifile.ReadBool('LED','Animation',false);

    CheckboxLedR.Checked:=LedR;
    CheckboxLedG.Checked:=LedG;
    CheckboxLedB.Checked:=LedB;
    CheckboxLedAnimation.Checked:=LedAnimation;
    UpdateLed;

    LedStripe:=ConfigPath+'ledstripe'+PathDelim+Inifile.ReadString('LED stripe','Pythonfile','');
    if FileExists(LedStripe)=true then UnixCommand('python '+LedStripe);

  finally
    Inifile.Free;
  end;



  ShowPage(PAGE_SELECT_RADIO);
  FormResize(Sender);
  Display.Active:=true;


  //GPIO-Simulation einschalten
  if GPIOsimulation=true then GpioSimForm.Show;

  IsPlaying:=false;
  TimerPlaying.Enabled:=true;

  StartTimerMotion;
  StartTimerIR;
end;

procedure TMainForm.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BitBtnRebootClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BitBtnShutdownClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  SaveConfig;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // BASS freigeben
  BASS_StreamFree(channel);
  BASS_Free();
  Unload_BASSDLL();

  Trans.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  i,j : integer;
  RadioItem : TItemPanel;
begin
  //Player
  PanelDisplay.Height:=Round(Height*0.3);
  Display.Font.Size:=Round(PanelDisplay.Height*0.4);

  Display.UpdateView;


  //Select
  PanelIconbar.Width:=Round(Width*0.2);
  PanelDivider.Width:=Round(Width*0.05);
  IplRadio.Resize(Round(Scrollbox1.Width*0.9),Scrollbox1.Height);

  for i:=0 to IplIconbar.Count-1 do
    begin
      RadioItem:=TItemPanel(IplIconbar.Data[i]);
      RadioItem.Resize(PanelIconbar.Width,Round(Height*0.14));
    end;

  j:=Round(PanelImage.Width/7);
  for i:=0 to IplPlayer.Count-1 do
    begin
      RadioItem:=TItemPanel(IplPlayer.Data[i]);
      if i=IplPlayer.Count-1 then RadioItem.Left:=(i+2)*j
      else RadioItem.Left:=(i+1)*j;

      RadioItem.Top:=Round(PanelImage.Height*0.3);
      RadioItem.Resize(Round(j*0.9),Round(PanelImage.Height*0.4));
    end;

end;

procedure TMainForm.ImageLineInClick(Sender: TObject);
begin
  DoEncoderKeyClick;
end;

procedure TMainForm.ImagePlayerClick(Sender: TObject);
begin
  DoEncoderKeyClick;
end;

procedure TMainForm.SaveConfig;
var
  Inifile : TInifile;
begin
  try
    Inifile:=TInifile.Create(ConfigFile);
    Inifile.WriteBool('LED','R',CheckboxLedR.Checked);
    Inifile.WriteBool('LED','G',CheckboxLedG.Checked);
    Inifile.WriteBool('LED','B',CheckboxLedB.Checked);
    Inifile.WriteBool('LED','Animation',CheckboxLedAnimation.Checked);
  finally
    Inifile.Free;
  end;
end;


procedure TMainForm.PanelDisplayClick(Sender: TObject);
begin
  ShowPage(PAGE_SELECT);
end;


procedure TMainForm.Scrollbox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  YstartRadio:=y;
  VpRadio:=Scrollbox1.VertScrollbar.Position;
end;

procedure TMainForm.Scrollbox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift=[ssLeft] then ScrollBox1.VertScrollbar.Position:=YstartRadio-y+VpRadio;
end;

procedure TMainForm.Scrollbox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if YstartRadio>y then RadioDir:=1
  else RadioDir:=-1;

  ScrollBox1.VertScrollbar.Position:=YstartRadio-y+VpRadio;
  //Je größer der Abstand von Y-Start bis zur aktuellen Position, desto länger
  //nachscrollen
  TimerScrolling.Tag:=Abs(Round((YstartRadio-y)*0.05));
  TimerScrolling.Enabled:=true;
end;


procedure TMainForm.TimerScrollingTimer(Sender: TObject);
begin
  ScrollBox1.VertScrollbar.Position:=ScrollBox1.VertScrollbar.Position+(RadioDir*TimerScrolling.Tag);
  TimerScrolling.Tag:=TimerScrolling.Tag-1;
  if TimerScrolling.Tag<=0 then TimerScrolling.Enabled:=false;
end;

procedure TMainForm.TimerIRTimer(Sender: TObject);
begin
  TickerIR:=TickerIR+1;
  GpioSimForm.SetTickerIR(TickerIR);
  if TickerIR>=TimeoutIR then
    begin
      TimerIR.Enabled:=false;
      ShowPlayerIcons(false);
    end;

end;

procedure TMainForm.TimerMotionTimer(Sender: TObject);
begin
  TickerMotion:=TickerMotion+1;
  GpioSimForm.SetTickerMotion(TickerMotion);
  if TickerMotion>=TimeoutMotion then
    begin
      TimerMotion.Enabled:=false;
      ScreenOn(false);
    end;
end;

procedure TMainForm.TimerPlayerIconsTimer(Sender: TObject);
const
  MAX_TICKER=15;
var
  i,j : integer;
  RadioItem : TItemPanel;
begin
  TickerPlayerIcons:=TickerPlayerIcons+1;
  if TickerPlayerIcons>=MAX_TICKER then
    begin
      TimerPlayerIcons.Enabled:=false;
      if TimerPlayerIcons.Tag=0 then IplPlayer.Visible:=false;
      exit;
    end;

  if TimerPlayerIcons.Tag=1 then
    begin
      //Einblenden
      j:=Round(PanelImage.Width/7*(TickerPlayerIcons/MAX_TICKER));
    end
  else
    begin
      //Ausblenden
      j:=Round(PanelImage.Width/7*((MAX_TICKER-TickerPlayerIcons)/MAX_TICKER));
    end;

    for i:=0 to IplPlayer.Count-1 do
    begin
      RadioItem:=TItemPanel(IplPlayer.Data[i]);
      if i=IplPlayer.Count-1 then RadioItem.Left:=(i+2)*j
      else RadioItem.Left:=(i+1)*j;

      //RadioItem.Top:=Round(PanelImage.Height*0.3);
      RadioItem.Resize(Round(j*0.9),Round(PanelImage.Height*0.4));
    end;

  if TickerPlayerIcons=1 then
    begin
      if TimerPlayerIcons.Tag=1 then IplPlayer.Visible:=true;
    end;

end;

procedure TMainForm.TimerPlayingTimer(Sender: TObject);
var
  title : string;
  p,t : dword;
  meta: PAnsiChar;
  i,po: Integer;
begin
  if Notebook1.PageIndex=MAIN_PAGE_PLAYER_SMILEY then
    begin
      TimerPlaying.Tag:=TimerPlaying.Tag+1;
      if TimerPlaying.Tag>10 then
        begin
          //alle 10 sek Bild wechseln
          TimerPlaying.Tag:=0;
          SmileyPic:=SmileyPic+1;
          if SmileyPic>SmileyPics.Count-1 then
            SmileyPic:=0;
          if FileExists(SmileyPics[SmileyPic])=true then
            ImageLineIn.Picture.LoadFromFile(SmileyPics[SmileyPic]);

        end;

      exit;
    end;

  if IsPlaying=false then exit;

  TimerPlaying.Tag:=TimerPlaying.Tag+1;
  if TimerPlaying.Tag>10 then
    begin   //alle 10 sek Meta-Tags abrufen
      TimerPlaying.Tag:=0;

      title:='';
      meta:=BASS_ChannelGetTags(channel,BASS_TAG_META);
      if (meta<>nil) then
        begin
          po:=Pos('StreamTitle=',String(AnsiString(meta)));
          if (po>0) then
            begin
              po:=po+13;
              title:=(SysToUTF8(Copy(meta,po,Pos(';',String(meta))-po-1)));
            end;
        end;
      TItemPanel(IplIconbar.Data[0]).Info:=title;
      TItemPanel(IplIconbar.Data[0]).UpdateView(false);
      Display.Text:=title;
    end;

  if LedAnimation=true then
    begin
      SetLedR(RandomBoolean);
      SetLedG(RandomBoolean);
      SetLedB(RandomBoolean);
    end;

end;


procedure TMainForm.RadioItemClick(Sender: TObject);
begin
  if not (Sender is TItemPanel) then exit;

  //TItemPanel(Sender).Selected:=true;

  CurrentRadioItem:=TItemPanel(Sender);
  PlayingType:=ptRadio;
  DoCommand(COMMAND_PLAY);

end;

procedure TMainForm.PlayRadio;
var
  icy: PAnsiChar;
  Len, Progress: DWORD;
  s,radio,bitrate : string;
begin
  Display.Text:=CurrentRadioItem.Url;

  BASS_StreamFree(channel);
  progress:=0;

  radio:='';
  bitrate:='';

  channel:=BASS_StreamCreateURL(PAnsiChar(CurrentRadioItem.Url),0,
    BASS_STREAM_BLOCK or BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE,nil,nil);
  if (channel<>0) then
  begin
    repeat
      len:=BASS_StreamGetFilePosition(channel,BASS_FILEPOS_END);
      if (len=DW_Error) then
        break; // something's gone wrong! (eg. BASS_Free called)
      progress:=BASS_StreamGetFilePosition(channel,BASS_FILEPOS_BUFFER)*100 div len;
      // percentage of buffer filled
    until
      (progress>75) or (BASS_StreamGetFilePosition(channel, BASS_FILEPOS_CONNECTED)=0);
      // over 75% full (or end of download)

    // get the broadcast name and bitrate
    icy:=BASS_ChannelGetTags(channel,BASS_TAG_ICY);
    if (icy=nil) then
      icy:=BASS_ChannelGetTags(channel,BASS_TAG_HTTP); // no ICY tags, try HTTP
    if (icy<>nil) then
      while (icy^<>#0) do
      begin
        if (Copy(icy,1,9)='icy-name:') then
          radio:=(trim(Copy(icy, 10, MaxInt)))
        else if (Copy(icy, 1, 7) = 'icy-br:') then
          bitrate:=('bitrate: '+Copy(icy,8,MaxInt));
        icy:=icy+Length(icy)+1;
      end;


    s:='';
    if radio<>'' then s:=radio;
    if bitrate<>'' then s:=s+' ('+bitrate+')';

    TItemPanel(IplIconbar.Data[0]).Title:=radio;
    TItemPanel(IplIconbar.Data[0]).Info:=bitrate;
    TItemPanel(IplIconbar.Data[0]).UpdateView(false);

    Display.Text:=s;
    BASS_ChannelPlay(channel,false);
  end;

  IsPlaying:=true;
  PlayingType:=ptRadio;

  s:=ExtractFilePath(RadioFile)+'radio'+PathDelim+CurrentRadioItem.FilenameLarge;
  if FileExists(s)=true then
    begin
      ImagePlayer.Picture.LoadFromFile(s);
    end
  else ImagePlayer.Picture:=nil;
  TItemPanel(IplIconbar.Data[0]).Image.Picture.Bitmap.Assign(ImagePlayer.Picture.Bitmap);

end;


procedure TMainForm.ShowPage(PageNumber : integer);
begin
  if PageNumber=PAGE_SCREENSAVER then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_SCREENSAVER;
    end;

  if PageNumber=PAGE_PLAYER then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_PLAYER;
    end;

  if PageNumber=PAGE_SELECT then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_SELECT;
    end;

  if PageNumber=PAGE_SELECT_RADIO then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_SELECT;
      Notebook2.PageIndex:=SELECT_PAGE_RADIO;
    end;

  if PageNumber=PAGE_SELECT_MP3 then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_SELECT;
      Notebook2.PageIndex:=SELECT_PAGE_MP3;
    end;

  if PageNumber=PAGE_SETUP then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_SELECT;
      Notebook2.PageIndex:=SELECT_PAGE_SETUP;
    end;

  if PageNumber=PAGE_ONOFF then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_SELECT;
      Notebook2.PageIndex:=SELECT_PAGE_ONOFF;
    end;

  if PageNumber=PAGE_PLAYER_SMILEY then
    begin
      Notebook1.PageIndex:=MAIN_PAGE_PLAYER_SMILEY;
    end;

  FormResize(nil);
end;

procedure TMainForm.ShowPlayerIcons(IconsVisible : boolean);
begin
  if IconsVisible=true then TimerPlayerIcons.Tag:=1
  else TimerPlayerIcons.Tag:=0;

  TickerPlayerIcons:=0;
  TimerPlayerIcons.Enabled:=true;
end;


procedure TMainForm.SelectPlay(PlayElement : TPlayElement);
var Ril : TItemPanelList;
begin
  Ril:=nil;
  if PlayingType=ptRadio then Ril:=IplRadio
  else Ril:=IplRadio;


  if CurrentRadioItem=nil then CurrentRadioItem:=TItemPanel(Ril.Data[0]);


  if PlayElement=peLast then CurrentRadioItem:=Ril.GetLast(CurrentRadioItem)
  else CurrentRadioItem:=Ril.GetNext(CurrentRadioItem);
end;

procedure TMainForm.RadioItemEvent(Sender: TObject);
begin
  if Sender is TItemPanel then
    DoCommand(TItemPanel(Sender).Command);
end;


procedure TMainForm.DoCommand(Command : integer);
begin
  //zurücksetzen, da User-Aktion
  StartTimerIR;
  StartTimerMotion;

  case Command of
    COMMAND_LAST:
    begin
      SelectPlay(peLast);
      IsPlaying:=false;
      DoCommand(COMMAND_PLAY);
    end;

    COMMAND_NEXT:
    begin
      SelectPlay(peNext);
      IsPlaying:=false;
      DoCommand(COMMAND_PLAY);
    end;

    COMMAND_PLAY:
    begin
      if IsPlaying=true then
        begin
          //Pause
          BASS_ChannelStop(channel);
          IsPlaying:=false;

          //LED's auf Standard setzen
          SetLedR(CheckboxLedR.Checked);
          SetLedG(CheckboxLedG.Checked);
          SetLedB(CheckboxLedB.Checked);

        end
      else
        begin
          //Play
          TimerPlaying.Tag:=99;
          if PlayingType=ptRadio then PlayRadio;
        end;

    end;

    COMMAND_ENCODER_LEFT : DoEncoderLeftClick;
    COMMAND_ENCODER_RIGHT : DoEncoderRightClick;
    COMMAND_ENCODER_KEY : DoEncoderKeyClick;

    COMMAND_SENSOR_MOTION : StartTimerMotion;
    COMMAND_SENSOR_IR : StartTimerIR;

    COMMAND_LED: UpdateLed;


    COMMAND_SELECT_MENU: ShowPage(PAGE_SELECT);
    COMMAND_SELECT_MENU_RADIO: ShowPage(PAGE_SELECT_RADIO);
    COMMAND_SELECT_MENU_MP3: ShowPage(PAGE_SELECT_MP3);
    COMMAND_SELECT_LINEIN: ShowPage(PAGE_PLAYER_SMILEY);
    COMMAND_SELECT_SETUP: ShowPage(PAGE_SETUP);
    COMMAND_SELECT_ONOFF: ShowPage(PAGE_ONOFF);
    COMMAND_PLAYER: ShowPage(PAGE_PLAYER);

  end;

end;


procedure TMainForm.DoEncoderLeftClick;
begin
  //Welche Seite ist sichtbar?
  if Notebook1.PageIndex=MAIN_PAGE_PLAYER then
    begin
      //Player-Leiste ist eingeblendet
      if IplPlayer.Visible=false then ShowPlayerIcons(true);
      if IplPlayer.Selected=-1 then IplPlayer.Selected:=0
      else if IplPlayer.Selected>0 then
          IplPlayer.Selected:=IplPlayer.Selected-1;
    end

  else if (Notebook1.PageIndex=MAIN_PAGE_SELECT)
    and (Notebook2.PageIndex=SELECT_PAGE_RADIO) then
    begin
      if IplRadio.Selected=-1 then
        begin //Iconbar ist aktiv
          if IplIconbar.Selected>0 then
            IplIconbar.Selected:=IplIconbar.Selected-1
          else
            begin
              IplIconbar.Selected:=-1;
              IplRadio.Selected:=IplRadio.Count-1;
            end;
          ScrollBox1.VertScrollBar.Position:=IplRadio.GetSelectedTop;
        end
      else
        begin //Radio ist aktiv
          if IplRadio.Selected>0 then
            begin
              IplRadio.Selected:=IplRadio.Selected-1;
              ScrollBox1.VertScrollBar.Position:=IplRadio.GetSelectedTop;
            end
          else
            begin
              IplRadio.Selected:=-1;
              IplIconbar.Selected:=IplIconbar.Count-1;
            end;
        end;
    end;

end;

procedure TMainForm.DoEncoderRightClick;
begin
  //Welche Seite ist sichtbar?
  if Notebook1.PageIndex=MAIN_PAGE_PLAYER then
    begin
      //Player-Leiste ist eingeblendet
      if IplPlayer.Visible=false then ShowPlayerIcons(true);
      if IplPlayer.Selected=-1 then IplPlayer.Selected:=0
      else if IplPlayer.Selected<IplPlayer.Count-1  then
          IplPlayer.Selected:=IplPlayer.Selected+1;
    end

  else if (Notebook1.PageIndex=MAIN_PAGE_SELECT)
    and (Notebook2.PageIndex=SELECT_PAGE_RADIO) then
    begin
      if IplRadio.Selected=-1 then
        begin //Iconbar ist aktiv
          if IplIconbar.Selected<IplIconbar.Count-1 then
            IplIconbar.Selected:=IplIconbar.Selected+1
          else
            begin
              IplIconbar.Selected:=-1;
              IplRadio.Selected:=0;
            end;
          ScrollBox1.VertScrollBar.Position:=IplRadio.GetSelectedTop;
        end
      else
        begin //Radio ist aktiv
          if IplRadio.Selected<IplRadio.Count-1 then
            begin
              IplRadio.Selected:=IplRadio.Selected+1;
              ScrollBox1.VertScrollBar.Position:=IplRadio.GetSelectedTop;
            end
          else
            begin
              IplRadio.Selected:=-1;
              IplIconbar.Selected:=0;
            end;
        end;
    end;
end;

procedure TMainForm.DoEncoderKeyClick;
begin
  //Welche Seite ist sichtbar?
  if Notebook1.PageIndex=MAIN_PAGE_PLAYER then
    begin
      DoCommand(IplPlayer.GetSelectedCommand);
    end
  else if Notebook1.PageIndex=MAIN_PAGE_PLAYER_SMILEY then
    begin
      DoCommand(COMMAND_SELECT_MENU);
    end

  else if (Notebook1.PageIndex=MAIN_PAGE_SELECT)
    and (Notebook2.PageIndex=SELECT_PAGE_RADIO) then
    begin
      if IplIconbar.Selected>-1 then DoCommand(IplIconbar.GetSelectedCommand);
      if IplRadio.Selected>-1 then
        begin
          CurrentRadioItem:=IplRadio.GetSelected;
          IsPlaying:=false;
          PlayingType:=ptRadio;
          DoCommand(COMMAND_PLAY);
        end;
    end;
end;

procedure TMainForm.ScreenOn(OnOff : boolean);
begin
  FScreenOn:=OnOff;
  if OnOff=true then
    begin
      if IsPlaying=true then DoCommand(COMMAND_PLAYER)
      else DoCommand(COMMAND_SELECT_MENU)
    end
  else
    begin
      ShowPage(PAGE_SCREENSAVER); //only a black screen
      //check page to turn backlight off and on
      //https://techpluscode.de/ein-ausschalter-fuer-das-raspberry-pi-display
    end;
end;


procedure TMainForm.StartTimerMotion;
begin
  TickerMotion:=0;
  GpioSimForm.SetTickerMotion(TickerMotion);
  if TimerMotion.Enabled=false then TimerMotion.Enabled:=true;
  if FScreenOn=false then ScreenOn(true);
end;



procedure TMainForm.StartTimerIR;
begin
  TickerIR:=0;
  GpioSimForm.SetTickerIR(TickerIR);

  //Falls Bewegungserkennung nicht klappt, Bildschirm ein
  if FScreenOn=false then ScreenOn(true);


  if IplPlayer.Visible=false then ShowPlayerIcons(true);

  if TimerIR.Enabled=false then TimerIR.Enabled:=true;
end;

end.

