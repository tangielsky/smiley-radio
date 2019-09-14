unit gpiofunc;

//{$mode objfpc}{$H+}
{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, DateUtils,
  Controls, Graphics, Dialogs, StdCtrls,
  myobjects, wiringPi;





type
  TISREvent = procedure(x: longint) of object;

var
  LedR : boolean;
  LedG : boolean;
  LedB : boolean;
  LedAnimation : boolean;


function RandomBoolean : boolean;
function BoolToLevel(b : boolean) : TLevel;

procedure UpdateLed;
procedure SetLedR(value : boolean);
procedure SetLedG(value : boolean);
procedure SetLedB(value : boolean);


var
  UseGpios : boolean;

implementation

uses main;


var
  EventRotaryTurn,EventRotaryClick,EventMotionSensor,EventIrSensor : TISREvent;
  debounce : double;


function RandomBoolean : boolean;
begin
  if Random(2)=0 then result:=true
  else result:=false;
end;

function BoolToLevel(b : boolean) : TLevel;
begin
  b:=not b; //Pull-Up
  if b=true then result:=levHIGH
  else result:=levLOW;
end;




//Funktionen für die LED'S

procedure UpdateLed;
begin
  SetLedR(LedR);
  SetLedG(LedG);
  SetLedB(LedB);
end;

procedure SetLedR(value : boolean);
begin
  LedR:=value;
  if UseGpios=true then
    begin
      {$IFDEF UNIX}
      digitalWrite(PIN_LED_R,BoolToLevel(value));
      {$ENDIF}
    end;
end;

procedure SetLedG(value : boolean);
begin
  LedG:=value;
  if UseGpios=true then
    begin
      {$IFDEF UNIX}
      digitalWrite(PIN_LED_G,BoolToLevel(value));
      {$ENDIF}
    end;
end;

procedure SetLedB(value : boolean);
begin
  LedB:=value;
  if UseGpios=true then
    begin
      {$IFDEF UNIX}
      digitalWrite(PIN_LED_B,BoolToLevel(value));
      {$ENDIF}
    end;
end;



//Funktionen für GPIO-INPUT's

procedure CallbackRotaryTurn;
begin
  Application.QueueAsyncCall(EventRotaryTurn, 0);
end;

procedure CallbackRotaryClick;
begin
  Application.QueueAsyncCall(EventRotaryClick, 0);
end;

procedure CallbackMotionSensor;
begin
  Application.QueueAsyncCall(EventMotionSensor, 0);
end;

procedure CallbackIrSensor;
begin
  Application.QueueAsyncCall(EventIrSensor, 0);
end;



procedure ChangedRotaryTurn(x: longint);
begin
  if MilliSecondsBetween(debounce, now) > 100 then
    begin //entprellen
      if digitalRead(PIN_ROTARY_TURN1)=digitalRead(PIN_ROTARY_TURN2) then
        MainForm.DoCommand(COMMAND_ENCODER_LEFT)
      else MainForm.DoCommand(COMMAND_ENCODER_RIGHT);
      debounce:= now;
    end;

end;


procedure ChangedRotaryClick(x: longint);
begin
  if MilliSecondsBetween(debounce, now) > 100 then
    begin //entprellen
      MainForm.DoCommand(COMMAND_ENCODER_KEY);
      debounce:= now;
    end;

end;


procedure ChangedMotionSensor(x: longint);
begin
  if MilliSecondsBetween(debounce,now)>100 then
    begin
      MainForm.DoCommand(COMMAND_SENSOR_MOTION);
      debounce:= now;
    end;
end;

procedure ChangedIrSensor(x: longint);
begin
  if MilliSecondsBetween(debounce,now)>100 then
    begin
      MainForm.DoCommand(COMMAND_SENSOR_ir);
      debounce:= now;
    end;
end;


procedure Setup;
begin
  Randomize;
  wiringPiSetup(); //Setup in WiringPi-Numbering

  debounce:=now;

  //Pin definitions
  pinmode(PIN_ROTARY_TURN1,pmINPUT);
  EventRotaryTurn:=ChangedRotaryTurn;
  wiringPiISR(PIN_ROTARY_TURN1,intBoth,@CallbackRotaryTurn);

  pinmode(PIN_ROTARY_TURN2,pmINPUT);

  pinmode(PIN_ROTARY_CLICK,pmINPUT);
  EventRotaryClick:=ChangedRotaryClick;
  wiringPiISR(PIN_ROTARY_CLICK,intRising,@CallbackRotaryClick);


  pinmode(PIN_MOTION_SENSOR,pmINPUT);
  EventMotionSensor:=ChangedMotionSensor;
  wiringPiISR(PIN_MOTION_SENSOR,intRising,@CallbackMotionSensor);

  pinmode(PIN_IR_SENSOR,pmINPUT);
  EventIrSensor:=ChangedIrSensor;
  wiringPiISR(PIN_IR_SENSOR,intRising,@CallbackIrSensor);

  pinmode(PIN_LED_R,pmOUTPUT);
  pinmode(PIN_LED_G,pmOUTPUT);
  pinmode(PIN_LED_B,pmOUTPUT);

  SetLedR(false);
  SetLedG(false);
  SetLedB(false);


end;

initialization
  Setup;


end.

