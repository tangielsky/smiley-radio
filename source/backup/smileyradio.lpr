program smileyradio;

(*
Werkstatt-Webradio für einen Raspberry Pi mit:
- Multi-LEDs
- Dreh-Encoder
- Bewegungsmelder
- Infrarotmelder
- LED Band

Das Projekt benötigt folgende Bibliotheken:
- wiringpi (http://wiringpi.com/download-and-install)
- Freepascal Wrapper für wiringpi (https://www.bw38.de/lazarusbasics)
- BASS (Source-Code unter www.un4seen.com)

Mehr Informationen unter zum Projekt:

Teil 1: Werkstatt-Radio
        https://techpluscode.de/werkstatt-radio-mit-raspberry-pi

Teil 2: Elektronik und Gehäuse
        https://techpluscode.de/raspberry-pi-werkstatt-radio-elektronik-und-gehaeuse

Teil 3: Software, GPIO und Webradio
        https://techpluscode.de/werkstatt-radio-software-gpio-und-webradio/



(C) T. Angielsky

Version 0.5, 14.09.2019

*)



//{$mode objfpc}{$H+}
{$mode delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, gpiosim, wiringPi;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Smiley-Radio';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TGpioSimForm, GpioSimForm);
  Application.Run;
end.

