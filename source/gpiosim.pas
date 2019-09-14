unit gpiosim;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TGpioSimForm }

  TGpioSimForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label4b: TLabel;
    Label5b: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure SetTickerSignal(Pin: integer; value: boolean);
    procedure SetTickerIR(ticker : integer);
    procedure SetTickerMotion(ticker: integer);
  end;

var
  GpioSimForm: TGpioSimForm;



implementation

uses main, myobjects;

{$R *.lfm}


function BooleanToStr(value : boolean) : string;
begin
  if value=true then result:='1'
  else result:='0';
end;


{ TGpioSimForm }



procedure TGpioSimForm.Button1Click(Sender: TObject);
begin
  MainForm.DoCommand(COMMAND_ENCODER_LEFT);
end;

procedure TGpioSimForm.Button2Click(Sender: TObject);
begin
  MainForm.DoCommand(COMMAND_ENCODER_RIGHT);
end;

procedure TGpioSimForm.Button3Click(Sender: TObject);
begin
  MainForm.DoCommand(COMMAND_ENCODER_KEY);
end;

procedure TGpioSimForm.Button4Click(Sender: TObject);
begin
  MainForm.DoCommand(COMMAND_SENSOR_MOTION);
end;

procedure TGpioSimForm.Button5Click(Sender: TObject);
begin
  MainForm.DoCommand(COMMAND_SENSOR_IR);
end;

procedure TGpioSimForm.CheckBox1Change(Sender: TObject);
begin
  MainForm.SetLedR(Checkbox1.Checked);
end;

procedure TGpioSimForm.CheckBox2Change(Sender: TObject);
begin
  MainForm.SetLedG(Checkbox2.Checked);
end;

procedure TGpioSimForm.CheckBox3Change(Sender: TObject);
begin
  MainForm.SetLedB(Checkbox3.Checked);
end;

procedure TGpioSimForm.FormCreate(Sender: TObject);
begin
  Left:=10;
  Top:=10;
end;

procedure TGpioSimForm.SetTickerSignal(Pin : integer; value : boolean);
begin
  if Pin=PIN_SENSOR_MOTION then
    Label4b.Caption:=BooleanToStr(value)+'-'+FormatDateTime('nn:ss',Now);
  if Pin=PIN_SENSOR_IR then
    Label5b.Caption:=BooleanToStr(value)+'-'+FormatDateTime('nn:ss',Now);
end;


procedure TGpioSimForm.SetTickerIR(ticker: integer);
begin
  Label5.Caption:=IntToStr(TimeoutIR-ticker)+'s';
end;

procedure TGpioSimForm.SetTickerMotion(ticker: integer);
begin
  Label4.Caption:=IntToStr(TimeoutMotion-ticker)+'s';
end;



end.

