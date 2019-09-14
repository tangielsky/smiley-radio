unit scrollpanel;

{$mode objfpc}{$H+}

interface

uses
  LResources, LMessages, Classes, SysUtils, ExtCtrls, Graphics, Controls;

type

  { TScrollPanel }

  TScrollPanel = class(TPanel)
    private
      FText : string;
      FInterval : integer;
      FStep : integer;
      FActive : boolean;

      Timer : TTimer;
      Bitmap : TBitmap;
      x : integer;

      procedure DoTimer(Sender : TObject);

      procedure SetInterval(value : integer);
      procedure SetStep(value : integer);
      procedure SetActive(value : boolean);
      procedure SetText(value : string);
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure UpdateView;

    protected
      procedure DoOnChangeBounds; override;

    published
      property Text : string read FText write SetText;
      property Interval : integer read FInterval write SetInterval;
      property Step : integer read FStep write SetStep;
      property Active : boolean read FActive write SetActive;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mylib', [TScrollPanel]);
end;

{ TScrollPanel }

procedure TScrollPanel.DoTimer(Sender: TObject);
begin
  Bitmap.Canvas.Font.Assign(Font);
  Bitmap.Canvas.Brush.Assign(Brush);

  x:=x-FStep;
  if x<-Bitmap.Canvas.TextWidth(FText) then x:=Width;

  Bitmap.Canvas.FillRect(0,0,Bitmap.Width,Bitmap.Height);
  Bitmap.Canvas.TextOut(x,10,FText);

  //nach links scrollen
  Canvas.Draw(0,0,Bitmap);
end;

procedure TScrollPanel.SetInterval(value: integer);
begin
  if value=FInterval then exit;
  if value>0 then
    begin
      FInterval:=value;
      Timer.Interval:=FInterval;
    end;
end;

procedure TScrollPanel.SetStep(value: integer);
begin
  if value=FStep then exit;
  if value>0 then
    begin
      FStep:=value;
    end;
end;

procedure TScrollPanel.SetActive(value: boolean);
begin
  if value=FActive then exit;
  FActive:=value;
  Timer.Enabled:=FActive;
  UpdateView;
end;

procedure TScrollPanel.SetText(value: string);
begin
  if value=FText then exit;
  FText:=value;
  x:=Width;
end;

procedure TScrollPanel.UpdateView;
begin
  Bitmap.Width:=Width;
  Bitmap.Height:=Height;
end;

constructor TScrollPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Bitmap:=TBitmap.Create;

  ControlStyle := ControlStyle + [csOpaque];

  FActive:=false;
  FStep:=0;
  FInterval:=0;
  FText:='';
  Width:=100;
  Height:=100;
  BorderStyle:=bsNone;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;

  Timer:=TTimer.Create(TheOwner);
  Timer.OnTimer:=@DoTimer;

  //Standardwerte
  SetStep(1);
  SetInterval(30);
  SetText(Caption);


end;

destructor TScrollPanel.Destroy;
begin
  Timer.Free;
  Bitmap.Free;
  inherited Destroy;
end;

procedure TScrollPanel.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

end;

initialization


end.

