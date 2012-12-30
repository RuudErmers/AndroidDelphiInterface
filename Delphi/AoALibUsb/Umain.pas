(*
 * (C) Copyright 2012 Ruud Ermers.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser General Public License
 * (LGPL) version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * Contributors:
 *     Ruud Ermers
 * History
     2012-12-28    1.0 Initial Release

   Usage:
     Simple application to test a Delphi <> Android Open Accesory interface

   Notes:
     1. To setup an Android <-> Delphi bridge you should first install a LibusbK
        driver for Android. See: http://sourceforge.net/projects/libusbk
        You could use the supplied DelphiAOABridge to test your communication and build from there
        You could also the use the Microchip example (Android side) available from Microchip
     2. Android Open Accessory protocol only works on specific devices with Android Version >= 2.3.3
     3. This library does not take in account removing or adding USB devices
     4. Tested with Delphi 2010
 *)

unit Umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,ULibUSBAOA, ExtCtrls, ComCtrls, Spin;

type
  TFormMain = class(TForm)
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Timer1: TTimer;
    RadioButton1: TRadioButton;
    rbadbon2d01: TRadioButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    cb1: TCheckBox;
    cb2: TCheckBox;
    cb3: TCheckBox;
    SpinEdit1: TSpinEdit;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure cb1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    LibUSBAOA:TLibUSBAOA;
    LoopEnabled,Inited:boolean;
    Tick:integer;
    procedure OnDebugEvent(s: string);
    procedure OnTextEvent(s: string);
    procedure Init;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses LibUSBK;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  Tick:=0;
  LoopEnabled:=true;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  LoopEnabled:=false;
end;

procedure TFormMain.Button3Click(Sender: TObject);
begin
  Init;
end;

procedure TFormMain.Init;
VAR adbchannel:integer;
begin
  LibUSBAOA:=TLibUSBAOA.Create(OnDebugEvent,NIL);
  if rbadbon2d01.Checked then adbchannel:=$2D01 else adbchannel:=$2D00;
  LibUSBAOA.Open(StrToInt(Edit1.Text),StrToInt(Edit2.Text),adbchannel);
end;

procedure TFormMain.Button4Click(Sender: TObject);
begin
  if LibUSBAOA = NIL then Init;
  LibUSBAOA.SwitchToAccessory;
  Inited:=true;
end;

const POT_STATUS_CHANGE = 3;
const PUSHBUTTON_STATUS_CHANGE = 2;

procedure TFormMain.Button5Click(Sender: TObject);
VAR buffer: array[0..1] of byte;
begin
  if not Inited then exit;
  buffer[0]:=PUSHBUTTON_STATUS_CHANGE;
  buffer[1]:=8;
  LibUSBAOA.WriteBytes(2,buffer);
end;

procedure TFormMain.Button6Click(Sender: TObject);
begin
  LibUSBAOA.WriteString('Hallo Fans');
end;

procedure TFormMain.cb1Click(Sender: TObject);
VAR buffer: array[0..1] of byte;
begin
  if not Inited then exit;
  buffer[0]:=PUSHBUTTON_STATUS_CHANGE;
  buffer[1]:=ord(cb1.Checked)+2*ord(cb2.Checked)+4*ord(cb3.Checked);
  LibUSBAOA.WriteBytes(2,buffer);
end;

procedure TFormMain.OnDebugEvent(s:string);
begin
  Memo1.Lines.Add('Debug: '+s);
end;

procedure TFormMain.OnTextEvent(s: string);
begin
  Memo1.Lines.Add('Receive: '+s);
end;

procedure TFormMain.SpinEdit1Change(Sender: TObject);
VAR buffer: array[0..1] of byte;
begin
  if not Inited then exit;
  buffer[0]:=POT_STATUS_CHANGE;
  buffer[1]:=SpinEdit1.Value;
  LibUSBAOA.WriteBytes(2,buffer);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  if LoopEnabled then
  begin
    Tick := (Tick + 1) MOD 60;
    LibUSBAOA.WriteString('Hello Android' + inttostr(Tick));
  end;
end;

end.
