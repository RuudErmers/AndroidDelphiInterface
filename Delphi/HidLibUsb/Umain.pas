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
     Simple application to test a Delphi <> HID connection (implementing a simple zero-based String protocol)

   Notes:
    1. This library does not take in account removing or adding USB devices
    2. Tested with Delphi 2010
    3. You might test this with the supplied Aeduino and MBed examples
 *)

unit Umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,ULibUSBHid, ExtCtrls;

type
  TFormMain = class(TForm)
    MemoDebug: TMemo;
    Button3: TButton;
    ComboBox1: TComboBox;
    btnEnable: TButton;
    btnDisable: TButton;
    Timer1: TTimer;
    procedure Button3Click(Sender: TObject);
    procedure btnEnableClick(Sender: TObject);
    procedure btnDisableClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    LibUSBHid:TLibUSBHid;
    LoopEnabled:boolean;
    Tick:integer;
    writectl:boolean;
    procedure OnDebugEvent(s: string);
    procedure OnTextEvent(s: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses LibUSBK;

procedure TFormMain.btnDisableClick(Sender: TObject);
begin
  LoopEnabled:=false;
end;

procedure TFormMain.btnEnableClick(Sender: TObject);
begin
  if LibUSBHid = NIL then exit;
  Tick:=0;
  LoopEnabled:=true;
end;

procedure TFormMain.Button3Click(Sender: TObject);
VAR vid,pid:integer;
begin
  if ComboBox1.ItemIndex = 0 then
  begin
    // Arduino
    vid := 1537;
    pid:=2;
    writectl:=true;
  end
  else
  begin
    // Embed
    vid := 1537;
    pid:=1;
    writectl:=false;
  end;
  LibUSBHid:=TLibUSBHid.Create(OnDebugEvent,OnTextEvent);
  LibUSBHid.Open(vid,pid);
end;

procedure TFormMain.OnTextEvent(s:string);
begin
  MemoDebug.Lines.Add('Device: '+s);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  if LoopEnabled then
  begin
    Tick := (Tick + 1) MOD 60;
    LibUSBHid.WriteString('Hello Android' + inttostr(Tick));
  end;
end;

procedure TFormMain.OnDebugEvent(s:string);
begin
  MemoDebug.Lines.Add('Debug'+s);
end;

end.
