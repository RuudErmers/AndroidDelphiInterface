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
 *     ...
 * History
     2012-12-28    1.0 Initial Release

   Usage:
     Simple application to test a Delphi <> Android Debug interface

   Note:
    1. To setup an Android <-> Delphi bridge you should first install a LibusbK
       driver for Android. See: http://sourceforge.net/projects/libusbk
       You could use the supplied DelphiAdbBridge to test your communication and build from there
    2. This library does not take in account removing or adding USB devices
    3. Tested with Delphi 2010
 *)
unit Umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,ULibUSBAdb, ExtCtrls;

type
  TFormMain = class(TForm)
    Memo1: TMemo;
    btnInit: TButton;
    btnEnable: TButton;
    Timer1: TTimer;
    btnDisable: TButton;
    btnOpen: TButton;
    procedure btnInitClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnEnableClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnDisableClick(Sender: TObject);
  private
    LibUSBAdb:TLibUsbAdb;
    LoopEnabled:boolean;
    Tick:integer;
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

procedure TFormMain.btnEnableClick(Sender: TObject);
begin
  if LibUSBAdb=NIL then exit;
  Tick:=0;
  LoopEnabled:=true;
end;

procedure TFormMain.btnDisableClick(Sender: TObject);
begin
  LoopEnabled:=false;
end;

procedure TFormMain.btnInitClick(Sender: TObject);
begin
  if LibUSBAdb=NIL then
    LibUSBAdb:=TLibUSBAdb.Create(OnDebugEvent,OnTextEvent);
  LibUSBAdb.Init;
end;

procedure TFormMain.btnOpenClick(Sender: TObject);
begin
  if LibUSBAdb.Open then
    Memo1.Lines.Add('Adb Succesfully Opened')
  else
    Memo1.Lines.Add('Adb NOT Opened (Application Not Running?')
end;

procedure TFormMain.OnDebugEvent(s:string);
begin
  Memo1.Lines.Add('Debug: '+s);
end;

procedure TFormMain.OnTextEvent(s:string);
begin
  Memo1.Lines.Add('Text: ' + s);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  if LoopEnabled then
  begin
    Tick := (Tick + 1) MOD 60;
    LibUSBAdb.WriteString('Hello Android' + inttostr(Tick));
  end;
end;

end.
