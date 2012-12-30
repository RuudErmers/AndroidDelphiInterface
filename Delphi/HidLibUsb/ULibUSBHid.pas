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
 *)

 (* Usage:
    TLibUSBHid is a simple class to communicate with a HID device.
    To create an object supply two eventhandlers to receive Debug information (_OnDebugInfoEvent_) and information send by the device (_OnReceiveEvent_)
    Use _Open_<vid,pid> to start communication with the device
    Use _WriteString_ to write a string to the device

    Notes:
    1. This library does not take in account removing or adding USB devices
    2. Tested with Delphi 2010
    3. You might test this with the supplied Aeduino and MBed examples
*)

unit ULibUSBHid;

interface

uses LibUSBK, ULibUSBDevice, StdCtrls,Classes,Messages;

type
     TOnTextReceived = procedure (s:string) of object;
     TLibUSBHid = class (TLibUSBDevice)
private
    TheText:string;
    FOnReceiveTextEvent:TOnInfoEvent;
    procedure OnReceiveEvent(nx:TLibUSBDeviceData);
public
    procedure WriteString(s: string);
    procedure Open(Vid,Pid:integer);
    constructor Create(OnDebugInfoEvent,OnReceiveTextEvent:TOnInfoEvent); virtual;
end;

implementation

uses SysUtils,Windows,Forms;


{ TLibUSBHid }

constructor TLibUSBHid.Create(OnDebugInfoEvent,OnReceiveTextEvent:TOnInfoEvent);
begin
  FOnReceiveTextEvent:=OnReceiveTextEvent;
  inherited Create(OnDebugInfoEvent,OnReceiveEvent);
  TheText:='';
end;

procedure TLibUSBHid.OnReceiveEvent(nx: TLibUSBDeviceData);
  procedure Emit;
  begin
    if TheText = '' then exit;
    if assigned(FOnReceiveTextEvent) then FOnReceiveTextEvent(TheText);
    TheText:='';
  end;
VAR i,c:integer;
begin
  for i:=0 to nx.length-1 do
  begin
    c:=nx.data[i];
    if c=1 then c:=97;
    if c=2 then c:=98;
    if c=0 then begin Emit; Exit; end
    else TheText:=TheText+chr(c);
  end;
end;

procedure TLibUSBHid.Open(Vid, Pid: integer);
begin
  inherited Open(Vid,Pid);
end;

procedure TLibUSBHid.WriteString(s: string);
begin
  inherited;
end;

end.
