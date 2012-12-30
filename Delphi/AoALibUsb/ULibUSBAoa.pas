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
 *)

 (* Usage:
    TLibUsbAOA is a class to communicate with any Android device supporting Adk. (aka: AOA = Android Open Accessory)
    You first have to setup your device with LibusbK driver (see Notes)
    To create an object supply two eventhandlers to receive Debug information (_OnDebugInfoEvent_) and information send by the device (_OnReceiveEvent_)

    Use _Open_ to open the Android device (supplying <vid,pid> and a second pid when the devices has changed to accessory mode (should be $2D00 or $2D01 )
    Use _SwitchToAccessory_ to switch to accessory mode
    Use _WriteString_ to write a string to the device

    Notes:
    1. To setup an Android <-> Delphi bridge you should first install a LibusbK
       driver for Android. See: http://sourceforge.net/projects/libusbk
       You could use the supply DelphiAOABridge to test your communication and build from there

    2. Android Open Accessory protocol only works on specific devices with Android Version >= 2.3.3
    3. This library does not take in account removing or adding USB devices
    4. Tested with Delphi 2010
*)

unit ULibUSBAoa;

interface

uses LibUSBK, ULibUSBDevice, StdCtrls,Classes,Messages;

type
     TLibUSBAoa = class (TLibUSBDevice)
private
  AOAPid:integer;
  FOnReceiveTextEvent:TOnInfoEvent;
  procedure OnReceiveEvent(nx: TLibUSBDeviceData);
public
  procedure SwitchToAccessory;
  function  Open(Vid,Pid,aAOAPid:integer): boolean;
  procedure WriteString(s:string);
  procedure WriteBytes(count: integer; buffer: array of byte);
  constructor Create(OnDebugInfoEvent,OnReceiveTextEvent:TOnInfoEvent); virtual;
end;

implementation

uses SysUtils,Windows,Forms;

constructor TLibUSBAoa.Create(OnDebugInfoEvent,  OnReceiveTextEvent: TOnInfoEvent);
begin
  FOnReceiveTextEvent:=OnReceiveTextEvent;
  inherited Create(OnDebugInfoEvent,OnReceiveEvent);
end;

function TLibUSBAoa.Open(Vid,Pid,aAOAPid:integer): boolean;
begin
  AOAPid:=aAOAPid;
  result:=inherited Open(Vid,Pid);
end;

procedure TLibUSBAoa.SwitchToAccessory;
VAR  req: Int64;
     procedure WriteString(index:Int64;s:string);
     VAR buffer: array[0..512] of byte;
         i:integer;
     begin
       for i:=0 to length(s)-1 do
         buffer[i]:=ord(s[i+1]);
       buffer[length(s)]:=0;
       UsbK_ControlTransfer(DeviceHandle,($40 SHL 0) + (52 SHL 8)+ (index SHL 32),@buffer,length(s)+1,NIL,NIL);
       Sleep(300);
     end;
VAR i,BytesTRansferred:cardinal;
    buffer:array[0..512] of byte;
begin
  Assert((SizeOf(WINUSB_SETUP_PACKET) = 8),'WINUSB_SETUP_PACKET size violation');
  BytesTRansferred:=0;
  UsbK_ControlTransfer(DeviceHandle,($C0 SHL 0) + (51 SHL 8),@buffer,2,@BytesTRansferred,NIL);
  WriteLn('Version Code: '+inttostr(buffer[0]+256*buffer[1])+' Bytes Transferred: '+inttostr(BytesTRansferred));
  Sleep(1000);
  WriteString(0,'Unknown');
  WriteString(1,'Model');
  WriteString(2,'Unknown');
  WriteString(3,'1.0');
  WriteString(4,'http://www.google.com');
  WriteString(5,'122156789');
  Writeln('Switch Command...');
  UsbK_ControlTransfer(DeviceHandle,($40 SHL 0) + (53 SHL 8),NIL,0,NIL,NIL);
  Writeln('Done');
////  Exit Prev Instance
  Sleep(300);
  Close;
////   Reclaim new Instance
  for i:=0 to 9 do
  begin
    if inherited Open($18D1, AOAPid) then exit;
    Sleep(500);
  end;
end;

procedure TLibUSBAoa.WriteBytes(count: integer; buffer: array of byte);
begin
  inherited;
end;

procedure TLibUSBAoa.WriteString(s: string);
begin
  inherited;
end;

procedure TLibUSBAoa.OnReceiveEvent(nx:TLibUSBDeviceData);  // synchronized, so safe to call UI thread
begin
  if assigned(FOnReceiveTextEvent) then
    FOnReceiveTextEvent(ByteArrayToString(nx.Data,nx.Length));
end;

end.
