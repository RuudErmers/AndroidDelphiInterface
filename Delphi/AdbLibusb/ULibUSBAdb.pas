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
    TLibUsbAdb is a class to communicate with any Android device supporting Adb.
    You first have to setup your device with LibusbK driver (see Notes)
    To create an object supply two eventhandlers to receive Debug information (_OnDebugInfoEvent_) and information send by the device (_OnReceiveEvent_)
    Use _Init_ to initialize the connection.
    Use _Open_ to start communication with the device
    Use _WriteString_ to write a string to the device

    Notes:
    1. To setup an Android <-> Delphi bridge you should first install a LibusbK
       driver for Android. See: http://sourceforge.net/projects/libusbk
       You could use the supplied DelphiAdbBridge to test your communication and build from there
    2. This library does not take in account removing or adding USB devices
    3. Tested with Delphi 2010

*)

unit ULibUSBAdb;

interface

uses LibUSBK, ULibUsbDevice,StdCtrls,Classes,Messages;

type  TadbByteArray = array[0..63] of byte;
      TLibUsbAdb = class(TLibUSBDevice)
private
        command,arg0,arg1,dataLength,datacheck,magic:cardinal;
        WriteChannel:integer;
        InWrite,IsOpen:boolean;
        FOnReceiveTextEvent:TOnInfoEvent;
        procedure ToRawArray(VAR data:TadbByteArray);
        procedure FromRawArray(data:array of byte);
        function ByteSize:integer;
        procedure SendMessage(st,_arg0,_arg1:cardinal;buffer:TadbByteArray; length:integer);overload;
        procedure SendMessage(st,arg0,arg1:cardinal;s:string);overload;
    procedure OnReceiveEvent(nx: TLibUSBDeviceData);
protected
        procedure OnReceiveMessage(nx:TLibUSBDeviceData); override;
public
        constructor Create(OnDebugInfoEvent,OnReceiveTextEvent:TOnInfoEvent); virtual;
        function Init: boolean;
        function  Open: boolean;
        function  WriteString(s: string): boolean;
     end;

implementation

uses Windows,SysUtils;

const hostname = 'host::fezbridge';
      portName = 'tcp:4567';

        const  A_SYNC = $434e5953; // cnyc
        const  A_CNXN = $4e584e43;
        const  A_OPEN = $4e45504f;
        const  A_OKAY = $59414b4f;
        const  A_CLSE = $45534c43;
        const  A_WRTE = $45545257;

function TLibUsbAdb.Init:boolean;
begin
  result:=inherited Open(0,0,$FF,$42,1);
  SendMessage(A_CNXN, $01000000, 4096, hostname);
  IsOpen:=false;
end;

function TLibUsbAdb.Open: boolean;
begin
  SendMessage(A_OPEN,1,0,portname);
  Sleep(300);
  result:=WriteChannel>0;
  IsOpen:=result;
end;

function TLibUsbAdb.WriteString(s: string): boolean;
VAR count:integer;
begin
  result:=false;
  if not IsOpen then Open;
  if IsOpen then
  begin
    count:=0;
    while (InWrite) and (count<10) do begin inc(count);Sleep(20); end;
    InWrite := true;
    SendMessage(A_WRTE, 1, WriteChannel, s);
    result:=true;
  end;
end;

constructor TLibUsbAdb.Create(OnDebugInfoEvent,OnReceiveTextEvent: TOnInfoEvent);
begin
  FOnReceiveTextEvent:=OnReceiveTextEvent;
  inherited Create(OnDebugInfoEvent,OnReceiveEvent);
end;

procedure TLibUsbAdb.OnReceiveMessage(nx:TLibUSBDeviceData);
// synchronous, DON'T CALL UI Thread!
VAR count:integer;
    msg:string;
    IsMessage:boolean;
begin
  count:=nx.Length;
  IsMessage:=false;
  msg:='';
  if Count = ByteSize then
  begin
    FromRawArray(nx.Data);
    case command of
     A_CNXN:
              begin
                msg:='In << CNXN ' + Inttostr(arg0) + ',' + Inttostr(arg1);
                IsMessage := true;
              end;
     A_OPEN:
              begin
                msg:='In << OPEN ' + Inttostr(arg0) + ',' + Inttostr(arg1);
                IsMessage := true;
              end;
     A_OKAY:
              begin
                msg:='In << OKAY ' + Inttostr(arg0) + ',' + Inttostr(arg1);
                IsMessage := true;
                if (WriteChannel = 0) then WriteChannel := arg0;
                InWrite := false;
              end;
     A_CLSE:
              begin
                msg:='In << CLSE ' + Inttostr(arg0) + ',' + Inttostr(arg1);
                IsMessage := true;
                WriteChannel := 0;
                IsOpen:=false;
                SendMessage(A_OKAY, arg1, arg0,'');
                // OnException();
              end;
     A_WRTE:
              begin
                msg:='In << WRTE ' + Inttostr(arg0) + ',' + Inttostr(arg1);
                IsMessage := true;
                SendMessage(A_OKAY, arg1, arg0,'');
              end;
     else
              begin
                msg:='In << UNKN (' + Inttostr(count) + ') ';
                IsMessage := false;
              end;
    end;
  end;
  if (count>0) and not IsMessage then
    PostReceiveMessage(nx)
  else if msg<>'' then
    PostDebugMessage(msg);
end;

procedure TLibUsbAdb.OnReceiveEvent(nx:TLibUSBDeviceData);  // synchronized, so safe to call UI thread
begin
  if assigned(FOnReceiveTextEvent) then
    FOnReceiveTextEvent(ByteArrayToString(nx.Data,nx.Length));
end;

function TLibUsbAdb.ByteSize: integer;
begin
  result:=24;
end;

procedure TLibUsbAdb.SendMessage(st, arg0, arg1: cardinal; s: string);
VAR buffer: TadbByteArray;
    i:integer;
begin
  for i:=0 to length(s)-1 do
    buffer[i]:=ord(s[i+1]);
  buffer[length(s)] := 0;
  SendMessage(st, arg0, arg1, buffer, length(s)+1);
end;

procedure TLibUsbAdb.SendMessage(st, _arg0, _arg1: cardinal; buffer: TadbByteArray; length: integer);
VAR cnt,i:integer;
    crc:cardinal;
    buffer2:TadbByteArray;
    procedure TransferData(data: TadbByteArray; length: integer);
    const BUFFERLENGTH = 512;
    VAR Buffer:array[0..BUFFERLENGTH-1] of byte;
      BytesTransferred:Cardinal;
      i,Result:integer;
    begin
      for i:=0 to length-1 do
        Buffer[i]:=data[i];
      result:=ord(UsbK_WritePipe(DeviceHandle,OutPipe,buffer, length,@BytesTransferred,NIL));
    end;
begin
  crc:=0;
  cnt:=length;
  for i:=0 to cnt-1 do
    crc:=crc+buffer[i];
  command:=st;
  arg0:=_arg0;
  arg1:=_arg1;
  dataLength:=cnt;
  datacheck:=crc;
  magic:=st XOR $FFFFFFFF;
  ToRawArray(buffer2);
  TransferData(buffer2,ByteSize);
  TransferData(buffer,length);
  FromRawArray(buffer2);
  ToRawArray(buffer2);
end;

procedure TLibUsbAdb.ToRawArray(var data: TadbByteArray);
  procedure putint(offset:integer;value:cardinal);
  begin
    data[offset]:=value AND $FF; value := value SHR 8;
    data[offset+1]:=value AND $FF; value := value SHR 8;
    data[offset+2]:=value AND $FF; value := value SHR 8;
    data[offset+3]:=value AND $FF; value := value SHR 8;
  end;

begin
  putInt(0,command);
  putInt(4,arg0);
  putInt(8,arg1);
  putInt(12,dataLength);
  putInt(16,datacheck);
  putInt(20,magic);
end;

procedure TLibUsbAdb.FromRawArray(data: array of byte);
  function getInt(offset:integer):cardinal;
  begin
    result:=(data[offset] + (data[offset+1] SHL 8) + (data[offset+2] SHL 16) + (data[offset+3] SHL 24));
  end;
begin
  command := getInt(0);
  arg0 := getInt(4);
  arg1 := getInt(8);
  dataLength := getInt(12);
  dataCheck := getint(16);
  magic := getInt(20);
end;

end.
