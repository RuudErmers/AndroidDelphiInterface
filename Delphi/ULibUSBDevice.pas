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
  TLibUSBDevice is a class to communicate with a USB Device.
  The USB-Device can only have one single outpipe and one single inPipe.
  To create an object supply two eventhandlers to receive Debug information (_OnDebugInfoEvent_) and information send by the device (_OnReceiveEvent_)
  To Open a Device use the _Open_ method (supplying <Vid,pid> or <iClass,iSubclass>
  Use _OnDeviceInit_ to send extra information to the device on initialization
  Note that Open is a protected method so you allways have to subclass TLibUSBDevice for your device
  To write data to the device use _SendBytes_ or _SendString_

  Notes:
  1. A note on receiving data:
     The default processing when receiving data is to call OnReceiveEvent asynchronous (in the UI thread)
     However, sometimes it is needed to reply immediately. You should override OnReveiveMessage in this case
  2. This library does not take in account removing or adding USB devices
  3. Tested with Delphi 2010

*)

unit ULibUSBDevice;

interface

uses LibUSBK, StdCtrls,Classes,Messages;

type TLibUSBDebugStr = class s:string; end;

type TByteBuffer512 = array[0..511] of byte;
     TLibUSBDeviceData = class
      Length:integer;
      data: TByteBuffer512;
      constructor Create(buf:TByteBuffer512;l:integer);overload;
      constructor Create; overload;
      procedure Copy(x:TLibUSBDeviceData);
      function Equal(x:TLibUSBDeviceData):boolean;
    end;

     TLibUSBDevice = class;
     TOnInfoEvent = procedure (s:string) of object;
     TonReceiveEvent = procedure (nx: TLibUSBDeviceData) of object;
     TDeviceThread = class(TThread)
       USBDEvice: TLibUSBDevice;
       itemsreceived:integer;
       constructor Create(CreateSuspended:boolean;parent:TLibUSBDevice);
       procedure Execute;override;
     end;

     TLibUSBDevice = class
private
  FOnDebugInfoEvent:TOnInfoEvent;
  FOnReceiveEvent:TonReceiveEvent;
  DeviceThread:TDeviceThread;
  FWinHandle:integer;
  procedure WndProc(var msg: TMessage);
  procedure InitDevice(lDeviceInfo: KLST_DEVINFO_HANDLE;iClass,iSubClass,TimeOut:integer);
  function CheckInterfaces(lDeviceInfo: KLST_DEVINFO_HANDLE;iClass,iSubClass:integer):boolean;
  function OpenDevice(Vid, pid, iClass, iSubClass,Timeout: integer): boolean;
protected
  PacketSize,InPipe,OutPipe:integer;
  DeviceHandle: KUSB_HANDLE;
  procedure WriteLn(s: string); overload;
  procedure PostDebugMessage(s: string);
  procedure PostReceiveMessage(nx:TLibUSBDeviceData);
  procedure OnDeviceInit(DeviceHandle: KUSB_HANDLE);virtual;
  procedure OnReceiveMessage(nx:TLibUSBDeviceData); virtual;
  function  Open(Vid,Pid:integer;iClass: integer =0;iSubClass:integer =0;Timeout: integer=100):boolean;
  procedure WriteBytes(count: integer; buffer: array of byte);
  procedure WriteString(s:string);
  procedure Close;
public
  constructor Create(OnDebugInfoEvent:TOnInfoEvent;OnReceiveEvent:TOnReceiveEvent);virtual;
  destructor Destroy;override;
end;

function ByteArrayToString(buffer:array of byte;count:integer):string;

implementation

uses SysUtils,Windows,Forms;

CONST WM_DeviceEVENT         = WM_USER+5;
CONST WM_DeviceDEBUG         = WM_USER+6;

function ByteArrayToString(buffer:array of byte;count:integer):string;
VAR i:integer;
begin
  result:='';
  for i:=0 to count-1 do result:=result+chr(buffer[i]);
end;

procedure TLibUSBDevice.WndProc(var msg: TMessage);
VAR ps:TLibUSBDebugStr;
    pe:TLibUSBDeviceData;
begin
  if (Msg.Msg = WM_DeviceEVENT)  then
  begin
    pe:=TLibUSBDeviceData(msg.lParam);
    if assigned (FOnReceiveEvent) then  FOnReceiveEvent(pe);
    pe.Free;
  end
  else
  if (Msg.Msg = WM_DeviceDebug) then
  begin
    ps:=TLibUSBDebugStr(msg.lParam);
    FOnDebugInfoEvent(ps.s);
    ps.Free;
  end;
  Msg.Result := DefWindowProc(FWinHandle, Msg.Msg,
                                Msg.wParam, Msg.lParam);
end;

procedure TLibUSBDevice.WriteLn(s:string);
begin
  FOnDebugInfoEvent(s);
end;

procedure TLibUSBDevice.WriteString(s: string);
const BUFFERLENGTH = 512;
VAR Buffer:array[0..BUFFERLENGTH-1] of byte;
    BytesTransferred:Cardinal;
    i,Result:integer;
begin
  for i:=0 to length(s) do
    Buffer[i]:=ord(s[i+1]);
  Buffer[length(s)]:=0;
  result:=ord(UsbK_WritePipe(DeviceHandle,OutPipe,buffer, PacketSize,@BytesTransferred,NIL));
  WriteLn('WriteString: Result='+inttostr(result)+' Transferred:'+inttostr(BytesTransferred));
end;

procedure TLibUSBDevice.WriteBytes(count:integer;buffer:array of byte);
VAR
    BytesTransferred:Cardinal;
    Result:integer;
begin
  result:=ord(UsbK_WritePipe(DeviceHandle,OutPipe,buffer, count,@BytesTransferred,NIL));
  WriteLn('WriteBytes: Result='+inttostr(result)+' Transferred:'+inttostr(BytesTransferred));
end;

function TLibUSBDevice.Open(Vid,Pid,iClass,iSubClass,Timeout:integer):boolean;
begin
  result:=OpenDevice(vid,pid,iClass,iSubClass,Timeout);
end;

function TLibUSBDevice.OpenDevice(Vid,pid,iClass,iSubClass,Timeout:integer):boolean;
VAR
  lDeviceList : KLST_HANDLE;
  i:integer;
  found:boolean;
  lDeviceInfo:KLST_DEVINFO_HANDLE;
begin
  Assert((SizeOf(WINUSB_SETUP_PACKET) = 8),'WINUSB_SETUP_PACKET size violation');
  LstK_Init(lDeviceList, 0);
  i:=-1;
  found:=false;
  while not found and LstK_MoveNext(lDeviceList, lDeviceInfo) do  // short circuit evaluation is essential here!
  begin
    Inc(i);
    if Assigned(lDeviceInfo) then
    begin
      WriteLn('Device '+inttostr(i)+' ---------------------------------');
      WriteLn('VID: '+inttohex(lDeviceInfo.Common.Vid,4));
      WriteLn('PID: '+inttohex(lDeviceInfo.Common.Pid,4));
      WriteLn('PID: '+lDeviceInfo.InstanceID);
      if (lDeviceInfo.Common.Vid = $18D1) and (lDeviceInfo.Common.Pid = $2D00)
      then Writeln('<<<<<<<<<< Accesory >>>>>>>>>>');
      if (lDeviceInfo.Common.Vid = $18D1) and (lDeviceInfo.Common.Pid = $2D01)
      then Writeln('<<<<<<<<<< Accesory DUAL Bridge >>>>>>>>>>');
      if lDeviceInfo.Connected then
        WriteLn('Connected: True')
      else
        WriteLn('Connected: False');
      WriteLn('SyncFlags: '+Format('%x',[lDeviceInfo.SyncFlags]));
      if (Vid = lDeviceInfo.Common.Vid) and (pid = lDeviceInfo.Common.Pid) then found:=true;
      if (Vid = 0) and (pid = 0) then found:=CheckInterfaces(lDeviceInfo,iClass,iSubClass);
      if found then InitDevice(lDeviceInfo,iClass,iSubClass,Timeout);
    end;
  end;
  LstK_Free(lDeviceList);
  result:=found;
end;

function TLibUSBDevice.CheckInterfaces(  lDeviceInfo : KLST_DEVINFO_HANDLE;iClass,iSubClass:integer):boolean;
VAR DeviceHandle:KUSB_HANDLE;
    cd:USB_CONFIGURATION_DESCRIPTOR;
    BytesTransferred:Cardinal;
    UsbAltInterfaceDescriptor: USB_INTERFACE_DESCRIPTOR;
    i:integer;
begin
  result:=false;
  USBK_Init(DeviceHandle,lDeviceInfo);
  UsbK_GetDescriptor(DeviceHandle, USB_DESCRIPTOR_TYPE_CONFIGURATION,0,0,cd,sizeof(cd),BytesTransferred);
  Writeln(' Get Config Descriptor nrInterfaces: '+inttostr(cd.bNumInterfaces));
  for i:=0 to cd.bNumInterfaces-1 do
    if UsbK_ClaimInterface(DeviceHandle,i,true) then
    begin
      WriteLn('Claimed Interface: '+inttostr(i));
      UsbK_QueryInterfaceSettings(DeviceHandle,0,UsbAltInterfaceDescriptor);
      if (UsbAltInterfaceDescriptor.bInterfaceClass = iClass) and (UsbAltInterfaceDescriptor.bInterfaceSubClass = iSubClass)
      then
        result:=true;
      UsbK_ReleaseInterface(DeviceHandle,i,true);
    end
    else
      WriteLn('Could not claim Interface: '+inttostr(i));
  USBK_Free(DeviceHandle);
end;

procedure TLibUSBDevice.OnReceiveMessage(nx: TLibUSBDeviceData);
begin
  // Default Processing: asynchronous call to UI thread
  PostReceiveMessage(nx);
end;

procedure TLibUSBDevice.PostDebugMessage(s:string);
VAR ps:TLibUSBDebugStr;
begin
  ps:=TLibUSBDebugStr.Create;
  ps.s:=s;
  PostMessage(FWinHandle,WM_DeviceDEBUG,0,integer(ps));
end;

procedure TLibUSBDevice.PostReceiveMessage(nx:TLibUSBDeviceData);
VAR nwx:TLibUSBDeviceData;
begin
  nwx:=TLibUSBDeviceData.Create(nx.data,nx.length);
  PostMessage(FWinHandle,WM_DeviceEVENT,0,integer(nwx));
end;

function UsbPipeTypeString(t:integer):string;
const UsbPipeTypeString : array[0..3] of String =
    (  'Control',  'Isochronous', 'Bulk,','Interrupt' );
begin
  result:=UsbPipeTypeString[t];
end;

procedure TLibUSBDevice.InitDevice(lDeviceInfo : KLST_DEVINFO_HANDLE;iClass,iSubClass,TimeOut:integer);
VAR policyLength:Cardinal;
    k,intf:integer;
    pipeinfo : WINUSB_PIPE_INFORMATION;
    UsbAltInterfaceDescriptor: USB_INTERFACE_DESCRIPTOR;
    ok:boolean;
begin

    policyLength:=4;
    USBK_Init(DeviceHandle,lDeviceInfo);
    OnDeviceInit(DeviceHandle);
    intf:=0;
    ok:=false;
    if (iClass<>0) or (iSubClass<>0) then
    while not ok and UsbK_ClaimInterface(DeviceHandle,intf,true) do
    begin
      WriteLn('Claimed Interface: '+inttostr(intf));
      UsbK_QueryInterfaceSettings(DeviceHandle,0,UsbAltInterfaceDescriptor);
      WriteLn(inttostr(UsbAltInterfaceDescriptor.bInterfaceSubClass));
      if (UsbAltInterfaceDescriptor.bInterfaceClass = iClass) and (UsbAltInterfaceDescriptor.bInterfaceSubClass = iSubClass) then
        ok:=true
       else UsbK_ReleaseInterface(DeviceHandle,intf,true);
      inc(intf);
    end;
    k:=0;
    while UsbK_QueryPipe(DeviceHandle,0,k,pipeinfo) do
      begin
        WriteLn(Format('  PipeId=0x%2.2x PipeType=0x%2.2x ("%s") Interval=%u MaximumPacketSize=%u',
                       [pipeInfo.PipeId, Byte(pipeInfo.PipeType),UsbPipeTypeString(ord(pipeInfo.PipeType)), pipeInfo.Interval, pipeInfo.MaximumPacketSize]));
        if PipeInfo.PipeId > $80 then
        begin
          InPipe:=PipeInfo.PipeId;
          PacketSize:=PipeInfo.MaximumPacketSize;
        end
        else
        begin
          OutPipe:=PipeInfo.PipeId;
        end;

        Inc(k);
      end;
    if UsbK_SetPipePolicy(DeviceHandle, InPipe, PIPE_TRANSFER_TIMEOUT, sizeof(integer), @TimeOut) then
      WriteLn('Policy Set');
    UsbK_GetPipePolicy(DeviceHandle, InPipe, PIPE_TRANSFER_TIMEOUT, @policyLength, @TimeOut);
      WriteLn('Get Policy'+ inttostr(TimeOut));
    FWinHandle := AllocateHWND(WndProc);
    DeviceThread:=TDeviceThread.Create(false,self);
end;

procedure TLibUSBDevice.Close;
begin
  UsbK_Free(DeviceHandle);
  FreeAndNIL(DeviceThread);
end;

constructor TLibUSBDevice.Create(OnDebugInfoEvent:TOnInfoEvent;OnReceiveEvent:TOnReceiveEvent);
begin
  FOnDebugInfoEvent:=OnDebugInfoEvent;
  FOnReceiveEvent:=OnReceiveEvent;
  DeviceThread:=NIL;
end;

destructor TLibUSBDevice.Destroy;
begin
  FreeAndNIL(DeviceThread);
end;

procedure TLibUSBDevice.OnDeviceInit(DeviceHandle: KUSB_HANDLE);
begin
  //
end;

{ TDeviceThread }

constructor TDeviceThread.Create(CreateSuspended: boolean; parent: TLibUSBDevice);
begin
  USBDevice:=parent;
  itemsreceived:=0;
  inherited Create(CreateSuspended);
end;

procedure TDeviceThread.Execute;

VAR buffer: TByteBuffer512;
    Count:Cardinal;
    procedure ShowDebug;
    VAR
        i:integer;
        msg:string;
    begin
      inc(itemsreceived);
      msg:='Read: ('+Inttostr(itemsreceived)+') '+IntTostr(Count)+':';
      for i:=0 to 15 do msg:=msg+' '+inttostr(buffer[i]);
      USBDevice.PostDebugMessage(msg);
    end;
    procedure Examine;
    VAR nx:TLibUSBDeviceData;
    begin
      nx:=TLibUSBDeviceData.Create(buffer,count);
      USBDevice.OnReceiveMessage(nx);
      nx.Free;
    end;
begin
  while not Terminated do
  begin
    USBK_ReadPipe(USBDevice.DeviceHandle, USBDevice.InPipe, buffer, USBDevice.PacketSize, @Count,NIL);  // 81
    if Count<>0 then
    begin
      ShowDebug;
      Examine;
    end;
  end;
end;

{ TLibUSBDeviceData }

constructor TLibUSBDeviceData.Create(buf: TByteBuffer512; l: integer);
VAR i:integer;
begin
  Length:=l;
  for i:=0 to Length-1 do Data[i]:=buf[i];
end;

constructor TLibUSBDeviceData.Create;
begin
  length:=0;
end;

function TLibUSBDeviceData.Equal(x: TLibUSBDeviceData): boolean;
VAR i:integer;
begin
  result:=Length=x.Length;
  for i:=0 to Length-1 do if Data[i]<>x.Data[i] then result:=false;
end;

procedure TLibUSBDeviceData.Copy(x: TLibUSBDeviceData);
VAR i:integer;
begin
  Length:=x.Length;
  for i:=0 to Length-1 do
    Data[i]:=x.Data[i];
end;

end.
