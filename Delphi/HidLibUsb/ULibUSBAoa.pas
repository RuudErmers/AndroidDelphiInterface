unit ULibUSBAoa;

interface

uses LibUSBK, ULibUSBDevice, StdCtrls,Classes,Messages;

type
     TLibUSBAoa = class (TLibUSBDevice)
private
  AOAPid:integer;
public
  procedure SwitchToAccesory;
  function  Open(Vid,Pid,aAOAPid:integer): boolean;
end;

implementation

uses SysUtils,Windows,Forms;

function TLibUSBAoa.Open(Vid,Pid,aAOAPid:integer): boolean;
begin
  AOAPid:=aAOAPid;
  result:=inherited Open(Vid,Pid);
end;

procedure TLibUSBAoa.SwitchToAccesory;
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
  WriteString(0,'Ruud');
  WriteString(1,'Model');
  WriteString(2,'Ruud');
  WriteString(3,'0.1');
  WriteString(4,'http://www.ermers.com');
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

end.
