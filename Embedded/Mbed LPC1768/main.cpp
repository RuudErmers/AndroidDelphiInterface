// Authour: Ruud Ermers
// Date: 26 & 27 october 2012
// File: main.cpp
// Purpose: Example how to communicate with USB Host, e.g. PC or Android Tablet

#include "mbed.h"
#include "USBHID.h"

DigitalOut myled(LED1);
DigitalOut iled(LED2);
// Serial pc(USBTX, USBRX); // tx, rx 
USBHID hid(60, 60,0x0601,0x0002);
HID_REPORT send_report,recv_report;

Timer myclock;

char lastReceive[32],lastTime[32];

void Write(char *s)
{
  int l = strlen(s);
  if (l>58) l=58;
  send_report.length = 60;
  for (int i=0;i<=l;i++)
    send_report.data[i]=s[i];    
  hid.send(&send_report);    
  iled = 1;
}  

void doTick()
{
  static int led=0;
  static int seconds=0;
  if (myclock.read_ms()>1000)
  {
    led = !led;
    myled = led;
    iled = 0;
    myclock.reset();
    seconds++;
    switch (seconds)
    {
        case 1:  Write("Hello Wereld");
                 break;
        case 3:  strcpy(lastTime,lastReceive);
                 break;
        case 5: seconds=0;         
                 break;
     } 
  }   
}

void doRead()
{
  static int ms;
  if (ms!=myclock.read_ms())
  {
    ms=myclock.read_ms();
    if (hid.readNB(&recv_report))
      for (int i=0;i<32;i++)
        lastReceive[i]=recv_report.data[i];
  }     
}


int main() 
{
  myclock.start();   
  while(1) 
  { 
    doTick();  
    doRead();   
    
  }
}

