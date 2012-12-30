This is a set of code to aid development for communication between an Android device and 
an Embedded Device, or PC. There are examples for the following:

Communication between PC and Android device, over USB, using the ADB (Android Debug) protocol.
Communication between PC and Android device, over USB, using the AOA (Android Open Accessory) protocol.
Communication between PC and a simple HID device (Arduino and LPC 1768 Microcontroller).

All PC code is written in Delphi.
Android code is in Java and the embedded code in C/C++.

So why would you want this?
We are developing a hardware device which needs to interact with an Android phone.
It needs to be robust and simple, so we opted for a USB interface. 
(Wireless connections were ruled out)
In this case the Android device is the USB client, and the hardware device is USB Host.
In development it became clear that it would be handy to change the hardware device for a PC,
to test the Android side of things without to much hassle on the hardware device.
Therefor we needed an Android <-> PC bridge.
I found a few (in C), but I opted to use Delphi for quick testing. 
Based on LibUSBK there is a class which implements a simple twoway USB connection.
From there, there are classes which implement the ADB protocol and the AOA protocol.

As you may know the ADB interface runs on devices supporting Android 1.5. AOA is only supported
by some devices, and need Android 2.3.3.
Personally, I don't see any advantage of using AOA as it is not widely supported. In fact, 
I was only able to get this working with one Android Device i have (a chines Android TV Stick)

All of this code becomes 'obsolete' when using an Android device which adds HID facility. 
This is supported from Android 4.0. Since all the basics wre there I decide to add an example where 
Delphi takes over the Android side to test the hardware. 
I have added two examples on Embedded platforms, for Arduino and LPC 1768 (from mBed).
I have also some examples for FEZ Domino or Microchip Cereboth (both implementing ADB host).
On the HID facility: This is also only implemented in some devices.

