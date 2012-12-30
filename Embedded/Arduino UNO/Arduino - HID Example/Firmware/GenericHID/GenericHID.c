/*
             LUFA Library
     Copyright (C) Dean Camera, 2012.

  dean [at] fourwalledcubicle [dot] com
           www.lufa-lib.org
*/

/*
  Copyright 2012  Dean Camera (dean [at] fourwalledcubicle [dot] com)

  Permission to use, copy, modify, distribute, and sell this
  software and its documentation for any purpose is hereby granted
  without fee, provided that the above copyright notice appear in
  all copies and that both that the copyright notice and this
  permission notice and warranty disclaimer appear in supporting
  documentation, and that the name of the author not be used in
  advertising or publicity pertaining to distribution of the
  software without specific, written prior permission.

  The author disclaim all warranties with regard to this
  software, including all implied warranties of merchantability
  and fitness.  In no event shall the author be liable for any
  special, indirect or consequential damages or any damages
  whatsoever resulting from loss of use, data or profits, whether
  in an action of contract, negligence or other tortious action,
  arising out of or in connection with the use or performance of
  this software.
*/

/** \file
 *
 *  Main source file for the GenericHID demo. This file contains the main tasks of the demo and
 *  is responsible for the initial application hardware configuration.
 */

#include "GenericHID.h"
#include "Lib/LightweightRingBuff.h"
#include <LUFA/Drivers/Peripheral/Serial.h>

void Other_Tasks(void);

/** Main program entry point. This routine configures the hardware required by the application, then
 *  enters a loop to run the application tasks in sequence.
 */

RingBuff_t USARTtoUSB_Buffer;
RingBuff_t USBToUSART_Buffer; 

#define LED_ON_TICKS 2000	/* Number of ticks to leave LEDs on */
volatile int led1_ticks = 0, interrupts,seconds, toggleLed=1;
 
int main(void)
{
	SetupHardware();

	LEDs_SetAllLEDs(LEDMASK_USB_NOTREADY);
	
	RingBuffer_InitBuffer(&USARTtoUSB_Buffer);
	RingBuffer_InitBuffer(&USBToUSART_Buffer);
	
	sei();

	for (;;)
	{
		HID_Task();
		USB_USBTask();
		Other_Tasks();
	}
}

/** Configures the board hardware and chip peripherals for the demo's functionality. */
void SetupHardware(void)
{
	/* Disable watchdog if enabled by bootloader/fuses */
	MCUSR &= ~(1 << WDRF);
	wdt_disable();
	Serial_Init(9600, true);

	/* Disable clock division */
//	clock_prescale_set(clock_div_1);

	/* Hardware Initialization */
	LEDs_Init();
	USB_Init();
	UCSR1B = ((1 << RXCIE1) | (1 << TXEN1) | (1 << RXEN1));   // Interrupt Enable ? 
}

/** Event handler for the USB_Connect event. This indicates that the device is enumerating via the status LEDs and
 *  starts the library USB task to begin the enumeration and USB management process.
 */
void EVENT_USB_Device_Connect(void)
{
	/* Indicate USB enumerating */
	LEDs_SetAllLEDs(LEDMASK_USB_ENUMERATING);
}

/** Event handler for the USB_Disconnect event. This indicates that the device is no longer connected to a host via
 *  the status LEDs and stops the USB management task.
 */
void EVENT_USB_Device_Disconnect(void)
{
	/* Indicate USB not ready */
	LEDs_SetAllLEDs(LEDMASK_USB_NOTREADY);
}

/** Event handler for the USB_ConfigurationChanged event. This is fired when the host sets the current configuration
 *  of the USB device after enumeration, and configures the generic HID device endpoints.
 */
void EVENT_USB_Device_ConfigurationChanged(void)
{
	bool ConfigSuccess = true;

	/* Setup HID Report Endpoints */
	ConfigSuccess &= Endpoint_ConfigureEndpoint(GENERIC_IN_EPADDR, EP_TYPE_INTERRUPT, GENERIC_REPORT_SIZE, 1);
	ConfigSuccess &= Endpoint_ConfigureEndpoint(GENERIC_OUT_EPADDR, EP_TYPE_INTERRUPT, GENERIC_REPORT_SIZE, 1);

	/* Indicate endpoint configuration success or failure */
	LEDs_SetAllLEDs(ConfigSuccess ? LEDMASK_USB_READY : LEDMASK_USB_ERROR);
}

/** Event handler for the USB_ControlRequest event. This is used to catch and process control requests sent to
 *  the device from the USB host before passing along unhandled control requests to the library for processing
 *  internally.
 */
uint8_t GenericData[GENERIC_REPORT_SIZE]; 
void EVENT_USB_Device_ControlRequest(void)
{
	/* Handle HID Class specific requests */
	switch (USB_ControlRequest.bRequest)
	{
		case HID_REQ_GetReport:
			if (USB_ControlRequest.bmRequestType == (REQDIR_DEVICETOHOST | REQTYPE_CLASS | REQREC_INTERFACE))
			{
				
				CreateGenericHIDReport(GenericData);

				Endpoint_ClearSETUP();

				/* Write the report data to the control endpoint */
				Endpoint_Write_Control_Stream_LE(&GenericData, sizeof(GenericData));
				Endpoint_ClearOUT();
			}

			break;
		case HID_REQ_SetReport:
			if (USB_ControlRequest.bmRequestType == (REQDIR_HOSTTODEVICE | REQTYPE_CLASS | REQREC_INTERFACE))
			{

				Endpoint_ClearSETUP();

				/* Read the report data from the control endpoint */
				Endpoint_Read_Control_Stream_LE(&GenericData, sizeof(GenericData));
				Endpoint_ClearIN();

				ProcessGenericHIDReport(GenericData);
			}

			break;
	}
}

/** Function to process the last received report from the host.
 *
 *  \param[in] DataArray  Pointer to a buffer where the last received report has been stored
 */
void ProcessGenericHIDReport(uint8_t* DataArray)
{
	/*
		This is where you need to process reports sent from the host to the device. This
		function is called each time the host has sent a new report. DataArray is an array
		holding the report sent from the host.
	*/

}

/** Function to create the next report to send back to the host at the next reporting interval.
 *
 *  \param[out] DataArray  Pointer to a buffer where the next report data should be stored
 */
void CreateGenericHIDReport(uint8_t* DataArray)
{
	/*
		This is where you need to create reports to be sent to the host from the device. This
		function is called each time the host is ready to accept a new report. DataArray is
		an array to hold the report to the host.
	*/

}

void ProcessDataFromHost(uint8_t* Data)
{
    int i;
	for (i=0;i<GENERIC_REPORT_SIZE;i++)
	{
	  uint8_t c = Data[i];	  
	  if(!RingBuffer_IsFull(&USBToUSART_Buffer)) 
		RingBuffer_Insert(&USBToUSART_Buffer, c);
      if (c==0) return; 		
    }       
}

bool CreateDataToHost(uint8_t* Data)
{  
    RingBuff_Count_t BufferCount = RingBuffer_GetCount(&USARTtoUSB_Buffer);
	
	if (BufferCount > 0) {
		uint8_t ind,count = BufferCount;
		if (count>GENERIC_REPORT_SIZE-1) 
		  count = GENERIC_REPORT_SIZE-1;
		for (ind=0; ind<count; ind++) 
	      Data[ind] = RingBuffer_Remove(&USARTtoUSB_Buffer);
		Data[ind]=0;
		return true;
    }
		
	return false;
}


uint8_t GenericDataIn[GENERIC_REPORT_SIZE];
uint8_t GenericDataOut[GENERIC_REPORT_SIZE];

void HID_Task(void)
{
	/* Device must be connected and configured for the task to run */
	if (USB_DeviceState != DEVICE_STATE_Configured)
	  return;

	Endpoint_SelectEndpoint(GENERIC_OUT_EPADDR);

	/* Check to see if a packet has been sent from the host */
	if (Endpoint_IsOUTReceived())
	{
		/* Check to see if the packet contains data */
		if (Endpoint_IsReadWriteAllowed())
		{

			/* Read Generic Report Data */
			Endpoint_Read_Stream_LE(&GenericDataIn, sizeof(GenericDataIn), NULL);

			/* Process Generic Report Data */
			ProcessDataFromHost(GenericDataIn);
		}

		/* Finalize the stream transfer to send the last packet */
		Endpoint_ClearOUT();
	}

	Endpoint_SelectEndpoint(GENERIC_IN_EPADDR);

	/* Check to see if the host is ready to accept another packet */
	if (Endpoint_IsINReady())
	{

		/* Create Generic Report Data */
		if (CreateDataToHost(GenericDataOut))
		{

			/* Write Generic Report Data */
			Endpoint_Write_Stream_LE(GenericDataOut, sizeof(GenericDataOut), NULL);
			LEDs_TurnOnLEDs(LEDS_LED1);
			led1_ticks = LED_ON_TICKS;
			/* Finalize the stream transfer to send the last packet */
			Endpoint_ClearIN();
		}
	}
}

volatile uint8_t    USARTTemp_Buffer[GENERIC_REPORT_SIZE];
volatile uint8_t    USARTPointer = 0;

/** ISR to manage the reception of data from the serial port, placing received bytes into a circular buffer
 *  for later transmission to the host.
 */
ISR(USART1_RX_vect, ISR_BLOCK)
{
    uint8_t ReceivedByte = UDR1;
	uint8_t i;
	if (ReceivedByte == 13) ReceivedByte = 0;
	if (ReceivedByte == 10) return;
	if (USB_DeviceState != DEVICE_STATE_Configured) return;
    USARTTemp_Buffer[USARTPointer]=ReceivedByte;
	USARTPointer++;
	if (ReceivedByte == 0 || USARTPointer == GENERIC_REPORT_SIZE)
	{
	   for (i=0;i<USARTPointer;i++) 
	     if (!RingBuffer_IsFull(&USARTtoUSB_Buffer)) 
	        RingBuffer_Insert(&USARTtoUSB_Buffer, USARTTemp_Buffer[i]);
	   USARTPointer = 0;
    }		
}

void CheckUSBToUSART()
{ 
  RingBuff_Count_t BufferCount = RingBuffer_GetCount(&USBToUSART_Buffer);
  uint8_t c;
  if (!(UCSR1A & (1 << UDRE1))) return; // Send busy
  if (!BufferCount) return; // No data
  c = RingBuffer_Remove(&USBToUSART_Buffer);
  Serial_SendByte(c);
}

void Other_Tasks(void)
{
	if (led1_ticks) {
		led1_ticks--;
		if (led1_ticks == 0) {
			LEDs_TurnOffLEDs(LEDS_LED1);
		}
	}
	CheckUSBToUSART();
}