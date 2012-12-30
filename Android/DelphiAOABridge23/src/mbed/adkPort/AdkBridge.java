package mbed.adkPort;
/*
 * AdkPort library
 * 
 * Written by Giles Barton-Owen
 * 
 * This library helps abstract the ADK interface into a simple sudo-serial port, the libary makes it simpler to use the ADK.
 * It is tested with mbed, not any other system.
 * To use the library one must import it into your Activity/class, make an instance of it, initialise it passing the context of the activity to it
 * If one wants a function called on a message received use a MessageNotifier listener implementing the onNew function.
 * Reading is done with read() read(byte[] toRead) or bytes[]<-readB()
 * Writing is done with sendString(String string) or sendBytes(byte[] bytes)
 * 
 * Adapted by Ruud Ermers on 1-4 November 2012
 * Tested with PC Host Application
 */

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

import org.ermers.delphibridge.MainApplication;

import com.android.future.usb.UsbAccessory;
import com.android.future.usb.UsbManager;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Handler;
import android.os.Message;
import android.os.ParcelFileDescriptor;


public class AdkBridge {

	// The permission action
	private static final String ACTION_USB_PERMISSION = "mbed.mbedwrapper.action.USB_PERMISSION";

	// mHandler what values
	private static final int IOERROR = -1;
	// RE private static final int BYTES = 1;

	// Buffer length, USB standard
	// RE private static final int Buflen = 16384;

	// An instance of accessory and manager
	private UsbAccessory mAccessory;
	private UsbManager mManager;
	
	// The file bits
	private ParcelFileDescriptor mFileDescriptor;
	private FileInputStream mInputStream;
	private FileOutputStream mOutputStream;
	
	// An instance of the ring buffer class
//RE	private RingBuffer buffer = new RingBuffer(Buflen);
	
	
	// Where the notifier is put
//RE	private MessageNotifier mMessage;
	
	// Has the accessory been opened? find out with this variable
	private boolean isOpen = false;
	private Handler mHandler;

	// A receiver for events on the UsbManager, when permission is given or when the cable is pulled
	BroadcastReceiver mUsbReceiver = new BroadcastReceiver() {
		public void onReceive(Context context, Intent intent) {
			String action = intent.getAction();

			if (ACTION_USB_PERMISSION.equals(action)) {
				synchronized (this) {
				    UsbAccessory accessory = UsbManager.getAccessory(intent);
					// RE API 12 UsbAccessory accessory = (UsbAccessory) intent.getParcelableExtra(UsbManager.EXTRA_ACCESSORY);
					if (intent.getBooleanExtra(
							UsbManager.EXTRA_PERMISSION_GRANTED, false)) {
						mHandler.obtainMessage(MainApplication.DEBUGADB, " Broadcast Receiver-> Open Accessory!").sendToTarget();				

						openAccessory(accessory);
					} else {

					}					
				}
			} else if (UsbManager.ACTION_USB_ACCESSORY_DETACHED.equals(action)) {
				UsbAccessory accessory = UsbManager.getAccessory(intent);
				// RE API 12 UsbAccessory accessory = (UsbAccessory) intent.getParcelableExtra(UsbManager.EXTRA_ACCESSORY);
				if (accessory != null && accessory.equals(mAccessory)) {
					closeAccessory();
				}
			}

		}
	};
	
	//Initialiser
	
	public AdkBridge(Activity parentActivity,Handler handler)
	{
		mHandler = handler;
		setup(parentActivity.getApplicationContext());
	}
	
	// Sets up all the requests for permission and attaches the USB accessory if permission is already granted
	public void setup(Context context)
	{

		mManager = UsbManager.getInstance(context);
		// RE API 12 mManager = (UsbManager) context.getSystemService(Context.USB_SERVICE);
		
		UsbAccessory[] accessoryList = mManager.getAccessoryList();
		PendingIntent mPermissionIntent = PendingIntent.getBroadcast(context, 0,
				new Intent(ACTION_USB_PERMISSION), 0);
		IntentFilter filter = new IntentFilter(ACTION_USB_PERMISSION);
		context.registerReceiver(mUsbReceiver, filter);
		mManager.requestPermission(accessoryList[0], mPermissionIntent);
		
		if (accessoryList[0] != null) {

			mAccessory = accessoryList[0];		
			if(mManager.hasPermission(mAccessory))
			{
				mHandler.obtainMessage(MainApplication.DEBUGADB, " Has Permission -> Open Accessory!").sendToTarget();				
			   // I get two events to open the accessory, this one has valid fds, the broadcast hasn't
				// to test if the broadcast is also valid I disable this one 
				//openAccessory(mAccessory);
			}
		}
		
	}
	// Gets a list of the accessories attached, at this point it only supports one
	private HidRunnable mLoop;
	private Thread mUsbThread;
	
	private void runAccessory()
	{
		mLoop = new HidRunnable();
		mUsbThread = new Thread(mLoop);
		mUsbThread.start();
	}
	

/*	
	public void onDestroy(Context context) {
		context.unregisterReceiver(mUsbReceiver);
		closeAccessory();
	}
*/	
	// Attaches the file streams to their pointers
	private void openAccessory(UsbAccessory accessory) {
		mAccessory = accessory;
		mFileDescriptor = mManager.openAccessory(accessory);
		if (mFileDescriptor != null) {
//			mHandler.obtainMessage(MainApplication.DEBUGADB, " Open Accessory! FileDescriptor OK").sendToTarget();				
			FileDescriptor fd = mFileDescriptor.getFileDescriptor();
			mInputStream = new FileInputStream(fd);
			mOutputStream = new FileOutputStream(fd);
//			mHandler.obtainMessage(MainApplication.DEBUGADB, " FDs: " + fd  + mInputStream + mOutputStream).sendToTarget();				
			
		}
		if (isOpen) return;
		isOpen = true;
		runAccessory();
	}
	
	private void closeAccessory() {
		try {
			if (mFileDescriptor != null) {
				mFileDescriptor.close();
			}
		} catch (IOException e) {
		} finally {
			mFileDescriptor = null;
			mAccessory = null;
		}
		isOpen = false;

	}
	// Packetizes the byte array into 32 byte blocks, to keep the port from restricting flow, not sure why this happens
	public boolean send(byte[] toSend) {
		int i = 0;
		mHandler.obtainMessage(MainApplication.DEBUGADB, " Send AOA!").sendToTarget();
		ByteBuffer temp = ByteBuffer.wrap(toSend);
		while (i < toSend.length) {
				
				byte[] buffer = new byte[32];
				int length = toSend.length-i;
				if (length>32) length = 32;
				temp.get(buffer, i, length);
				
				byte[] newbuf = new byte[32];
				for (int m = 0; m < 32; m++) {
					if (m < toSend.length - i) {
						newbuf[m] = buffer[m];
					} else {
						newbuf[m] = 0;
					}
				}
				if (mOutputStream != null) {
					try {
						//mHandler.obtainMessage(MainApplication.DEBUGADB, " In OutputStream!").sendToTarget();
						if(isOpen)
						{
							//mHandler.obtainMessage(MainApplication.DEBUGADB, " In Is Open!").sendToTarget();
						  mOutputStream.write(newbuf);
						  mOutputStream.flush();
						}
					} catch (IOException e) {
						//mHandler.obtainMessage(MainApplication.DEBUGADB, " CATCH IO EXCEPTION!"+e).sendToTarget();
						//writeToConsole("Failed to send\n\r");
						return false;
					}
				}
				i = i + 32;
		}
		return true;
	}
	private class HidRunnable implements Runnable {
	  public void run() {
		byte[] lbuffer = new byte[512];
		mHandler.obtainMessage(MainApplication.DEBUGADB, " Start Run Thread!").sendToTarget();
		while (true) {
			try {
				int p,ret = mInputStream.read(lbuffer);
				String s ="";
				for (p=0;p<ret && (lbuffer[p]!=0);p++) s=s+(char)lbuffer[p];
				mHandler.obtainMessage(MainApplication.RECEIVEADB, s).sendToTarget();
			
			} catch (IOException e) {
				Message m = Message.obtain(mHandler, IOERROR);
				mHandler.sendMessage(m);
				try {
					mInputStream.close();
					openAccessory(mAccessory);
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}

		}
	  }
	}
}
