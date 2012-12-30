/*
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
 */
package org.ermers.delphibridge;

import java.util.Calendar;

import org.ermers.delphibridge.R;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;

public class MainApplication extends Activity implements Handler.Callback 
{

	TextView mTextViewDebug, mTextViewReceive;
	private Handler mHandler;
	public static final int RECEIVEADB = 4;
	public  static final int DEBUGADB = 5;
	private static final int MENU_GETTIME = 10;	
	
	private DelphiBridge delphiBridge;
	
	
	/**
	 * Called when the activity is first created.
	 */
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		mTextViewDebug = (TextView) findViewById(R.id.adb_debug);
		mTextViewReceive = (TextView) findViewById(R.id.adb_receive);
		android.view.Display display = getWindowManager().getDefaultDisplay(); 
		int DisplayHeight = display.getHeight();
		mTextViewDebug.setHeight(DisplayHeight/2);
		mTextViewReceive.setHeight(DisplayHeight/2);
		mHandler = new Handler(this);
		delphiBridge = new DelphiBridge(this, mHandler);	
	}
	public boolean handleMessage(Message msg) {
		switch (msg.what) {	 
		case RECEIVEADB:
			AddReceive((String)msg.obj);
			return true;
		case DEBUGADB:
			AddDebug((String)msg.obj);
			return true;
		default:
			return false;
		}
	}
		
    private int debugStrings = 0;	
    public void AddDebug(String s)  
    {
    	debugStrings++;
    	if (debugStrings>10)
    	{
    		debugStrings = 0;
    		mTextViewDebug.setText(s+"\n");
    	}
    	else
    	    mTextViewDebug.append(s+"\n");
    }
    private int receiveStrings = 0;    
    private void AddReceive(String s)  
    {
    	receiveStrings++;
    	if (receiveStrings>10)
    	{
    		receiveStrings = 0;
    		mTextViewReceive.setText(s+"\n");
    	}
    	else
    		mTextViewReceive.append(s+"\n");
    }
    public String TwoStr(int n)
    {
    	return n<10 ? "0" + n : "" + n;
    }
    public String getDateTime() // yymmddhhmmss
    {
        Calendar c = Calendar.getInstance();        
        String s =TwoStr(c.get(Calendar.YEAR)-2000)+TwoStr(c.get(Calendar.MONTH)+1)+TwoStr(c.get(Calendar.DAY_OF_MONTH))+TwoStr(c.get(Calendar.HOUR_OF_DAY))+TwoStr(c.get(Calendar.MINUTE))+TwoStr(c.get(Calendar.SECOND));
        AddDebug(s);
        return s;
    }
     
    public void InetGetTime()
    {
    	String s = getDateTime();
    	byte [] data = new byte[12];
    	char [] sdata = s.toCharArray();
    	for (int i=0;i<12;i++) data[i]=(byte)sdata[i];
    	delphiBridge.send(data);
    }    
	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		menu.add(Menu.NONE, MENU_GETTIME, Menu.NONE,"DateTime To Device");			
		return true;
	}
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MENU_GETTIME:
			InetGetTime();
			return true;			
		default:
			return super.onOptionsItemSelected(item);
		}
	}
}