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

import java.io.IOException;

import org.microbridge.server.AbstractServerListener;
import org.microbridge.server.Server;

import android.app.Activity;
import android.os.Handler;
import android.util.Log;
public class DelphiBridge {
	Server server = null;
	Handler mHandler;
	DelphiBridge(Activity parentActivity, Handler handler)
	{
		// Create TCP server
		mHandler = handler;
		try
		{
			server = new Server(4567);
			server.start();
		} catch (IOException e)
		{
			Log.e("delphibridge", "Unable to start TCP server.", e);
			System.exit(-1);
		}
		server.addListener(new AbstractServerListener() {
			
			private void SendPartial(byte [] data, int start, int length)
			{
			  byte [] send = new byte[length];
			  for (int i=0;i<length;i++)
				  send[i]=data[start+i];
			  String s = new String(send);
			  mHandler.obtainMessage(MainApplication.DEBUGADB, "Partial:"+s).sendToTarget();
			  mHandler.obtainMessage(MainApplication.RECEIVEADB, s).sendToTarget();
			}
			@Override
			public void onReceive(org.microbridge.server.Client client, byte[] data)
			{
				int start=0;
				String s = new String(data);
				mHandler.obtainMessage(MainApplication.DEBUGADB, "All:"+s).sendToTarget();
				int p=start;
				while (start<data.length)
				{
					if (p==data.length)
					{
						// send part from start
						SendPartial(data,start,data.length-start);					
						start = data.length;
					}
					else if (data[p]==10 || data[p]==13)
					{
						// send part from [start,p>
						SendPartial(data,start,p-start);
						start=p+1;
					}
					p++;
				}								
			}					
		});
	}
	public boolean send(byte [] data)
	{
		try
		{
			server.send(data);
		} catch (IOException e)
		{
			Log.e("delphibridge", "problem sending USB message", e);
			return false;
		}	
		return true;
	}
};
