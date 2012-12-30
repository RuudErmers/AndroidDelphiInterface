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

import mbed.adkPort.AdkBridge;
import android.app.Activity;
import android.os.Handler;

public class DelphiBridge {
	private static AdkBridge adkBridge;
	DelphiBridge(Activity parentActivity, Handler handler)
	{
		adkBridge = new AdkBridge(parentActivity, handler);
	}
	public boolean send(byte [] data)
	{
		return adkBridge.send(data);
	}    
};
