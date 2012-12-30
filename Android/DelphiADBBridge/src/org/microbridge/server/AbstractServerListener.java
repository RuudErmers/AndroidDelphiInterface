package org.microbridge.server;

/**
 * 
 * Base class for implementing a ServerListener. Extend this class to capture a subset of the server events.
 * 
 * @author Niels Brouwers
 *
 */
public class AbstractServerListener implements ServerListener
{

	public void onServerStarted(Server server)
	{
	}

	public void onServerStopped(Server server)
	{
	}

	public void onClientConnect(Server server, Client client)
	{
	}

	public void onClientDisconnect(Server server, Client client)
	{
	}

	public void onReceive(Client client, byte[] data)
	{
	}

}
