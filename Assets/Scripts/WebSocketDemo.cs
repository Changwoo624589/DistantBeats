using System.Collections;
using System.Collections.Generic;
using System.Text;
using UnityEngine;

using HybridWebSocket;

public class WebSocketDemo : MonoBehaviour
{
    public int numOneVal = 0;
    public int numTwoVal = 0;
    public int distValOne;
    public int distValTwo = 0;
    public string comingweb;

    void Start()
    {
        WebSocket ws = new WebSocket("wss://webscoket-unity.herokuapp.com/");
        //Add OnOpen event listener
        ws.OnOpen += () =>
        {
            // Debug.Log("WS connected!");
            // Debug.Log("WS state: " + ws.GetState().ToString());
            ws.Send(Encoding.UTF8.GetBytes("Hello from Unity 3D!"));
        };

        // Add OnMessage event listener
        ws.OnMessage += (byte[] msg) =>
        {
            // Debug.Log("WS received message: " + Encoding.UTF8.GetString(msg));
            comingweb = Encoding.UTF8.GetString(msg);
            if (comingweb.Contains("Client One"))
            {
                string firstClient = comingweb.Replace("Client One", "");
                firstClient = firstClient.Trim('"');
                numOneVal = int.Parse(firstClient);
                //Debug.Log(numOneVal);
            }
            if (comingweb.Contains("Client Two"))
            {
                string secondClient = comingweb.Replace("Client Two", "");
                secondClient = secondClient.Trim('"');

           
                numTwoVal = int.Parse(secondClient);
               // Debug.Log(numTwoVal);
            }
            if (comingweb.Contains("DistanceOne"))
            {

                string distanceClientOne = comingweb.Replace("DistanceOne", string.Empty);
                distanceClientOne = distanceClientOne.Trim('"');
              
                
                //distanceClientOne = "3";
                
                float disOne = float.Parse(distanceClientOne);
                
                //Debug.Log("distValOne: "+ distValOne);
                distValOne = (int)disOne;
                
            }
            if (comingweb.Contains("DistanceTwo"))
            {
                string distanceClientTwo = comingweb.Replace("DistanceTwo", string.Empty);
                distanceClientTwo = distanceClientTwo.Trim('"');

                float disTwo = float.Parse(distanceClientTwo);

                distValTwo = (int)disTwo;
               // Debug.Log(distValTwo);
            }
        };

        // Add OnClose event listener
        ws.OnClose += (WebSocketCloseCode code) =>
        {
            Debug.Log("WS closed with code: " + code.ToString());
        };

        // Connect to the server
        ws.Connect();
    }


}