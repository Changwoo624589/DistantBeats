using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ReciverSocket : MonoBehaviour
{
    [SerializeField]
    private WebSocketDemo webData;

    void Start()
    {
        // Debug.Log("START RECIVER");
        // if(webData.dataArrived == true){
            // Debug.Log("START DEBUGGER");
            // Debug.Log(webData.webvalue);
        // }
    }

    void Update() {
    //    Debug.Log(webData.webvalue);
    }
}
