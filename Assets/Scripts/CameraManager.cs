using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class CameraManager : MonoBehaviour
{
    [Range(15,60)]
    public float rawData;
    public float normalData;
    public float currentNorData;
    public float fov;
    [Header("Sensor Data")]
    public float minData;
    public float maxData;
    [Header("Camera FOV")]
    public float fovMin;
    public float fovMax;
    [Header("Canvas")]
    public CanvasManager canvasScript;
    public Slider sliderFOV;
    public WebSocketDemo webDemo;
    void Start()
    {
        rawData = maxData;
        currentNorData = 1f;
    }

    void Update()
    {
        if (canvasScript.uiBool)
        {
            rawData = sliderFOV.value;
        }
        else 
        {
            if (webDemo.distValOne > 0)
            {
                rawData = webDemo.distValOne;
            }
            else { rawData = maxData; }

          //  Debug.Log("distance: " + rawData);
        }

        normalData = Mathf.InverseLerp(minData, maxData, rawData);
        if (Mathf.Abs(normalData - currentNorData) > 0.005f) 
        {
            //float speed = 0.25f;
            float speed = Mathf.Abs(normalData - currentNorData) * 0.2f;
            if (normalData > currentNorData && currentNorData < 1)
            {
                currentNorData += speed * Time.deltaTime;
            }
            else if (currentNorData > normalData && currentNorData > 0) 
            {
                currentNorData -= speed * Time.deltaTime;
            }
        }
        fov = Mathf.Lerp(fovMin, fovMax, Mathf.Clamp(currentNorData,0,1));
        Camera.main.fieldOfView = fov;
    }
}
