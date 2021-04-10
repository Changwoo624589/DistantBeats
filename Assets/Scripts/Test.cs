using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Test : MonoBehaviour
{
    public float heartRate;
    public Material mat;

    public GameObject obj;
    public Color colTo;

    void Start()
    {
        
        Renderer renderer = obj.GetComponent<Renderer>();
        renderer.sharedMaterial.SetColor("Col_Lea2_From", colTo);
    }

    void Update()
    {
        /*        float normal = Mathf.InverseLerp(40,100,heartRate);
                float blue = Mathf.Lerp(1.8f, 0, normal);
                print(blue);*/

        //heartRate += 0.1f  *Time.deltaTime;
       // print(heartRate);
    }
}
