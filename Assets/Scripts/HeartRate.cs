using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

[RequireComponent(typeof(AudioSource))]

public class HeartRate : MonoBehaviour
{

    [SerializeField]
    [Range(0, 100f)]
    public float heartRate_One;
    [SerializeField]
    [Range(0, 100f)]
    public float heartRate_Two;
    public float heartRateNormal_One;
    public float heartRateNormal_Two;
    public float currentHRNormal_One;
    public float currentHRNormal_Two;
    //public float currentHRValue;
    public float hrMin, hrMax;
    [Header("Skybox")]
    public Color col_SkyFrom;
    public Color col_SkyTo;
    
    public Color col_GroundFrom;
    public Color col_GroundTo;

    private float exposureCurrent = 0.2f;
    //private float exposureTo = 0.7f;

    [Header("Tree1")]  
    private Color colFrom;
    private Color colTo;
    [Header("Tree2")]
    private Color colFrom2;
    private Color colTo2;
    private Color colGrassFrom;
    private Color colGrassTo;
    private Color colFlowerFrom;
    private Color colFlowerTo;

    private Renderer render;
    //prefab
    [Header("Prefab")]
    public GameObject grass;
    public GameObject tree1;
    public GameObject tree2;
    public GameObject flower;
    public GameObject cloud;
    public GameObject [] butterfly;
    [Header("Canvas")]
    public Text hRText_One;
    public Text hRText_Two;
    public Slider slider_One;
    public Slider slider_Two;
    public CanvasManager canvasScript;
    
    [Header("Script")]
    public WebSocketDemo web;
    public AudioFade audioFade;
    [Header("Music")]
    public AudioClip[] backgroundMusics;
    public bool leaves;
    public bool One_LoggedIn;
    public bool Two_LoggedIn;
    public bool one_Ready;
    public bool two_Ready;
    bool b0 = false;
    bool b1 = false;
    private float alpha_Tree1 = 1;
    private float cloudStrength = 0f;

    //private float alpha_Tree2 = 1;
    //WebSocketDemo.webvalue
  
    
    void Start()
    {

        butterfly[0].SetActive(false);
        butterfly[1].SetActive(false);
        alpha_Tree1 = 1;

        exposureCurrent = 0.2f;
        cloudStrength = 0f;
        //exposureTo = 0.7f;
        
        leaves = false;
        currentHRNormal_One = 0;
        currentHRNormal_Two = 0;

        GetColor();
        SkyBoxColor(currentHRNormal_Two);
        GrassColor(currentHRNormal_One);
        Tree_1_Color(currentHRNormal_One);
        Tree_2_Color(currentHRNormal_One);
        Flower_Color(currentHRNormal_One);
       // StartCoroutine(FadeMixerGroup.StartFade(AudioMixer audioMixer, String exposedParameter, float duration, float targetVolume));
    }

    void Update()
    {
        if (canvasScript.uiBool)
        {
            heartRate_One = slider_One.value;
            heartRate_Two = slider_Two.value;
        }
        else {
            heartRate_One = (float)web.numOneVal;
            heartRate_Two = (float)web.numTwoVal;
        }
        CheckLoggedIn(heartRate_One,heartRate_Two);
        AudioFadeFunction();
        heartRateNormal_One = Mathf.InverseLerp(hrMin, hrMax, heartRate_One);
        
        heartRateNormal_Two = Mathf.InverseLerp(hrMin, hrMax, heartRate_Two);

        // right after logged in
        Leaves();  //user_one logged in 
        Clouds(currentHRNormal_Two);
        SkyBoxExposure(); //user_two logged in

        #region Calculation
        float speed = 0.35f;
        //clinet one
        if (Mathf.Abs(heartRateNormal_One - currentHRNormal_One) > 0.005f && one_Ready )
        {
            
            if (heartRateNormal_One > currentHRNormal_One && currentHRNormal_One < 1)
            {
                currentHRNormal_One += speed * Time.deltaTime;
            }
            else if (currentHRNormal_One > heartRateNormal_One && currentHRNormal_One>0) 
            {
                currentHRNormal_One -= speed * Time.deltaTime;
            }
        }
        //client two
        if (Mathf.Abs(heartRateNormal_Two - currentHRNormal_Two) > 0.005f && two_Ready) 
        {
            if (heartRateNormal_Two > currentHRNormal_Two && currentHRNormal_Two < 1)
            {
                currentHRNormal_Two += speed * Time.deltaTime;
            }
            else if (currentHRNormal_Two > heartRateNormal_Two && currentHRNormal_Two > 0)
            {
                currentHRNormal_Two -= speed * Time.deltaTime;
            }
        }
        #endregion



        // after both logged in and ready

        //Clouds(currentHRNormal_Two);
        SkyBoxColor(currentHRNormal_Two);
            
            GrassColor(currentHRNormal_One);
            Tree_1_Color(currentHRNormal_One);
            Tree_2_Color(currentHRNormal_One);
            Flower_Color(currentHRNormal_One);
        Butterfly(currentHRNormal_One);
        #region CanvasText
        hRText_One.text = Mathf.RoundToInt(heartRate_One).ToString();
        hRText_Two.text = Mathf.RoundToInt(heartRate_Two).ToString();
        #endregion
    }

    public void GetColor()
    {
        render = grass.GetComponent<Renderer>();
        //colGrassCurrent = render.sharedMaterial.GetColor("Col_Grass_Current");
        colGrassFrom = render.sharedMaterial.GetColor("Col_Grass_From");
        colGrassTo = render.sharedMaterial.GetColor("Col_Grass_To");

        render = tree1.GetComponent<Renderer>();
        //colCurrent = render.sharedMaterial.GetColor("Col_Lea1_Current");
        colFrom = render.sharedMaterial.GetColor("Col_Lea1_From");
        colTo = render.sharedMaterial.GetColor("Col_Lea1_To");

        render = tree2.GetComponent<Renderer>();
        //colCurrent2 = render.sharedMaterial.GetColor("Col_Lea2_Current");
        colFrom2 = render.sharedMaterial.GetColor("Col_Lea2_From");
        colTo2 = render.sharedMaterial.GetColor("Col_Lea2_To");
       
        render = flower.GetComponent<Renderer>();
        colFlowerFrom = render.sharedMaterial.GetColor("Col_Flower_From");
        colFlowerTo = render.sharedMaterial.GetColor("Col_Flower_To");

        render = cloud.GetComponent<Renderer>();
        render.sharedMaterial.SetFloat("_CloudStrength", cloudStrength);
    }

    public void CheckLoggedIn(float oneHR, float twoHR) {
        One_LoggedIn = oneHR > 0 ? true : false;
        Two_LoggedIn = twoHR > 0 ? true : false;
    }


    public void Leaves()
    {
        render = tree1.GetComponent<Renderer>();
        render.sharedMaterial.SetFloat("Tree1_Alpha", alpha_Tree1);
        render = tree2.GetComponent<Renderer>();
        render.sharedMaterial.SetFloat("Tree2_Alpha", alpha_Tree1);

        if (One_LoggedIn && alpha_Tree1 > 0.09f)  //start from 1
        {
            alpha_Tree1 -= 0.28f * Time.deltaTime;
        }
        else if (!One_LoggedIn && alpha_Tree1 < 1f && currentHRNormal_One <= 0.005f) 
        {
            alpha_Tree1 += 0.28f * Time.deltaTime;
        }
        if (!One_LoggedIn && one_Ready && currentHRNormal_One <= 0.005f)
        {
            if (alpha_Tree1 < 1f)
            {
                alpha_Tree1 += 0.28f * Time.deltaTime;
            }
        }
        one_Ready = alpha_Tree1 <= 0.09f ? true : false;

    }

    public void SkyBoxExposure() 
    {

        if (Two_LoggedIn && exposureCurrent < 0.8f)
        {
            exposureCurrent += 0.3f * Time.deltaTime;
        }
        else if (!Two_LoggedIn && exposureCurrent > 0.2f && currentHRNormal_Two <= 0.005f) //reset
        {
            exposureCurrent -= 0.3f * Time.deltaTime;
        }
        if (!Two_LoggedIn && two_Ready && currentHRNormal_Two <= 0.005f) {
            if (exposureCurrent < 0.2f)
            {
                exposureCurrent -= 0.3f * Time.deltaTime;
            }
        }
        two_Ready = exposureCurrent >= 0.7f ? true : false;
        RenderSettings.skybox.SetFloat("_Exposure", exposureCurrent);
    }

    public void Clouds(float heartRateNormal) {
        render = cloud.GetComponent<Renderer>();
       
        if (Two_LoggedIn && cloudStrength <= 0.27f) {

            cloudStrength += 0.2f * Time.deltaTime;
            
            render.sharedMaterial.SetFloat("_CloudStrength",cloudStrength);
            //Debug.Log(Mathf.Clamp(cloudStrength, 0f, 0.27f));
            
        }
        else if (!Two_LoggedIn && currentHRNormal_Two < 0.005f && cloudStrength > 0)
        {
            cloudStrength -= 0.2f * Time.deltaTime;
            render.sharedMaterial.SetFloat("_CloudStrength", Mathf.Clamp(cloudStrength, 0f, 0.27f));
        }
        //max 0.38  min: 0.27
        if (Two_LoggedIn && cloudStrength >= 0.27f)
        {
            cloudStrength = Mathf.Lerp(0.27f, 0.39f, heartRateNormal);
            render.sharedMaterial.SetFloat("_CloudStrength", cloudStrength);
            
        }
    }

    public void SkyBoxColor(float heartRateNormal) {

        RenderSettings.skybox.SetColor("_SkyTint", Color.Lerp(col_SkyFrom, col_SkyTo, heartRateNormal));
        RenderSettings.skybox.SetColor("_GroundColor", Color.Lerp(col_GroundFrom, col_GroundTo, heartRateNormal));
        
    }

    public void GrassColor(float heartRateNormal)
    {

        render = grass.GetComponent<Renderer>();
        render.sharedMaterial.SetColor("Col_Grass_Current", Color.Lerp(colGrassFrom, colGrassTo, heartRateNormal));

    }

    public void Tree_1_Color(float heartRateNormal) 
    {
        render = tree1.GetComponent<Renderer>();
        render.sharedMaterial.SetColor("Col_Lea1_Current", Color.Lerp(colFrom, colTo, heartRateNormal));
        
    }

    public void Tree_2_Color(float heartRateNormal) 
    {
        
        render = tree2.GetComponent<Renderer>();
        render.sharedMaterial.SetColor("Col_Lea2_Current", Color.Lerp(colFrom2, colTo2, heartRateNormal));

    }

    public void Flower_Color(float heartRateNormal) 
    {
        render = flower.GetComponent<Renderer>();
        render.sharedMaterial.SetColor("Col_Flower_Current", Color.Lerp(colFlowerFrom, colFlowerTo, heartRateNormal));
    
    }
    public void Butterfly(float hearRateNormal) {


        if (hearRateNormal > 0.45f && !b0)
        {
            butterfly[0].SetActive(true);
            b0 = true;
        }
        else if (hearRateNormal < 0.35f && b0) 
        
        {
            butterfly[0].SetActive(false);
            b0 = false;
        }
        if (hearRateNormal > 0.8f && !b1)
        {
            butterfly[1].SetActive(true);
            b1 = true;
        }
        else if (hearRateNormal < 0.65f && b1) 
        { 
            butterfly[1].SetActive(false);
            b1 = false;
        }
    }

    public void AudioFadeFunction() {
        // Update AudioFade
        if (One_LoggedIn == true && Two_LoggedIn == true && !audioFade.playersActive)
        {
            audioFade.playersActive = true;
        }
        else
        {
            audioFade.playersActive = false;
        }
        //
    }
 
}
