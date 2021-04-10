using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(AudioSource))]

public class CanvasManager : MonoBehaviour
{
    public bool uiBool;
    public bool heartBool;

    public GameObject menuPanel;
    public GameObject heartPanel;
    public GameObject heart_One;
    public GameObject heart_Two;
    private Animator ani;


    public HeartRate hr;

    private string currentState;
    //Animation States
    const string One_Heart = "One_Heart";
    const string Two_Heart = "Two_Heart";
    const string One_Heart_Out = "One_Heart_Out";
    const string Two_Heart_Out = "Two_Heart_Out";

    private bool one;
    private bool two;

    public AudioClip[] clips;
    private AudioSource audioSource;

    void Start()
    {
        uiBool = false;
        menuPanel.SetActive(uiBool);
        ani = this.GetComponent<Animator>();
        
        audioSource = GetComponent<AudioSource>();
        
        one = false;
        two = false;
        heartBool = true;
    }

    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Escape)) 
        {
            uiBool = !uiBool;
            menuPanel.SetActive(uiBool);
        }

        if (Input.GetKeyDown(KeyCode.Space))
        {
            heartBool = !heartBool;
           
            StartCoroutine(HeartUI(heartBool));
        }

        if (hr.One_LoggedIn && !one && heartBool)
        {
            
            ChangeAnimationState(One_Heart);
            audioSource.PlayOneShot(clips[0], 0.5f);
            one = true;
            
        }
        else if(!hr.One_LoggedIn && one && heartBool)
        {
            
            ChangeAnimationState(One_Heart_Out);
            audioSource.PlayOneShot(clips[1], 1f);
            one = false;
            
        }

        if (hr.Two_LoggedIn && !two && heartBool)
        {
            
            ChangeAnimationState(Two_Heart);
            audioSource.PlayOneShot(clips[0], 0.5f);
            two = true;
        }
        else if(!hr.Two_LoggedIn && two && heartBool)
        {
            
            ChangeAnimationState(Two_Heart_Out);
            audioSource.PlayOneShot(clips[1], 1f);
            two = false;
        }
        
    }

    void ChangeAnimationState(string newState) 
    {
        if (newState == null) return;
        if (currentState == newState) return;

        ani.Play(newState);
        
        currentState = newState;
    }

    public void ExitButton() 
    {
        Application.Quit();
       
    }

    IEnumerator HeartUI(bool heartBool) {
        //ani = GetComponent<Animator>();
        
        if (heartBool)
        { 
            heartPanel.SetActive(heartBool);
            yield break; 
        }
        while(ani.GetCurrentAnimatorStateInfo(0).normalizedTime <1.0f ||
            ani.GetCurrentAnimatorStateInfo(1).normalizedTime < 1.0f)  //GetCurrentAnimatorStateInfo(1) = second layer 가 작동중이면
        { yield return null; }
      
        heartPanel.SetActive(heartBool);

    }
   

}
