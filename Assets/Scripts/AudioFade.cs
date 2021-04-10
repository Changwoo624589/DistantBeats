using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(AudioSource))]
public class AudioFade : MonoBehaviour
{
    /// <summary>
    /// This script fades the audio in and out depending on whether playersActive is true or not. 
    /// Relies on being called by HeartRate.cs.
    /// </summary>

    private AudioSource audioSource;
    public bool playersActive = false;

    [SerializeField][Range(0,1)] private float maxVolume = 1f;
    [SerializeField] private float timeToFade = 5f;

    private Coroutine audioRoutine;
    private bool checkBool = false;

    #region Singleton
    public static AudioFade instance;
    private void Awake()
    {
        if (instance != null)
        {
            Debug.LogWarning("More than one instance of AudioFade found!");
            return;
        }
        instance = this;
    }
    #endregion

    private void Start()
    {
        audioSource = GetComponent<AudioSource>();
        audioSource.volume = 0f;
        Invoke("StartFade", 3f);
    }

    private void Update()
    {
        CheckPlayers();
    }
    private void CheckPlayers()
    {
        if (playersActive != checkBool)
        {
            checkBool = playersActive;
        } else
        {
            return;
        }

        StartFade();
    }
    private void StartFade()
    {
        if (playersActive)
        {
            FadeIn();
        }
        else
        {
            FadeOut();
        }
    }

    private void FadeIn()
    {
        if (audioRoutine != null)
        {
            StopCoroutine(audioRoutine);
        }
        audioRoutine = StartCoroutine(StartFading(timeToFade, maxVolume));
    }
    private void FadeOut()
    {
        if (audioRoutine != null)
        {
            StopCoroutine(audioRoutine);
        }
        audioRoutine = StartCoroutine(StartFading(timeToFade, 0f));
    }

    private IEnumerator StartFading(float duration, float targetVolume)
    {
        float currentTime = 0;
        float startVolume = audioSource.volume;

        while (currentTime < duration)
        {
            currentTime += Time.deltaTime;
            audioSource.volume = Mathf.Lerp(startVolume, targetVolume, currentTime / duration);
            yield return null;
        }
        yield break;
    }



}
