using System.Collections;
using System.Collections.Generic;
using UnityEngine;

//Xander 4.6.21:
//I got this script from this set of tutorials originally: https://www.patreon.com/posts/21646034

public class DrawClouds : MonoBehaviour {

    
    private Material cloudMaterial; // getting it in Start instead to avoid accidentally forgetting to set it here to the correct material
    private Transform thisTransform;
    public Camera camera;

    public int layer = 30; // if you want your clouds to be on a certain rendering layer

    private Matrix4x4 matrix;
    private Matrix4x4[] matrices;
    private float offset = 0.005f;



    [Header("Horizontal Stack")]
    public Mesh meshHorizontalStack;
    public bool drawHorizontalStack = true;
    public float cloudHeight = 3f;
    [Range(0, 60)] public int horizontalStackDensity = 20;
    private int densityHorizontalStackApplied;


    [Header("Lock to Camera")] // for infinite clouds
    public bool followYLocked = true;
    public Transform cameraTransform;
    private Vector3 lockedPosition;

    [Header("Shadows")]
    public bool castShadows = true;
    public bool castShadowCentre = true;
    public bool recieveShadows = false;


    [Header("Optimisation")]
    public int minimumHorizontalAmount = 1;
    public float fadeInDistanceStack = 30f;

    public float nearFadeDistance = 1f;
    public bool useGpuInstancing = false; // works best if always on one side of the clouds, because drawing in one pass makes z sorting problems with itself?
    public bool drawTopToBottom = false;



    void Start () {
        if (thisTransform == null)
            thisTransform = transform;
        if (meshHorizontalStack == null)
            meshHorizontalStack = GetComponent<MeshFilter>().sharedMesh;
        if (camera == null)
            camera = Camera.main;
        if (!cameraTransform)
            cameraTransform = camera.transform;
        Renderer thisRenderer = GetComponent<Renderer>();
        cloudMaterial = thisRenderer.sharedMaterial;
        thisRenderer.enabled = false;
        matrix = Matrix4x4.TRS(thisTransform.position + (Vector3.up * offset), thisTransform.rotation, thisTransform.localScale);

    }


    void Update()
    {
        

        cloudMaterial.SetFloat("_midYValue", thisTransform.position.y);
        cloudMaterial.SetFloat("_cloudHeight", cloudHeight);



        float heightDifference = Mathf.Abs(thisTransform.position.y - cameraTransform.position.y);
        float floorMultiplier = 1f / (1f - nearFadeDistance); // so that when it gets close it fades in fully

        densityHorizontalStackApplied = 0;


        if (heightDifference < fadeInDistanceStack)
        {
            densityHorizontalStackApplied = (int)(horizontalStackDensity * Mathf.Clamp01(((1f - (heightDifference / fadeInDistanceStack)) * floorMultiplier)));

        }

        if (densityHorizontalStackApplied < minimumHorizontalAmount)
        {
            minimumHorizontalAmount = Mathf.Clamp(minimumHorizontalAmount, 0, horizontalStackDensity);
            densityHorizontalStackApplied = minimumHorizontalAmount;
        }


        if (drawHorizontalStack && densityHorizontalStackApplied != 0)
        {
            DrawHorizontalStack();
        }

        if (cameraTransform != null)
        {
            lockedPosition = cameraTransform.position;
            lockedPosition.y = thisTransform.position.y;

            if (followYLocked)
                thisTransform.position = lockedPosition;

        }



    }

    void DrawHorizontalStack()
    {
        if (meshHorizontalStack == null)
            return;

        offset = cloudHeight / densityHorizontalStackApplied / 2f;

        matrix = Matrix4x4.TRS(thisTransform.position, thisTransform.rotation, thisTransform.localScale);// ((thisTransform.localScale * (baseScaleLocalVolume - (scaleDownStepPerLayer * i * baseScaleLocalVolume / densityVertical)))));

        if (useGpuInstancing)
        {
            matrices = new Matrix4x4[densityHorizontalStackApplied]; // the extra one is for the middle
        }
        else
            Graphics.DrawMesh(meshHorizontalStack, matrix, cloudMaterial, layer, camera, 0, null, castShadowCentre, recieveShadows, false);

        //alternative top then bottom
        if (!drawTopToBottom)
        {
            for (int i = 1; i <= densityHorizontalStackApplied; i++)
            {
                matrix = Matrix4x4.TRS(thisTransform.position + (Vector3.up * offset * i), thisTransform.rotation, thisTransform.localScale);// ((thisTransform.localScale * (baseScaleLocalVolume - (scaleDownStepPerLayer * i * baseScaleLocalVolume / densityVertical)))));

                if (!useGpuInstancing)
                {
                    Graphics.DrawMesh(meshHorizontalStack, matrix, cloudMaterial, layer, camera, 0, null, castShadows, recieveShadows, false);
                }
                else
                {
                    matrices[i - 1] = matrix; // build the matrices array if using GPU instancing
                }
                offset *= -1; // alternative above and then below, to make clouds on both sides of the mesh
            }
        }
        else
        {
            //OR all in a row, top to bottom - works better with GPU Instancing if seen from below.
            Vector3 startPosition = thisTransform.position + (Vector3.up * (offset * densityHorizontalStackApplied / 2f));
            for (int i = 0; i < densityHorizontalStackApplied; i++)
            {
                matrix = Matrix4x4.TRS(startPosition - (Vector3.up * offset * i), thisTransform.rotation, thisTransform.localScale);

                if (!useGpuInstancing)
                {
                    Graphics.DrawMesh(meshHorizontalStack, matrix, cloudMaterial, layer, camera, 0, null, castShadows, recieveShadows, false);
                }
                else
                {
                    matrices[i] = matrix; // build the matrices array if using GPU instancing
                }
            }
        }
        if (useGpuInstancing)
        {
            //draw all those matrices you built
            UnityEngine.Rendering.ShadowCastingMode shadowCasting = UnityEngine.Rendering.ShadowCastingMode.Off;
            if (castShadows)
                shadowCasting = UnityEngine.Rendering.ShadowCastingMode.On;

            Graphics.DrawMeshInstanced(meshHorizontalStack, 0, cloudMaterial, matrices, densityHorizontalStackApplied, null, shadowCasting, recieveShadows, layer, camera);

        }


    }

















}
