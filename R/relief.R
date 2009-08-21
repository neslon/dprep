relief <-
function (data,nosample,threshold,vnom=0) 
{if(vnom[1]>0)
{reliefcat(data,nosample,threshold,vnom)}
else{reliefcont(data,nosample,threshold)}
}

