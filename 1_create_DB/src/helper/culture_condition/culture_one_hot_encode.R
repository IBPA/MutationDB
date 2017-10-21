culture_one_hot_encode = function(input_path, output_path)

{
    data= read.csv(input_path, header=TRUE,stringsAsFactors=FALSE)
    cat(colnames(data))
    df = data[,c(1:20)]
    
    # Exclude conentration columns
    
    con_index = grep("concent.*",colnames(df))
    df = df[,-con_index]
    df = df[,-grep("Medium$",colnames(df))]
    df = df[,-grep("Oxygen",colnames(df))]
    
    
    # add a head to strain features
    strain = grep("Strain.*",colnames(df))
    strain_head = rep("Strain",dim(df)[1])
    df[,strain] = paste(strain_head,df[,strain],sep="_")
    
    
    # add a head to medium 
    # Extract unique Carbon source
    Medium = grep("Medium",colnames(df))
    Medium_head = rep("Medium",dim(df)[1])
    df[,Medium] = paste(Medium_head,df[,Medium],sep="_")
    
    
    Carbon_source =  grep("Carbon.*Source",colnames(df))
    
    chem=df[,Carbon_source]
    unique_index = which(!duplicated(chem))
    total=length(unique_index)
    
    
    # label the concentration under each Carbon source with a label
    i=1
    Carbon_cent =  grep("Carbon_con.*",colnames(df))
    
    for(i in 1:total)
    {
      group = which(df[,Carbon_source]==df[unique_index[i],Carbon_source])
      sub_g_l = which(df[group,Carbon_cent]<0.03)
      sub_g_m = which(df[group,Carbon_cent]<0.21&df[group,Carbon_cent]>=0.03)
      sub_g_h = which(df[group,Carbon_cent]>=0.21)
      
      df[group[sub_g_l],Carbon_cent] = "low"
      df[group[sub_g_m],Carbon_cent] = "medium"
      df[group[sub_g_h],Carbon_cent] = "high"
      
    }
    
    
    # combine Carbon source and concentration columns
    df[is.na(df[,Carbon_cent]),Carbon_cent]=""    # reaplace NA cells with blank
    com=paste(df[,Carbon_source],df[,Carbon_cent],sep="_")
    com[which(com=="_")]=""
    
    df[,Carbon_source]=com
    
    
    Carbon_head = rep("Carbon",dim(df)[1])
    df[,Carbon_source] = paste(Carbon_head,com,sep="_")
    
    
    # supplemental column
    supp_1 =   grep("Supplement_1",colnames(df))
    df[,supp_1] = sub(".*cetate.*","Sodium acetate",df[,supp_1])
    
    
    Supp_head = rep("Suppl",dim(df)[1])
    df[,supp_1] = paste(Supp_head,df[,supp_1],sep="_")
    
    supp_2 =   grep("Supplement_2",colnames(df))
    Supp_head = rep("Suppl",dim(df)[1])
    df[,supp_2] = paste(Supp_head,df[,supp_2],sep="_")
    
    
    # sort out time (Discard profiles without generation infor)
    time = grep("Time.*",colnames(df))
    day_time = grep(".*Days",df[,time])
    df = df[-day_time,]
    df[,time] = as.numeric(df[,time])
    
    
    
    # exclude missing profiles
    
    miss = which(is.na(df[,time])==TRUE)
    df = df[-miss,]
    total = dim(df)[1]-2
    
    
    summary(df[,time])
    
    # bin time to seven intervals
    sub_g_ss = which(df[,time]<=500)
    sub_g_s = which(df[,time]>500&df[,time]<=1000)
    sub_g_sm = which(df[,time]>1000&df[,time]<=5000)
    sub_g_m = which(df[,time]>5000&df[,time]<=10000)
    sub_g_ml = which(df[,time]>10000&df[,time]<=20000)
    sub_g_l = which(df[,time]>20000&df[,time]<=30000)
    sub_g_ll = which(df[,time]>30000&df[,time]<=40000)
    
    df[sub_g_ss,time] = "0-500"
    df[sub_g_s,time] = "500-1K"
    df[sub_g_sm,time] = "1K-5K"
    df[sub_g_m,time] = "5K-10K"
    df[sub_g_ml,time] = "10K-20K"
    df[sub_g_l,time] = "20K-30K"
    df[sub_g_ll,time] = "30K-40K"
    
    
    
    time_head = rep("time",dim(df)[1])
    df[,time] = paste(time_head,df[,time],sep="_")
    
    
    # stress column
    stress = grep("Stress.*",colnames(df))
    Butanol = grep (".*tanol",df[,stress])
    df[Butanol,stress] = "Butanol"
    
    
    Acid = which(df[,13]<7)
    df[Acid,stress] = "Acid"
    
    df[,stress] = sub(".*g/mL","",df[,stress])
    
    
    df[,stress] = sub("1/8MIC","",df[,stress])
    df[,stress] = sub("1/2MIC","",df[,stress])
    
    df[,stress] = sub("1/2 MIC ","",df[,stress])
    
    df[,stress] = sub(" ","",df[,stress])
    
    
    
    stress_head = rep("stress",dim(df)[1])
    df[,stress] = paste(stress_head,df[,stress],sep="_")
    
    #temperature
    Temp = grep("Temp.*",colnames(df))
    
    normal_i = which (df[,Temp] == "37")
    high_i = grep(".*4.*",df[,Temp])
    not_low = c(normal_i,high_i)
    
    df[normal_i,Temp] = "normal"
    df[high_i,Temp] = "high"
    
    df[-not_low,Temp] = "low"
    
    
    Tem_head = rep("Tem",dim(df)[1])
    df[,Temp] = paste(Tem_head,df[,Temp],sep="_")
    
    
    # Pert  . No need to unify the entries.  df[,14]
    # add head to Pert column
    Oxygen = grep("Oxygen",colnames(df))
    Oxygen_head = rep("Oxygen",dim(df)[1])
    df[,Oxygen] = paste(Oxygen_head,df[,Oxygen],sep="_")
    
    
    Pert = grep("Perturbations",colnames(df))
    Pert_head = rep("Pert",dim(df)[1])
    df[,Pert] = paste(Pert_head,df[,Pert],sep="_")
    
    
    
    # Include unique features
    ex_col=c(grep("X.*Carbon",colnames(df)),grep("Increasing",colnames(df)),grep("pH",colnames(df)),
             grep("Deletion",colnames(df)),grep("knockout",colnames(df)))
    
    df = df[,-ex_col]
    
    p=dim(df)[2]
    
    colnames(df)
    
    # initiate a vector to hold the features
    feature=as.character()
    i=3
    
    seq = (3:p)[-(Carbon_cent-2)]
    
    
    for (i in seq)
    {
      feature = c(feature,df[which(!duplicated(df[,i])),i])
     
      cat(df[which(!duplicated(df[,i])),i] ,"\n\n")
    }
    feature = feature[which(!duplicated(feature))]
    
    n=dim(df)[1]
    pp=length(feature)+1
    
    culture = matrix(nrow=n,ncol=pp)
    
    
    i=1
    for ( i in 1:n) 
    {
      
     j=3
     for (j in 3:p)
     {
       index=match(df[i,j],feature)
      culture[i,index]=1
     }
    
    }
    
    culture[is.na(culture)]=""  
    
    output=data.frame(df[,1:2],culture)
    
    names(output) = c("ID","PMID",feature)
    
    
    j=1
    indicator = as.numeric()
    
    p=dim(output)[2]
    
    for (j in 1:p)
    {
      indicator[j] = sum(which(!output[,j]==""))
      
    } 
    blank = which(indicator==0)
    
    Not_NA_space = grep ("[a-zA-Z]",colnames(output))
    
    
    out = output[,Not_NA_space]
    
    final = data.frame(out)
    
    # search exactly since some columns are not full
    
    b=  grep("Carbon_$",colnames(final))
    d=  grep("Suppl_$",colnames(final))
    e =  grep("stress_$",colnames(final))
    f =  grep("Pert_$",colnames(final))
    
    out_index = (1:dim(final)[2])[-c(b,d,e,f)]
    
    write.csv(final[,out_index],output_path,row.names=FALSE)


}


# strain, medium, pert and son on, captial or lower case, it should be unified. 
# bugs 40 41 41 42 43 should be treated as one unique condition if you neglect the difference in time
# No 500- 1K although there is some in fact.  