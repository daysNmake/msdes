

## Generalising discrete event simulation model for relapsing remitting multiple sclerosis, and creating a user-friendly interface to the R model (NIHR177949)



### Plain English Summary:
This project aimed to address the limitations of a Discrete Event Simulation (DES)
model in relapsing-remitting multiple sclerosis (RRMS) that was previously developed 
as part of a NICE Multiple Technology Appraisal for [Natalizumab and Tyruko (natalizumab biosimilar) 
for treating highly active RRMS](#ref-200). This will allow the model to be used in other RRMS NICE appraisals and academic research. 
The specific objectives were to:

   1. Re-implement the model using the [WARDEN](#ref-201) R package, 
   speeding execution and reducing the need for high performance computing. 
   
   2. Modularise the model in separate scripts and functions, enabling subsections to be tested and edited without modifying the whole model. 
   
   3. Develop a [RShiny](#ref-202) GUI that allows users to run the model and change settings without having to run or edit the R code itself.
   
   4. Write a detailed user guide, explaining how to adapt the model to other RRMS settings.
    


### Abstract:
Multiple sclerosis (MS) is a common lifelong condition affecting the brain and spine. 
It can cause symptoms like vision problems, trouble with balance, movement, thinking, and bladder or bowel control. 
MS often starts in early adulthood and usually worsens over time, though this varies. 
Some patients, known as having "highly active RRMS (HARRMS)", continue to have relapses despite treatment and may need different medications. 
In the UK the National Institute for Health and Care Excellence (NICE) uses models to decide which treatments provide the most health and best value for money. 
Models in Relapsing Remitting Multiple Sclerosis (RRMS) have much simplified the disease. 
These old models did not match real-world RRMS data. 

The [Bristol Technology Assessment Group (TAG)](#ref-203) built a new type of HARRMS model called a Discrete Event Simulation (DES) in the R software. 
This much better matched real-world data. The code was slow and hard to use as not many people know R. 
This prevents it being used in other RRMS decisions. We will update the DES models to use new R tools which will make it faster.
We will also make the model simpler by breaking it into smaller chunks. 
We will write an easy-to-use interface like you would have for a calculator. 
This will allow people who can't use R to use the model. We will also write good instructions for using the model. 
This will make it easier for decision makers and researchers to use our [DES](#ref-204) in future RRMS decisions.



#### Refernces:
###### Lopez Manzano C, Sadek A, Cooper C, Tomlinson E, Wang H, Rice C, et al. Natalizumab and Tyruko (natalizumab biosimilar) for treating highly active relapsing-remitting multiple sclerosis after at least one disease-modifying therapy: a systematic review and economic model. Health Technol Assess 2026;30(60). https://doi.org/10.3310/GJHT1811 {#ref-200}
###### Sanchez Alvarez J (2025). WARDEN: Workflows for Health Technology Assessments in R using Discrete EveNts. R package version 2.0.1, https://jsanchezalv.github.io/WARDEN/ {#ref-201}
###### https://shiny.posit.co/ {#ref-202}
###### https://www.bristol.ac.uk/population-health-sciences/centres/beam-centre/bristol-tag/ {#ref-203}
###### https://github.com/makeNdays/MS-DES {#ref-204}

