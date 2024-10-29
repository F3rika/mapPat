def read_REF(refFile, lim):
     ref_ADM={}

     with open(refFile, encoding = 'UTF-8') as refFile_ADM:
         refFile_ADM.readline()
         
         for line in refFile_ADM:
             l=line.replace('\n', '').split('\t')
             refCountry=l[1]
             scoCountry=int(l[2])
             refRegion=l[4]
             scoRegion=l[5]
             
             if scoCountry<=lim:
                 if refCountry not in ref_ADM.keys():
                     ref_ADM[refCountry]=[]
                     
                     if scoRegion!='NA':
                         scoRegion=int(scoRegion)
                         
                         if scoRegion<=lim:
                             if refRegion not in ref_ADM[refCountry]:
                                 ref_ADM[refCountry].append(refRegion)
                      
                     else:
                         ref_ADM[refCountry].append('NA')
                 
                 else:
                     if scoRegion!='NA':
                         scoRegion=int(scoRegion)
                         
                         if scoRegion<=lim:
                             if refRegion not in ref_ADM[refCountry]:
                                 ref_ADM[refCountry].append(refRegion)
     
     return ref_ADM


def read_CORR(corrFile):
     CORR_dict={}
         
     with open(corrFile, encoding = 'UTF-8') as CORR_table:
         CORR_table.readline()
         
         for line in CORR_table:
             l=line.replace('\n','').split('\t')
             original=l[1].strip()
             corrected=l[2].strip()
             
             if original not in CORR_dict.keys():
                 CORR_dict[original]=corrected
     
     return CORR_dict


def matchCounting(testFile, dbName, refADM1, refADM2, corrCou, corrReg):
     TEST_counts={}
     
     with open(testFile, encoding = 'UTF-8') as TEST_table:
         TEST_table.readline()
         
         for line in TEST_table:
             if dbName=='GISAID':
                 l=line.replace('\n','').split('\t')
                 loc=l[6].split('/')
             
             elif dbName=='Nextstrain':
                 l=line.replace('\n','').split('\t')
                 loc=l[4:7]
             
             if len(loc)>2:
                 cou=loc[1].strip()
                 reg=loc[2].strip()
                 
                 if cou in corrCou.keys():
                     cou=corrCou[cou]
                     
                     if cou in refADM1.keys() or cou in refADM2.keys():
                         if cou not in TEST_counts:
                             TEST_counts[cou]=[1,0,0,0]
                             
                             if reg in corrReg.keys():
                                 reg=corrReg[reg]
                                 
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                             
                             else:
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                         
                         else:
                             TEST_counts[cou][0]+=1
                             
                             if reg in corrReg.keys():
                                 reg=corrReg[reg]
                                 
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                             
                             else:
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                 else:
                     if cou in refADM1.keys() or cou in refADM2.keys():
                         if cou not in TEST_counts:
                             TEST_counts[cou]=[1,0,0,0]
                             
                             if reg in corrReg.keys():
                                 reg=corrReg[reg]
                                 
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                             
                             else:
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                         
                         else:
                             TEST_counts[cou][0]+=1
                             
                             if reg in corrReg.keys():
                                 reg=corrReg[reg]
                                 
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
                             
                             else:
                                 if cou in refADM1.keys() and reg in refADM1[cou]:
                                     TEST_counts[cou][1]+=1
                                 
                                 if cou in refADM2.keys() and reg in refADM2[cou]:
                                     TEST_counts[cou][2]+=1
             
             elif len(loc)==2:
                 cou=loc[1].strip()
                 
                 if cou in corrCou.keys():
                     cou=corrCou[cou]
                     
                     if cou in refADM1.keys() or cou in refADM2.keys():
                         if cou not in TEST_counts:
                             TEST_counts[cou]=[1,0,0,1]
                         
                         else:
                             TEST_counts[cou][0]+=1
                             TEST_counts[cou][3]+=1
                 
                 else:
                     if cou in refADM1.keys() or cou in refADM2.keys():
                         if cou not in TEST_counts:
                             TEST_counts[cou]=[1,0,0,1]
                         
                         else:
                             TEST_counts[cou][0]+=1
                             TEST_counts[cou][3]+=1
     
     return TEST_counts


def buildOutputCounts(outName, testCounts):
     outFile=open(outName, 'w', encoding = 'UTF-8')
     outHeader='Country\tADM0\tADM1\tADM2\tRegNA\n'
     outFile.write(outHeader)

     for c in sorted(testCounts.keys()):
         num_ADM0=str(testCounts[c][0])
         num_ADM1=str(testCounts[c][1])
         num_ADM2=str(testCounts[c][2])
         num_RegNA=str(testCounts[c][3])
         
         outLine=c+'\t'+num_ADM0+'\t'+num_ADM1+'\t'+num_ADM2+'\t'+num_RegNA+'\n'
         outFile.write(outLine)

     outFile.close()


def main():
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-r1', '--refADM1_file', help='Reference file in .txt format. Geographic data evaluated at ADM1 level.')
     parser.add_argument('-r2', '--refADM2_file', help='Reference file in .txt format. Geographic data evaluated at ADM2 level.')
     parser.add_argument('-l', '--limit', type=int, help='Maximum acceptable value for the Levenshtein distance between compared strings.')
     parser.add_argument('-cc', '--corrCou_file', help='Correction file for country names in .txt format.')
     parser.add_argument('-cr', '--corrReg_file', help='Correction file for regions names in .txt format.')
     parser.add_argument('-t', '--test_file', help='Test file in .tsv format. Metadata table from which information about sampe location (country and region names) is extracted.')
     parser.add_argument('-db', '--database', choices=['GISAID', 'Nextstrain'], help='Name of the database from which the test file is dowloaded. Allowed values: GISAID, Nextstrain.')
     parser.add_argument('-o', '--out_name', help='Name of the output file. Output must be in .txt format.')
     
     inputs=parser.parse_args()
     
     REF_Adm1=read_REF(inputs.refADM1_file, inputs.limit)
     REF_Adm2=read_REF(inputs.refADM2_file, inputs.limit)
     
     CORR_Cou=read_CORR(inputs.corrCou_file)
     CORR_Reg=read_CORR(inputs.corrReg_file)
     
     TEST_Counts=matchCounting(inputs.test_file, inputs.database, REF_Adm1, REF_Adm2, CORR_Cou, CORR_Reg)
     
     buildOutputCounts(inputs.out_name, TEST_Counts)


if __name__=='__main__':
     main()
