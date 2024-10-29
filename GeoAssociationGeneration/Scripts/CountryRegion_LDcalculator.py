def read_REF(refFile):
     REF_countries={}
     REF_regions={}
     
     with open(refFile, encoding = 'UTF-8') as REF_table:
         REF_table.readline()
         
         for line in REF_table:
             location=line.replace('\n','').split('\t')
             country=location[0].strip()
             region=location[2].strip()
             
             if ',' in country:
                 countryElements=country.split(',')
                 cEl_1=countryElements[0].strip()
                 cEl_2=countryElements[1].strip()
                 country=cEl_2+' '+cEl_1
                 
                 string=country.replace('-','').replace('\'','').replace(' ','').lower()
                 i=0
                 
                 while i<=(len(string)-3):
                     triplet=string[i:i+3]
                     
                     if triplet in REF_countries.keys() and country not in REF_countries[triplet]:
                         REF_countries[triplet].append(country)
                     
                     elif triplet not in REF_countries.keys():
                         REF_countries[triplet]=[country]
                     
                     i+=1
                 
                 if country in REF_regions.keys():
                     string=region.replace('-','').replace('\'','').replace(' ','').lower()
                     i=0
                     
                     while i<=(len(string)-3):
                         triplet=string[i:i+3]
                         
                         if triplet in REF_regions[country].keys() and region not in REF_regions[country][triplet]:
                             REF_regions[country][triplet].append(region)
                         
                         elif triplet not in REF_regions[country].keys():
                             REF_regions[country][triplet]=[region]
                         
                         i+=1
                 
                 elif country not in REF_regions.keys():
                     REF_regions[country]={}
                     
                     string=region.replace('-','').replace('\'','').replace(' ','').lower()
                     i=0
                     
                     while i<=(len(string)-3):
                         triplet=string[i:i+3]
                         
                         REF_regions[country][triplet]=[region]
                         
                         i+=1
             
             else:
                 string=country.replace('-','').replace('\'','').replace(' ','').lower()
                 i=0
                 
                 while i<=(len(string)-3):
                     triplet=string[i:i+3]
                     
                     if triplet in REF_countries.keys() and country not in REF_countries[triplet]:
                         REF_countries[triplet].append(country)
                     
                     elif triplet not in REF_countries.keys():
                         REF_countries[triplet]=[country]
                     
                     i+=1
                 
                 if country in REF_regions.keys():
                     string=region.replace('-','').replace('\'','').replace(' ','').lower()
                     i=0
                     
                     while i<=(len(string)-3):
                         triplet=string[i:i+3]
                         
                         if triplet in REF_regions[country].keys() and region not in REF_regions[country][triplet]:
                             REF_regions[country][triplet].append(region)
                         
                         elif triplet not in REF_regions[country].keys():
                             REF_regions[country][triplet]=[region]
                         
                         i+=1
                 
                 elif country not in REF_regions.keys():
                     REF_regions[country]={}
                     
                     string=region.replace('-','').replace('\'','').replace(' ','').lower()
                     i=0
                     
                     while i<=(len(string)-3):
                         triplet=string[i:i+3]
                         
                         REF_regions[country][triplet]=[region]
                         
                         i+=1
     
     return REF_countries, REF_regions


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


def read_TEST(testFile, dbName, corrCou, corrReg): 
     TEST_countries=[]
     TEST_regions={}
     
     with open(testFile, encoding = 'UTF-8') as TEST_table:
         TEST_table.readline()
         
         for line in TEST_table:
             if dbName=='GISAID':
                 location=line.replace('\n','').split('\t')[6].split('/')
             
             elif dbName=='Nextstrain':
                 location=line.replace('\n','').split('\t')[4:7]
             
             if len(location)>2:
                 country=location[1].strip()
                 
                 if country in corrCou.keys():
                     country=corrCou[country]
                     
                     if country not in TEST_countries:
                         TEST_countries.append(country)
                     
                     region=location[2].strip()
                     
                     if region in corrReg.keys():
                         region=corrReg[region]
                         
                         if country in TEST_regions.keys() and region not in TEST_regions[country]:
                             TEST_regions[country].append(region)
                         
                         elif country not in TEST_regions.keys():
                             TEST_regions[country]=[region]
                     
                     else:
                         if country in TEST_regions.keys() and region not in TEST_regions[country]:
                             TEST_regions[country].append(region)
                         
                         elif country not in TEST_regions.keys():
                             TEST_regions[country]=[region]
                     
                 else:
                     if country not in TEST_countries:
                         TEST_countries.append(country)
                     
                     region=location[2].strip()
                     
                     if region in corrReg.keys():
                         region=corrReg[region]
                         
                         if country in TEST_regions.keys() and region not in TEST_regions[country]:
                             TEST_regions[country].append(region)
                         
                         elif country not in TEST_regions.keys():
                             TEST_regions[country]=[region]
                     
                     else:
                         if country in TEST_regions.keys() and region not in TEST_regions[country]:
                             TEST_regions[country].append(region)
                         
                         elif country not in TEST_regions.keys():
                             TEST_regions[country]=[region]
             
             elif len(location)==2:
                 country=location[1].strip()
                 
                 if country in corrCou.keys():
                     country=corrCou[country]
                     
                     if country not in TEST_countries:
                         TEST_countries.append(country)
                 
                 else:
                     if country not in TEST_countries:
                         TEST_countries.append(country)
     
     return TEST_countries, TEST_regions


def levenshtein_distance(s, t):
     m = len(s)
     n = len(t)
     d = [[0] * (n + 1) for i in range(m + 1)]
     
     for i in range(1, m + 1):
         d[i][0] = i
     
     for j in range(1, n + 1):
         d[0][j] = j
    
     for j in range(1, n + 1):
         for i in range(1, m + 1):
             if s[i - 1] == t[j - 1]:
                 cost = 0
             else:
                 cost = 1
             
             d[i][j] = min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost)

     return d[m][n]


def matchGeographic(testCountries, refCountries, testRegions, refRegions, outFile, admMode):
     noMatch=open('%s_NoMatch.txt'%admMode, 'w', encoding = 'UTF-8')
     
     for testC in testCountries:
         testC_String=testC.replace('-','').replace('\'','').replace(' ','').lower()
         refC_toTest=[]
         
         for refTripletC in refCountries.keys():
             if refTripletC in testC_String:
                 refC_toTest+=refCountries[refTripletC]
         
         refC_toTest=list(set(refC_toTest))
         
         if len(refC_toTest)>=1:
             refMatchC='placeholder'
             refC_LD=1000
             
             for refC in refC_toTest:
                 refC_String=refC.replace('-','').replace('\'','').replace(' ','').lower()
                 testC_LD=levenshtein_distance(testC_String, refC_String)
                 
                 if testC_LD<refC_LD:
                     refMatchC=refC
                     refC_LD=testC_LD
             
             outLine=refMatchC+'\t'+testC+'\t'+str(refC_LD)+'\t'
             
             if testC in testRegions.keys():
                 for testR in testRegions[testC]:
                     testR_String=testR.replace('-','').replace('\'','').replace(' ','').lower()
                     refR_toTest=[]
                     
                     for refTripletR in refRegions[refMatchC].keys():
                         if refTripletR in testR_String:
                             refR_toTest+=refRegions[refMatchC][refTripletR]
                     
                     refR_toTest=list(set(refR_toTest))
                     
                     if len(refR_toTest)>=1:
                         refMatchR='placeholder'
                         refR_LD=1000
                         
                         for refR in refR_toTest:
                             refR_String=refR.replace('-','').replace('\'','').replace(' ','').lower()
                             testR_LD=levenshtein_distance(testR_String, refR_String)
                             
                             if testR_LD<refR_LD:
                                 refMatchR=refR
                                 refR_LD=testR_LD
                         
                         outLineWrite=outLine+refMatchR+'\t'+testR+'\t'+str(refR_LD)+'\n'
                         outFile.write(outLineWrite)
                     
                     else:
                         noMatch.write('Region: '+testR+' ('+testC+')\n')
             
             else:
                 outLineWrite=outLine+'NA\tNA\tNA\n'
                 outFile.write(outLineWrite)
         
         else:
             noMatch.write('Country: '+testC+'\n')
     
     noMatch.close()


def main():
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-r', '--ref_file', help='Reference file in .txt format.')
     parser.add_argument('-cc', '--corrCou_file', help='Correction file for country names in .txt format.')
     parser.add_argument('-cr', '--corrReg_file', help='Correction file for regions names in .txt format.')
     parser.add_argument('-t', '--test_file', help='Test file in .tsv format. Metadata table from which information about sampe location (country and region names) is extracted.')
     parser.add_argument('-db', '--database', choices=['GISAID', 'Nextstrain'], help='Name of the database from which the test file is dowloaded. Allowed values: GISAID, Nextstrain.')
     parser.add_argument('-o', '--out_name', help='Name of the output file. Output must be in .txt format.')
     parser.add_argument('-am', '--adm_mode', help='The ADM mode of the reference file.')

     
     inputs=parser.parse_args()
     
     REF_Dict=read_REF(inputs.ref_file)
     REF_CountryTab=REF_Dict[0]
     REF_RegionTab=REF_Dict[1]
     
     CORR_Cou=read_CORR(inputs.corrCou_file)
     CORR_Reg=read_CORR(inputs.corrReg_file)
     
     TEST_Dict=read_TEST(inputs.test_file, inputs.database, CORR_Cou, CORR_Reg)
     TEST_CountryTab=TEST_Dict[0]
     TEST_RegionTab=TEST_Dict[1]
     
     OUT_File=open(inputs.out_name, 'w', encoding = 'UTF-8')
     OUT_Header='REF_Country\tTEST_Country\tLD_Country\tREF_Region\tTEST_Region\tLD_Region\n'
     OUT_File.write(OUT_Header)
     
     matchGeographic(TEST_CountryTab, REF_CountryTab, TEST_RegionTab, REF_RegionTab, OUT_File, inputs.adm_mode)
     
     OUT_File.close()


if __name__=='__main__':
     main()
