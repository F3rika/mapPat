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
                 
                 else:
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
                 
                 else:
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


def read_TEST(testFile, dbName): 
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
                 
                 if country not in TEST_countries:
                     TEST_countries.append(country)
                     
                 region=location[2].strip()
                 
                 if country in TEST_regions.keys() and region not in TEST_regions[country]:
                     TEST_regions[country].append(region)
                     
                 elif country not in TEST_regions.keys():
                     TEST_regions[country]=[region]
             
             elif len(location)==2:
                 country=location[1].strip()
                 
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


def matchGeographics(testCountries, refCountries, testRegions, refRegions, corrCountries, corrRegions, lim):
     MATCH_countries={}
     MATCH_regions={}
     
     for testC in testCountries:
         if testC in corrCountries.keys():
             testC_corr=corrCountries[testC]
             
             testC_String=testC_corr.replace('-','').replace('\'','').replace(' ','').lower()
             refC_toTest=[]
             
             for refTripletC in refCountries.keys():
                 if refTripletC in testC_String:
                     refC_toTest+=refCountries[refTripletC]
             
             refC_toTest=list(set(refC_toTest))
             
             if len(refC_toTest)>=1:
                 refMatchC='placeholder'
                 refC_LD=1000000
                 
                 for refC in refC_toTest:
                     refC_String=refC.replace('-','').replace('\'','').replace(' ','').lower()
                     testC_LD=levenshtein_distance(testC_String, refC_String)
                     
                     if testC_LD<refC_LD:
                         refMatchC=refC
                         refC_LD=testC_LD
                 
                 if refC_LD<=lim:
                     if refMatchC in MATCH_countries.keys() and testC not in MATCH_countries[refMatchC]:
                         MATCH_countries[refMatchC].append(testC)
                     
                     elif refMatchC not in MATCH_countries.keys():
                         MATCH_countries[refMatchC]=[testC]
                     
                     if testC in testRegions.keys():
                         for testR in testRegions[testC]:
                             if testR in corrRegions.keys():
                                 testR_corr=corrRegions[testR]
                                 
                                 testR_String=testR_corr.replace('-','').replace('\'','').replace(' ','').lower()
                                 refR_toTest=[]
                                 
                                 for refTripletR in refRegions[refMatchC].keys():
                                     if refTripletR in testR_String:
                                         refR_toTest+=refRegions[refMatchC][refTripletR]
                                 
                                 refR_toTest=list(set(refR_toTest))
                                 
                                 if len(refR_toTest)>=1:
                                     refMatchR='placeholder'
                                     refR_LD=1000000
                                     
                                     for refR in refR_toTest:
                                         refR_String=refR.replace('-','').replace('\'','').replace(' ','').lower()
                                         testR_LD=levenshtein_distance(testR_String, refR_String)
                                         
                                         if testR_LD<refR_LD:
                                             refMatchR=refR
                                             refR_LD=testR_LD
                                     
                                     if refR_LD<=lim:
                                         if refMatchC in MATCH_regions.keys():
                                             if refMatchR in MATCH_regions[refMatchC].keys() and testR not in MATCH_regions[refMatchC][refMatchR]:
                                                 MATCH_regions[refMatchC][refMatchR].append(testR)
                                             
                                             elif refMatchR not in MATCH_regions[refMatchC].keys():
                                                 MATCH_regions[refMatchC][refMatchR]=[testR]
                                         
                                         else:
                                             MATCH_regions[refMatchC]={}
                                             MATCH_regions[refMatchC][refMatchR]=[testR]
                             
                             else:
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
                                     
                                     if refR_LD<=lim:
                                         if refMatchC in MATCH_regions.keys():
                                             if refMatchR in MATCH_regions[refMatchC].keys() and testR not in MATCH_regions[refMatchC][refMatchR]:
                                                 MATCH_regions[refMatchC][refMatchR].append(testR)
                                             
                                             elif refMatchR not in MATCH_regions[refMatchC].keys():
                                                 MATCH_regions[refMatchC][refMatchR]=[testR]
                                         
                                         else:
                                             MATCH_regions[refMatchC]={}
                                             MATCH_regions[refMatchC][refMatchR]=[testR]
         
         else:
             testC_String=testC.replace('-','').replace('\'','').replace(' ','').lower()
             refC_toTest=[]
             
             for refTripletC in refCountries.keys():
                 if refTripletC in testC_String:
                     refC_toTest+=refCountries[refTripletC]
             
             refC_toTest=list(set(refC_toTest))
             
             if len(refC_toTest)>=1:
                 refMatchC='placeholder'
                 refC_LD=1000000
                 
                 for refC in refC_toTest:
                     refC_String=refC.replace('-','').replace('\'','').replace(' ','').lower()
                     testC_LD=levenshtein_distance(testC_String, refC_String)
                     
                     if testC_LD<refC_LD:
                         refMatchC=refC
                         refC_LD=testC_LD
                 
                 if refC_LD<=lim:
                     if refMatchC in MATCH_countries.keys() and testC not in MATCH_countries[refMatchC]:
                         MATCH_countries[refMatchC].append(testC)
                     
                     elif refMatchC not in MATCH_countries.keys():
                         MATCH_countries[refMatchC]=[testC]
                     
                     if testC in testRegions.keys():
                         for testR in testRegions[testC]:
                             if testR in corrRegions.keys():
                                 testR_corr=corrRegions[testR]
                                 
                                 testR_String=testR_corr.replace('-','').replace('\'','').replace(' ','').lower()
                                 refR_toTest=[]
                                 
                                 for refTripletR in refRegions[refMatchC].keys():
                                     if refTripletR in testR_String:
                                         refR_toTest+=refRegions[refMatchC][refTripletR]
                                 
                                 refR_toTest=list(set(refR_toTest))
                                 
                                 if len(refR_toTest)>=1:
                                     refMatchR='placeholder'
                                     refR_LD=1000000
                                     
                                     for refR in refR_toTest:
                                         refR_String=refR.replace('-','').replace('\'','').replace(' ','').lower()
                                         testR_LD=levenshtein_distance(testR_String, refR_String)
                                         
                                         if testR_LD<refR_LD:
                                             refMatchR=refR
                                             refR_LD=testR_LD
                                     
                                     if refR_LD<=lim:
                                         if refMatchC in MATCH_regions.keys():
                                             if refMatchR in MATCH_regions[refMatchC].keys() and testR not in MATCH_regions[refMatchC][refMatchR]:
                                                 MATCH_regions[refMatchC][refMatchR].append(testR)
                                             
                                             elif refMatchR not in MATCH_regions[refMatchC].keys():
                                                 MATCH_regions[refMatchC][refMatchR]=[testR]
                                         
                                         else:
                                             MATCH_regions[refMatchC]={}
                                             MATCH_regions[refMatchC][refMatchR]=[testR]
                             
                             else:
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
                                     
                                     if refR_LD<=lim:
                                         if refMatchC in MATCH_regions.keys():
                                             if refMatchR in MATCH_regions[refMatchC].keys() and testR not in MATCH_regions[refMatchC][refMatchR]:
                                                 MATCH_regions[refMatchC][refMatchR].append(testR)
                                             
                                             elif refMatchR not in MATCH_regions[refMatchC].keys():
                                                 MATCH_regions[refMatchC][refMatchR]=[testR]
                                         
                                         else:
                                             MATCH_regions[refMatchC]={}
                                             MATCH_regions[refMatchC][refMatchR]=[testR]
     
     return MATCH_countries, MATCH_regions


def buildOutputTable(outName, refFile, matchCountries, matchRegions):
     outFile=open(outName, 'w', encoding = 'UTF-8')
     outHeader='Country_REF\tCountry_ISO\tCountry_ALT\tRegion_REF\tRegion_ALT\n'
     
     outFile.write(outHeader)
     
     with open(refFile,  encoding = 'UTF-8') as REF_table:
         REF_table.readline()
         
         for line in REF_table:
             info=line.replace('\n','').split('\t')
             country=info[0]
             iso=info[1]
             region=info[2]
             
             if ',' in country:
                 countryElements=country.split(',')
                 cEl_1=countryElements[0].strip()
                 cEl_2=countryElements[1].strip()
                 country=cEl_2+' '+cEl_1
                 
                 if country in matchCountries.keys():
                     matchC=','.join(matchCountries[country])
                     outLine=country+'\t'+iso+'\t'+matchC
                     
                     if country in matchRegions.keys():
                         if region in matchRegions[country].keys():
                             matchR=','.join(matchRegions[country][region])
                             outLine=outLine+'\t'+region+'\t'+matchR+'\n'
                             
                             outFile.write(outLine)
                         
                         else:
                             outLine=outLine+'\t'+region+'\tNA\n'
                             
                             outFile.write(outLine)
                     else:
                         outLine=outLine+'\t'+region+'\tNA\n'
                             
                         outFile.write(outLine)
                 
                 else:
                     outLine=country+'\t'+iso+'\tNA\t'+region+'\tNA\n'
                     
                     outFile.write(outLine)
             
             else:
                 if country in matchCountries.keys():
                     matchC=','.join(matchCountries[country])
                     outLine=country+'\t'+iso+'\t'+matchC
                     
                     if country in matchRegions.keys():
                         if region in matchRegions[country].keys():
                             matchR=','.join(matchRegions[country][region])
                             outLine=outLine+'\t'+region+'\t'+matchR+'\n'
                             
                             outFile.write(outLine)
                         
                         else:
                             outLine=outLine+'\t'+region+'\tNA\n'
                             
                             outFile.write(outLine)
                     else:
                         outLine=outLine+'\t'+region+'\tNA\n'
                             
                         outFile.write(outLine)
                 
                 else:
                     outLine=country+'\t'+iso+'\tNA\t'+region+'\tNA\n'
                     
                     outFile.write(outLine)
     
     outFile.close()


def main():
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-r', '--ref_file', help='Reference file in .txt format.')
     parser.add_argument('-cc', '--corrCou_file', help='Correction file for country names in .txt format.')
     parser.add_argument('-cr', '--corrReg_file', help='Correction file for regions names in .txt format.')
     parser.add_argument('-t', '--test_file', help='Test file in .tsv format. Metadata table from which information about sampe location (country and region names) is extracted.')
     parser.add_argument('-db', '--database', choices=['GISAID', 'Nextstrain'], help='Name of the database from which the test file is dowloaded. Allowed values: GISAID, Nextstrain.')
     parser.add_argument('-l', '--limit', type=int, help='Maximum acceptable value for the Levenshtein distance between compared strings.')
     parser.add_argument('-o', '--out_name', help='Name of the output file. Output must be in .txt format.')
     
     inputs=parser.parse_args()
     
     REF_All=read_REF(inputs.ref_file)
     REF_Cou=REF_All[0]
     REF_Reg=REF_All[1]
     
     CORR_Cou=read_CORR(inputs.corrCou_file)
     CORR_Reg=read_CORR(inputs.corrReg_file)
     
     TEST_All=read_TEST(inputs.test_file, inputs.database)
     TEST_Cou=TEST_All[0]
     TEST_Reg=TEST_All[1]
     
     MATCH_All=matchGeographics(TEST_Cou, REF_Cou, TEST_Reg, REF_Reg, CORR_Cou, CORR_Reg, inputs.limit)
     MATCH_Cou=MATCH_All[0]
     MATCH_Reg=MATCH_All[1]
     
     buildOutputTable(inputs.out_name, inputs.ref_file, MATCH_Cou, MATCH_Reg)

if __name__=='__main__':
     main()
