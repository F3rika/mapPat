def read_VAR(varFile):
     ref_LinVar={}
     ref_pScore={}
     
     with open(varFile) as VAR_table:
         VAR_table.readline()
         
         for line in VAR_table:
             l=line.replace('\n','').split('\t')
             lin=l[0]
             info='\t'.join(l[1:3])
             pScore=int(l[3])
             
             if lin not in ref_LinVar.keys():
                 ref_LinVar[lin]=info
             
             if lin not in ref_pScore.keys():
                 ref_pScore[lin]=pScore
     
     return ref_LinVar, ref_pScore


def read_VBM(VbmFile):
     ref_LinVBM=[]
     
     with open(VbmFile) as VBM_Table:
         VBM_Table.readline()
         
         for line in VBM_Table:
             lin=line.replace('\n','')
             
             if lin not in ref_LinVBM:
                 ref_LinVBM.append(lin)
     
     return ref_LinVBM


def read_TEST(testFile):
     test_Lin=[]
     
     with open(testFile) as TEST_table:
         TEST_table.readline()
         
         for line in TEST_table:
             l=line.replace('\n', '').replace('*', '').split('\t')
             lin=l[13]
             
             if lin not in test_Lin:
                 test_Lin.append(lin)
     
     return test_Lin


def searchParentLin(testTag, refAlias, refPS):
     linTag=testTag
     parLin=''
     
     if linTag in refAlias.keys():
         hpLin=refAlias[linTag]
         
         hpList=[]
         
         if hpLin!='':
             if type(hpLin)==str:
                 toAdd_hpList=hpLin.replace('*', '')
                 hpList.append(toAdd_hpList)
             
             elif type(hpLin)==list:
                 for lin in hpLin:
                     toAdd_hpList=lin.replace('*', '')
                     hpList.append(toAdd_hpList)
             
             hpList=list(set(hpList))
             toRemove_hpList=[]
             idx=0
             
             while idx<len(hpList):
                 toTest_Lin=hpList[idx].replace('*','')
                 toTest_Tag=toTest_Lin.split('.')[0]
                 
                 if toTest_Tag in refAlias.keys():
                     toTest_hpLin=refAlias[toTest_Tag]
                     
                     if toTest_hpLin!='':
                         if type(toTest_hpLin)==str:
                             toTest_addLin=toTest_hpLin.replace('*','')
                             hpList.append(toTest_addLin)
                             
                             toRemove_hpList.append(toTest_Lin)
                             
                             idx+=1
                         
                         elif type(toTest_hpLin)==list:
                             toTest_hpLin=list(set(toTest_hpLin))
                             
                             for lin in toTest_hpLin:
                                 toTest_addLin=lin.replace('*','')
                                 hpList.append(toTest_addLin)
                             
                             toRemove_hpList.append(toTest_Lin)
                             
                             idx+=1
                     
                     else:
                         idx+=1
                 
                 else:
                     idx+=1
             
             hpList=list(set(hpList))
             toRemove_hpList=list(set(toRemove_hpList))
             
             for rem in toRemove_hpList:
                 hpList.remove(rem)
             
             hpLin_pScore=0
             
             for lin in hpList:
                 for rLin in refPS.keys():
                     if rLin in lin:
                         toTest_pScore=refPS[rLin]
                         
                         if toTest_pScore>hpLin_pScore:
                             parLin=rLin
                             hpLin_pScore=toTest_pScore
                     
                     else:
                         toTest_pScore=0.5
                         
                         if toTest_pScore>hpLin_pScore:
                             parLin=lin
                             hpLin_pScore=toTest_pScore
         
         else:
             parLin=linTag
     
     else:
         parLin=linTag
     
     return parLin


def buildVarConverter(testLin, refAlias, refPS, refVar, refVBM, outName):
     outFile=open(outName, 'w')
     outHeader='Lin\tVariant\tStatus\tisVBM\n'
     
     outFile.write(outHeader)
     
     for lin in testLin:
         testLin_break=lin.split('.')
         testLin_tag=testLin_break[0]
         testLin_ver='.'+'.'.join(testLin_break[1:len(testLin_break)])
         
         outLine=lin+'\t'
         
         testLin_par=searchParentLin(testLin_tag, refAlias, refPS)
         
         testLin_ext=testLin_par+testLin_ver
         
         testLin_var='None\tNone\t'
         
         for var in refVar.keys():
             if var in testLin_ext:
                 testLin_var=refVar[var]+'\t'
         
         outLine=outLine+testLin_var
         
         testLin_vbm='F\n'
         
         for vbm in refVBM:
             if vbm in lin:
                 testLin_vbm='T\n'
         
         outLine=outLine+testLin_vbm
         
         outFile.write(outLine)
     
     outFile.close()


def main():
     import json
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-rv', '--refVar_file', help='Reference file in .txt format. Lists lineages which at any point of their evolution were assigned a variant by WHO. Every lineage is associate with its variant name, status (VOC, VOI, VUM) and a priority score.')
     parser.add_argument('-rs', '--refStatVBM_file', help='Reference file in .txt format. Lists lineages that are currently classified as VBM by WHO.')
     parser.add_argument('-ra', '--refAlias_file', help='Reference file in .json format. Lists the aliases used for lineages designation and associates them with the corresponding parent lineage.')
     parser.add_argument('-t', '--test_file', help='Test file in .tsv format. Metadata table from which information about lineage designation of each sample is extracted.')
     parser.add_argument('-o', '--out_name', help='Name of the output file. Output must be in .txt format.')
     
     inputs=parser.parse_args()
     
     REF_VarAll=read_VAR(inputs.refVar_file)
     REF_Var=REF_VarAll[0]
     REF_PrioScore=REF_VarAll[1]
     
     REF_StatVBM=read_VBM(inputs.refStatVBM_file)
     
     REF_AliasFile=open(inputs.refAlias_file)
     REF_AliasDict=json.load(REF_AliasFile)
     
     TEST_Lin=read_TEST(inputs.test_file)
     
     buildVarConverter(TEST_Lin, REF_AliasDict, REF_PrioScore, REF_Var, REF_StatVBM, inputs.out_name)


if __name__=='__main__':
     main()
