def read_COUNTRY(countryFile):
     ref_Country=[]
     
     with open(countryFile) as COU_Table:
         for line in COU_Table:
             l=line.replace('\n','').split('_')
             country=l[0]
             
             if country not in ref_Country:
                 ref_Country.append(country)
     
     return ref_Country


def read_VAR(varFile):
     ref_Var={}
     
     ref_VBM=[]
     
     with open(varFile) as VAR_Table:
         VAR_Table.readline()
         
         for line in VAR_Table:
             l=line.replace('\n','').split('\t')
             lin=l[0]
             var=l[1]
             vbm=l[3]
             
             if lin not in ref_Var.keys():
                 ref_Var[lin]=var
                 
             if vbm=='T' and lin not in ref_VBM:
                 ref_VBM.append(lin)
     
     return ref_Var, ref_VBM


def buildVarCounts(linCounts, refCountry, refVariant, refVBM):
     varCounts={}
     outFile=open('Epiweek.Var.%s.csv'%refCountry, 'w')
     
     with open(linCounts) as COUNTS_Table:
         header=COUNTS_Table.readline()
         outFile.write(header)
         
         for line in COUNTS_Table:
             l=line.replace('\n','').split(' ')
             lin=l[0].replace('\"', '')
             counts=[int(x) for x in l[1:len(l)]]
             
             if lin in refVariant.keys():
                 var=refVariant[lin]
                 
                 if var in varCounts.keys():
                     varCounts[var]=[x+y for x,y in zip(varCounts[var], counts)]
                 
                 else:
                     varCounts[var]=counts
             
             if lin in refVBM:
                 cat='VBM'
                 
                 if cat in varCounts.keys():
                     varCounts[cat]=[x+y for x,y in zip(varCounts[cat], counts)]
                 
                 else:
                     varCounts[cat]=counts
     
     for v in varCounts.keys():
         countsStr=' '.join([str(x) for x in varCounts[v]])
         outLine=v+' '+countsStr+'\n'
         
         outFile.write(outLine)
     
     outFile.close()


def main():
     import argparse
          
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-rc', '--refCountry_file', help='Reference file in .txt format. Complete list of countries for which counts tables are generated.')
     parser.add_argument('-rv', '--refVar_file', help='Reference file in .txt format. Associates each lineage to the corresponding variant classification as per WHO guidelines.')
     
     inputs=parser.parse_args()
     
     REF_Country=read_COUNTRY(inputs.refCountry_file)
     
     REF_varAll=read_VAR(inputs.refVar_file)
     REF_Var=REF_varAll[0]
     REF_Vbm=REF_varAll[1]
     
     for c in REF_Country:
         REF_linCounts='Epiweek.%s.csv'%c
         
         buildVarCounts(REF_linCounts, c, REF_Var, REF_Vbm)


if __name__=='__main__':
     main()
