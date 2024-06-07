def read_COUNTRY(countryFile):
     ref_Country=[]
     
     with open(countryFile) as COU_Table:
         for line in COU_Table:
             l=line.replace('\n','').split('_')
             country=l[0]
             
             if country not in ref_Country:
                 ref_Country.append(country)
     
     return ref_Country


def update_inAvTab(inAvFile, selPat, refCountryList=None):
     outFile=open('inTab_avCheck.txt', 'w', encoding = 'UTF-8')
     
     with open(inAvFile, encoding = 'UTF-8') as AVA_Table:
         header=AVA_Table.readline().replace('\n','')+'\tVar\n'
         
         outFile.write(header)
     
         if selPat=='SARS-CoV-2':
             for line in AVA_Table:
                 country=line.replace('\n', '').split('\t')[0]
                 
                 if country in refCountryList:
                     outLine=line.replace('\n', '')+('\tEpiweek.Var.%s.csv\n'%country)
                     
                     outFile.write(outLine)
                 
                 else:
                     outLine=line.replace('\n', '')+'\tNA\n'
                     
                     outFile.write(outLine)
         
         else:
             for line in AVA_Table:
                 outLine=line.replace('\n', '')+'\tNA\n'
                 
                 outFile.write(outLine)
     
     outFile.close()


def main():
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-p', '--pathogen', help='Name of the pathogen of interest.')
     parser.add_argument('-rc', '--refCountry_file', required=False, help='Reference file in .txt format. Complete list of countries for which counts tables are generaed.')
     parser.add_argument('-iav', '--inAvail_file', help='Input availability file in .txt format. Configuration table that lists which input tables are available for each analysed country')
     
     inputs=parser.parse_args()
     
     if inputs.pathogen=='SARS-CoV-2':
         REF_Country=read_COUNTRY(inputs.refCountry_file)
         
         update_inAvTab(inputs.inAvail_file, inputs.pathogen, REF_Country)
     
     else:
         update_inAvTab(inputs.inAvail_file, inputs.pathogen)


if __name__=='__main__':
     main()
