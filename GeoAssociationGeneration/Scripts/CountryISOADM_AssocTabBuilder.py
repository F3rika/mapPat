def read_ISO(IsoFile):
     ref_ISO={}
     
     with open(IsoFile, encoding = 'UTF-8') as ISO_Table:
         ISO_Table.readline()
         
         for line in ISO_Table:
             l=line.replace('\n','').split('\t')
             country=l[0]
             iso=l[1]
             
             if ',' in country:
                 countryElements=country.split(',')
                 cEl_1=countryElements[0].strip()
                 cEl_2=countryElements[1].strip()
                 country=cEl_2+' '+cEl_1
                 
                 if country not in ref_ISO.keys():
                     ref_ISO[country]=iso
             
             else:
                 if country not in ref_ISO.keys():
                     ref_ISO[country]=iso
     
     return ref_ISO


def read_ADM(AdmFile):
     ref_ADM={}
     
     with open(AdmFile, encoding = 'UTF-8') as ADM_Table:
         ADM_Table.readline()
         
         for line in ADM_Table:
             l=line.replace('\n','').split('\t')
             country=l[0]
             adm=l[1]
             
             if country not in ref_ADM.keys():
                 ref_ADM[country]=adm
     
     return ref_ADM


def buildOutputISOADM(outName, refISO, refADM):
     outFile=open(outName, 'w', encoding = 'UTF-8')
     outHeader='Country\tCountry_ISO\tCountry_ADM\n'
     
     outFile.write(outHeader)
     
     for country in refISO.keys():
         if country in refADM.keys():
             outLine=country+'\t'+refISO[country]+'\t'+refADM[country]+'\n'
             outFile.write(outLine)
     
     outFile.close()


def main():
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-ri', '--refISO_file', help='Reference file in .txt format. Associates countries to the corresponding ISO-3 code.')
     parser.add_argument('-ra', '--refADM_file', help='Reference file in .txt format. Associates countries to the best ADM level for choropleth maps design.')
     parser.add_argument('-o', '--out_name', help='Name of the output file. Output must be in .txt format.')
     
     inputs=parser.parse_args()
     
     REF_Iso=read_ISO(inputs.refISO_file)
     
     REF_Adm=read_ADM(inputs.refADM_file)
     
     buildOutputISOADM(inputs.out_name, REF_Iso, REF_Adm)


if __name__=='__main__':
     main()
