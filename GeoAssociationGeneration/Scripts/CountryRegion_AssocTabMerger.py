def read_coutryADM(CouAdm1File, CouAdm2File):
     ref_countryADM={'ADM1':[], 'ADM2':[]}
     
     with open(CouAdm1File, encoding = 'UTF-8') as ADM1_Table:
         for line in ADM1_Table:
             ISO_ADM1=line.replace('\n', '')
             
             ref_countryADM['ADM1'].append(ISO_ADM1)
     
     with open(CouAdm2File, encoding = 'UTF-8') as ADM2_Table:
         for line in ADM2_Table:
             ISO_ADM2=line.replace('\n', '')
             
             ref_countryADM['ADM2'].append(ISO_ADM2)
     
     return ref_countryADM


def mergeTables(outName, refCouADM, CouRegAdm1File, CouRegAdm2File):
     outFile=open(outName, 'w', encoding = 'UTF-8')
     outHeader='Country_REF\tCountry_ISO\tCountry_ALT\tRegion_REF\tRegion_ALT\n'
     outFile.write(outHeader)
     
     with open(CouRegAdm1File, encoding = 'UTF-8') as couRegADM1_Table:
         couRegADM1_Table.readline()
         
         for line in couRegADM1_Table:
             l=line.replace('\n', '').split('\t')
             countryISO=l[1]
             
             if countryISO in refCouADM['ADM1']:
                 outFile.write(line)
     
     with open(CouRegAdm2File, encoding = 'UTF-8') as couRegADM2_Table:
         couRegADM2_Table.readline()
         
         for line in couRegADM2_Table:
             l=line.replace('\n', '').split('\t')
             countryISO=l[1]
             
             if countryISO in refCouADM['ADM2']:
                 outFile.write(line)
     
     outFile.close()


def main():
     import argparse
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-rc1', '--refCouADM1_file', help='Reference file in .txt format. Lists the ISO-3 codes of countries which have ADM1 as the preferred ADM level for choropleth maps design.')
     parser.add_argument('-rc2', '--refCouADM2_file', help='Reference file in .txt format. Lists the ISO-3 codes of countries which have ADM2 as the preferred ADM level for choropleth maps design.')
     parser.add_argument('-o', '--out_name', help='Name of the output file. Output must be in .txt format.')
     parser.add_argument('-cr1', '--CouRegADM1_file', help='File in .txt format. Associates countries, ISO-3 codes and regions evaluated at ADM1 level.')
     parser.add_argument('-cr2', '--CouRegADM2_file', help='File in .txt format. Associates countries, ISO-3 codes and regions evaluated at ADM2 level.')
     
     inputs=parser.parse_args()
     
     REF_CountryADM=read_coutryADM(inputs.refCouADM1_file, inputs.refCouADM2_file)
     
     mergeTables(inputs.out_name, REF_CountryADM, inputs.CouRegADM1_file, inputs.CouRegADM2_file)


if __name__=='__main__':
     main()
