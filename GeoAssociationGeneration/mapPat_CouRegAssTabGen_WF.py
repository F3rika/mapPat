def build_ISO_ADMList(assFile, admMode):
     outFile=open('ISO_%sList.txt'%admMode, 'w', encoding = 'UTF-8')
     
     with open(assFile, encoding = 'UTF-8') as ASS_table:
         ASS_table.readline()
         
         for line in ASS_table:
             l=line.replace('\n', '').split('\t')
             isoCode=l[1]
             admLvl=l[2]
             
             if admLvl==admMode:
                 outLine=isoCode+'\n'
                 outFile.write(outLine)
     
     outFile.close()


def main():
     import argparse
     from os import system as command
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-i', '--input_file', help='Input file. Metadata dowloaded from either GISAID or Nexstrain.')
     parser.add_argument('-pc', '--path_config', default='./Config', help='Path to the "Config" folder which collects required configuration files.')
     parser.add_argument('-ps', '--path_scripts', default='./Scripts', help='Path to the "Scripts" folder which collects required scripts.')
     parser.add_argument('-db', '--database', choices=['GISAID', 'Nextstrain'], help='Name of the database from which metadata are dowloaded. Allowed values: GISAID, Nextstrain.')
     
     inputs=parser.parse_args()
     
     command('mkdir CouRegAssTab_InterOut')
     
     filesToCopy=['ADM0.txt', 'ADM1.txt', 'ADM2.txt', 'countriesNamesCorrectionTable.txt', 'regionsNamesCorrectionTable.txt']
     
     for fileName in filesToCopy:
         command('cp %s/%s .'%(inputs.path_config, fileName))
     
     command('python3 %s/CountryRegion_LDcalculator.py -r ADM1.txt -cc countriesNamesCorrectionTable.txt -cr regionsNamesCorrectionTable.txt -t %s -db %s -am ADM1 -o ADM1_CountryRegion_LDTab.txt'%(inputs.path_scripts, inputs.input_file, inputs.database))
     
     command('python3 %s/CountryRegion_LDcalculator.py -r ADM2.txt -cc countriesNamesCorrectionTable.txt -cr regionsNamesCorrectionTable.txt -t %s -db %s -am ADM2 -o ADM2_CountryRegion_LDTab.txt'%(inputs.path_scripts, inputs.input_file, inputs.database))
     
     command('python3 %s/DataADM_distrCalculator.py -r1 ADM1_CountryRegion_LDTab.txt -r2 ADM2_CountryRegion_LDTab.txt -l 3 -cc countriesNamesCorrectionTable.txt -cr regionsNamesCorrectionTable.txt -t %s -db %s -o DataADM_DistrTab.txt'%(inputs.path_scripts, inputs.input_file, inputs.database))
     
     command('Rscript --vanilla %s/CountryADM_AssocTabBuilder.R DataADM_DistrTab.txt'%(inputs.path_scripts))

     command('python3 %s/CountryISOADM_AssocTabBuilder.py -ri ADM0.txt -ra CountryADM_AssocTab.txt -o CountryISOADM_AssocTab.txt'%(inputs.path_scripts))

     command('python3 %s/CountryRegion_AssocTabBuilder.py -r ADM1.txt -cc countriesNamesCorrectionTable.txt -cr regionsNamesCorrectionTable.txt -t %s -db %s -l 3 -o ADM1_CountryRegion_AssocTab.txt'%(inputs.path_scripts, inputs.input_file, inputs.database))

     command('python3 %s/CountryRegion_AssocTabBuilder.py -r ADM2.txt -cc countriesNamesCorrectionTable.txt -cr regionsNamesCorrectionTable.txt -t %s -db %s -l 3 -o ADM2_CountryRegion_AssocTab.txt'%(inputs.path_scripts, inputs.input_file, inputs.database))

     build_ISO_ADMList('CountryISOADM_AssocTab.txt', 'ADM1')
     
     build_ISO_ADMList('CountryISOADM_AssocTab.txt', 'ADM2')

     command('python3 %s/CountryRegion_AssocTabMerger.py -rc1 ISO_ADM1List.txt -rc2 ISO_ADM2List.txt -cr1 ADM1_CountryRegion_AssocTab.txt -cr2 ADM2_CountryRegion_AssocTab.txt -o allADM_CountryRegion_AssocTab.txt'%(inputs.path_scripts))
     
     command('rm ADM0.txt ADM1.txt ADM2.txt countriesNamesCorrectionTable.txt regionsNamesCorrectionTable.txt')
     
     command('mv -t ./CouRegAssTab_InterOut ADM1_CountryRegion_LDTab.txt ADM1_NoMatch.txt ADM2_CountryRegion_LDTab.txt ADM2_NoMatch.txt DataADM_DistrTab.txt CountryADM_AssocTab.txt CountryISOADM_AssocTab.txt ADM1_CountryRegion_AssocTab.txt ADM2_CountryRegion_AssocTab.txt ISO_ADM1List.txt ISO_ADM2List.txt')
     
     command('tar -czvf CouRegAssTab_InterOut.tar.gz ./CouRegAssTab_InterOut --remove-files')

if __name__=='__main__':
     main()
