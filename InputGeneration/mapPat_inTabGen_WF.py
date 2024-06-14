def build_coutriesListTracker(inAvFile):
     outFile=open('countriesListTracker.txt', 'w')
     
     addCountries=[]
     
     with open(inAvFile, encoding = 'UTF-8') as AVA_Table:
         AVA_Table.readline()
         
         for line in AVA_Table:
             l=line.replace('\n', '').split('\t')
             country=l[0]
             
             if l.count('NA')<4 and country not in addCountries:
                 addCountries.append(country)
                 
                 outLine=country+'\n'
                 outFile.write(outLine)
     
     outFile.close()


def main():
     import argparse
     from os import system as command
     
     parser=argparse.ArgumentParser()
     
     parser.add_argument('-i', '--input_file', help='Input file. Metadata dowloaded from either GISAID or Nexstrain.')
     parser.add_argument('-pc', '--path_config', default='./Config', help='Path to the "Config" folder which collects required configuration files.')
     parser.add_argument('-ps', '--path_scripts', default='./Scripts', help='Path to the "Scripts" folder which collects required scripts.')
     parser.add_argument('-s', '--seq', required=False, help='Sequencing file in FASTA format.')
     parser.add_argument('-rs', '--refSeq', required=False, help='Reference sequence in FASTA format.')
     parser.add_argument('-p', '--pathogen', help='Name of the pathogen of interest.')
     parser.add_argument('-db', '--database', choices=['GISAID', 'Nextstrain'], help='Name of the database from which metadata are dowloaded. Allowed values: GISAID, Nextstrain.')
     parser.add_argument('-o', '--output_file', help='Basename of the compressed file where mapPat outputs are stored.')
     
     inputs=parser.parse_args()
     
     if inputs.database=='GISAID' and inputs.pathogen=='SARS-CoV-2':
         command('mkdir %s_mapPatInterOut'%(inputs.output_file))
         command('mkdir %s_mapPatOut'%(inputs.output_file))

         filesToCopy=['LinVar_AssocTab.txt', 'LinVBM_AssocTab.txt', 'SARS-CoV-2_LineagesAliases.json', 'coutryToISO.txt', 'areaFile', 'NEW_allADM_CountryRegion_AssocTab.txt']

         for fileName in filesToCopy:
             command('cp %s/%s .'%(inputs.path_config, fileName))
         
         command('perl %s/GisaidToHaploCoV.pl --metadata %s --outfile SARS-CoV-2.HaploCoV'%(inputs.path_scripts, inputs.input_file))
         
         command('perl %s/countMlin.pl SARS-CoV-2.HaploCoV > linDefMut.csv'%(inputs.path_scripts))
         
         command('perl %s/BuildTables.pl SARS-CoV-2.HaploCoV SI'%(inputs.path_scripts))
         
         build_coutriesListTracker('inTab_avCheck_tmp.txt')
         
         command('python3 %s/LinVar_ConvTabBuilder.py -rv LinVar_AssocTab.txt -rs LinVBM_AssocTab.txt -ra SARS-CoV-2_LineagesAliases.json -t %s -o LinVar_ConvTabTracker.txt'%(inputs.path_scripts, inputs.input_file))
         
         command('python3 %s/Var_CountsTabBuilder.py -rc countriesListTracker.txt -rv LinVar_ConvTabTracker.txt'%(inputs.path_scripts))
         
         command('python3 %s/InAvailability_ConfigTabUpdater.py -iav inTab_avCheck_tmp.txt -rc countriesListTracker.txt -p SARS-CoV-2'%(inputs.path_scripts))

         command('rm LinVar_AssocTab.txt LinVBM_AssocTab.txt SARS-CoV-2_LineagesAliases.json coutryToISO.txt areaFile NEW_allADM_CountryRegion_AssocTab.txt')
         
         command('mv -t ./%s_mapPatInterOut linDefMut.csv SARS-CoV-2.HaploCoV inTab_avCheck_tmp.txt'%(inputs.output_file))
         command('mv -t ./%s_mapPatOut Epiweek.*.csv *_muts_perLin.csv HeatmapRegLin_*.csv Total_*_regions.csv countriesListTracker.txt inTab_avCheck.txt LinVar_ConvTabTracker.txt'%(inputs.output_file))
         
         command('tar -czvf %s_mapPatInterOut.tar.gz ./%s_mapPatInterOut --remove-files'%(inputs.output_file, inputs.output_file))
         command('tar -czvf %s_mapPatOut.tar.gz ./%s_mapPatOut --remove-files'%(inputs.output_file, inputs.output_file))
     
     elif inputs.database=='Nextstrain' and inputs.pathogen!='SARS-CoV-2':
         command('mkdir %s_mapPatInterOut'%(inputs.output_file))
         command('mkdir %s_mapPatOut'%(inputs.output_file))

         filesToCopy=['coutryToISO.txt', 'areaFile', 'NEW_allADM_CountryRegion_AssocTab.txt', 'metaDkeep']

         for fileName in filesToCopy:
             command('cp %s/%s .'%(inputs.path_config, fileName))
         
         command('perl %s/addToTableNextstrain.pl --metadata %s --seq %s --outfile %sTable.HaploCoV --nproc 18 --ref %s'%(inputs.path_scripts, inputs.input_file, inputs.seq, inputs.pathogen, inputs.refSeq))
         
         command('perl %s/countMlin.pl %sTable.HaploCoV > linDefMut.csv'%(inputs.path_scripts, inputs.pathogen))
         
         command('perl %s/BuildTables.pl %sTable.HaploCoV NO'%(inputs.path_scripts, inputs.pathogen))
         
         build_coutriesListTracker('inTab_avCheck_tmp.txt')
         
         command('python3 %s/InAvailability_ConfigTabUpdater.py -iav inTab_avCheck_tmp.txt -p mPox'%(inputs.path_scripts))

         command('rm coutryToISO.txt areaFile NEW_allADM_CountryRegion_AssocTab.txt metaDkeep')
         
         command('mv -t ./%s_mapPatInterOut %sTable.HaploCoV linDefMut.csv inTab_avCheck_tmp.txt'%(inputs.output_file, inputs.pathogen))
         command('mv ./Tgenomes ./%s_mapPatInterOut'%(inputs.output_file))
         command('mv -t ./%s_mapPatOut Epiweek.*.csv *_muts_perLin.csv HeatmapRegLin_*.csv Total_*_regions.csv countriesListTracker.txt inTab_avCheck.txt'%(inputs.output_file))
         
         command('tar -czvf %s_mapPatInterOut.tar.gz ./%s_mapPatInterOut --remove-files'%(inputs.output_file, inputs.output_file))
         command('tar -czvf %s_mapPatOut.tar.gz ./%s_mapPatOut --remove-files'%(inputs.output_file, inputs.output_file))


if __name__=='__main__':
     main()
