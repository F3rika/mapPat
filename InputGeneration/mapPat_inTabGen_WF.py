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
     parser.add_argument('-s', '--seq', required=False, help='Sequencing file in FASTA format.')
     parser.add_argument('-p', '--pathogen', choices=['SARS-CoV-2', 'mPox'], help='Name of the pathogen of interest. Allowed values: SARS-CoV-2, mPox.')
     parser.add_argument('-db', '--database', choices=['GISAID', 'Nextstrain'], help='Name of the database from which metadata are dowloaded. Allowed values: GISAID, Nextstrain.')
     parser.add_argument('-o', '--output_file', help='Basename of the compressed file where mapPat outputs are stored.')
     
     inputs=parser.parse_args()
     
     if inputs.database=='GISAID' and inputs.pathogen=='SARS-CoV-2':
         command('mkdir %s_mapPatInterOut'%(inputs.output_file))
         command('mkdir %s_mapPatOut'%(inputs.output_file))
         
         command('perl ./Scripts/GisaidToHaploCoV.pl --metadata %s --outfile SARS-CoV-2.HaploCoV'%(inputs.input_file))
         
         command('perl ./Scripts/countMlin.pl SARS-CoV-2.HaploCoV > linDefMut.csv')
         
         command('perl ./Scripts/BuildTables.pl SARS-CoV-2.HaploCoV SI')
         
         build_coutriesListTracker('inTab_avCheck_tmp.txt')
         
         command('python3 ./Scripts/LinVar_ConvTabBuilder.py -rv ./Config/LinVar_AssocTab.txt -rs ./Config/LinVBM_AssocTab.txt -ra ./Config/SARS-CoV-2_LineagesAliases.json -t %s -o LinVar_ConvTabTracker.txt'%(inputs.input_file))
         
         command('python3 ./Scripts/Var_CountsTabBuilder.py -rc countriesListTracker.txt -rv LinVar_ConvTabTracker.txt')
         
         command('python3 ./Scripts/InAvailability_ConfigTabUpdater.py -iav inTab_avCheck_tmp.txt -rc countriesListTracker.txt -p SARS-CoV-2')
         
         command('mv -t ./%s_mapPatInterOut linDefMut.csv SARS-CoV-2.HaploCoV inTab_avCheck_tmp.txt'%(inputs.output_file))
         command('mv -t ./%s_mapPatOut Epiweek.*.csv *_muts_perLin.csv HeatmapRegLin_*.csv Total_*_regions.csv countriesListTracker.txt inTab_avCheck.txt LinVar_ConvTabTracker.txt'%(inputs.output_file))
         
         command('tar -czvf %s_mapPatInterOut.tar.gz ./%s_mapPatInterOut --remove-files'%(inputs.output_file, inputs.output_file))
         command('tar -czvf %s_mapPatOut.tar.gz ./%s_mapPatOut --remove-files'%(inputs.output_file, inputs.output_file))
     
     elif inputs.database=='Nextstrain' and inputs.pathogen=='mPox':
         command('mkdir %s_mapPatInterOut'%(inputs.output_file))
         command('mkdir %s_mapPatOut'%(inputs.output_file))
         
         command('perl ./Scripts/addToTableNextstrain.pl --metadata %s --seq %s --outfile MpoxTable.HaploCoV --nproc 18 --ref ./Config/mPox_reference.fasta'%(inputs.input_file, inputs.seq))
         
         command('perl ./Scripts/countMlin.pl MpoxTable.HaploCoV > linDefMut.csv')
         
         command('perl ./Scripts/BuildTables.pl MpoxTable.HaploCoV NO')
         
         build_coutriesListTracker('inTab_avCheck_tmp.txt')
         
         command('python3 ./Scripts/InAvailability_ConfigTabUpdater.py -iav inTab_avCheck_tmp.txt -p mPox')
         
         command('mv -t ./%s_mapPatInterOut MpoxTable.HaploCoV linDefMut.csv inTab_avCheck_tmp.txt'%(inputs.output_file))
         command('mv ./Tgenomes ./%s_mapPatInterOut'%(inputs.output_file))
         command('mv -t ./%s_mapPatOut Epiweek.*.csv *_muts_perLin.csv HeatmapRegLin_*.csv Total_*_regions.csv countriesListTracker.txt inTab_avCheck.txt'%(inputs.output_file))
         
         command('tar -czvf %s_mapPatInterOut.tar.gz ./%s_mapPatInterOut --remove-files'%(inputs.output_file, inputs.output_file))
         command('tar -czvf %s_mapPatOut.tar.gz ./%s_mapPatOut --remove-files'%(inputs.output_file, inputs.output_file))


if __name__=='__main__':
     main()
