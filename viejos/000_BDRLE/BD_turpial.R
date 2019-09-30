##ssh root@185.43.108.36 -p 3838
##ssh gismin@185.43.108.36 -p 3838

scp root@185.43.108.36 -P 3838 ~/gisdata/sensores/Modis/BurnedArea/*tif .
nohup scp -B -P 3838 root@185.43.108.36:~/gisdata/clima/CMIP/02.5m/h[ed]*bi50.zip gisdata/clima/CMIP/02.5m/ &

## para buscar archivos modificados recientemente...
find librorojo -mtime -3 -ls

## who has been messing around (besides CRON jobs)
grep -v CRON /var/log/auth.log

## check updates for animales amenazados...
##http://animalesamenazados.provita.org.ve/#overlay=admin/reports/status


##https://www.postgresql.org/docs/9.1/static/backup-dump.html
##en el servidor:
## como root...
pg_dump gis_rle > 20170603_CaseStudy.sql
## en la cuenta de gismin...
set HOY=`date +%Y%m%d`

pg_dump -U postgres gis_rle > `$HOY`_CaseStudy.sql
tar -cjvf respaldo_casestudy.tar.bz2 *CaseStudy*sql
rm *CaseStudy*sql
##local
create user jferrer createdb createuser password 'luz de la mañana, alegre...';
create database gis_rle owner jferrer;
sed s/provita/jferrer/g home/gismin/20170723_CaseStudy.sql > caseStudy.sql
psql gis_rle < caseStudy.sql 

 psql gis_rle < home/gismin/20170723_CaseStudy.sql 


## no pide password al usuario postgres, 
## #psql gis_rle postgres

##documentation: https://www.postgresql.org/docs/9.1/static/app-psql.html

## #\dt
insert INTO case_study_author(first_name,middle_name_initial,last_name,title,email,corresponding,address) VALUES('José','R.','Ferrer','Dr.','jose.ferrer@provitaonline.org.ve','t','Centro de Estudios Botánicos y Agroforestales, IVIC, Maracaibo Venezuela');

\d case_study_apiversionthreatenedspecies 
select * from case_study_apiversionthreatenedspecies;
\d case_study_author  
update case_study_author set institution_id=5 where last_name='Ferrer';

\a \t \x
select * from case_study_casestudy where id=15;


public | case_study_apiversionthreatenedspecies       | table | provita
 public | case_study_author                            | table | provita
 public | case_study_casestudy                         | table | provita
 public | case_study_casestudy_author                  | table | provita
 public | case_study_casestudy_country                 | table | provita
 public | case_study_casestudy_criterion               | table | provita
 public | case_study_casestudy_risk_assessment_country | table | provita
 public | case_study_conservationaction                | table | provita
 public | case_study_conservationactionkind            | table | provita
 public | case_study_continent                         | table | provita
 public | case_study_country                           | table | provita
 public | case_study_criterion                         | table | provita
 public | case_study_dataentryresponsible              | table | provita
 public | case_study_distributionmap                   | table | provita
 public | case_study_ecosystemclassification           | table | provita
 public | case_study_ecosystemtype                     | table | provita
 public | case_study_habitatsclassificationscheme      | table | provita
 public | case_study_institution                       | table | provita
 public | case_study_iucnclassification                | table | provita
 public | case_study_realm                             | table | provita
 public | case_study_reference                         | table | provita
 public | case_study_researchaction                    | table | provita
 public | case_study_researchactionkind                | table | provita
 public | case_study_riskassessmentcriteriona          | table | provita
 public | case_study_riskassessmentcriterionb          | table | provita
 public | case_study_riskassessmentcriterionc          | table | provita
 public | case_study_riskassessmentcriteriond          | table | provita
 public | case_study_riskassessmentcriterione          | table | provita
 public | case_study_riskcategory                      | table | provita
 public | case_study_spatialdata                       | table | provita
 public | case_study_stresses                          | table | provita
 public | case_study_threat                            | table | provita
 public | case_study_threatenedspecie                  | table | provita
 public | case_study_threatenedspecieincasestudy       | table | provita
 public | case_study_threatkind                        | table | provita

id                                       | integer                | not null va
lor por omisión nextval('case_study_casestudy_id_seq'::regclass)
 ecosystem_id                             | integer                | 
 description                              | text                   | 
 description_references                   | character varying(100) | 
 native_biota                             | text                   | 
 abiotic_environment                      | text                   | 
 processes_interactions                   | text                   | 
 services                                 | character varying(100) | 
 conceptual_model_image                   | character varying(100) | 
 conceptual_model_author                  | character varying(150) | 
 conceptual_model_title                   | character varying(150) | 
 conceptual_model_caption                 | text                   | 
 conceptual_model_url                     | character varying(200) | 
 image                                    | character varying(100) | 
 image_author                             | character varying(150) | 
 image_type                               | character varying(10)  | 
 image_title                              | character varying(150) | 
 image_caption                            | text                   | 
 description_comment                      | text                   | 
 distribution                             | text                   | 
 distribution_summary                     | character varying(200) | 
 biogeographical_realms_id                | integer                | 
 marine_realms_id                         | integer                | 
 date                                     | date                   | 
 distribution_risk_assessment             | text                   | 
 risk_assessment_distribution_summary     | character varying(200) | 
 latitude                                 | double precision       | 
 longitude                                | double precision       | 
 scope                                    | character varying(10)  | not null
 level                                    | character varying(13)  | not null
 extent                                   | boolean                | not null
 risk_assessment_distribution_map         | character varying(100) | 
 risk_assessment_distribution_author      | character varying(100) | 
 risk_assessment_distribution_map_title   | character varying(100) | 
 risk_assessment_distribution_map_caption | text                   | 
 risk_assessment_spatial_data_file        | character varying(100) | 
 risk_assessment_spatial_data_format      | character varying(100) | 
 risk_assessment_spatial_data_extension   | character varying(100) | 
 risk_assessment_spatial_data_reference   | character varying(100) | 
 current                                  | boolean                | not null
 version                                  | double precision       | 
 rationale                                | text                   | 
 pre_approved                             | boolean                | not null
 identifier                               | character varying(50)  | 
 core_identifier                          | integer                | 
 name                                     | character varying(150) | 
summary                                  | text                   | 
 keywords                                 | character varying(150) | 
 citation                                 | text                   | 
 data_entry_responsible_id                | integer                | 
 data_received                            | date                   | not null
 date_accepted                            | date                   | 
 date_published                           | date                   | 
 date_rejected                            | date                   | 
 date_returned                            | date                   | 
 case_map_id                              | integer                | 
 is_published                             | boolean                | not null
 status                                   | character varying(30)  | not null
 modif_count                              | smallint               | not null
 threatening_process                      | text                   | 
 collapse_definition                      | text                   | 
 iucn_classification_id                   | integer                | 
 iucn_habitat_name_id                     | integer                | 
 overall_category_is_range                | boolean                | not null
 overall_category_id                      | integer                | 
 overall_category_low_id                  | integer                | 
