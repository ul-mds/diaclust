# DiaClusT --> read data with fhircrackr
# https://github.com/POLAR-fhiR/fhircrackr
####################################################################
rm(list=ls())

library(fhircrackr)

# load data 
URL <- "https://fhir.server.url"

# # # Patient
request <- fhir_url(url = URL, resource = "Patient")
patient_bundles <- fhir_search(request = request, verbose = 0)       #  max_bundles = n

patient_table <- fhir_table_description(
  resource = "Patient",
  cols     = c(
    PID         = "id",
    gender      = "gender",
    birthDate    = "birthDate",
    PatVID  = "identifier/value"
  ), 
  sep = "|",
  #brackets = c("[","]"),
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE)

patients <- fhir_crack(bundles = patient_bundles, design = patient_table, verbose = 0)
write.csv(patients, file="DiaClusT_patients.csv", row.names=FALSE)

print("Patients done.")

# # # Condition
request <- fhir_url(url = URL, resource = "Condition")
condition_bundles <- fhir_search(request = request, verbose = 0) 

condition_table <- fhir_table_description(
  resource = "Condition",
  cols     = c(
               CID = "id",
               category_coding_code = "category/coding/code",        
                          # look up the codes --> https://terminology.hl7.org/5.0.0/CodeSystem-diagnosis-role.html
               code_coding_code = "code/coding/code",
               code_coding_system = "code/coding/system",
               code_coding_version = "code/coding/version",
               
               PID ="subject/reference",
               EID = "encounter/reference",
               record_date = "recordedDate/extension/valueCode"
               ),
        sep = "|",
        #brackets = c("[","]"),
        rm_empty_cols = FALSE,
        format = 'compact',
        keep_attr = FALSE
  )

conditions <- fhir_crack(bundles=condition_bundles, design=condition_table, verbose=0)

write.csv(conditions, file="DiaClusT_conditions.csv", row.names=FALSE)

print("Conditions done.")


# # # Encounter

request <- fhir_url(url = URL, resource = "Encounter")
encounter_bundles <- fhir_search(request = request, verbose = 0)

enc_table <- fhir_table_description(
  resource = "Encounter",
  cols     = c(
    EID = "id",
    #ex_valueCoding_system = "extension/extension/valueCoding/system",
      # valueCoding_code: Aufnahmegrund --> ersten vier Stellen, Entlassgrund letzten drei
    ex_valueCoding_code = "extension/extension/valueCoding/code",
    id_type_system = "identifier/system",
    id_fallnummer = "identifier/value",
    class_code = "class/code",
    class_display = "class/display",
    type_code ="type/coding/code",
    service_type = "serviceType/coding/code",
        # look up the code --> https://www.medizininformatik-initiative.de/fhir/core/modul-fall/CodeSystem/Fachabteilungsschluessel
    PID ="subject/reference",
    period_start = "period/start",
    period_end = "period/end",
    partOf_ref = "partOf/reference",
    diagnosis_ref = "diagnosis/condition/reference",
    diagnosis_ref_code = "diagnosis/use/coding/code",
        # look up the code --> http://terminology.hl7.org/CodeSystem/diagnosis-role
    hosp_reason = "hospitalization/admitSource/coding/code"
        # look up the codes --> https://www.medizininformatik-initiative.de/fhir/core/modul-fall/CodeSystem/Aufnahmeanlass
  ),
  sep="|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

encounters <- fhir_crack(bundles=encounter_bundles, design=enc_table, verbose=0)

write.csv(encounters, file="DiaClusT_encounters.csv", row.names=FALSE)

print("Encounter done.")

# # # Procedure

request <- fhir_url(url = URL, resource = "Procedure")
procedure_bundles <- fhir_search(request = request, verbose = 0)

proc_table <- fhir_table_description(
  resource = "Procedure",
  cols     = c(
    ProID = "id",
    category_code ="category/coding/code",
    category_display = "category/coding/display",
    code_system = "code/coding/system",
    code_version= "code/coding/version",
    code_code = "code/coding/code",
    PID ="subject/reference",
    EID = "encounter/reference",
    performed_DateTime = "performedDateTime"
  ),
  sep="|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

procedures <- fhir_crack(bundles=procedure_bundles, design=proc_table, verbose=0)

write.csv(procedures,file="DiaClusT_procedures.csv", row.names=FALSE)

print("Procedures done.")

# # # Organization

request <- fhir_url(url = URL, resource = "Organization")
organization_bundles <- fhir_search(request = request, verbose = 0)

org_table <- fhir_table_description(
  resource = "Organization",
  cols     = c(
    OrgID = "id",
    identifier_system = "identifier/system",
    identifier_value = "identifier/value",
    type_code = "type/coding/code"
  ),
  sep="|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

organizations <- fhir_crack(bundles=organization_bundles, design=org_table, verbose=0)

write.csv(organizations,file="DiaClusT_organizations.csv", row.names=FALSE)
