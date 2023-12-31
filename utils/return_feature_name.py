def return_var_name():
    fea_name = ['stay_id', 'admission_age', 'height', 'weight_admit', 'bmi', 'gender', 'admissiontype', 'ethni',
    'infectionsite', 'invasVent', 'rrt_firstday',
    # 'early_antibiotic',
    'apsiii',
    'nee_max',
    'heartrate_mean', 'heartrate_sd', 'heartrate_vr',
    'isbp_mean', 'isbp_sd', 'isbp_vr',
    'idbp_mean', 'idbp_sd', 'idbp_vr',
    'imbp_mean', 'imbp_sd', 'imbp_vr',
    'nisbp_mean', 'nisbp_sd', 'nisbp_vr',
    'nidbp_mean', 'nidbp_sd', 'nidbp_vr',
    'nimbp_mean', 'nimbp_sd', 'nimbp_vr',
    'resprate_mean', 'resprate_sd', 'rr_vr',
    'temp_mean', 'temp_sd', 'temp_vr',
    'spo2_mean', 'spo2_sd', 'spo2_vr',
    'ph_mean', 'ph_sd', 'ph_vr',
    'lac_mean', 'lac_sd', 'lac_vr',
    'po2_mean', 'po2_sd', 'po2_vr',
    'pco2_mean', 'pco2_sd', 'pco2_vr',
    'fio2_mean', 'fio2_sd', 'fio2_vr',
    'pf_mean', 'pf_sd', 'pf_vr',
    'baseexcess_mean', 'baseexcess_sd', 'baseexcess_vr',
    'alt_mean', 'alt_sd', 'alt_vr',
    'ast_mean', 'ast_sd', 'ast_vr',
    'bili_mean', 'bili_sd', 'bili_vr',
    'albumin_mean', 'albumin_sd', 'albumin_vr',
    'bicarbonate_mean', 'bicarbonate_sd', 'bicarbonate_vr',
    'bun_mean', 'bun_sd', 'bun_vr',
    'calcium_mean', 'calcium_sd', 'calcium_vr',
    'chloride_mean', 'chloride_sd', 'chloride_vr',
    'creatinine_mean', 'creatinine_sd', 'creatinine_vr',
    'glucose_mean', 'glucose_sd', 'glucose_vr',
    'sodium_mean', 'sodium_sd', 'sodium_vr',
    'potassium_mean', 'potassium_sd', 'potassium_vr',
    'inr_mean', 'inr_sd', 'inr_vr',
    'pt_mean', 'pt_sd', 'pt_vr',
    'ptt_mean', 'ptt_sd', 'ptt_vr',
    'hematocrit_mean', 'hematocrit_sd', 'hematocrit_vr',
    'hemoglobin_mean', 'hemoglobin_sd', 'hemoglobin_vr',
    'platelet_mean', 'platelet_sd', 'platelet_vr',
    'wbc_mean', 'wbc_sd', 'wbc_vr',
    'gcs_mean', 'gcs_sd', 'gcs_vr',
    'urine_total',
    'input_total',
    'Group'
     ]

    return fea_name


def return_var_name_2():
    fea_name = ['stay_id', 'admission_age','height', 'weight_admit', 'bmi', 'gender', 'admissiontype', 'ethni',
    'infectionsite', 'invasVent', 'rrt_firstday',
    # 'early_antibiotic',
    'apsiii',
    'nee_max',
    'heartrate_max', 'heartrate_min', 'heartrate_mean', 'heartrate_vr', 'isbp_max',
    'isbp_min', 'isbp_mean', 'isbp_vr', 'idbp_max',
    'idbp_min', 'idbp_mean', 'idbp_vr', 'imbp_max',
    'imbp_min', 'imbp_mean', 'imbp_vr', 'nisbp_max',
    'nisbp_min', 'nisbp_mean', 'nisbp_vr', 'nidbp_max',
    'nidbp_min', 'nidbp_mean', 'nidbp_vr', 'nimbp_max',
    'nimbp_min', 'nimbp_mean', 'nimbp_vr', 'resprate_max',
    'resprate_min', 'resprate_mean', 'rr_vr',
    'temp_max', 'temp_min', 'temp_mean', 'temp_vr',
    'spo2_max', 'spo2_min', 'spo2_mean', 'spo2_vr',
    'ph_max', 'ph_min', 'ph_mean', 'ph_vr', 'lac_max',
    'lac_min', 'lac_mean', 'lac_vr', 'po2_max', 'po2_min',
    'po2_mean', 'po2_vr', 'fio2_max', 'fio2_min',
    'fio2_mean', 'fio2_vr', 'pco2_max', 'pco2_min',
    'pco2_mean', 'pco2_vr', 'pf_max', 'pf_min', 'pf_mean', 'pf_vr', 'baseexcess_max', 'baseexcess_min',
    'baseexcess_mean', 'baseexcess_vr',
    'alt_max', 'alt_min', 'alt_mean', 'alt_vr',
    'ast_max', 'ast_min', 'ast_mean', 'ast_vr', 'bili_max',
    'bili_min', 'bili_mean', 'bili_vr',
    'albumin_max', 'albumin_min', 'albumin_mean', 'albumin_vr',
    'bicarbonate_max', 'bicarbonate_min', 'bicarbonate_mean',
    'bicarbonate_vr', 'bun_max', 'bun_min',
    'bun_mean', 'bun_vr', 'calcium_max', 'calcium_min',
    'calcium_mean', 'calcium_vr', 'chloride_max',
    'chloride_min', 'chloride_mean', 'chloride_vr',
    'creatinine_max', 'creatinine_min', 'creatinine_mean',
    'creatinine_vr', 'glucose_max', 'glucose_min',
    'glucose_mean', 'glucose_vr', 'sodium_max',
    'sodium_min', 'sodium_mean', 'sodium_vr',
    'potassium_max', 'potassium_min', 'potassium_mean',
    'potassium_vr', 'inr_max', 'inr_min', 'inr_mean',
    'inr_vr', 'pt_max', 'pt_min', 'pt_mean', 'pt_vr',
    'ptt_max', 'ptt_min', 'ptt_mean', 'ptt_vr',
    'hematocrit_max', 'hematocrit_min', 'hematocrit_mean',
    'hematocrit_vr', 'hemoglobin_max',
    'hemoglobin_min', 'hemoglobin_mean',
    'hemoglobin_vr', 'platelet_max', 'platelet_min', 'platelet_mean',
    'platelet_vr', 'wbc_max', 'wbc_min', 'wbc_mean', 'wbc_vr', 'gcs_max', 'gcs_min', 'gcs_mean',
    'gcs_vr',
    'urine_total',
    'input_total',
    'Group'
     ]

    return fea_name

def return_var_name_all():
    fea_name = ['stay_id', 'admission_age', 'height', 'weight_admit', 'bmi', 'gender', 'admissiontype', 'ethni',
    'infectionsite', 'invasVent', 'rrt_firstday',
    'early_antibiotic', 'sofa_score', 'respiration', 'coagulation', 'liver', 'cardiovascular', 'cns', 'renal', 'charlson_comorbidity_index', 'heart_rtythm',
    'apsiii',
    'nee_max',
    'heartrate_mean', 'heartrate_sd', 'heartrate_vr',
    'isbp_mean', 'isbp_sd', 'isbp_vr',
    'idbp_mean', 'idbp_sd', 'idbp_vr',
    'imbp_mean', 'imbp_sd', 'imbp_vr',
    'nisbp_mean', 'nisbp_sd', 'nisbp_vr',
    'nidbp_mean', 'nidbp_sd', 'nidbp_vr',
    'nimbp_mean', 'nimbp_sd', 'nimbp_vr',
    'resprate_mean', 'resprate_sd', 'rr_vr',
    'temp_mean', 'temp_sd', 'temp_vr',
    'spo2_mean', 'spo2_sd', 'spo2_vr',
    'ph_mean', 'ph_sd', 'ph_vr',
    'lac_mean', 'lac_sd', 'lac_vr',
    'po2_mean', 'po2_sd', 'po2_vr',
    'pco2_mean', 'pco2_sd', 'pco2_vr',
    'fio2_mean', 'fio2_sd', 'fio2_vr',
    'pf_mean', 'pf_sd', 'pf_vr',
    'baseexcess_mean', 'baseexcess_sd', 'baseexcess_vr',
    'alt_mean', 'alt_sd', 'alt_vr',
    'ast_mean', 'ast_sd', 'ast_vr',
     # eicu loss
    'alp_mean', 'alp_sd', 'alp_vr',
    'ldh_mean', 'ldh_sd', 'ldh_vr',

    'bili_mean', 'bili_sd', 'bili_vr',
    'albumin_mean', 'albumin_sd', 'albumin_vr',
    'bicarbonate_mean', 'bicarbonate_sd', 'bicarbonate_vr',
    'bun_mean', 'bun_sd', 'bun_vr',
    'calcium_mean', 'calcium_sd', 'calcium_vr',
    'chloride_mean', 'chloride_sd', 'chloride_vr',
    'creatinine_mean', 'creatinine_sd', 'creatinine_vr',
    'glucose_mean', 'glucose_sd', 'glucose_vr',
    'sodium_mean', 'sodium_sd', 'sodium_vr',
    'potassium_mean', 'potassium_sd', 'potassium_vr',
    'inr_mean', 'inr_sd', 'inr_vr',
    'pt_mean', 'pt_sd', 'pt_vr',
    'ptt_mean', 'ptt_sd', 'ptt_vr',
    'hematocrit_mean', 'hematocrit_sd', 'hematocrit_vr',
    'hemoglobin_mean', 'hemoglobin_sd', 'hemoglobin_vr',
    'platelet_mean', 'platelet_sd', 'platelet_vr',
    'wbc_mean', 'wbc_sd', 'wbc_vr',
    'gcs_mean', 'gcs_sd', 'gcs_vr',
    'urine_total',
    'input_total',
    'Group'
     ]

    return fea_name

def return_var_name_final():
    fea_name = ['stay_id',
    # categary_var
    'gender', 'admissiontype', 'ethni', 'infectionsite', 'invasVent', 'rrt_firstday',
    'admission_age', 'height', 'weight_admit', 'bmi',
    # 'early_antibiotic',
    'apsiii',
    'nee_max',
    'heartrate_max', 'heartrate_min', 'heartrate_mean', 'heartrate_vr', 'isbp_max',
    'isbp_min', 'isbp_mean', 'isbp_vr', 'idbp_max',
    'idbp_min', 'idbp_mean', 'idbp_vr', 'imbp_max',
    'imbp_min', 'imbp_mean', 'imbp_vr', 'nisbp_max',
    'nisbp_min', 'nisbp_mean', 'nisbp_vr', 'nidbp_max',
    'nidbp_min', 'nidbp_mean', 'nidbp_vr', 'nimbp_max',
    'nimbp_min', 'nimbp_mean', 'nimbp_vr', 'resprate_max',
    'resprate_min', 'resprate_mean', 'rr_vr',
    'temp_max', 'temp_min', 'temp_mean', 'temp_vr',
    'spo2_max', 'spo2_min', 'spo2_mean', 'spo2_vr',
    'ph_max', 'ph_min', 'ph_mean', 'ph_vr', 'lac_max',
    'lac_min', 'lac_mean', 'lac_vr', 'po2_max', 'po2_min',
    'po2_mean', 'po2_vr', 'fio2_max', 'fio2_min',
    'fio2_mean', 'fio2_vr', 'pco2_max', 'pco2_min',
    'pco2_mean', 'pco2_vr', 'pf_max', 'pf_min', 'pf_mean', 'pf_vr', 'baseexcess_max', 'baseexcess_min',
    'baseexcess_mean', 'baseexcess_vr',
    'alt_max', 'alt_min', 'alt_mean', 'alt_vr',
    'ast_max', 'ast_min', 'ast_mean', 'ast_vr', 'bili_max',
    'bili_min', 'bili_mean', 'bili_vr',
    'albumin_max', 'albumin_min', 'albumin_mean', 'albumin_vr',
    'bicarbonate_max', 'bicarbonate_min', 'bicarbonate_mean',
    'bicarbonate_vr', 'bun_max', 'bun_min',
    'bun_mean', 'bun_vr', 'calcium_max', 'calcium_min',
    'calcium_mean', 'calcium_vr', 'chloride_max',
    'chloride_min', 'chloride_mean', 'chloride_vr',
    'creatinine_max', 'creatinine_min', 'creatinine_mean',
    'creatinine_vr', 'glucose_max', 'glucose_min',
    'glucose_mean', 'glucose_vr', 'sodium_max',
    'sodium_min', 'sodium_mean', 'sodium_vr',
    'potassium_max', 'potassium_min', 'potassium_mean',
    'potassium_vr', 'inr_max', 'inr_min', 'inr_mean',
    'inr_vr', 'pt_max', 'pt_min', 'pt_mean', 'pt_vr',
    'ptt_max', 'ptt_min', 'ptt_mean', 'ptt_vr',
    'hematocrit_max', 'hematocrit_min', 'hematocrit_mean',
    'hematocrit_vr', 'hemoglobin_max',
    'hemoglobin_min', 'hemoglobin_mean',
    'hemoglobin_vr', 'platelet_max', 'platelet_min', 'platelet_mean',
    'platelet_vr', 'wbc_max', 'wbc_min', 'wbc_mean', 'wbc_vr', 'gcs_max', 'gcs_min', 'gcs_mean',
    'gcs_vr',
    'urine_total',
    'input_total',
    'Group'
     ]

    return fea_name