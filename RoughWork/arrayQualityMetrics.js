// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false ];
var arrayMetadata    = [ [ "1", "GSM624925", "Muscle-FamilyHistoryNegative-1", "Mar 01 2011", "gender: female", "age (years): 39", "family history: Family history negative", "body mass index (kg/m2): 20", "fasting glucose (ogtt): 85", "2 hour glucose (ogtt): 125", "hemoglobin a1c: 5.6", "fasting glucose (iv0gavg): 90", "fasting insulin (iv0inavg): 2.7", "si: 11.91", "waist hip ratio: 1.364", "Gene expression of skeletal muscle from subject without diabetes", "125", "39", "20", "Family history negative", "90", "85", "2.7", "female", "5.6", "11.91", "1.364", "low", "normal levels" ], [ "2", "GSM624926", "Muscle-FamilyHistoryNegative-2", "Mar 01 2011", "gender: male", "age (years): 23", "family history: Family history negative", "body mass index (kg/m2): 23", "fasting glucose (ogtt): 84.5", "2 hour glucose (ogtt): 112", "hemoglobin a1c: 5", "fasting glucose (iv0gavg): 87", "fasting insulin (iv0inavg): 4.4", "si: 8.31", "waist hip ratio: 0.792", "Gene expression of skeletal muscle from subject without diabetes", "112", "23", "23", "Family history negative", "87", "84.5", "4.4", "male", "5", "8.31", "0.792", "optimal", "normal levels" ], [ "3", "GSM624927", "Muscle-FamilyHistoryNegative-3", "Mar 01 2011", "gender: female", "age (years): 42", "family history: Family history negative", "body mass index (kg/m2): 21", "fasting glucose (ogtt): 67", "2 hour glucose (ogtt): 124", "hemoglobin a1c: 4.6", "fasting glucose (iv0gavg): 79", "fasting insulin (iv0inavg): 6.1", "si: 1.59", "waist hip ratio: 0.824", "Gene expression of skeletal muscle from subject without diabetes", "124", "42", "21", "Family history negative", "79", "67", "6.1", "female", "4.6", "1.59", "0.824", "optimal", "normal levels" ], [ "4", "GSM624928", "Muscle-FamilyHistoryNegative-4", "Mar 01 2011", "gender: male", "age (years): 40", "family history: Family history negative", "body mass index (kg/m2): 27", "fasting glucose (ogtt): 103", "2 hour glucose (ogtt): 150", "hemoglobin a1c: 5.1", "fasting glucose (iv0gavg): 91", "fasting insulin (iv0inavg): 5.6", "si: 8.38", "waist hip ratio: 0.981", "Gene expression of skeletal muscle from subject without diabetes", "150", "40", "27", "Family history negative", "91", "103", "5.6", "male", "5.1", "8.38", "0.981", "optimal", "normal levels" ], [ "5", "GSM624929", "Muscle-FamilyHistoryNegative-5", "Nov 28 2018", "gender: female", "age (years): 57", "family history: Family history negative", "body mass index (kg/m2): 22", "fasting glucose (ogtt): 81.5", "2 hour glucose (ogtt): 95", "hemoglobin a1c: 5.5", "fasting glucose (iv0gavg): 94", "fasting insulin (iv0inavg): 5", "si: 4.06", "waist hip ratio: missing value", "Gene expression of skeletal muscle from subject without diabetes", "95", "57", "22", "Family history negative", "94", "81.5", "5", "female", "5.5", "4.06", "missing value", "optimal", "normal levels" ], [ "6", "GSM624930", "Muscle-FamilyHistoryNegative-6", "Mar 01 2011", "gender: female", "age (years): 23", "family history: Family history negative", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 77", "2 hour glucose (ogtt): 90", "hemoglobin a1c: 4.3", "fasting glucose (iv0gavg): 91", "fasting insulin (iv0inavg): 8", "si: 6.53", "waist hip ratio: 0.833", "Gene expression of skeletal muscle from subject without diabetes", "90", "23", "25", "Family history negative", "91", "77", "8", "female", "4.3", "6.53", "0.833", "diabetic", "normal levels" ], [ "7", "GSM624931", "Muscle-FamilyHistoryNegative-7", "Mar 01 2011", "gender: female", "age (years): 46", "family history: Family history negative", "body mass index (kg/m2): 26", "fasting glucose (ogtt): 85.5", "2 hour glucose (ogtt): 77", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 88", "fasting insulin (iv0inavg): 4.4", "si: 10.39", "waist hip ratio: 0.845", "Gene expression of skeletal muscle from subject without diabetes", "77", "46", "26", "Family history negative", "88", "85.5", "4.4", "female", "5.4", "10.39", "0.845", "optimal", "normal levels" ], [ "8", "GSM624932", "Muscle-FamilyHistoryNegative-8", "Mar 01 2011", "gender: male", "age (years): 28", "family history: Family history negative", "body mass index (kg/m2): 24", "fasting glucose (ogtt): 78.5", "2 hour glucose (ogtt): 58", "hemoglobin a1c: 4.7", "fasting glucose (iv0gavg): 83", "fasting insulin (iv0inavg): 1.7", "si: 14.28", "waist hip ratio: 0.865", "Gene expression of skeletal muscle from subject without diabetes", "58", "28", "24", "Family history negative", "83", "78.5", "1.7", "male", "4.7", "14.28", "0.865", "low", "normal levels" ], [ "9", "GSM624933", "Muscle-FamilyHistoryNegative-9", "Mar 01 2011", "gender: male", "age (years): 38", "family history: Family history negative", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 91.5", "2 hour glucose (ogtt): 38", "hemoglobin a1c: 5.1", "fasting glucose (iv0gavg): 88", "fasting insulin (iv0inavg): 4.6", "si: 10.29", "waist hip ratio: 0.902", "Gene expression of skeletal muscle from subject without diabetes", "38", "38", "25", "Family history negative", "88", "91.5", "4.6", "male", "5.1", "10.29", "0.902", "optimal", "normal levels" ], [ "10", "GSM624934", "Muscle-FamilyHistoryNegative-10", "Mar 01 2011", "gender: male", "age (years): 50", "family history: Family history negative", "body mass index (kg/m2): 33", "fasting glucose (ogtt): 105", "2 hour glucose (ogtt): 146", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 109", "fasting insulin (iv0inavg): 15.9", "si: 1.7", "waist hip ratio: 1.018", "Gene expression of skeletal muscle from subject without diabetes", "146", "50", "33", "Family history negative", "109", "105", "15.9", "male", "5.4", "1.7", "1.018", "diabetic", "normal levels" ], [ "11", "GSM624935", "Muscle-FamilyHistoryNegative-11", "Mar 01 2011", "gender: female", "age (years): 24", "family history: Family history negative", "body mass index (kg/m2): 24", "fasting glucose (ogtt): 92", "2 hour glucose (ogtt): 99", "hemoglobin a1c: 4.2", "fasting glucose (iv0gavg): 95", "fasting insulin (iv0inavg): 4.8", "si: 4.19", "waist hip ratio: 0.832", "Gene expression of skeletal muscle from subject without diabetes", "99", "24", "24", "Family history negative", "95", "92", "4.8", "female", "4.2", "4.19", "0.832", "optimal", "normal levels" ], [ "12", "GSM624936", "Muscle-FamilyHistoryNegative-12", "Mar 01 2011", "gender: female", "age (years): 32", "family history: Family history negative", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 86.5", "2 hour glucose (ogtt): 105", "hemoglobin a1c: 5.3", "fasting glucose (iv0gavg): 85", "fasting insulin (iv0inavg): 5.3", "si: 6.95", "waist hip ratio: 0.836", "Gene expression of skeletal muscle from subject without diabetes", "105", "32", "25", "Family history negative", "85", "86.5", "5.3", "female", "5.3", "6.95", "0.836", "optimal", "normal levels" ], [ "13", "GSM624937", "Muscle-FamilyHistoryNegative-13", "Mar 01 2011", "gender: female", "age (years): 24", "family history: Family history negative", "body mass index (kg/m2): 30", "fasting glucose (ogtt): 89", "2 hour glucose (ogtt): 94", "hemoglobin a1c: 5", "fasting glucose (iv0gavg): 83", "fasting insulin (iv0inavg): 8.5", "si: 5.66", "waist hip ratio: 0.892", "Gene expression of skeletal muscle from subject without diabetes", "94", "24", "30", "Family history negative", "83", "89", "8.5", "female", "5", "5.66", "0.892", "diabetic", "normal levels" ], [ "14", "GSM624938", "Muscle-FamilyHistoryPositive-1", "Mar 01 2011", "gender: female", "age (years): 45", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 36", "fasting glucose (ogtt): 91.5", "2 hour glucose (ogtt): 106", "hemoglobin a1c: 5", "fasting glucose (iv0gavg): 96", "fasting insulin (iv0inavg): 4.6", "si: 9.64", "waist hip ratio: 0.836", "Gene expression of skeletal muscle from subject without diabetes", "106", "45", "36", "Family history positive - 1 parent", "96", "91.5", "4.6", "female", "5", "9.64", "0.836", "optimal", "normal levels" ], [ "15", "GSM624939", "Muscle-FamilyHistoryPositive-2", "Mar 01 2011", "gender: male", "age (years): 43", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 30", "fasting glucose (ogtt): 91.5", "2 hour glucose (ogtt): 97", "hemoglobin a1c: 5.3", "fasting glucose (iv0gavg): 93", "fasting insulin (iv0inavg): 4.8", "si: 3.04", "waist hip ratio: 0.925", "Gene expression of skeletal muscle from subject without diabetes", "97", "43", "30", "Family history positive - 1 parent", "93", "91.5", "4.8", "male", "5.3", "3.04", "0.925", "optimal", "normal levels" ], [ "16", "GSM624940", "Muscle-FamilyHistoryPositive-3", "Mar 01 2011", "gender: female", "age (years): 37", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 102.5", "2 hour glucose (ogtt): 141", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 95", "fasting insulin (iv0inavg): 3.7", "si: 2.56", "waist hip ratio: 0.805", "Gene expression of skeletal muscle from subject without diabetes", "141", "37", "25", "Family history positive - 1 parent", "95", "102.5", "3.7", "female", "5.4", "2.56", "0.805", "optimal", "normal levels" ], [ "17", "GSM624941", "Muscle-FamilyHistoryPositive-4", "Mar 01 2011", "gender: female", "age (years): 31", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 20", "fasting glucose (ogtt): 93", "2 hour glucose (ogtt): 128", "hemoglobin a1c: 4.9", "fasting glucose (iv0gavg): 96", "fasting insulin (iv0inavg): 5.1", "si: 7.61", "waist hip ratio: 0.745", "Gene expression of skeletal muscle from subject without diabetes", "128", "31", "20", "Family history positive - 1 parent", "96", "93", "5.1", "female", "4.9", "7.61", "0.745", "optimal", "normal levels" ], [ "18", "GSM624942", "Muscle-FamilyHistoryPositive-5", "Mar 01 2011", "gender: female", "age (years): 26", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 26", "fasting glucose (ogtt): 86.5", "2 hour glucose (ogtt): 169", "hemoglobin a1c: 5", "fasting glucose (iv0gavg): 92", "fasting insulin (iv0inavg): 2.1", "si: 4.5", "waist hip ratio: 0.755", "Gene expression of skeletal muscle from subject without diabetes", "169", "26", "26", "Family history positive - 1 parent", "92", "86.5", "2.1", "female", "5", "4.5", "0.755", "low", "normal levels" ], [ "19", "GSM624943", "Muscle-FamilyHistoryPositive-6", "Mar 01 2011", "gender: female", "age (years): 25", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 39", "fasting glucose (ogtt): 100.5", "2 hour glucose (ogtt): 149", "hemoglobin a1c: 5.7", "fasting glucose (iv0gavg): 93", "fasting insulin (iv0inavg): 22", "si: 1.06", "waist hip ratio: 0.957", "Gene expression of skeletal muscle from subject without diabetes", "149", "25", "39", "Family history positive - 1 parent", "93", "100.5", "22", "female", "5.7", "1.06", "0.957", "diabetic", "normal levels" ], [ "20", "GSM624944", "Muscle-FamilyHistoryPositive-7", "Mar 01 2011", "gender: male", "age (years): 31", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 26", "fasting glucose (ogtt): 85.5", "2 hour glucose (ogtt): 115", "hemoglobin a1c: 4.9", "fasting glucose (iv0gavg): 85", "fasting insulin (iv0inavg): 1.9", "si: 6.02", "waist hip ratio: 0.824", "Gene expression of skeletal muscle from subject without diabetes", "115", "31", "26", "Family history positive - 1 parent", "85", "85.5", "1.9", "male", "4.9", "6.02", "0.824", "low", "normal levels" ], [ "21", "GSM624945", "Muscle-FamilyHistoryPositive-8", "Mar 01 2011", "gender: female", "age (years): 47", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 23", "fasting glucose (ogtt): 91", "2 hour glucose (ogtt): 123", "hemoglobin a1c: 4.7", "fasting glucose (iv0gavg): 86", "fasting insulin (iv0inavg): 5.6", "si: 3.18", "waist hip ratio: 0.805", "Gene expression of skeletal muscle from subject without diabetes", "123", "47", "23", "Family history positive - 1 parent", "86", "91", "5.6", "female", "4.7", "3.18", "0.805", "optimal", "normal levels" ], [ "22", "GSM624946", "Muscle-FamilyHistoryPositive-9", "Mar 01 2011", "gender: female", "age (years): 44", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 31", "fasting glucose (ogtt): 98.5", "2 hour glucose (ogtt): 72", "hemoglobin a1c: 5", "fasting glucose (iv0gavg): 94", "fasting insulin (iv0inavg): 14.3", "si: 4.45", "waist hip ratio: 0.914", "Gene expression of skeletal muscle from subject without diabetes", "72", "44", "31", "Family history positive - 1 parent", "94", "98.5", "14.3", "female", "5", "4.45", "0.914", "diabetic", "normal levels" ], [ "23", "GSM624947", "Muscle-FamilyHistoryPositive-10", "Mar 01 2011", "gender: male", "age (years): 44", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 93.5", "2 hour glucose (ogtt): 151", "hemoglobin a1c: 4.8", "fasting glucose (iv0gavg): 98", "fasting insulin (iv0inavg): 10", "si: 1.77", "waist hip ratio: 0.905", "Gene expression of skeletal muscle from subject without diabetes", "151", "44", "25", "Family history positive - 1 parent", "98", "93.5", "10", "male", "4.8", "1.77", "0.905", "diabetic", "normal levels" ], [ "24", "GSM624948", "Muscle-FamilyHistoryPositive-11", "Mar 01 2011", "gender: male", "age (years): 21", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 27", "fasting glucose (ogtt): 84.5", "2 hour glucose (ogtt): 114", "hemoglobin a1c: 4.5", "fasting glucose (iv0gavg): 83", "fasting insulin (iv0inavg): 14.3", "si: 1.34", "waist hip ratio: 0.948", "Gene expression of skeletal muscle from subject without diabetes", "114", "21", "27", "Family history positive - 1 parent", "83", "84.5", "14.3", "male", "4.5", "1.34", "0.948", "diabetic", "normal levels" ], [ "25", "GSM624949", "Muscle-FamilyHistoryPositive-12", "Mar 01 2011", "gender: female", "age (years): 25", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 22", "fasting glucose (ogtt): 90.5", "2 hour glucose (ogtt): 127", "hemoglobin a1c: 4.9", "fasting glucose (iv0gavg): 81", "fasting insulin (iv0inavg): 6.5", "si: 6.9", "waist hip ratio: 0.885", "Gene expression of skeletal muscle from subject without diabetes", "127", "25", "22", "Family history positive - 1 parent", "81", "90.5", "6.5", "female", "4.9", "6.9", "0.885", "optimal", "normal levels" ], [ "26", "GSM624950", "Muscle-FamilyHistoryPositive-13", "Mar 01 2011", "gender: male", "age (years): 27", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 24", "fasting glucose (ogtt): 87", "2 hour glucose (ogtt): 148", "hemoglobin a1c: 4.7", "fasting glucose (iv0gavg): 86", "fasting insulin (iv0inavg): 4.2", "si: 5.65", "waist hip ratio: 0.865", "Gene expression of skeletal muscle from subject without diabetes", "148", "27", "24", "Family history positive - 1 parent", "86", "87", "4.2", "male", "4.7", "5.65", "0.865", "optimal", "normal levels" ], [ "27", "GSM624951", "Muscle-FamilyHistoryPositive-14", "Mar 01 2011", "gender: female", "age (years): 51", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 26", "fasting glucose (ogtt): 101", "2 hour glucose (ogtt): 146", "hemoglobin a1c: 4.8", "fasting glucose (iv0gavg): 98", "fasting insulin (iv0inavg): 5.4", "si: 4.21", "waist hip ratio: 0.8", "Gene expression of skeletal muscle from subject without diabetes", "146", "51", "26", "Family history positive - 1 parent", "98", "101", "5.4", "female", "4.8", "4.21", "0.8", "optimal", "normal levels" ], [ "28", "GSM624952", "Muscle-FamilyHistoryPositive-15", "Mar 01 2011", "gender: female", "age (years): 25", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 42", "fasting glucose (ogtt): 99.5", "2 hour glucose (ogtt): 170", "hemoglobin a1c: 5.6", "fasting glucose (iv0gavg): 112", "fasting insulin (iv0inavg): 15.6", "si: 1.62", "waist hip ratio: 0.808", "Gene expression of skeletal muscle from subject without diabetes", "170", "25", "42", "Family history positive - 2 parents", "112", "99.5", "15.6", "female", "5.6", "1.62", "0.808", "diabetic", "normal levels" ], [ "29", "GSM624953", "Muscle-FamilyHistoryPositive-16", "Mar 01 2011", "gender: male", "age (years): 24", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 20", "fasting glucose (ogtt): 94.5", "2 hour glucose (ogtt): 148", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 89", "fasting insulin (iv0inavg): 5.2", "si: 3.84", "waist hip ratio: 0.791", "Gene expression of skeletal muscle from subject without diabetes", "148", "24", "20", "Family history positive - 2 parents", "89", "94.5", "5.2", "male", "5.4", "3.84", "0.791", "optimal", "normal levels" ], [ "30", "GSM624954", "Muscle-FamilyHistoryPositive-17", "Mar 01 2011", "gender: male", "age (years): 46", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 23", "fasting glucose (ogtt): 90", "2 hour glucose (ogtt): 105", "hemoglobin a1c: 5.2", "fasting glucose (iv0gavg): 91", "fasting insulin (iv0inavg): 7.7", "si: 6.11", "waist hip ratio: 0.953", "Gene expression of skeletal muscle from subject without diabetes", "105", "46", "23", "Family history positive - 2 parents", "91", "90", "7.7", "male", "5.2", "6.11", "0.953", "optimal", "normal levels" ], [ "31", "GSM624955", "Muscle-FamilyHistoryPositive-18", "Mar 01 2011", "gender: female", "age (years): 60", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 32", "fasting glucose (ogtt): 94.5", "2 hour glucose (ogtt): 163", "hemoglobin a1c: 5.7", "fasting glucose (iv0gavg): 102", "fasting insulin (iv0inavg): 16.7", "si: 0.91", "waist hip ratio: 0.911", "Gene expression of skeletal muscle from subject without diabetes", "163", "60", "32", "Family history positive - 2 parents", "102", "94.5", "16.7", "female", "5.7", "0.91", "0.911", "diabetic", "normal levels" ], [ "32", "GSM624956", "Muscle-FamilyHistoryPositive-19", "Mar 01 2011", "gender: female", "age (years): 25", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 22", "fasting glucose (ogtt): 90", "2 hour glucose (ogtt): 36", "hemoglobin a1c: 4.6", "fasting glucose (iv0gavg): 88", "fasting insulin (iv0inavg): 8.6", "si: 4.02", "waist hip ratio: 0.71", "Gene expression of skeletal muscle from subject without diabetes", "36", "25", "22", "Family history positive - 2 parents", "88", "90", "8.6", "female", "4.6", "4.02", "0.71", "diabetic", "normal levels" ], [ "33", "GSM624957", "Muscle-FamilyHistoryPositive-20", "Mar 01 2011", "gender: female", "age (years): 43", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 83.5", "2 hour glucose (ogtt): 90", "hemoglobin a1c: 4.6", "fasting glucose (iv0gavg): 77", "fasting insulin (iv0inavg): 1.5", "si: 8.43", "waist hip ratio: 0.784", "Gene expression of skeletal muscle from subject without diabetes", "90", "43", "25", "Family history positive - 2 parents", "77", "83.5", "1.5", "female", "4.6", "8.43", "0.784", "low", "normal levels" ], [ "34", "GSM624958", "Muscle-FamilyHistoryPositive-21", "Mar 01 2011", "gender: male", "age (years): 46", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 31", "fasting glucose (ogtt): 99.5", "2 hour glucose (ogtt): 144", "hemoglobin a1c: 4.8", "fasting glucose (iv0gavg): 98", "fasting insulin (iv0inavg): 10.5", "si: 2.02", "waist hip ratio: 0.968", "Gene expression of skeletal muscle from subject without diabetes", "144", "46", "31", "Family history positive - 2 parents", "98", "99.5", "10.5", "male", "4.8", "2.02", "0.968", "diabetic", "normal levels" ], [ "35", "GSM624959", "Muscle-FamilyHistoryPositive-22", "Mar 01 2011", "gender: male", "age (years): 52", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 30", "fasting glucose (ogtt): 91.5", "2 hour glucose (ogtt): 118", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 90", "fasting insulin (iv0inavg): 11", "si: 1.71", "waist hip ratio: 0.985", "Gene expression of skeletal muscle from subject without diabetes", "118", "52", "30", "Family history positive - 2 parents", "90", "91.5", "11", "male", "5.4", "1.71", "0.985", "diabetic", "normal levels" ], [ "36", "GSM624960", "Muscle-FamilyHistoryPositive-23", "Mar 01 2011", "gender: male", "age (years): 40", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 35", "fasting glucose (ogtt): 98", "2 hour glucose (ogtt): 155", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 101", "fasting insulin (iv0inavg): 13.8", "si: 2.2", "waist hip ratio: 0.958", "Gene expression of skeletal muscle from subject without diabetes", "155", "40", "35", "Family history positive - 2 parents", "101", "98", "13.8", "male", "5.4", "2.2", "0.958", "diabetic", "normal levels" ], [ "37", "GSM624961", "Muscle-Diabetes-1", "Mar 01 2011", "gender: male", "age (years): 62", "family history: DM", "body mass index (kg/m2): 28", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 5.9", "fasting glucose (iv0gavg): 105", "fasting insulin (iv0inavg): 24.75", "si: 0.49", "waist hip ratio: 0.88", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "62", "28", "DM", "105", "NA", "24.75", "male", "5.9", "0.49", "0.88", "diabetic", "normal levels" ], [ "38", "GSM624962", "Muscle-Diabetes-2", "Mar 01 2011", "gender: female", "age (years): 52", "family history: DM", "body mass index (kg/m2): 37", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 5.5", "fasting glucose (iv0gavg): 94", "fasting insulin (iv0inavg): 21.1", "si: 1.34", "waist hip ratio: 0.923", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "52", "37", "DM", "94", "NA", "21.1", "female", "5.5", "1.34", "0.923", "diabetic", "normal levels" ], [ "39", "GSM624963", "Muscle-Diabetes-3", "Mar 01 2011", "gender: female", "age (years): 55", "family history: DM", "body mass index (kg/m2): 44", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 5.3", "fasting glucose (iv0gavg): 88", "fasting insulin (iv0inavg): 12.9", "si: 1.22", "waist hip ratio: 0.905", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "55", "44", "DM", "88", "NA", "12.9", "female", "5.3", "1.22", "0.905", "diabetic", "normal levels" ], [ "40", "GSM624964", "Muscle-Diabetes-4", "Mar 01 2011", "gender: male", "age (years): 60", "family history: DM", "body mass index (kg/m2): 27", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 8.5", "fasting glucose (iv0gavg): 119", "fasting insulin (iv0inavg): 5.3", "si: NA", "waist hip ratio: 0.94", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "60", "27", "DM", "119", "NA", "5.3", "male", "8.5", "NA", "0.94", "optimal", "diabetic levels" ], [ "41", "GSM624965", "Muscle-Diabetes-5", "Mar 01 2011", "gender: male", "age (years): 36", "family history: DM", "body mass index (kg/m2): 29", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 6.6", "fasting glucose (iv0gavg): 121", "fasting insulin (iv0inavg): 15.7", "si: 1.43", "waist hip ratio: 1.085", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "36", "29", "DM", "121", "NA", "15.7", "male", "6.6", "1.43", "1.085", "diabetic", "diabetic levels" ], [ "42", "GSM624966", "Muscle-Diabetes-6", "Mar 01 2011", "gender: male", "age (years): 59", "family history: DM", "body mass index (kg/m2): 25", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 9.2", "fasting glucose (iv0gavg): 222", "fasting insulin (iv0inavg): 10.95", "si: 2.07", "waist hip ratio: 0.953", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "59", "25", "DM", "222", "NA", "10.95", "male", "9.2", "2.07", "0.953", "diabetic", "diabetic levels" ], [ "43", "GSM624967", "Muscle-Diabetes-7", "Mar 01 2011", "gender: female", "age (years): 26", "family history: DM", "body mass index (kg/m2): 49", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 7.8", "fasting glucose (iv0gavg): 185", "fasting insulin (iv0inavg): 94.38", "si: NA", "waist hip ratio: 0.886", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "26", "49", "DM", "185", "NA", "94.38", "female", "7.8", "NA", "0.886", "diabetic", "diabetic levels" ], [ "44", "GSM624968", "Muscle-Diabetes-8", "Mar 01 2011", "gender: female", "age (years): 35", "family history: DM", "body mass index (kg/m2): 28", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 8.5", "fasting glucose (iv0gavg): 213", "fasting insulin (iv0inavg): 10.93", "si: 3.6", "waist hip ratio: 0.906", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "35", "28", "DM", "213", "NA", "10.93", "female", "8.5", "3.6", "0.906", "diabetic", "diabetic levels" ], [ "45", "GSM624969", "Muscle-Diabetes-9", "Mar 01 2011", "gender: female", "age (years): 51", "family history: DM", "body mass index (kg/m2): 27", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 6.6", "fasting glucose (iv0gavg): 101", "fasting insulin (iv0inavg): 33", "si: 3.79", "waist hip ratio: 0.822", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "51", "27", "DM", "101", "NA", "33", "female", "6.6", "3.79", "0.822", "diabetic", "diabetic levels" ], [ "46", "GSM624970", "Muscle-Diabetes-10", "Mar 01 2011", "gender: female", "age (years): 67", "family history: DM", "body mass index (kg/m2): 34", "fasting glucose (ogtt): NA", "2 hour glucose (ogtt): NA", "hemoglobin a1c: 6.4", "fasting glucose (iv0gavg): 102", "fasting insulin (iv0inavg): 17.8", "si: 0.58", "waist hip ratio: 0.963", "Gene expression of skeletal muscle from subject with type 2 diabetes", "NA", "67", "34", "DM", "102", "NA", "17.8", "female", "6.4", "0.58", "0.963", "diabetic", "pre-diabetic levels" ], [ "47", "GSM624971", "Muscle-FamilyHistoryNegative-14", "Mar 01 2011", "gender: male", "age (years): 50", "family history: Family history negative", "body mass index (kg/m2): 25", "fasting glucose (ogtt): 84", "2 hour glucose (ogtt): 65", "hemoglobin a1c: 5.4", "fasting glucose (iv0gavg): 93", "fasting insulin (iv0inavg): 5.3", "si: 4.67", "waist hip ratio: 0.929", "Gene expression of skeletal muscle from subject without diabetes", "65", "50", "25", "Family history negative", "93", "84", "5.3", "male", "5.4", "4.67", "0.929", "optimal", "normal levels" ], [ "48", "GSM624972", "Muscle-FamilyHistoryPositive-24", "Mar 01 2011", "gender: male", "age (years): 34", "family history: Family history positive - 2 parents", "body mass index (kg/m2): 24", "fasting glucose (ogtt): 99.5", "2 hour glucose (ogtt): 146", "hemoglobin a1c: 4.7", "fasting glucose (iv0gavg): 105", "fasting insulin (iv0inavg): 9.8", "si: 3.25", "waist hip ratio: 0.903", "Gene expression of skeletal muscle from subject without diabetes", "146", "34", "24", "Family history positive - 2 parents", "105", "99.5", "9.8", "male", "4.7", "3.25", "0.903", "diabetic", "normal levels" ], [ "49", "GSM624973", "Muscle-FamilyHistoryNegative-15", "Mar 01 2011", "gender: male", "age (years): 52", "family history: Family history negative", "body mass index (kg/m2): 27", "fasting glucose (ogtt): 91.5", "2 hour glucose (ogtt): 117", "hemoglobin a1c: 4.9", "fasting glucose (iv0gavg): 94", "fasting insulin (iv0inavg): 4.2", "si: 6.28", "waist hip ratio: 0.828", "Gene expression of skeletal muscle from subject without diabetes", "117", "52", "27", "Family history negative", "94", "91.5", "4.2", "male", "4.9", "6.28", "0.828", "optimal", "normal levels" ], [ "50", "GSM624974", "Muscle-FamilyHistoryPositive-25", "Mar 01 2011", "gender: female", "age (years): 49", "family history: Family history positive - 1 parent", "body mass index (kg/m2): 36", "fasting glucose (ogtt): 88.5", "2 hour glucose (ogtt): 99", "hemoglobin a1c: 5.3", "fasting glucose (iv0gavg): 94", "fasting insulin (iv0inavg): 4.5", "si: 6.76", "waist hip ratio: 0.853", "Gene expression of skeletal muscle from subject without diabetes", "99", "49", "36", "Family history positive - 1 parent", "94", "88.5", "4.5", "female", "5.3", "6.76", "0.853", "optimal", "normal levels" ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
    for(i=0; i<ssrules.length; i++) {
        if (ssrules[i].selectorText == (".aqm" + reportObjId)) {
		ssrules[i].style.cssText = cssText[0+status];
		break;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
