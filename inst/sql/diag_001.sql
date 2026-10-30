-- Imaging exams requested from ED, dated by request.
SELECT CAST(request_datetime AS DATE) AS event_date
FROM derived.imaging_exams
WHERE patient_source_setting = '05'
  AND request_datetime IS NOT NULL
