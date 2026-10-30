-- Minutes spent in the department by attendances that ended in admission.
SELECT CAST(arrival AS DATE) AS event_date,
       minutes_in_department AS value
FROM derived.ed_attendances
WHERE admitted
  AND minutes_in_department IS NOT NULL
