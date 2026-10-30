-- Minutes spent in the department by attendances that did not end in admission.
SELECT CAST(arrival AS DATE) AS event_date,
       minutes_in_department AS value,
       arrival_mode
FROM derived.ed_attendances
WHERE NOT admitted
  AND minutes_in_department IS NOT NULL
