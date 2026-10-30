-- Minutes spent in the department by attendances that ended in admission.
SELECT CAST(arrival AS DATE) AS event_date,
       minutes_in_department AS value,
       arrival_mode,
       tfc_decision_to_admit AS specialty
FROM derived.ed_attendances
WHERE admitted
  AND minutes_in_department IS NOT NULL
