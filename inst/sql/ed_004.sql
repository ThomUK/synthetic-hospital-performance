-- ED attendances that ended in admission, dated by arrival.
SELECT CAST(arrival AS DATE) AS event_date,
       arrival_mode,
       tfc_decision_to_admit AS specialty
FROM derived.ed_attendances
WHERE admitted
