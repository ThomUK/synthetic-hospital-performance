-- ED attendances that ended in admission, dated by arrival.
SELECT CAST(arrival AS DATE) AS event_date
FROM derived.ed_attendances
WHERE admitted
