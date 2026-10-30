-- ED attendances that did not end in admission, dated by arrival.
SELECT CAST(arrival AS DATE) AS event_date,
       arrival_mode
FROM derived.ed_attendances
WHERE NOT admitted
