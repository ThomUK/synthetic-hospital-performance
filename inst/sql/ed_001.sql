-- One row per ED attendance, dated by arrival.
SELECT CAST(arrival AS DATE) AS event_date
FROM derived.ed_attendances
