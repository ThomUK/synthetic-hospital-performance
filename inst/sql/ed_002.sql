-- ED attendances that left within four hours of arrival, dated by arrival.
SELECT CAST(arrival AS DATE) AS event_date,
       arrival_mode
FROM derived.ed_attendances
WHERE within_4h
