-- Spells admitted as an emergency, dated by admission.
SELECT CAST(admidate AS DATE) AS event_date
FROM derived.spells
WHERE emergency
