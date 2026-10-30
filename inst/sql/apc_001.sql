-- Spells admitted as an emergency, dated by admission.
SELECT CAST(admidate AS DATE) AS event_date,
       sitetret AS site,
       tretspef_first AS specialty
FROM derived.spells
WHERE emergency
