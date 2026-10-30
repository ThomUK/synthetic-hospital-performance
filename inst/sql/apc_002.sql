-- Closed, non-day-case spells, dated by discharge.
SELECT CAST(disdate AS DATE) AS event_date,
       sitetret AS site,
       tretspef_last AS specialty
FROM derived.spells
WHERE NOT open_spell
  AND NOT day_case
  AND disdate IS NOT NULL
  AND disdate >= admidate
