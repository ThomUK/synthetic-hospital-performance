-- Closed, non-day-case spells, dated by discharge.
SELECT CAST(disdate AS DATE) AS event_date
FROM derived.spells
WHERE NOT open_spell
  AND NOT day_case
  AND disdate IS NOT NULL
  AND disdate >= admidate
