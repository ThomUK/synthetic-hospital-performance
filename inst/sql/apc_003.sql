-- Minutes from admission to discharge for closed, non-day-case spells.
SELECT CAST(disdate AS DATE) AS event_date,
       date_diff('minute', admidate, disdate) AS value
FROM derived.spells
WHERE NOT open_spell
  AND NOT day_case
  AND disdate IS NOT NULL
  AND disdate >= admidate
