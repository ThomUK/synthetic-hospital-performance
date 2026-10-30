-- Live discharges followed by an emergency admission within 30 days,
-- dated by the index discharge (the same date the denominator uses).
SELECT CAST(d.disdate AS DATE) AS event_date
FROM derived.spells AS d
WHERE NOT d.open_spell
  AND NOT d.day_case
  AND d.disdate IS NOT NULL
  AND d.disdate >= d.admidate
  AND NOT coalesce(d.died, FALSE)
  AND EXISTS (
    SELECT 1
    FROM derived.spells AS r
    WHERE r.person_id = d.person_id
      AND r.provspno <> d.provspno
      AND r.emergency
      AND r.admidate > d.disdate
      AND r.admidate <= d.disdate + INTERVAL 30 DAY
  )
