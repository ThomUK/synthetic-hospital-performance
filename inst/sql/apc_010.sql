-- Live inpatient discharges on discharge pathway P2, dated by discharge.
-- derived.spells normalises the pathway to P0-P3; anything else is null.
SELECT CAST(disdate AS DATE) AS event_date,
       sitetret AS site,
       tretspef_last AS specialty
FROM derived.spells
WHERE NOT open_spell
  AND NOT day_case
  AND disdate IS NOT NULL
  AND disdate >= admidate
  AND NOT coalesce(died, FALSE)
  AND discharge_pathway = 'P2'
