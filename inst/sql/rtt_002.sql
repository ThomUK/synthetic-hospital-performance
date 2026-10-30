-- Open pathways that had waited 18 weeks or less on each snapshot date.
-- {{snapshot_dates}} is supplied by the package as a list of dates.
WITH snapshots AS (
  SELECT unnest({{snapshot_dates}}) AS snapshot_date
)
SELECT s.snapshot_date AS event_date,
       p.tretspef AS specialty
FROM derived.rtt_pathways AS p
JOIN snapshots AS s
  ON p.rttperstart <= s.snapshot_date
 AND (p.rttperend IS NULL OR p.rttperend > s.snapshot_date)
WHERE date_diff('day', p.rttperstart, s.snapshot_date) <= 126
