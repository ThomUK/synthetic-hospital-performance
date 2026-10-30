-- Open pathways that had waited 18 weeks or less at each month end.
WITH month_ends AS (
  SELECT CAST(last_day(d) AS DATE) AS month_end
  FROM generate_series(date_trunc('month', {{window_start}}), {{window_end}}, INTERVAL 1 MONTH) AS t(d)
  WHERE last_day(d) <= {{window_end}}
)
SELECT m.month_end AS event_date,
       p.tretspef AS specialty
FROM derived.rtt_pathways AS p
JOIN month_ends AS m
  ON p.rttperstart <= m.month_end
 AND (p.rttperend IS NULL OR p.rttperend > m.month_end)
WHERE date_diff('day', p.rttperstart, m.month_end) <= 126
