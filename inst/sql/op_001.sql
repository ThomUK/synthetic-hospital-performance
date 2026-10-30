-- Appointments attended or not attended; cancellations excluded.
SELECT CAST(apptdate AS DATE) AS event_date,
       sitetret AS site,
       tretspef AS specialty
FROM derived.op_appointments
WHERE attended IN ('3', '5', '6', '7')
