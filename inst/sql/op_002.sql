-- Appointments the patient did not attend.
SELECT CAST(apptdate AS DATE) AS event_date,
       sitetret AS site,
       tretspef AS specialty
FROM derived.op_appointments
WHERE attended IN ('3', '7')
