\pset pager off
-- The scheduler's own record of firing the expiry job, and the job row it
-- fires from. The inbox wrote the row at start; the scheduler read it and
-- fired it on the schedule the variability setting states.
select d.job_name, d.schedule_expression, d.action_type, d.action_payload,
       i.status, i.triggered_at, i.completed_at, i.error_message
from ores_scheduler_job_instances_tbl i
join ores_scheduler_job_definitions_tbl d on d.id = i.job_definition_id
where d.job_name = 'ores.inbox.approval_expiry'
order by i.triggered_at desc
limit 5;
