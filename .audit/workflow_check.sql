-- Whether every workflow run reached a terminal state, and which steps did not.
-- Run after a provision or a report trigger:
--   ./compass.sh sql -- -f .audit/workflow_check.sql

\echo 'Instances by type and state'
select wf.type, coalesce(st.name, 'unknown') as state, count(*) as instances
from ores_workflow_workflow_instances_tbl wf
left join ores_dq_fsm_states_tbl st on st.id = wf.state_id
where wf.valid_to > now()
group by 1, 2
order by 1, 2;

\echo 'Steps that are not in a terminal state'
select wf.type, s.step_index, s.name as step, coalesce(st.name, 'unknown') as state,
       left(coalesce(s.error, ''), 60) as err
from ores_workflow_workflow_steps_tbl s
join ores_workflow_workflow_instances_tbl wf
  on wf.id = s.workflow_id and wf.valid_to > now()
left join ores_dq_fsm_states_tbl st on st.id = s.state_id
where s.valid_to > now()
  and coalesce(st.name, 'unknown') not in ('completed', 'completed_with_warnings', 'failed')
order by wf.valid_from desc, s.step_index;
