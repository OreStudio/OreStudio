-- The properties task 0349A777 asks for, read straight off the store. Every
-- statement answers a question a poller asks; none of them is a proxy.

\echo '--- current rows by instance state, with the times each carries ---'
select s.name as instance_state, count(*) as instances,
       count(i.last_event_at) as with_last_event_at,
       count(i.completed_at) as with_completed_at
from ores_workflow_workflow_instances_tbl i
join ores_dq_fsm_states_tbl s on s.id = i.state_id
where i.valid_to = ores_utility_infinity_timestamp_fn()
group by 1 order by 2 desc;

\echo '--- any terminal run left without a completion time? ---'
select count(*) as terminal_without_completed_at
from ores_workflow_workflow_instances_tbl i
join ores_dq_fsm_states_tbl s on s.id = i.state_id
where i.valid_to = ores_utility_infinity_timestamp_fn()
  and s.name in ('completed', 'failed', 'compensated')
  and i.completed_at is null;

\echo '--- the completed_at poll: what a shell waits on ---'
select count(*) as finished_runs
from ores_workflow_workflow_instances_tbl i
join ores_dq_fsm_states_tbl s on s.id = i.state_id
where i.valid_to = ores_utility_infinity_timestamp_fn()
  and s.name = 'completed'
  and i.completed_at is not null;

\echo '--- the abandoned-run read: what is live is what was touched recently ---'
select s.name as instance_state, count(*) as instances,
       min(i.last_event_at) as oldest_event,
       max(i.last_event_at) as newest_event
from ores_workflow_workflow_instances_tbl i
join ores_dq_fsm_states_tbl s on s.id = i.state_id
where i.valid_to = ores_utility_infinity_timestamp_fn()
group by 1;

\echo '--- current steps by state, with the times each carries ---'
select s.name as step_state, count(*) as steps,
       count(st.started_at) as with_started_at,
       count(st.completed_at) as with_completed_at
from ores_workflow_workflow_steps_tbl st
join ores_dq_fsm_states_tbl s on s.id = st.state_id
where st.valid_to = ores_utility_infinity_timestamp_fn()
group by 1 order by 2 desc;

\echo '--- any step in a terminal state left without a completion time? ---'
select count(*) as terminal_without_completed_at
from ores_workflow_workflow_steps_tbl st
join ores_dq_fsm_states_tbl s on s.id = st.state_id
where st.valid_to = ores_utility_infinity_timestamp_fn()
  and s.name in ('completed', 'completed_with_warnings', 'failed', 'compensated')
  and st.completed_at is null;
