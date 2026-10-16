-- Proof that the step claim is atomic, run directly against the database.
-- The C++ repository issues exactly this statement; this shows its behaviour
-- without waiting for a build.
--
--   ./compass.sh sql -- -o .audit/claim_proof.txt -f .audit/claim_proof.sql
--
-- Every SELECT labels itself, so the transcript reads on its own.

\set step  '''00000000-0000-4000-8000-00000000c1a1'''
\set tenant '''00000000-0000-4000-8000-00000000c1b2'''

-- Clean any earlier run of this proof, so it is safe to re-run.
delete from ores_workflow_step_claims_tbl where step_id = :step;

-- 1. The first delivery claims. A row comes back, with one attempt.
with claimed as (
    insert into ores_workflow_step_claims_tbl
        (step_id, work_tenant_id, owner, lease_expires_at, attempts)
    values (:step::uuid, :tenant::uuid, 'first/1', now() + make_interval(secs => 300), 1)
    on conflict (step_id) do update
        set owner = excluded.owner,
            claimed_at = now(),
            lease_expires_at = excluded.lease_expires_at,
            attempts = ores_workflow_step_claims_tbl.attempts + 1
        where ores_workflow_step_claims_tbl.lease_expires_at <= now()
    returning step_id, owner, attempts
)
select '1 first claim' as proof, step_id, owner, attempts from claimed;

-- 2. A second delivery asks while the lease is live. Nothing comes back: it
--    does not own the step and must not do its work. An empty result here is
--    the pass condition, not a failure.
with claimed as (
    insert into ores_workflow_step_claims_tbl
        (step_id, work_tenant_id, owner, lease_expires_at, attempts)
    values (:step::uuid, :tenant::uuid, 'second/2', now() + make_interval(secs => 300), 1)
    on conflict (step_id) do update
        set owner = excluded.owner,
            claimed_at = now(),
            lease_expires_at = excluded.lease_expires_at,
            attempts = ores_workflow_step_claims_tbl.attempts + 1
        where ores_workflow_step_claims_tbl.lease_expires_at <= now()
    returning step_id, owner, attempts
)
select '2 second claim while the lease is live' as proof, count(*) as rows_returned from claimed;

-- 3. The holder is unchanged, and the attempt count did not move.
select '3 holder after the refused claim' as proof, owner, attempts
from ores_workflow_step_claims_tbl where step_id = :step;

-- 4. The holder died and its lease ran out.
update ores_workflow_step_claims_tbl
set lease_expires_at = now() - interval '1 second'
where step_id = :step;

-- 5. The same statement now takes the claim over, and the attempt count says
--    this is the second attempt at the step.
with claimed as (
    insert into ores_workflow_step_claims_tbl
        (step_id, work_tenant_id, owner, lease_expires_at, attempts)
    values (:step::uuid, :tenant::uuid, 'second/2', now() + make_interval(secs => 300), 1)
    on conflict (step_id) do update
        set owner = excluded.owner,
            claimed_at = now(),
            lease_expires_at = excluded.lease_expires_at,
            attempts = ores_workflow_step_claims_tbl.attempts + 1
        where ores_workflow_step_claims_tbl.lease_expires_at <= now()
    returning step_id, owner, attempts
)
select '5 takeover after the lease ended' as proof, step_id, owner, attempts from claimed;

-- 6. The engine closes the window when the step leaves in progress. It releases
--    without naming a holder, because the step's own state is what refuses a
--    replay from here on.
delete from ores_workflow_step_claims_tbl where step_id = :step;

select '6 rows left after release' as proof, count(*) as rows_left
from ores_workflow_step_claims_tbl where step_id = :step;
