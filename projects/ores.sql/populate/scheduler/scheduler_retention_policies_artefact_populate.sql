/**
 * Retention Policies Artefact Population Script
 *
 * Derives the policies the installation ships with from the default bounds the
 * store catalogue states. A policy is created for every store whose bound is a
 * policy, with the period, the cadence and the reason the catalogue gives, so
 * one statement of a default serves both and the two cannot drift apart.
 *
 * The code names the store and the period, which is how an operator reads a
 * policy without a join: report_cashflows_90d is the cashflows store kept for a
 * quarter.
 */

\echo '--- Retention Policies Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl
where code = 'scheduler.retention_policies'
  and valid_to = ores_utility_infinity_timestamp_fn() \gset

select id as v_stores_dataset_id from ores_dq_datasets_tbl
where code = 'scheduler.retained_stores'
  and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_retention_policies_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_retention_policies_artefact_tbl (
    tenant_id, dataset_id, code, version, store_code, target_kind, target,
    action, retention_days, cadence_hours, enabled, description
)
select
    s.tenant_id,
    :'v_dataset_id',
    regexp_replace(s.code, '^.*\.', '') || '_' || s.default_retention_days || 'd',
    0,
    s.code,
    s.target_kind,
    s.target,
    s.default_action,
    s.default_retention_days,
    s.default_cadence_hours,
    true,
    s.default_reason
from ores_dq_retained_stores_artefact_tbl s
where s.dataset_id = :'v_stores_dataset_id'
  and s.tenant_id = ores_utility_system_tenant_id_fn()
  and s.bound_kind = 'policy'
order by s.code;
