-- SPDX-License-Identifier: GPL-3.0-or-later
-- Run inside the disposable clone. Existing report figures are preserved.
begin;

-- The recorded Greek fixture used prepared inputs (substitute). For the
-- executable risk case, version only that selected, unscheduled test definition
-- in the clone so OreStudio generates its own inputs using the same settings.
with updated as (
    insert into ores_reporting_report_definitions_tbl
    select (jsonb_populate_record(null::ores_reporting_report_definitions_tbl,
        to_jsonb(d) || jsonb_build_object(
            'pre_processing', 'execute',
            'prepared_input_key', null,
            'change_reason_code', 'system.update',
            'change_commentary', 'Generate risk report inputs in the disposable agent test'))).*
    from ores_reporting_report_definitions_tbl d
    where d.tenant_id = :'tenant'::uuid and d.party_id = :'party'::uuid
      and d.name = :'riskDefinition'
      and d.valid_to = ores_utility_infinity_timestamp_fn()
      and d.report_type = 'risk'
      and d.pre_processing = 'substitute' and d.post_processing = 'execute'
      and d.scheduler_job_id is null and d.schedule_expression = ''
      and d.cut_id is null and d.cut_kind_code is null
      and exists (
          select 1
          from ores_reporting_report_configurations_tbl b
          join ores_reporting_risk_report_configs_tbl r
            on r.tenant_id = b.tenant_id and r.party_id = b.party_id
           and r.configuration_id = b.configuration_id
          join ores_reporting_configuration_analytics_tbl a
            on a.tenant_id = b.tenant_id and a.configuration_id = b.configuration_id
          where b.tenant_id = d.tenant_id and b.party_id = d.party_id
            and b.report_definition_id = d.id
            and b.configuration_type_code = 'risk_report_config'
            and b.valid_to = ores_utility_infinity_timestamp_fn()
            and r.valid_to = ores_utility_infinity_timestamp_fn()
            and a.valid_to = ores_utility_infinity_timestamp_fn()
            and r.market_data_type = 'date' and r.market_data_date = :'riskDate'::date
            and a.analytic_type_code = 'sensitivity' and a.active = 'Y'
      )
    returning id, tenant_id, party_id
), scope_added as (
    insert into ores_reporting_risk_report_config_portfolios_tbl
        (tenant_id, risk_report_config_id, portfolio_id, valid_from, valid_to)
    select d.tenant_id, r.id, p.id, clock_timestamp(), ores_utility_infinity_timestamp_fn()
    from updated d
    join ores_reporting_report_configurations_tbl b
      on b.tenant_id = d.tenant_id and b.party_id = d.party_id
     and b.report_definition_id = d.id and b.configuration_type_code = 'risk_report_config'
     and b.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_reporting_risk_report_configs_tbl r
      on r.tenant_id = b.tenant_id and r.party_id = b.party_id
     and r.configuration_id = b.configuration_id
     and r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_refdata_portfolios_tbl p
      on p.tenant_id = d.tenant_id and p.party_id = d.party_id
     and p.name = :'portfolio' and p.valid_to = ores_utility_infinity_timestamp_fn()
    where not exists (
        select 1 from ores_reporting_risk_report_config_books_tbl s
        where s.tenant_id = d.tenant_id and s.risk_report_config_id = r.id
          and s.valid_to = ores_utility_infinity_timestamp_fn()
    ) and not exists (
        select 1 from ores_reporting_risk_report_config_portfolios_tbl s
        where s.tenant_id = d.tenant_id and s.risk_report_config_id = r.id
          and s.valid_to = ores_utility_infinity_timestamp_fn()
    )
    returning portfolio_id
)
select set_config('ores_agent.risk_definition_prepared', exists(select 1 from updated)::text, true) as risk_prepared,
       set_config('ores_agent.risk_portfolio_added', exists(select 1 from scope_added)::text, true) as risk_scope_added
\gset

-- Copy the clean NPV run and its metrics once for the partial-result case.
select set_config('ores_agent.seed_tenant', :'tenant', true) as seed_tenant,
       set_config('ores_agent.seed_party', :'party', true) as seed_party,
       set_config('ores_agent.seed_run', :'run', true) as seed_run,
       set_config('ores_agent.seed_partial_run', :'partialRun', true) as seed_partial_run
\gset

-- The native report lifecycle rejects inserts that start in a terminal state.
-- Replay the original run's stored state transitions through the normal triggers.
do $seed$
declare
    v_tenant uuid := current_setting('ores_agent.seed_tenant')::uuid;
    v_party uuid := current_setting('ores_agent.seed_party')::uuid;
    v_run uuid := current_setting('ores_agent.seed_run')::uuid;
    v_partial uuid := current_setting('ores_agent.seed_partial_run')::uuid;
    v_version integer := 0;
    r ores_reporting_report_instances_tbl%rowtype;
begin
    perform set_config('ores_agent.seeded_partial_run', '', true);
    if exists (
        select 1 from ores_reporting_report_instances_tbl p
        where p.tenant_id = v_tenant and p.id = v_partial
          and p.valid_to = ores_utility_infinity_timestamp_fn()
    ) or not exists (
        select 1 from ores_reporting_report_instances_tbl p
        where p.tenant_id = v_tenant and p.party_id = v_party and p.id = v_run
          and p.completed_at is not null
          and p.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return;
    end if;

    for r in
        select p.* from ores_reporting_report_instances_tbl p
        where p.tenant_id = v_tenant and p.party_id = v_party and p.id = v_run
        order by p.version
    loop
        insert into ores_reporting_report_instances_tbl
        select (jsonb_populate_record(null::ores_reporting_report_instances_tbl,
            to_jsonb(r) || jsonb_build_object(
                'id', v_partial, 'version', v_version,
                'name', 'Agent integration partial ' || v_partial::text,
                'description', 'Synthetic completed-with-errors copy of ' || v_run::text,
                'output_message', 'Synthetic integration test error',
                'output_storage_key', null))).*
        returning version into v_version;
    end loop;

    perform set_config('ores_agent.seeded_partial_run', v_partial::text, true);
end
$seed$;

insert into ores_reporting_report_metrics_tbl
select (jsonb_populate_record(null::ores_reporting_report_metrics_tbl,
    to_jsonb(m) || jsonb_build_object('id', gen_random_uuid(), 'report_instance_id', :'partialRun'))).*
from ores_reporting_report_metrics_tbl m
where current_setting('ores_agent.seeded_partial_run') <> ''
  and m.tenant_id = :'tenant'::uuid and m.party_id = :'party'::uuid
  and m.report_instance_id = :'run'::uuid and m.family = 'npv_report'
  and m.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_report_error_events_tbl
    (id, tenant_id, report_instance_id, party_id, scope_code, severity, message, event_time)
select gen_random_uuid(), tenant_id, id, party_id, 'Report', 'error',
       'Synthetic error for the disposable agent integration test', completed_at
from ores_reporting_report_instances_tbl
where id = nullif(current_setting('ores_agent.seeded_partial_run'), '')::uuid
  and tenant_id = :'tenant'::uuid and party_id = :'party'::uuid
  and valid_to = ores_utility_infinity_timestamp_fn();

-- Derive the trades from the portfolio and its booking history at run start.
-- Equal synthetic contributions make the reference total independent of row order.
create temporary table agent_risk_seed on commit drop as
with recursive risk_run as (
    select * from ores_reporting_report_instances_tbl
    where tenant_id = :'tenant'::uuid and party_id = :'party'::uuid
      and id = :'riskRun'::uuid and started_at is not null and completed_at is not null
      and valid_to = ores_utility_infinity_timestamp_fn()
), portfolios(id) as (
    select p.id
    from ores_refdata_portfolios_tbl p cross join risk_run r
    where p.tenant_id = r.tenant_id and p.party_id = r.party_id and p.name = :'portfolio'
      and p.valid_from <= r.started_at and p.valid_to > r.started_at
    union
    select p.id
    from portfolios parent join ores_refdata_portfolios_tbl p on p.parent_portfolio_id = parent.id
    cross join risk_run r
    where p.tenant_id = r.tenant_id and p.party_id = r.party_id
      and p.valid_from <= r.started_at and p.valid_to > r.started_at
)
select distinct b.trade_id, r.tenant_id, r.party_id, r.id, r.definition_id, r.priced_at
from risk_run r
join ores_trading_trade_bookings_tbl b on b.tenant_id = r.tenant_id
    and b.valid_from <= r.started_at and b.valid_to > r.started_at
join ores_refdata_books_tbl book on book.tenant_id = r.tenant_id and book.party_id = r.party_id
    and book.id = b.book_id and book.valid_from <= r.started_at and book.valid_to > r.started_at
join portfolios p on p.id = book.parent_portfolio_id;

-- Add two vega buckets only when this run has no EUR/USD 3M volatility rows.
create temporary table agent_seed_decisions on commit drop as
select count(*) as trades,
       not exists (
           select 1 from ores_reporting_report_metrics_tbl m
           where m.tenant_id = :'tenant'::uuid and m.party_id = :'party'::uuid
             and m.report_instance_id = :'riskRun'::uuid and m.measure = 'Delta'
             and m.family = 'sensitivity_report'
             and m.valid_to = ores_utility_infinity_timestamp_fn()
             and m.qualifiers::jsonb ->> 'Factor_1' like 'FXVolatility/EURUSD/%/3M/%'
       ) as seed_vega,
       not exists (
           select 1 from ores_reporting_report_metrics_tbl m
           where m.tenant_id = :'tenant'::uuid and m.party_id = :'party'::uuid
             and m.report_instance_id = :'riskRun'::uuid and m.scenario = :'scenario'
             and m.family = 'sensitivity_report' and m.valid_to = ores_utility_infinity_timestamp_fn()
       ) as seed_scenario
from agent_risk_seed;

with dimensions as (
    select * from (values
        ('FXVolatility/EURUSD/122/3M/-0.05', '', 2, '0.0001'),
        ('FXVolatility/EURUSD/134/3M/ATM', '', 4, '0.0001'),
        ('DiscountCurve/GBP/100/1Y', :'scenario', 3, '0.0001'),
        ('DiscountCurve/GBP/101/2Y', :'scenario', 3, '0.0001'),
        ('DiscountCurve/GBP/102/3Y', :'scenario', 3, '0.0001'),
        ('DiscountCurve/GBP/103/4Y', :'scenario', 3, '0.0001'),
        ('DiscountCurve/GBP/104/5Y', :'scenario', 3, '0.0001'),
        ('DiscountCurve/GBP/105/6Y', :'scenario', 3, '0.0001')
    ) as d(factor, scenario, amount, shift)
)
insert into ores_reporting_report_metrics_tbl
    (id, tenant_id, party_id, report_instance_id, result_id, as_of, measure, source,
     line, report_definition_id, family, subject_type, subject_id, currency, scenario,
     factor, qualifiers, value, valid_from, valid_to)
select gen_random_uuid(), s.tenant_id, s.party_id, s.id, gen_random_uuid(), s.priced_at,
       'Delta', 'sensitivity.csv', 2, s.definition_id, 'sensitivity_report',
       'trade', s.trade_id::text, 'GBP', d.scenario, '',
       json_build_object('IsPar', 'false', 'Factor_1', d.factor, 'ShiftSize_1', d.shift,
                         'Factor_2', '', 'ShiftSize_2', '0', 'Currency', 'GBP')::text,
       d.amount, current_timestamp, ores_utility_infinity_timestamp_fn()
from agent_risk_seed s cross join dimensions d cross join agent_seed_decisions decision
where decision.trades >= 2
  and ((d.scenario = '' and decision.seed_vega) or (d.scenario <> '' and decision.seed_scenario));

select json_build_object(
    'riskDefinitionPrepared', current_setting('ores_agent.risk_definition_prepared')::boolean,
    'riskPortfolioAdded', current_setting('ores_agent.risk_portfolio_added')::boolean,
    'partialCopied', current_setting('ores_agent.seeded_partial_run') <> '',
    'riskTrades', trades,
    'vegaSeeded', trades >= 2 and seed_vega,
    'scenarioSeeded', trades >= 2 and seed_scenario
) from agent_seed_decisions;
commit;
