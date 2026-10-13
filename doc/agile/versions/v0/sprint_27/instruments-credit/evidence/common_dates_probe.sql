begin;
-- The replica role turns off the foreign key and trigger checks, so the probe
-- can use random party, book and activity ids. The transaction rolls back.
set local session_replication_role = replica;
create temp table ids as
  select ores_utility_system_tenant_id_fn() tn, gen_random_uuid() a, gen_random_uuid() p, gen_random_uuid() b, gen_random_uuid() cds;

-- One rates trade: a forward rate agreement with start and maturity dates.
insert into ores_trading_rate_instruments_tbl
 (tenant_id, trade_id, version, trade_type_code, party_id, trade_activity_id, start_date, maturity_date,
  modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
select tn, gen_random_uuid(), 0, 'ForwardRateAgreement', p, a, date '2026-04-01', date '2026-10-01',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn() from ids;

-- Two credit trades: a credit default swap and a risk participation agreement.
insert into ores_trading_credit_instruments_tbl
 (tenant_id, trade_id, version, trade_type_code, party_id, trade_activity_id, reference_entity,
  start_date, maturity_date,
  modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
select tn, cds, 0, 'CreditDefaultSwap', p, a, 'ACME Corp', date '2026-05-01', date '2031-05-01',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn() from ids
union all
select tn, gen_random_uuid(), 0, 'RiskParticipationAgreement', p, a, 'ACME Corp', date '2026-06-01', date '2037-12-16',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn() from ids;

-- The credit swap has a booking, so the view reads its trade date.
insert into ores_trading_trade_bookings_tbl
 (tenant_id, trade_id, version, trade_activity_id, book_id, trade_date,
  modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
select h.tenant_id, h.trade_id, 0, h.trade_activity_id, (select b from ids), date '2026-04-28',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn()
from ores_trading_credit_instruments_tbl h where h.trade_id = (select cds from ids);

-- One date column across both families.
select family, trade_type_code, trade_date, start_date, expiry_date, maturity_date
from (
  select 'rates' family, r.trade_type_code, v.trade_date, v.start_date, v.expiry_date, v.maturity_date
  from ores_trading_rate_instruments_common_dates_vw v
  join ores_trading_rate_instruments_tbl r using (tenant_id, trade_id)
  where r.modified_by = 'probe'
  union all
  select 'credit', c.trade_type_code, v.trade_date, v.start_date, v.expiry_date, v.maturity_date
  from ores_trading_credit_instruments_common_dates_vw v
  join ores_trading_credit_instruments_tbl c using (tenant_id, trade_id)
  where c.modified_by = 'probe'
) all_families
order by family, trade_type_code;
rollback;
