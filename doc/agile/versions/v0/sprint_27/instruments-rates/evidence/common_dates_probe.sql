begin;
set local session_replication_role = replica;
create temp table ids as
  select gen_random_uuid() t, gen_random_uuid() a, ores_utility_system_tenant_id_fn() tn,
         gen_random_uuid() p, gen_random_uuid() b;
insert into ores_trading_rate_instruments_tbl
 (tenant_id, trade_id, version, trade_type_code, party_id, trade_activity_id, start_date, maturity_date,
  modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
select tn, t, 0, 'ForwardRateAgreement', p, a, date '2026-04-01', date '2026-10-01',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn() from ids
union all
select tn, gen_random_uuid(), 0, 'Swaption', p, a, null, null,
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn() from ids;
insert into ores_trading_swaption_instruments_tbl
 (tenant_id, trade_id, version, trade_activity_id, expiry_date, exercise_type, settlement_type, long_short,
  modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
select tenant_id, trade_id, 0, trade_activity_id, date '2027-01-15', 'European','Physical','Long',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn()
from ores_trading_rate_instruments_tbl where trade_type_code = 'Swaption';
insert into ores_trading_trade_bookings_tbl
 (tenant_id, trade_id, version, trade_activity_id, book_id, trade_date,
  modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
select h.tenant_id, h.trade_id, 0, h.trade_activity_id, (select b from ids), date '2026-03-30',
  'probe','probe','system.initial_load','probe', now(), ores_utility_infinity_timestamp_fn()
from ores_trading_rate_instruments_tbl h where trade_type_code = 'ForwardRateAgreement';
select r.trade_type_code, v.trade_date, v.start_date, v.expiry_date, v.maturity_date
from ores_trading_rate_instruments_common_dates_vw v
join ores_trading_rate_instruments_tbl r using (tenant_id, trade_id)
order by r.trade_type_code;
rollback;
