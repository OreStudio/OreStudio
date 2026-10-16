begin;
select set_config('app.current_tenant_id', 'b7fe3d66-2aed-4d1c-a19b-0406b689640b', false);
select set_config('app.current_party_id', '', false);
select 'session_app_current_party_id' as what, quote_literal(current_setting('app.current_party_id')) as value;
insert into ores_trading_bond_instruments_tbl
    (trade_id, trade_type_code, trade_activity_id, issue_id, party_id, notional, tenant_id, version,
     modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
values
    ('fe21a60d-4d15-5601-b650-7ca3f316d8cf', 'Bond',
     '010c22be-139d-4c76-9b41-7867d0e01b77', '9a68ed8a-f85d-54ce-b3a6-c5f9dac5fe2e',
     'ddb2008f-a010-4953-bcf9-adc76bb56732', 5565273.69,
     'b7fe3d66-2aed-4d1c-a19b-0406b689640b', 2, 'tenant_admin', 'tenant_admin', 'system.test',
     'party proof', '2026-01-01 00:00:00+00', '9999-12-31 23:59:59+00');
rollback;
