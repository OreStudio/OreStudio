/**
 * The adapter catalogue the release provides.
 *
 * An adapter is code. These rows say what each module can do and what a session
 * of it must be configured with. Nobody adds an adapter from a screen.
 */

\echo '--- Market Data Adapters ---'

insert into ores_marketdata_adapters_tbl (
    tenant_id, code, version, name, module_version, description, process,
    asset_classes, services, configuration_schema, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'frankfurter', 0, 'Frankfurter', '1.4.0',
     'Reads a publisher''s daily rates from a Frankfurter endpoint.', 'ores.marketdata.official',
     'fx', 'end_of_day,reference',
     '{"url":{"type":"url","required":true},"backfill_days":{"type":"int","required":false,"default":400},"timeout_ms":{"type":"int","required":false,"default":10000}}',
     10, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'ecb-sdw', 0, 'ECB Data Portal', '1.0.0',
     'Reads ECB Data Portal series: rates and curve pillars.', 'ores.marketdata.official',
     'ir', 'end_of_day,reference',
     '{"url":{"type":"url","required":true},"backfill_days":{"type":"int","required":false,"default":400}}',
     20, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'fred-api', 0, 'FRED', '2.1.0',
     'Reads FRED series by id.', 'ores.marketdata.official',
     'fx,ir', 'end_of_day,reference',
     '{"url":{"type":"url","required":true},"api_key_ref":{"type":"secret_ref","required":true}}',
     30, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'boe-stats', 0, 'Bank of England statistics', '1.3.0',
     'Reads Bank of England statistics series.', 'ores.marketdata.official',
     'fx,ir', 'end_of_day,reference',
     '{"url":{"type":"url","required":true},"backfill_days":{"type":"int","required":false,"default":400}}',
     40, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'treasury-feed', 0, 'US Treasury feed', '1.0.0',
     'Reads the US Treasury par yield curve.', 'ores.marketdata.official',
     'ir', 'end_of_day',
     '{"url":{"type":"url","required":true},"curve_year":{"type":"int","required":true}}',
     50, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'bloomberg-api', 0, 'Bloomberg session', '3.2.0',
     'Reads a Bloomberg session by ticker and field.', 'ores.marketdata.official',
     'fx,ir', 'real_time,delayed,end_of_day,reference',
     '{"endpoint":{"type":"url","required":true},"api_key_ref":{"type":"secret_ref","required":true},"api_secret_ref":{"type":"secret_ref","required":true}}',
     60, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'lseg-api', 0, 'LSEG session', '2.8.0',
     'Reads an LSEG session by RIC.', 'ores.marketdata.official',
     'fx,ir', 'real_time,reference',
     '{"endpoint":{"type":"url","required":true},"credential_ref":{"type":"secret_ref","required":true}}',
     70, current_user, current_user, 'system.initial_load', 'Initial population of adapters'),
    (ores_utility_system_tenant_id_fn(), 'simulation', 0, 'Simulator', '1.0.0',
     'Runs the synthetic generators the runner authors.', 'ores.marketdata.synthetic',
     'fx,ir', 'real_time,end_of_day', '{}',
     80, current_user, current_user, 'system.initial_load', 'Initial population of adapters')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
