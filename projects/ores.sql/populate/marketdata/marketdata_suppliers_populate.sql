/**
 * The supplier catalogue the demo installs.
 *
 * A supplier is who we contract with and connect to. It is not the publisher:
 * Frankfurter delivers the ECB's data; the ECB is the publisher.
 */

\echo '--- Market Data Suppliers ---'

insert into ores_marketdata_suppliers_tbl (
    tenant_id, code, version, name, kind, description, contract_reference, support,
    display_order, modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'frankfurter', 0, 'Frankfurter', 'aggregator',
     'An open service that republishes many publishers'' daily rates over one endpoint.',
     '', 'community', 10, current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'bloomberg', 0, 'Bloomberg', 'vendor',
     'The Bloomberg session; also a publisher of its own composites.',
     'vault:contracts/bloomberg', 'Bloomberg support', 20, current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'lseg', 0, 'LSEG', 'vendor',
     'The LSEG session.', 'vault:contracts/lseg', 'LSEG support', 30,
     current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'ecb-data-portal', 0, 'ECB Data Portal', 'authority',
     'The ECB''s own data API; supplier and publisher are the same party.',
     '', 'ECB', 40, current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'fred', 0, 'FRED', 'authority',
     'The Federal Reserve Bank of St. Louis data service.',
     '', 'St. Louis Fed', 50, current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'boe', 0, 'Bank of England statistics', 'authority',
     'The Bank of England statistics database.', '', 'Bank of England', 60,
     current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'treasury', 0, 'US Treasury', 'authority',
     'The US Treasury data feed.', '', 'US Treasury', 70,
     current_user, current_user, 'system.initial_load', 'Initial population of suppliers'),
    (ores_utility_system_tenant_id_fn(), 'synthetic', 0, 'Synthetic simulator', 'internal',
     'The simulator the synthetic runner drives.', '', 'platform team', 80,
     current_user, current_user, 'system.initial_load', 'Initial population of suppliers')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
