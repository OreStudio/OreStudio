/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * IBOR Curve Bootstrap Config Population Script
 *
 * Seeds one bootstrap config per IBOR curve: the ten curves of LIBOR 1M and 6M for
 * USD, LIBOR 1M and 3M for GBP and JPY, LIBOR 3M and 6M for CHF, and EURIBOR 1M and 6M
 * for EUR. Each is a PROJECTION curve that names the currency's overnight config as its
 * discount curve, and its pillars name the quote series that start where the pillar does.
 *
 * Each curve has one output series (the bootstrapped curve the republish
 * service stamps) and three quote series, one for each forward start its
 * pillar ladder uses. The series are seeded with fixed ids so the config seed
 * and the pillars can name them on a fresh database.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO NOTHING (a rerun must
 * not reset a row the republish service has already stamped).
 */

\echo '--- IBOR Curve Bootstrap Configs ---'

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    '05fdf8e7-1cde-5171-beca-a93f58bb5e24',
    ores_utility_system_tenant_id_fn(),
    0,
    '7ad0da2a-861d-57cb-8ca4-c46c55dd8e71',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'USD-LIBOR-1M', 'USD', '1M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'USD-SOFR-FOMC' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'e834fa19-8426-5c9e-9d2b-221669f118d6',
    ores_utility_system_tenant_id_fn(),
    0,
    '9cf958f1-a517-5624-86f4-10ea1bceb366',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'USD-LIBOR-6M', 'USD', '6M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'USD-SOFR-FOMC' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'c972609e-dda0-5fc7-8478-97796756879d',
    ores_utility_system_tenant_id_fn(),
    0,
    'd8999934-af04-5418-b2e5-06b37d6f29a7',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'GBP-LIBOR-1M', 'GBP', '1M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'GBP-SONIA' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A365F', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    '0b7539c8-76b5-506c-a6b8-8d9546b22540',
    ores_utility_system_tenant_id_fn(),
    0,
    '5d6b61bf-d012-528f-b95d-96f138286009',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'GBP-LIBOR-3M', 'GBP', '3M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'GBP-SONIA' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A365F', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'f30589d6-5caf-521e-889b-9ecb830ae33a',
    ores_utility_system_tenant_id_fn(),
    0,
    '1985b124-9544-56c7-8132-afb34c0a4b21',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'JPY-LIBOR-1M', 'JPY', '1M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'JPY-TONA' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A365F', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2',
    ores_utility_system_tenant_id_fn(),
    0,
    '1bda6efe-efe3-5c67-a353-8137b5a4120d',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'JPY-LIBOR-3M', 'JPY', '3M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'JPY-TONA' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A365F', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40',
    ores_utility_system_tenant_id_fn(),
    0,
    'd83aeb20-a33e-59aa-bf35-1397d1b0d930',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'CHF-LIBOR-3M', 'CHF', '3M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'CHF-SARON' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f',
    ores_utility_system_tenant_id_fn(),
    0,
    'f1591e40-5a2a-55ab-8abf-d17bd048a0ba',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'CHF-LIBOR-6M', 'CHF', '6M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'CHF-SARON' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    '5b1a608d-6842-584c-ab35-36e00e93d97f',
    ores_utility_system_tenant_id_fn(),
    0,
    'a77ce851-6443-5a71-ba8e-029c6c813355',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'EUR-EURIBOR-1M', 'EUR', '1M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'EUR-ESTR' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    '72bf29f0-8c37-5176-a6a0-c12f71193318',
    ores_utility_system_tenant_id_fn(),
    0,
    '57c4331e-495a-50b8-8281-2fd75497bfdd',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'EUR-EURIBOR-6M', 'EUR', '6M',
    'PROJECTION',
    (select id from ores_refdata_ir_curve_bootstrap_configs_tbl
      where name = 'EUR-ESTR' and valid_to = ores_utility_infinity_timestamp_fn()
      order by version desc limit 1),
    'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IBOR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

\echo '--- IBOR Curve Bootstrap Pillars ---'

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('91a72955-b307-50c1-beb8-f47a6ccd8894', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 0, 'SPOT', '1M', 'DEPOSIT',
     'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('4a1b6a56-bbf7-5375-aa8e-b7452243d8be', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 1, 'SPOT', '3M', 'DEPOSIT',
     'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('f37b97f1-34eb-546f-bd96-c44d95763ef0', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 2, '3M', '6M', 'FRA',
     '322dad22-8f38-5261-ae2d-36440d15d488',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('7e2dc93e-7417-528a-9d5d-8e235d016b95', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 3, '6M', '12M', 'FRA',
     '44b276a9-b260-5894-9b54-fbac1f327ba3',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('08987e5d-601f-52d8-b1b8-96298d55a3c0', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 4, 'SPOT', '2Y', 'SWAP',
     'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('753550e4-0aa4-5306-9e75-2d18464d08ba', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 5, 'SPOT', '5Y', 'SWAP',
     'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('ae1e3e0e-e86a-5c5d-a9a3-587aa3b55aa1', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '05fdf8e7-1cde-5171-beca-a93f58bb5e24', 6, 'SPOT', '10Y', 'SWAP',
     'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('56b998bb-ccfc-5777-8228-9335f3ca3144', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 0, 'SPOT', '1M', 'DEPOSIT',
     '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('0c8c9cf5-a58d-5a98-bf8e-0895eb0da50b', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 1, 'SPOT', '3M', 'DEPOSIT',
     '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('44a02bf7-f402-5861-aceb-b891fc7f8c6a', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 2, '3M', '6M', 'FRA',
     '22042c87-dffe-50b9-9466-95d5b9f530aa',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('d9c86032-f3b5-52c0-b0e9-66c7776cf4e6', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 3, '6M', '12M', 'FRA',
     'd34d2d8a-399f-59fd-a2e4-f53464277c79',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('9f5b4d6e-cf6c-54ab-8282-64bf315332e0', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 4, 'SPOT', '2Y', 'SWAP',
     '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('c022d3c7-a840-5dcd-af0c-28f390abcc1e', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 5, 'SPOT', '5Y', 'SWAP',
     '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('d2225854-4d6a-57a2-97dc-1fd64fcdf67f', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'e834fa19-8426-5c9e-9d2b-221669f118d6', 6, 'SPOT', '10Y', 'SWAP',
     '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('8991b62e-d05f-5b7c-a475-9fa90940b572', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 0, 'SPOT', '1M', 'DEPOSIT',
     '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('d5adf261-eb07-53ee-9a20-e873272d0b1a', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 1, 'SPOT', '3M', 'DEPOSIT',
     '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('5c66223f-61cd-5c97-81fb-348945980b44', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 2, '3M', '6M', 'FRA',
     'e7e50334-04ef-53be-80d2-d3f231bec8a5',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('99255f8d-e6c4-50a2-8ea9-f7e3e429d819', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 3, '6M', '12M', 'FRA',
     '593d45d7-61ce-514f-b2cd-abca5ef3c792',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('a81e54bf-6d85-5011-8af4-76b5a354a85b', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 4, 'SPOT', '2Y', 'SWAP',
     '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('59cf6627-bad1-54c7-81a9-65b14d06bd7e', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 5, 'SPOT', '5Y', 'SWAP',
     '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('05ff7e55-6415-5f04-9368-86be7ad3c1ed', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c972609e-dda0-5fc7-8478-97796756879d', 6, 'SPOT', '10Y', 'SWAP',
     '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('d4eb5816-ad0c-5c82-94ca-8e4e7d658bc5', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 0, 'SPOT', '1M', 'DEPOSIT',
     '3a967211-5656-54be-870a-2f719689c570',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('460a3488-2dc7-5340-a4bf-73c3409a1c8e', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 1, 'SPOT', '3M', 'DEPOSIT',
     '3a967211-5656-54be-870a-2f719689c570',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('e992d0a4-615c-5d66-ada0-a3551327ce9d', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 2, '3M', '6M', 'FRA',
     'faf90611-9f08-5b9e-8ac7-5aac7fccc425',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('9031c391-2935-55c2-8842-5807142cf3ae', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 3, '6M', '12M', 'FRA',
     '135ce892-d6d1-515a-b3da-24b2feb52ff4',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('a95daf01-5c7d-5e4c-951d-425b0e97e997', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 4, 'SPOT', '2Y', 'SWAP',
     '3a967211-5656-54be-870a-2f719689c570',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('61d81862-3bb7-5f32-ac76-4fa6353018d5', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 5, 'SPOT', '5Y', 'SWAP',
     '3a967211-5656-54be-870a-2f719689c570',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('823a6516-e376-5e4a-bfbd-dd2292f4fa1d', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '0b7539c8-76b5-506c-a6b8-8d9546b22540', 6, 'SPOT', '10Y', 'SWAP',
     '3a967211-5656-54be-870a-2f719689c570',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('0c1ef2f0-7131-5d77-bc5d-1c03a5d696e7', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 0, 'SPOT', '1M', 'DEPOSIT',
     'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('6a5831a6-2226-5e6a-beda-26e9fe1ee54a', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 1, 'SPOT', '3M', 'DEPOSIT',
     'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('89dca81a-6fa7-54eb-ab8d-abf37a9ce5b7', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 2, '3M', '6M', 'FRA',
     '45567a26-245b-5add-aa04-7109ca4e21f1',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('3a3d8a6e-4bb9-5e56-9e8a-6a50ccb82643', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 3, '6M', '12M', 'FRA',
     '6ca1e194-9747-5fbb-9904-2289f29ddbfa',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('0ceda727-870b-59b7-8852-0bf303a4574c', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 4, 'SPOT', '2Y', 'SWAP',
     'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('237cece9-8dbb-5488-a507-9ec324a02d7b', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 5, 'SPOT', '5Y', 'SWAP',
     'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('1dfe07ef-b30c-59c8-a885-88d17a7b2cd9', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'f30589d6-5caf-521e-889b-9ecb830ae33a', 6, 'SPOT', '10Y', 'SWAP',
     'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('4af44e6b-059a-55c7-a685-1fbeaa7622e9', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 0, 'SPOT', '1M', 'DEPOSIT',
     'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('c61bd9e8-1865-5fa2-a2c4-63ddede07d41', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 1, 'SPOT', '3M', 'DEPOSIT',
     'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('8ca11130-a813-5262-83a1-b80d67eecdc0', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 2, '3M', '6M', 'FRA',
     '8075cf1f-1140-5b71-a46c-04a0141d3903',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('e6816cdd-dda6-5f7d-b508-5af3b666b2ae', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 3, '6M', '12M', 'FRA',
     '8739ab6e-210c-5ee6-a60a-aed6151dc4c1',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('f743a6c2-3f66-575b-b516-d1151e47fc65', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 4, 'SPOT', '2Y', 'SWAP',
     'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('521f01a4-2397-5ed1-aa50-2bb5deef6697', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 5, 'SPOT', '5Y', 'SWAP',
     'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('547e0041-e5a5-55d6-a5ef-d9958be300d8', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'a53a66c8-76eb-5ac0-bf47-5fdf40c7c4d2', 6, 'SPOT', '10Y', 'SWAP',
     'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('89214784-2d21-5e7c-ba0a-5c9db7354ef0', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 0, 'SPOT', '1M', 'DEPOSIT',
     'c9a8af50-fe23-581c-afbb-69c043c8eac0',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('af8f4b0f-953d-5d9c-be98-d8e63a87eea4', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 1, 'SPOT', '3M', 'DEPOSIT',
     'c9a8af50-fe23-581c-afbb-69c043c8eac0',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('c1fd89df-7b97-5697-899f-e9f2cd3a2169', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 2, '3M', '6M', 'FRA',
     'b338e61b-c104-51f5-96cc-bada8ff59377',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('f5ebd117-19ed-574e-b70d-a4a50c3594a0', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 3, '6M', '12M', 'FRA',
     'b8ff1529-400d-5a34-8a9a-2813145eaca2',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('8b82527e-95da-5ca2-8adc-98bac64e8ce1', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 4, 'SPOT', '2Y', 'SWAP',
     'c9a8af50-fe23-581c-afbb-69c043c8eac0',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('4adf8379-4695-5473-891b-ae821cbbda14', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 5, 'SPOT', '5Y', 'SWAP',
     'c9a8af50-fe23-581c-afbb-69c043c8eac0',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('ed2f5945-2169-5967-bd5c-4a73f0a5ab06', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'c08ca56a-b8ca-53b2-8f52-b8ccecc5fe40', 6, 'SPOT', '10Y', 'SWAP',
     'c9a8af50-fe23-581c-afbb-69c043c8eac0',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('3e0c8913-30c1-564a-b3cf-d67a5b9cdf38', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 0, 'SPOT', '1M', 'DEPOSIT',
     'be0546fe-956a-5643-8756-0f52c9897c6f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('aca3effc-96a4-5e3f-9806-4e6afdaaf18e', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 1, 'SPOT', '3M', 'DEPOSIT',
     'be0546fe-956a-5643-8756-0f52c9897c6f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('d0bc87bc-3f4b-5ca1-8594-75552b0ee952', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 2, '3M', '6M', 'FRA',
     'a8cf0a3a-9cbc-5933-b9e4-4ba4e4a4e00b',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('911abba3-0616-5be6-90fe-f45d8610edfd', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 3, '6M', '12M', 'FRA',
     '461964e6-1a61-5849-af74-8de935fd79ce',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('2eaf5e61-9710-593c-893e-c2b9a61534b6', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 4, 'SPOT', '2Y', 'SWAP',
     'be0546fe-956a-5643-8756-0f52c9897c6f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('b8cdb60e-8363-5062-a3aa-42cc5e310c3c', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 5, 'SPOT', '5Y', 'SWAP',
     'be0546fe-956a-5643-8756-0f52c9897c6f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('d6799e3b-ef60-5bf2-b8f5-2b09193bb1ad', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd3c4f9ce-6f3c-55a3-ae74-2782d760e79f', 6, 'SPOT', '10Y', 'SWAP',
     'be0546fe-956a-5643-8756-0f52c9897c6f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('bdd6462f-3193-52a7-8070-fc0c3e7bc1d6', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 0, 'SPOT', '1M', 'DEPOSIT',
     '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('c2c21787-e3d1-5c2d-ad18-c616353f6b30', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 1, 'SPOT', '3M', 'DEPOSIT',
     '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('bfcb1c38-7204-5fc2-a531-04f0dbce973a', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 2, '3M', '6M', 'FRA',
     'ab8834d9-183f-5a17-b2b5-ebfdf8c20551',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('1470865b-cc71-58ff-a9cf-b536e349b854', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 3, '6M', '12M', 'FRA',
     '211da8c5-a3fa-5bcc-aecc-f8d32adf53be',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('b439efd9-7f9e-5d37-afa1-c964ff720727', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 4, 'SPOT', '2Y', 'SWAP',
     '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('cc77e1e5-0e35-55c7-b71d-0032311cda14', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 5, 'SPOT', '5Y', 'SWAP',
     '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('0d9758dc-d8a1-5e10-bd98-1761dd37ba6f', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '5b1a608d-6842-584c-ab35-36e00e93d97f', 6, 'SPOT', '10Y', 'SWAP',
     '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('bf9cce2a-ab7d-5926-aeae-0c19a429bffb', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 0, 'SPOT', '1M', 'DEPOSIT',
     '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('545b7dcc-f8ef-5588-81ab-4634f53a609e', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 1, 'SPOT', '3M', 'DEPOSIT',
     '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('a4b88191-e7ec-5838-957e-bd3f4c41d503', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 2, '3M', '6M', 'FRA',
     'b98e8a64-8097-512e-9052-b468cf6af6ae',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('e32c8360-20f9-53bd-8b60-8e4812f339fe', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 3, '6M', '12M', 'FRA',
     '2e1c6ebf-0711-5683-b2d7-2c2fb09c2742',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('62306066-f055-558d-a28b-817af4a2c8e4', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 4, 'SPOT', '2Y', 'SWAP',
     '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('caaaecea-1a7f-5043-9686-2018fc4921de', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 5, 'SPOT', '5Y', 'SWAP',
     '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars'),
    ('6ce8a732-6f86-584f-b28a-07d3b70503d4', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     '72bf29f0-8c37-5176-a6a0-c12f71193318', 6, 'SPOT', '10Y', 'SWAP',
     '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
     current_user, current_user, 'system.initial_load',
     'Initial population of IBOR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
