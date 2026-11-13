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
 * IBOR Curve Market Series Population Script
 *
 * Seeds the output and quote series the ten IBOR curves are built from: LIBOR 1M and 6M
 * for USD, LIBOR 1M and 3M for GBP and JPY, LIBOR 3M and 6M for CHF, and EURIBOR 1M
 * and 6M for EUR.
 *
 * Each curve has one output series (the bootstrapped curve the republish
 * service stamps) and three quote series, one for each forward start its
 * pillar ladder uses. The series are seeded with fixed ids so the config seed
 * and the pillars can name them on a fresh database.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO NOTHING (a rerun must
 * not reset a row the republish service has already stamped).
 */

\echo '--- IBOR Curve Market Series ---'

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (
        '7ad0da2a-861d-57cb-8ca4-c46c55dd8e71',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-LIBOR-1M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped USD-LIBOR-1M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The USD-LIBOR-1M quotes that start at 0D: the ladder the USD-LIBOR-1M blueprint pillars name'
    ),
    (
        '322dad22-8f38-5261-ae2d-36440d15d488',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The USD-LIBOR-1M quotes that start at 3M: the ladder the USD-LIBOR-1M blueprint pillars name'
    ),
    (
        '44b276a9-b260-5894-9b54-fbac1f327ba3',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=1M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The USD-LIBOR-1M quotes that start at 6M: the ladder the USD-LIBOR-1M blueprint pillars name'
    ),
    (
        '9cf958f1-a517-5624-86f4-10ea1bceb366',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-LIBOR-6M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped USD-LIBOR-6M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The USD-LIBOR-6M quotes that start at 0D: the ladder the USD-LIBOR-6M blueprint pillars name'
    ),
    (
        '22042c87-dffe-50b9-9466-95d5b9f530aa',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=6M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The USD-LIBOR-6M quotes that start at 3M: the ladder the USD-LIBOR-6M blueprint pillars name'
    ),
    (
        'd34d2d8a-399f-59fd-a2e4-f53464277c79',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=6M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The USD-LIBOR-6M quotes that start at 6M: the ladder the USD-LIBOR-6M blueprint pillars name'
    ),
    (
        'd8999934-af04-5418-b2e5-06b37d6f29a7',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=discount&quote=rate&curve_id=GBP-LIBOR-1M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped GBP-LIBOR-1M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The GBP-LIBOR-1M quotes that start at 0D: the ladder the GBP-LIBOR-1M blueprint pillars name'
    ),
    (
        'e7e50334-04ef-53be-80d2-d3f231bec8a5',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The GBP-LIBOR-1M quotes that start at 3M: the ladder the GBP-LIBOR-1M blueprint pillars name'
    ),
    (
        '593d45d7-61ce-514f-b2cd-abca5ef3c792',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The GBP-LIBOR-1M quotes that start at 6M: the ladder the GBP-LIBOR-1M blueprint pillars name'
    ),
    (
        '5d6b61bf-d012-528f-b95d-96f138286009',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=discount&quote=rate&curve_id=GBP-LIBOR-3M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped GBP-LIBOR-3M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        '3a967211-5656-54be-870a-2f719689c570',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The GBP-LIBOR-3M quotes that start at 0D: the ladder the GBP-LIBOR-3M blueprint pillars name'
    ),
    (
        'faf90611-9f08-5b9e-8ac7-5aac7fccc425',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The GBP-LIBOR-3M quotes that start at 3M: the ladder the GBP-LIBOR-3M blueprint pillars name'
    ),
    (
        '135ce892-d6d1-515a-b3da-24b2feb52ff4',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The GBP-LIBOR-3M quotes that start at 6M: the ladder the GBP-LIBOR-3M blueprint pillars name'
    ),
    (
        '1985b124-9544-56c7-8132-afb34c0a4b21',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=discount&quote=rate&curve_id=JPY-LIBOR-1M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped JPY-LIBOR-1M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The JPY-LIBOR-1M quotes that start at 0D: the ladder the JPY-LIBOR-1M blueprint pillars name'
    ),
    (
        '45567a26-245b-5add-aa04-7109ca4e21f1',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The JPY-LIBOR-1M quotes that start at 3M: the ladder the JPY-LIBOR-1M blueprint pillars name'
    ),
    (
        '6ca1e194-9747-5fbb-9904-2289f29ddbfa',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The JPY-LIBOR-1M quotes that start at 6M: the ladder the JPY-LIBOR-1M blueprint pillars name'
    ),
    (
        '1bda6efe-efe3-5c67-a353-8137b5a4120d',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=discount&quote=rate&curve_id=JPY-LIBOR-3M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped JPY-LIBOR-3M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The JPY-LIBOR-3M quotes that start at 0D: the ladder the JPY-LIBOR-3M blueprint pillars name'
    ),
    (
        '8075cf1f-1140-5b71-a46c-04a0141d3903',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The JPY-LIBOR-3M quotes that start at 3M: the ladder the JPY-LIBOR-3M blueprint pillars name'
    ),
    (
        '8739ab6e-210c-5ee6-a60a-aed6151dc4c1',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/JPY?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The JPY-LIBOR-3M quotes that start at 6M: the ladder the JPY-LIBOR-3M blueprint pillars name'
    ),
    (
        'd83aeb20-a33e-59aa-bf35-1397d1b0d930',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=discount&quote=rate&curve_id=CHF-LIBOR-3M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped CHF-LIBOR-3M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'c9a8af50-fe23-581c-afbb-69c043c8eac0',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The CHF-LIBOR-3M quotes that start at 0D: the ladder the CHF-LIBOR-3M blueprint pillars name'
    ),
    (
        'b338e61b-c104-51f5-96cc-bada8ff59377',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The CHF-LIBOR-3M quotes that start at 3M: the ladder the CHF-LIBOR-3M blueprint pillars name'
    ),
    (
        'b8ff1529-400d-5a34-8a9a-2813145eaca2',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=3M',
        'ice', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The CHF-LIBOR-3M quotes that start at 6M: the ladder the CHF-LIBOR-3M blueprint pillars name'
    ),
    (
        'f1591e40-5a2a-55ab-8abf-d17bd048a0ba',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=discount&quote=rate&curve_id=CHF-LIBOR-6M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped CHF-LIBOR-6M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'be0546fe-956a-5643-8756-0f52c9897c6f',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The CHF-LIBOR-6M quotes that start at 0D: the ladder the CHF-LIBOR-6M blueprint pillars name'
    ),
    (
        'a8cf0a3a-9cbc-5933-b9e4-4ba4e4a4e00b',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=6M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The CHF-LIBOR-6M quotes that start at 3M: the ladder the CHF-LIBOR-6M blueprint pillars name'
    ),
    (
        '461964e6-1a61-5849-af74-8de935fd79ce',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/CHF?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=6M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The CHF-LIBOR-6M quotes that start at 6M: the ladder the CHF-LIBOR-6M blueprint pillars name'
    ),
    (
        'a77ce851-6443-5a71-ba8e-029c6c813355',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=EUR-EURIBOR-1M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped EUR-EURIBOR-1M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The EUR-EURIBOR-1M quotes that start at 0D: the ladder the EUR-EURIBOR-1M blueprint pillars name'
    ),
    (
        'ab8834d9-183f-5a17-b2b5-ebfdf8c20551',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The EUR-EURIBOR-1M quotes that start at 3M: the ladder the EUR-EURIBOR-1M blueprint pillars name'
    ),
    (
        '211da8c5-a3fa-5bcc-aecc-f8d32adf53be',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=1M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The EUR-EURIBOR-1M quotes that start at 6M: the ladder the EUR-EURIBOR-1M blueprint pillars name'
    ),
    (
        '57c4331e-495a-50b8-8281-2fd75497bfdd',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=EUR-EURIBOR-6M',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped EUR-EURIBOR-6M projection curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The EUR-EURIBOR-6M quotes that start at 0D: the ladder the EUR-EURIBOR-6M blueprint pillars name'
    ),
    (
        'b98e8a64-8097-512e-9052-b468cf6af6ae',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=6M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The EUR-EURIBOR-6M quotes that start at 3M: the ladder the EUR-EURIBOR-6M blueprint pillars name'
    ),
    (
        '2e1c6ebf-0711-5683-b2d7-2c2fb09c2742',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=6M&tenor=6M',
        'tradeweb', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The EUR-EURIBOR-6M quotes that start at 6M: the ladder the EUR-EURIBOR-6M blueprint pillars name'
    )
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- The curve points and the quotes they are built from are interest rates
-- series, which the junction carries rather than the series row.
insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (
        '7ad0da2a-861d-57cb-8ca4-c46c55dd8e71',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'e9fdf2c4-2aab-5c8a-ad91-784536a0a0b5',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '322dad22-8f38-5261-ae2d-36440d15d488',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '44b276a9-b260-5894-9b54-fbac1f327ba3',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '9cf958f1-a517-5624-86f4-10ea1bceb366',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '1a17bcaa-e315-5933-99c3-4e843fbd3bd5',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '22042c87-dffe-50b9-9466-95d5b9f530aa',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'd34d2d8a-399f-59fd-a2e4-f53464277c79',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'd8999934-af04-5418-b2e5-06b37d6f29a7',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '6165968c-d92e-50fd-9e2d-e50fd9b9f190',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'e7e50334-04ef-53be-80d2-d3f231bec8a5',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '593d45d7-61ce-514f-b2cd-abca5ef3c792',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '5d6b61bf-d012-528f-b95d-96f138286009',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '3a967211-5656-54be-870a-2f719689c570',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'faf90611-9f08-5b9e-8ac7-5aac7fccc425',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '135ce892-d6d1-515a-b3da-24b2feb52ff4',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '1985b124-9544-56c7-8132-afb34c0a4b21',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'be496b8b-1e4d-5a81-971f-5ffc5df2d454',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '45567a26-245b-5add-aa04-7109ca4e21f1',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '6ca1e194-9747-5fbb-9904-2289f29ddbfa',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '1bda6efe-efe3-5c67-a353-8137b5a4120d',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'b2f570f2-61ae-53ec-b6f3-5fa23aad487e',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '8075cf1f-1140-5b71-a46c-04a0141d3903',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '8739ab6e-210c-5ee6-a60a-aed6151dc4c1',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'd83aeb20-a33e-59aa-bf35-1397d1b0d930',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'c9a8af50-fe23-581c-afbb-69c043c8eac0',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'b338e61b-c104-51f5-96cc-bada8ff59377',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'b8ff1529-400d-5a34-8a9a-2813145eaca2',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'f1591e40-5a2a-55ab-8abf-d17bd048a0ba',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'be0546fe-956a-5643-8756-0f52c9897c6f',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'a8cf0a3a-9cbc-5933-b9e4-4ba4e4a4e00b',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '461964e6-1a61-5849-af74-8de935fd79ce',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'a77ce851-6443-5a71-ba8e-029c6c813355',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '11e635b4-4ba8-5da5-ad03-105b9e8483ec',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'ab8834d9-183f-5a17-b2b5-ebfdf8c20551',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '211da8c5-a3fa-5bcc-aecc-f8d32adf53be',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '57c4331e-495a-50b8-8281-2fd75497bfdd',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '9325d15a-c9ef-5c14-99d0-3516dc1a819f',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        'b98e8a64-8097-512e-9052-b468cf6af6ae',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    ),
    (
        '2e1c6ebf-0711-5683-b2d7-2c2fb09c2742',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'An IBOR curve series is an interest rates series'
    )
on conflict (tenant_id, market_series_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary.
select 'marketdata_market_series (IBOR curves)' as entity, count(*) as count
from ores_marketdata_market_series_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and oresmd_uri like 'oresmd://ir/%curve_id=%';
