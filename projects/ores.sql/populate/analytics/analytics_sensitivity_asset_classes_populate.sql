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
 * Sensitivity Asset Classes Population Script
 *
 * The asset classes ORE presents a sensitivity template prefix for: the
 * reference data's code, the prefix a template key spells, and the words a
 * person reads.
 *
 * The prefix is not the reference data's code. ORE's template keys grew their
 * own vocabulary, and it spells commodity COMM where the rest of ORE spells it
 * COM, so the two are held apart and joined here.
 *
 * Bond and inflation are asset classes of the reference data with no row, because
 * ORE's examples present no template prefix for them.
 *
 * This script is idempotent.
 */

\echo '--- Sensitivity Asset Classes ---'

insert into ores_analytics_sensitivity_asset_classes_tbl (
    asset_class_code, tenant_id, version, template_prefix, name, description, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('interest_rates', ores_utility_system_tenant_id_fn(), 0, 'IR', 'Interest rate',
     'Products whose value follows the level or the shape of a yield curve.',
     1, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity asset classes'),
    ('fx', ores_utility_system_tenant_id_fn(), 0, 'FX', 'Foreign exchange',
     'Products whose value follows the exchange rate between two currencies.',
     2, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity asset classes'),
    ('equity', ores_utility_system_tenant_id_fn(), 0, 'EQ', 'Equity',
     'Products whose value follows a stock, a basket of stocks or an equity index.',
     3, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity asset classes'),
    ('commodity', ores_utility_system_tenant_id_fn(), 0, 'COMM', 'Commodity',
     'Products whose value follows a physical commodity or a commodity index.',
     4, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity asset classes'),
    ('credit', ores_utility_system_tenant_id_fn(), 0, 'CR', 'Credit',
     'Products whose value follows the likelihood that an issuer or reference entity defaults.',
     5, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity asset classes')
on conflict (tenant_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Sensitivity Asset Classes' as entity, count(*) as count
from ores_analytics_sensitivity_asset_classes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
