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
 * Calibration Policies Population Script
 *
 * Seeds the FX calibration policy: the official publishers in the order the
 * installation trusts them (ECB, IMF, Bank of Canada, Central Bank of the
 * Republic of Turkiye), a 90-day window, an inverse allowed, and the fixed
 * level kept when no publisher fixes a pair. Every FX feed derives its
 * candidates from this order; no feed is seeded with items of its own.
 *
 * This script is idempotent: it inserts the policy only when the class has
 * none, and the order only when the policy has none.
 */

\echo '--- Calibration Policies ---'

do $$
declare
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_policy_id uuid;
begin
    select id into v_policy_id
    from ores_synthetic_calibration_policies_tbl
    where tenant_id = v_tenant_id
      and asset_class_code = 'fx'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_policy_id is null then
        v_policy_id := gen_random_uuid();
        insert into ores_synthetic_calibration_policies_tbl (
            id, tenant_id, version, asset_class_code, window_days, allow_inverse,
            fallback_code, modified_by, performed_by, change_reason_code, change_commentary
        )
        values (
            v_policy_id, v_tenant_id, 0, 'fx', 90, true, 'keep_fixed',
            current_user, current_user, 'system.initial_load',
            'Initial population of the FX calibration policy'
        );
    end if;

    insert into ores_synthetic_calibration_provider_orders_tbl (
        id, tenant_id, version, policy_id, provider_code, rank,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select gen_random_uuid(), v_tenant_id, 0, v_policy_id, o.provider_code, o.rank,
           current_user, current_user, 'system.initial_load',
           'Initial population of the FX calibration provider order'
    from (values ('ecb', 1), ('imf', 2), ('boc', 3), ('tcmb', 4)) as o(provider_code, rank)
    where not exists (
        select 1 from ores_synthetic_calibration_provider_orders_tbl x
        where x.tenant_id = v_tenant_id
          and x.policy_id = v_policy_id
          and x.valid_to = ores_utility_infinity_timestamp_fn()
    );
end $$;

-- Summary
select 'synthetic_calibration_policies' as entity, count(*) as count
from ores_synthetic_calibration_policies_tbl;
