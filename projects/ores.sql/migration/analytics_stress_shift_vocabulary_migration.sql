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
 * One-shot migration: seed the stress shift families and shift types
 *
 * A stress shift's family and shift type were free text. Both are now seeded
 * lookups read out of the ORE schemas, and the shift's insert trigger refuses
 * a value they do not hold.
 *
 * The migration refuses to start while a current stress shift names a family
 * or a shift type the schemas do not define, because the next version of that
 * shift would be refused. The stress test mapper writes only schema values, so
 * a database it filled migrates in place.
 *
 * On a freshly recreated database the create and populate scripts already do
 * this, and this migration is unnecessary. It exists for databases created
 * before the change.
 */

begin;

do $$
declare
    v_unknown text;
begin
    select string_agg(distinct coalesce(family, '') || '/' || coalesce(shift_type, ''), ', ')
    into v_unknown
    from ores_analytics_stress_test_shifts_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
      and (family not in ('ParShifts', 'DiscountCurves', 'IndexCurves', 'YieldCurves',
                          'FxSpots', 'FxVolatilities', 'SwaptionVolatilities',
                          'CapFloorVolatilities', 'EquitySpots', 'EquityVolatilities',
                          'CommodityCurves', 'IntradayPowerCurves', 'CommodityVolatilities',
                          'SecuritySpreads', 'RecoveryRates', 'SurvivalProbabilities')
           or shift_type not in ('Relative', 'Absolute', 'EqualTo'));

    if v_unknown is not null then
        raise exception 'Stress shifts name values the schemas do not define (family/type): %',
            v_unknown;
    end if;
end;
$$;

\ir ../create/analytics/analytics_stress_shift_families_create.sql
\ir ../create/analytics/analytics_stress_shift_families_notify_trigger_create.sql
\ir ../create/analytics/analytics_shift_types_create.sql
\ir ../create/analytics/analytics_shift_types_notify_trigger_create.sql
\ir ../create/analytics/analytics_stress_test_shifts_create.sql
\ir ../create/analytics/analytics_rls_policies_create.sql

\ir ../populate/analytics/analytics_stress_shift_families_populate.sql
\ir ../populate/analytics/analytics_shift_types_populate.sql
\ir ../populate/iam/iam_permissions_populate.sql

commit;
