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
 * The Derivation of a Curve's Output Series
 *
 * A curve is bootstrapped, and its output series says so: =derivation_kind= is
 * =IR_CURVE_BOOTSTRAP= and =derivation_config_id= names the refdata blueprint and
 * version that built it. The republish service stamps the same fields on its
 * first republish, and a blueprint created later in the application is stamped
 * when it is first republished. Nothing stamps a seeded blueprint, so a curve
 * that has never been republished reads as observed and a screen that states how
 * the curve was made would say the opposite of the truth.
 *
 * This script stamps every seeded blueprint's output series once the blueprints
 * exist, so a fresh installation states the derivation before anything runs. A
 * series the republish service has already stamped is left as it is: its kind and
 * its config are the same, and its own change record says what claimed it.
 *
 * One kind names one table: =IR_CURVE_BOOTSTRAP= resolves to
 * =ores_refdata_ir_curve_bootstrap_configs_tbl=. A rate compounded from an
 * overnight curve is =IR_TERM_FIXING=, which resolves to the synthetic generation
 * config; see marketdata_market_series_computed_fixings_populate.sql.
 *
 * This script is idempotent. populate.sql states the heading this step prints.
 */

update ores_marketdata_market_series_tbl s
set derivation_kind = 'IR_CURVE_BOOTSTRAP',
    derivation_config_id = c.id,
    derivation_config_version = c.version,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.derived_series',
    change_commentary = 'Claimed by the blueprint ' || c.name || ' when the curve was created'
from ores_refdata_ir_curve_bootstrap_configs_tbl c
where c.valid_to = ores_utility_infinity_timestamp_fn()
  and c.output_series_id = s.id
  and c.tenant_id = s.tenant_id
  and s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.derivation_kind = 'OBSERVED';

-- Summary
select 'curve_output_series' as entity, count(*) as count
from ores_marketdata_market_series_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
  and derivation_kind = 'IR_CURVE_BOOTSTRAP'
  and oresmd_uri like 'oresmd://ir/%instrument=discount%';
