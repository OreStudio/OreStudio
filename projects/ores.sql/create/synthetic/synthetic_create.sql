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

\ir ./synthetic_market_data_generation_configs_create.sql
\ir ./synthetic_market_data_generation_configs_notify_trigger_create.sql
\ir ./synthetic_fx_spot_generation_configs_create.sql
\ir ./synthetic_fx_spot_generation_configs_notify_trigger_create.sql
\ir ./synthetic_gmm_components_create.sql
\ir ./synthetic_gmm_components_notify_trigger_create.sql

-- IR curve generation: config before the template entries that reference it.
\ir ./synthetic_ir_curve_generation_configs_create.sql
\ir ./synthetic_ir_curve_generation_configs_notify_trigger_create.sql
\ir ./synthetic_ir_curve_template_entries_create.sql
\ir ./synthetic_ir_curve_template_entries_notify_trigger_create.sql

-- Publish-from-DQ
\ir ./synthetic_publish_from_dq_create.sql
