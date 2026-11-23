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

-- =============================================================================
-- ORE Tables
-- =============================================================================
-- The description of ORE itself: the files of an ORE run, the groups they fall
-- in and the contents of each file as an ORE release ships it.

\ir ./ore_ore_file_categories_create.sql
\ir ./ore_ore_file_categories_notify_trigger_create.sql
\ir ./ore_ore_files_create.sql
\ir ./ore_ore_files_notify_trigger_create.sql
\ir ./ore_ore_file_versions_create.sql
\ir ./ore_ore_file_versions_notify_trigger_create.sql

-- The parameters of ORE's documents: their value domains, their value sets and
-- the values they allow, the vocabulary that names a set for each leaf, the
-- element map, and the functions that check a value against its set.
\ir ./ore_ore_parameter_value_domains_create.sql
\ir ./ore_ore_parameter_value_domains_notify_trigger_create.sql
\ir ./ore_ore_parameters_create.sql
\ir ./ore_ore_parameters_notify_trigger_create.sql
\ir ./ore_ore_parameter_value_set_catalogues_create.sql
\ir ./ore_ore_parameter_value_set_catalogues_notify_trigger_create.sql
\ir ./ore_ore_element_map_columns_create.sql
\ir ./ore_ore_element_map_columns_notify_trigger_create.sql
\ir ./ore_ore_parameter_vocabularies_create.sql
\ir ./ore_ore_parameter_vocabularies_notify_trigger_create.sql
\ir ./ore_ore_parameter_value_validate_fn_create.sql
\ir ./ore_ore_parameter_value_sets_create.sql
\ir ./ore_ore_parameter_value_sets_notify_trigger_create.sql
\ir ./ore_ore_parameter_value_set_values_create.sql
\ir ./ore_ore_parameter_value_set_values_notify_trigger_create.sql
\ir ./ore_ore_parameter_setting_value_sets_create.sql
\ir ./ore_ore_parameter_setting_value_sets_notify_trigger_create.sql
\ir ./ore_ore_parameter_canonical_value_fn_create.sql

-- The publish-from-DQ functions of the description of ORE.
\ir ./ore_publish_from_dq_create.sql

\ir ./ore_ore_parameter_settings_view_create.sql
