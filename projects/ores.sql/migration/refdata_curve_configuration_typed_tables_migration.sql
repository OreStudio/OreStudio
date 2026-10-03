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
 * One-shot migration: the curve configuration vocabulary and the typed yield
 * curve tables
 *
 * The curve tables held each entry's settings as text and the rest in a
 * semicolon and pipe encoded extras column, and nothing wrote to them. They are
 * replaced by typed tables: a document header, the sections it writes, the
 * curve header, the yield curve settings, the bootstrap settings, segments with
 * a column per setting, the curves a segment lists, and quotes. The curve
 * sections, segment types and day counter spellings are seeded from the ORE
 * schemas, and the tenant provisioner copies them into each tenant.
 *
 * The migration refuses to start while any of the old curve tables holds a
 * row, because their shape cannot be carried into the new tables. A database
 * whose curve tables are empty, which is every database the old tables shipped
 * to, migrates in place.
 *
 * On a freshly recreated database the create and populate scripts already do
 * this, and this migration is unnecessary. It exists for databases created
 * before the change.
 */

begin;

do $$
begin
    if exists (select 1 from ores_refdata_curve_definitions_tbl)
       or exists (select 1 from ores_refdata_curve_segments_tbl)
       or exists (select 1 from ores_refdata_curve_quotes_tbl) then
        raise exception 'The curve tables hold rows in their old shape. '
            'Remove them, or recreate the database, before running this migration.';
    end if;
end;
$$;

\ir ../drop/refdata/refdata_curve_quotes_notify_trigger_drop.sql
\ir ../drop/refdata/refdata_curve_quotes_drop.sql
\ir ../drop/refdata/refdata_curve_segments_notify_trigger_drop.sql
\ir ../drop/refdata/refdata_curve_segments_drop.sql
\ir ../drop/refdata/refdata_curve_definitions_notify_trigger_drop.sql
\ir ../drop/refdata/refdata_curve_definitions_drop.sql

\ir ../create/refdata/refdata_curve_segment_types_create.sql
\ir ../create/refdata/refdata_curve_segment_types_notify_trigger_create.sql
\ir ../create/refdata/refdata_day_counters_create.sql
\ir ../create/refdata/refdata_day_counters_notify_trigger_create.sql
\ir ../create/refdata/refdata_conventions_validate_fn_create.sql
\ir ../create/refdata/refdata_curve_configurations_create.sql
\ir ../create/refdata/refdata_curve_configurations_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_configuration_sections_create.sql
\ir ../create/refdata/refdata_curve_configuration_sections_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_definitions_create.sql
\ir ../create/refdata/refdata_curve_definitions_notify_trigger_create.sql
\ir ../create/refdata/refdata_yield_curves_create.sql
\ir ../create/refdata/refdata_yield_curves_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_bootstrap_configs_create.sql
\ir ../create/refdata/refdata_curve_bootstrap_configs_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_segments_create.sql
\ir ../create/refdata/refdata_curve_segments_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_segment_curves_create.sql
\ir ../create/refdata/refdata_curve_segment_curves_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_quotes_create.sql
\ir ../create/refdata/refdata_curve_quotes_notify_trigger_create.sql
\ir ../create/refdata/refdata_rls_policies_create.sql
\ir ../create/iam/iam_tenant_provisioner_create.sql

\ir ../populate/refdata/refdata_floating_index_types_populate.sql
\ir ../populate/refdata/refdata_curve_sections_populate.sql
\ir ../populate/refdata/refdata_curve_segment_types_populate.sql
\ir ../populate/refdata/refdata_day_counters_populate.sql
\ir ../populate/iam/iam_permissions_populate.sql

commit;
