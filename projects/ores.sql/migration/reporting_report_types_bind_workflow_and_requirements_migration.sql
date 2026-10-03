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
 * One-shot migration: bind the report types to their workflow and their
 * required configuration types
 *
 * A report type now names the workflow its reports run, and a junction lists
 * the configuration types a run of it requires. Only risk is seeded: grid
 * named a tabular export no workflow can run, so it is removed. The report
 * definition's report_type gains its foreign key check against the seeded
 * types.
 *
 * The migration refuses to start while a current report definition names a
 * report type other than risk, because the new check would reject the next
 * version of that definition. Such a definition needs its type changed by
 * hand first.
 *
 * grid is deleted physically, every version of it, with the delete rule
 * disabled for that statement. Closing it as the rule would leaves history
 * rows with no workflow, and the workflow column is required.
 *
 * Setting the workflow column to not null fails loudly if a database holds a
 * report type this migration does not know. A development database that has
 * run the generated eventing tests holds such rows, under test tenants and
 * with generated codes; recreate it rather than migrate it.
 *
 * On a freshly recreated database the create and populate scripts already do
 * this, and this migration is unnecessary. It exists for databases created
 * before the change.
 *
 * The whole migration runs in one transaction, so a failure part-way cannot
 * leave the delete rule disabled or the lookup half seeded.
 */

begin;

do $$
declare
    v_unknown text;
begin
    select string_agg(distinct report_type, ', ' order by report_type)
    into v_unknown
    from ores_reporting_report_definitions_tbl
    where report_type <> 'risk'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_unknown is not null then
        raise exception 'Report definitions name report types other than risk: %. '
            'Change them to risk before running this migration.', v_unknown;
    end if;
end;
$$;

alter table ores_reporting_report_types_tbl
    add column if not exists "workflow_type" text;

alter table ores_reporting_report_types_tbl
    disable rule ores_reporting_report_types_delete_rule;

delete from ores_reporting_report_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and code = 'grid';

alter table ores_reporting_report_types_tbl
    enable rule ores_reporting_report_types_delete_rule;

\ir ../create/reporting/reporting_report_type_configuration_type_create.sql
\ir ../create/reporting/reporting_report_definitions_create.sql

\ir ../populate/reporting/reporting_report_types_populate.sql
\ir ../populate/reporting/reporting_report_type_configuration_types_populate.sql

alter table ores_reporting_report_types_tbl
    alter column "workflow_type" set not null;

commit;
