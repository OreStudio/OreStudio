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
 * One-shot migration: seed the configuration types with their ORE mapping
 *
 * The configuration_types lookup existed with a code, a name and a display
 * order, and no rows, so no reporting configuration could be registered. Each
 * kind now also records the root element of the ORE document it serialises to,
 * the component that owns its entities, and the run-document parameter that
 * names its file. The seventeen kinds are seeded by the same script a recreate
 * runs.
 *
 * The seed fills the mapping columns of a row that already holds one of the
 * seventeen codes. Setting the two required columns to not null afterwards
 * fails loudly only if a database held a configuration type this migration
 * does not know, which is the right outcome: such a row needs its mapping
 * written by hand. A development database that has run the generated eventing
 * tests holds such rows, under test tenants and with generated codes; recreate
 * it rather than migrate it.
 *
 * On a freshly recreated database the create and populate scripts already do
 * this, and this migration is unnecessary. It exists for databases created
 * before the change.
 */

alter table ores_reporting_configuration_types_tbl
    add column if not exists "ore_root_element" text,
    add column if not exists "owning_component" text,
    add column if not exists "run_parameter" text;

\ir ../populate/reporting/reporting_configuration_types_populate.sql

alter table ores_reporting_configuration_types_tbl
    alter column "ore_root_element" set not null,
    alter column "owning_component" set not null;
