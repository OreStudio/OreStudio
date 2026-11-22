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
 * Knowledge pages DQ dataset
 *
 * Registers the dataset orgmode.knowledge_pages, ORE's user-facing org pages,
 * a member of the system_core bundle, so the publication writes them into the
 * system tenant at bring-up and every tenant reads them there.
 *
 * The pages themselves are not in the tree. The build generates their artefact
 * rows from every page that declares #+audience: user, and compass db recreate
 * passes that file as the psql variable knowledge_pages_populate. A run without
 * it, such as a test database, registers the dataset and stages no pages.
 */

\echo '--- DQ Dataset: Knowledge Pages ---'

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'orgmode.knowledge_pages',
        'ORE',
        'General',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'ORE Internal',
        'Knowledge Pages',
        'ORE Studio''s user-facing knowledge base: every org page that declares #+audience: user, as the repository holds it.',
        'ORE Studio',
        'The pages the knowledge search reads and ores.web shows',
        current_date,
        'GPL-3.0',
        'knowledge_pages'
    );
end $$;

\if :{?knowledge_pages_populate}
\echo '--- DQ Artefact: Knowledge Pages (' :knowledge_pages_populate ') ---'
\i :knowledge_pages_populate
\else
\echo '--- DQ Artefact: Knowledge Pages: no generated pages passed; none staged ---'
\endif
