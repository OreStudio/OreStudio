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
-- Indexes and Constraints for Workflow Tables
-- =============================================================================

-- -----------------------------------------------------------------------------
-- Hand-written SQL the generator cannot express
-- -----------------------------------------------------------------------------
-- Two things live here because no model can state them: a partial index, and a
-- cascading foreign key. The four other indexes this file used to carry are
-- generated now, from the models' * Indexes sections.

create index if not exists workflow_instances_correlation_id_idx
on ores_workflow_workflow_instances_tbl (correlation_id)
where correlation_id is not null;

alter table ores_workflow_workflow_steps_tbl
    add constraint ores_workflow_workflow_steps_workflow_id_fk
    foreign key (workflow_id)
    references ores_workflow_workflow_instances_tbl (id)
    on delete cascade;
