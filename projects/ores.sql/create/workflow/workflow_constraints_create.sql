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
-- The partial index lives here because no model can state it. The four other
-- indexes this file used to carry are generated now, from the models' * Indexes
-- sections.

create index if not exists workflow_instances_correlation_id_idx
on ores_workflow_workflow_instances_tbl (correlation_id)
where correlation_id is not null;

-- A cascading foreign key from a step to its instance used to sit here, and it
-- cannot exist. The parent is temporal, so its only uniqueness on
-- (tenant_id, id) is a partial index over the open row, and PostgreSQL refuses a
-- partial index as a foreign key target. Its full key adds the parent's own
-- lifetime, which the child does not carry.
--
-- Nothing is lost by removing it: no database ever accepted it, so the create
-- path failed outright and it protected nothing. What it was meant to close --
-- step rows left behind when an instance row is removed -- is a real gap, and it
-- cannot be closed in the schema. The removal is a physical delete, so an
-- instance can orphan its steps, and the delete path is where that belongs.
