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
-- Workflow batch links: async bridge between compute batches and workflow steps.
--
-- When the compute service receives a submit_compute workflow step command, it
-- creates a batch and inserts a row here linking the batch_id to the workflow
-- step context (step_id + instance_id). The batch_workflow_bridge polls this
-- table; when the batch reaches a terminal state it publishes a
-- step_completed_event and deletes the row.
-- =============================================================================

CREATE TABLE ores_compute_workflow_batch_links_tbl (
    batch_id           UUID    NOT NULL,
    tenant_id          UUID    NOT NULL,
    workflow_step_id   TEXT    NOT NULL,
    workflow_instance_id TEXT  NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    PRIMARY KEY (batch_id)
);

CREATE INDEX ores_compute_workflow_batch_links_tenant_idx
    ON ores_compute_workflow_batch_links_tbl(tenant_id);
