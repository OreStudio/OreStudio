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

-- Drop functions (without signatures to avoid NOTICE when function doesn't exist)
drop function if exists metadata.dq_get_bundle_publication_history_fn;
drop function if exists metadata.dq_bundles_publish_fn;
drop function if exists metadata.dq_bundle_preview_fn;
drop function if exists metadata.dq_bundle_datasets_list_fn;
drop function if exists metadata.dq_bundles_list_fn;

-- Drop indexes
drop index if exists metadata.dq_bundle_publications_published_at_idx;
drop index if exists metadata.dq_bundle_publications_bundle_code_idx;

-- Drop table
drop table if exists metadata.dq_bundle_publications_tbl;
