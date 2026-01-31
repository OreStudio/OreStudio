/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

drop rule if exists ores_dq_dataset_dependencies_delete_rule on "ores_dq_dataset_dependencies_tbl";
drop trigger if exists ores_dq_dataset_dependencies_insert_trg on "ores_dq_dataset_dependencies_tbl";
drop function if exists ores_dq_dataset_dependencies_insert_fn();
drop index if exists ores_dq_dataset_dependencies_dependency_idx;
drop index if exists ores_dq_dataset_dependencies_dataset_idx;
drop table if exists "ores_dq_dataset_dependencies_tbl";
