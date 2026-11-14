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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * Removes the retired "base" bundle and its members.
 *
 * The bundle was renamed "world", and the configuration it carried became the
 * "tenant_defaults" bundle. A database that predates the rename holds the old
 * rows, in the system tenant and in every tenant the provisioner copied them
 * to, so a second run of populate would leave two bundles where the model has
 * one. The rows are deleted rather than closed: a closed row is still read by
 * the next session, which is the failure this script removes.
 *
 * Each table turns a delete into an update of =valid_to= through a rule, so the
 * rule is disabled around the delete, the way the tenant purger removes a
 * tenant's rows. The script is idempotent: a second run finds no "base" row.
 */

\echo '--- Retire the base bundle ---'

alter table ores_dq_dataset_bundle_members_tbl
    disable rule ores_dq_dataset_bundle_members_delete_rule;
delete from ores_dq_dataset_bundle_members_tbl
where bundle_code = 'base';
alter table ores_dq_dataset_bundle_members_tbl
    enable rule ores_dq_dataset_bundle_members_delete_rule;

alter table ores_dq_dataset_bundles_tbl
    disable rule ores_dq_dataset_bundles_delete_rule;
delete from ores_dq_dataset_bundles_tbl
where code = 'base';
alter table ores_dq_dataset_bundles_tbl
    enable rule ores_dq_dataset_bundles_delete_rule;
