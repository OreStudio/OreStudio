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
 * Badge Severities Artefact Population Script
 *
 * Populates the dq_badge_severities_artefact_tbl staging table. Mirrors the
 * same six severities seeded directly into ores_dq_badge_severities_tbl (for
 * the system tenant itself) by dq_badge_system_populate.sql -- kept in sync
 * by hand; this script is what makes those rows publishable to other tenants.
 *
 * To publish to a tenant:
 *   SELECT * FROM ores_dq_badge_severities_publish_fn(
 *       (SELECT id FROM ores_dq_datasets_tbl WHERE code = 'ore.badge_severities' AND valid_to = ores_utility_infinity_timestamp_fn()),
 *       <target_tenant_id>,
 *       'upsert'
 *   );
 */

\echo '--- Badge Severities Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl where code = 'ore.badge_severities' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_badge_severities_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_badge_severities_artefact_tbl (
    tenant_id, dataset_id, code, version, name, description, display_order
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'secondary', 0, 'Secondary',
     'Muted or neutral state. No special attention required.', 1),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'info', 0, 'Info',
     'Informational state. General context, no action needed.', 2),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'success', 0, 'Success',
     'Positive or active state. Everything is in order.', 3),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'warning', 0, 'Warning',
     'Caution state. Attention may be required.', 4),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'danger', 0, 'Danger',
     'Error or critical state. Immediate attention required.', 5),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'primary', 0, 'Primary',
     'Highlighted or primary action state.', 6);
