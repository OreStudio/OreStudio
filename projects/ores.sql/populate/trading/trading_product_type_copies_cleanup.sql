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
 * Removes the per-tenant copies of the product type catalogue.
 *
 * The catalogue was seeded into every tenant, so a tenant read its own copy of
 * the product types, their markets and the product families. It is one
 * catalogue now: the rows live in the system tenant, every tenant reads them,
 * and a tenant's writes stay its own. The tables carry the own-or-system-tenant
 * policy, so a tenant that held a copy would read both it and the system's and
 * see every type twice.
 *
 * The script is idempotent: it deletes rows no tenant may write any more, and a
 * second run finds none.
 */

\echo '--- Remove the per-tenant product type catalogues ---'

delete from ores_trading_product_type_asset_classes_tbl
where tenant_id <> ores_utility_system_tenant_id_fn();

delete from ores_trading_product_types_tbl
where tenant_id <> ores_utility_system_tenant_id_fn();

delete from ores_refdata_product_family_codes_tbl
where tenant_id <> ores_utility_system_tenant_id_fn();
