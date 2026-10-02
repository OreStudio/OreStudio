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
 * One-shot migration: the order of pricing model products and parameters
 *
 * A PricingEngines document can repeat a product type and a parameter name, so
 * neither identifies a row and neither can recover the order the document
 * wrote. Both tables gain a position column. A product's position is its place
 * in its configuration; a parameter's is its place among the parameters of the
 * same product and scope, which is how the mapper numbers them.
 *
 * The order the document wrote cannot be recovered for rows written before the
 * column existed: rows from one import share their valid_from, and a UUIDv7 id
 * is random within a millisecond. Those rows are numbered deterministically,
 * by their first valid_from and then their id, so an export is at least stable.
 * Re-importing the document restores its own order. Every version of a row
 * shares one id, so each row gets one position across all its versions.
 *
 * On a freshly recreated database the create scripts already emit the column
 * and this migration is unnecessary. It exists for databases created before
 * the change.
 */

alter table ores_analytics_pricing_model_products_tbl
    add column if not exists "position" integer;

update ores_analytics_pricing_model_products_tbl t
set "position" = r.position
from (
    select tenant_id, id,
           row_number() over (partition by tenant_id, pricing_model_config_id
                              order by first_valid_from, id) - 1 as position
    from (select tenant_id, pricing_model_config_id, id,
                 min(valid_from) as first_valid_from
          from ores_analytics_pricing_model_products_tbl
          group by tenant_id, pricing_model_config_id, id) d
) r
where t.tenant_id = r.tenant_id
  and t.id = r.id
  and t."position" is null;

alter table ores_analytics_pricing_model_products_tbl
    alter column "position" set not null;

alter table ores_analytics_pricing_model_product_parameters_tbl
    add column if not exists "position" integer;

update ores_analytics_pricing_model_product_parameters_tbl t
set "position" = r.position
from (
    select tenant_id, id,
           row_number() over (partition by tenant_id, pricing_model_config_id,
                                           pricing_model_product_id, parameter_scope
                              order by first_valid_from, id) - 1 as position
    from (select tenant_id, pricing_model_config_id, pricing_model_product_id,
                 parameter_scope, id, min(valid_from) as first_valid_from
          from ores_analytics_pricing_model_product_parameters_tbl
          group by tenant_id, pricing_model_config_id, pricing_model_product_id,
                   parameter_scope, id) d
) r
where t.tenant_id = r.tenant_id
  and t.id = r.id
  and t."position" is null;

alter table ores_analytics_pricing_model_product_parameters_tbl
    alter column "position" set not null;
