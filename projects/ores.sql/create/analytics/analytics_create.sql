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
-- Analytics Component
-- =============================================================================
-- Creates analytics tables for pricing configuration, model selection, and
-- valuation settings. Depends on:
-- - ores.iam (for tenant validation)
-- - ores.dq (for change reason validation)
-- - ores.trading (for trade_types reference in pricing_engine_types)

-- Reference data (pricing engine type taxonomy)
\ir ./analytics_pricing_engine_types_create.sql
\ir ./analytics_pricing_engine_types_notify_trigger_create.sql

-- Pricing model configuration (header)
\ir ./analytics_pricing_model_configs_create.sql
\ir ./analytics_pricing_model_configs_notify_trigger_create.sql

-- Pricing model products (detail rows)
\ir ./analytics_pricing_model_products_create.sql
\ir ./analytics_pricing_model_products_notify_trigger_create.sql

-- Pricing model product parameters (normalised key-value pairs)
\ir ./analytics_pricing_model_product_parameters_create.sql
\ir ./analytics_pricing_model_product_parameters_notify_trigger_create.sql
