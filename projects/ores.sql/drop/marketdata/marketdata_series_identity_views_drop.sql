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

-- The asset-class views depend on the identity table, so they go before it.

drop view if exists ores_marketdata_series_identity_commodity_vw;
drop view if exists ores_marketdata_series_identity_correlation_vw;
drop view if exists ores_marketdata_series_identity_credit_vw;
drop view if exists ores_marketdata_series_identity_equity_vw;
drop view if exists ores_marketdata_series_identity_fx_vw;
drop view if exists ores_marketdata_series_identity_inflation_vw;
drop view if exists ores_marketdata_series_identity_ir_vw;
drop view if exists ores_marketdata_series_identity_rating_vw;
drop view if exists ores_marketdata_series_identity_security_vw;
drop view if exists ores_marketdata_series_identity_shape_profile_vw;
