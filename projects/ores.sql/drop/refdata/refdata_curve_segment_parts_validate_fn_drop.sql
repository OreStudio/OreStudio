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

drop function if exists ores_refdata_validate_curve_segment_convention_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_pillar_rule_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_projection_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_tenor_basis_curves_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_cross_currency_terms_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_reference_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_weighted_average_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_discount_ratio_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_extrapolation_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_ibor_fallback_fn(uuid, uuid);
drop function if exists ores_refdata_validate_curve_segment_part_fn(uuid, uuid, text, text[]);
