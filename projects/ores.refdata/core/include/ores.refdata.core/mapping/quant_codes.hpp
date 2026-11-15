/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_REFDATA_CORE_MAPPING_QUANT_CODES_HPP
#define ORES_REFDATA_CORE_MAPPING_QUANT_CODES_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.analytics.quant/domain/calendar_rule.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include "ores.refdata.core/export.hpp"
#include <string_view>

/**
 * @brief The codes reference data stores, read into the quant library's types and written back.
 *
 * The quant library holds its own types and knows no stored spelling. A row of reference data
 * names a convention by a code, such as ModifiedFollowing or DEPOSIT, and these functions are the
 * one place that says which code means which value. A reader throws std::invalid_argument for a
 * code it does not know, naming the code, so a row the engines cannot honour is refused rather than
 * mapped to the nearest value. A writer gives the code a value was read from.
 */
namespace ores::refdata::mapping {

namespace quant_domain = ores::analytics::quant::domain;
namespace quant_service = ores::analytics::quant::service;

/// An ores_refdata_business_day_convention_types_tbl code: QuantLib's own enumerator names.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_domain::business_day_convention
parse_business_day_convention_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_business_day_convention_code(quant_domain::business_day_convention convention);

/// An ores_refdata_tenor_units_tbl code.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_domain::period_unit
parse_period_unit_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_period_unit_code(quant_domain::period_unit unit);

/// An ores_refdata_tenor_kinds_tbl code.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_domain::tenor_kind
parse_tenor_kind_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_tenor_kind_code(quant_domain::tenor_kind kind);

/// An ores_refdata_tenor_anchors_tbl code.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_domain::tenor_anchor
parse_tenor_anchor_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_tenor_anchor_code(quant_domain::tenor_anchor anchor);

/// An ores_refdata_tenor_resolution_algorithms_tbl code.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_domain::resolution_algorithm
parse_resolution_algorithm_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_resolution_algorithm_code(quant_domain::resolution_algorithm algorithm);

/// An ores_refdata_calendar_rules_tbl.shift code.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_domain::observance_shift
parse_observance_shift_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_observance_shift_code(quant_domain::observance_shift shift);

/// A curve_role code of an IR curve bootstrap pillar.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_service::bootstrap_curve_role_code
parse_bootstrap_curve_role_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_bootstrap_curve_role_code(quant_service::bootstrap_curve_role_code role);

/// An interpolation_method code of an IR curve bootstrap config.
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_service::interpolation_method_code
parse_interpolation_method_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_interpolation_method_code(quant_service::interpolation_method_code method);

/**
 * @brief A day_count_fraction_type code the day count calculator implements: A360, A365, A365F
 * or 30/360.
 *
 * Every other code the table holds is refused rather than mapped to the nearest implemented one.
 */
[[nodiscard]] ORES_REFDATA_CORE_EXPORT quant_service::day_count_convention_code
parse_day_count_convention_code(std::string_view code);
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::string_view
to_day_count_convention_code(quant_service::day_count_convention_code convention);

}

#endif
