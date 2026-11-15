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
#include "ores.refdata.core/mapping/quant_codes.hpp"
#include <array>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::refdata::mapping {

namespace {

template <class Value, std::size_t Size>
using code_table = std::array<std::pair<std::string_view, Value>, Size>;

template <class Value, std::size_t Size>
Value parse(const code_table<Value, Size>& table, std::string_view code, std::string_view what) {
    for (const auto& [spelling, value] : table)
        if (spelling == code)
            return value;
    throw std::invalid_argument("Unrecognised " + std::string(what) + " code: '" +
                                std::string(code) + "'");
}

template <class Value, std::size_t Size>
std::string_view spell(const code_table<Value, Size>& table, Value value, std::string_view what) {
    for (const auto& [spelling, held] : table)
        if (held == value)
            return spelling;
    throw std::invalid_argument("The " + std::string(what) + " has no stored code");
}

using bdc = quant_domain::business_day_convention;
constexpr code_table<bdc, 7> business_day_conventions{{
    {"Unadjusted", bdc::unadjusted},
    {"Following", bdc::following},
    {"ModifiedFollowing", bdc::modified_following},
    {"Preceding", bdc::preceding},
    {"ModifiedPreceding", bdc::modified_preceding},
    {"NEAREST", bdc::nearest},
    {"HalfMonthModifiedFollowing", bdc::half_month_modified_following},
}};

using unit = quant_domain::period_unit;
constexpr code_table<unit, 5> period_units{{
    {"DAY", unit::day},
    {"WEEK", unit::week},
    {"MONTH", unit::month},
    {"YEAR", unit::year},
    {"NONE", unit::none},
}};

using kind = quant_domain::tenor_kind;
constexpr code_table<kind, 2> tenor_kinds{{
    {"PERIOD", kind::period},
    {"SPECIAL", kind::special},
}};

using anchor = quant_domain::tenor_anchor;
constexpr code_table<anchor, 6> tenor_anchors{{
    {"IMM_ROLL", anchor::imm_roll},
    {"NEAR_LEG", anchor::near_leg},
    {"NONE", anchor::none},
    {"SPOT", anchor::spot},
    {"TODAY", anchor::today},
    {"TOMORROW", anchor::tomorrow},
}};

using algorithm = quant_domain::resolution_algorithm;
constexpr code_table<algorithm, 2> resolution_algorithms{{
    {"ANCHOR_OFFSET", algorithm::anchor_offset},
    {"SCHEDULE_STEP", algorithm::schedule_step},
}};

using shift = quant_domain::observance_shift;
constexpr code_table<shift, 4> observance_shifts{{
    {"none", shift::none},
    {"nearest_weekday", shift::nearest_weekday},
    {"roll_forward_to_monday", shift::roll_forward_to_monday},
    {"sunday_to_monday", shift::sunday_to_monday},
}};

using role = quant_service::bootstrap_curve_role_code;
constexpr code_table<role, 3> curve_roles{{
    {"DEPOSIT", role::DEPOSIT},
    {"FRA", role::FRA},
    {"SWAP", role::SWAP},
}};

using method = quant_service::interpolation_method_code;
constexpr code_table<method, 3> interpolation_methods{{
    {"LOG_LINEAR_DISCOUNT", method::LOG_LINEAR_DISCOUNT},
    {"FLAT_FORWARD_THEN_LOG_LINEAR", method::FLAT_FORWARD_THEN_LOG_LINEAR},
    {"CUBIC_SPLINE", method::CUBIC_SPLINE},
}};

using day_count = quant_service::day_count_convention_code;
constexpr code_table<day_count, 4> day_counts{{
    {"A360", day_count::A360},
    {"A365", day_count::A365},
    {"A365F", day_count::A365F},
    {"30/360", day_count::THIRTY_360},
}};

}

bdc parse_business_day_convention_code(std::string_view code) {
    return parse(business_day_conventions, code, "business day convention");
}

std::string_view to_business_day_convention_code(bdc convention) {
    return spell(business_day_conventions, convention, "business day convention");
}

unit parse_period_unit_code(std::string_view code) {
    return parse(period_units, code, "tenor unit");
}

std::string_view to_period_unit_code(unit value) {
    return spell(period_units, value, "tenor unit");
}

kind parse_tenor_kind_code(std::string_view code) {
    return parse(tenor_kinds, code, "tenor kind");
}

std::string_view to_tenor_kind_code(kind value) {
    return spell(tenor_kinds, value, "tenor kind");
}

anchor parse_tenor_anchor_code(std::string_view code) {
    return parse(tenor_anchors, code, "tenor anchor");
}

std::string_view to_tenor_anchor_code(anchor value) {
    return spell(tenor_anchors, value, "tenor anchor");
}

algorithm parse_resolution_algorithm_code(std::string_view code) {
    return parse(resolution_algorithms, code, "tenor resolution algorithm");
}

std::string_view to_resolution_algorithm_code(algorithm value) {
    return spell(resolution_algorithms, value, "tenor resolution algorithm");
}

shift parse_observance_shift_code(std::string_view code) {
    return parse(observance_shifts, code, "observance shift");
}

std::string_view to_observance_shift_code(shift value) {
    return spell(observance_shifts, value, "observance shift");
}

role parse_bootstrap_curve_role_code(std::string_view code) {
    return parse(curve_roles, code, "curve role");
}

std::string_view to_bootstrap_curve_role_code(role value) {
    return spell(curve_roles, value, "curve role");
}

method parse_interpolation_method_code(std::string_view code) {
    return parse(interpolation_methods, code, "interpolation method");
}

std::string_view to_interpolation_method_code(method value) {
    return spell(interpolation_methods, value, "interpolation method");
}

day_count parse_day_count_convention_code(std::string_view code) {
    return parse(day_counts, code, "day count convention");
}

std::string_view to_day_count_convention_code(day_count value) {
    return spell(day_counts, value, "day count convention");
}

}
