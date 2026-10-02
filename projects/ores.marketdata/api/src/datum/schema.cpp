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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.api/datum/schema.hpp"

namespace ores::marketdata::datum {

namespace {

constexpr std::array<std::string_view, field_count> field_names{"ccy",
                                                                "curve_id",
                                                                "day_counter",
                                                                "term",
                                                                "index_name",
                                                                "fwd_start",
                                                                "contract_month",
                                                                "contract",
                                                                "tenor",
                                                                "imm1",
                                                                "imm2",
                                                                "flat_term",
                                                                "maturity",
                                                                "identifier",
                                                                "flat_ccy",
                                                                "float_ccy",
                                                                "float_tenor",
                                                                "fixed_ccy",
                                                                "fixed_tenor",
                                                                "underlying_name",
                                                                "seniority",
                                                                "doc_clause",
                                                                "running_spread",
                                                                "cds_index_name",
                                                                "attachment_point",
                                                                "detachment_point",
                                                                "unit_ccy",
                                                                "quote_tag",
                                                                "expiry",
                                                                "dimension",
                                                                "strike_level",
                                                                "payer_receiver",
                                                                "index_tenor",
                                                                "atm",
                                                                "relative",
                                                                "cap_floor",
                                                                "strike_label",
                                                                "index",
                                                                "seasonality_type",
                                                                "month",
                                                                "eq_name",
                                                                "strike",
                                                                "option_type",
                                                                "security_id",
                                                                "future_contract",
                                                                "qualifier",
                                                                "contract_name",
                                                                "index_term",
                                                                "side",
                                                                "commodity_name",
                                                                "offset",
                                                                "index1",
                                                                "index2",
                                                                "quote_name",
                                                                "delivery_date",
                                                                "start_time_in_sec",
                                                                "time_unit",
                                                                "dst",
                                                                "rating_name",
                                                                "from_rating",
                                                                "to_rating"};

// The vocabularies ORE's parser checks, from OREData's marketdatumparser.cpp
// and parsers.cpp, and QuantExt's intradaypower.cpp.
constexpr std::array<std::string_view, 2> dimensions{"ATM", "Smile"};
constexpr std::array<std::string_view, 2> payer_receiver{"P", "R"};
constexpr std::array<std::string_view, 12> booleans{
    "Y", "YES", "TRUE", "True", "true", "1", "N", "NO", "FALSE", "False", "false", "0"};
constexpr std::array<std::string_view, 2> cap_floor{"C", "F"};
constexpr std::array<std::string_view, 2> option_types{"C", "P"};
constexpr std::array<std::string_view, 6> sides{"Buyer", "Seller", "B", "S", "Payer", "Receiver"};
constexpr std::array<std::string_view, 10> time_units{
    "HOUR", "HOURS", "H", "HR", "HRS", "SECOND", "SECONDS", "S", "SEC", "SECS"};
constexpr std::array<std::string_view, 1> dst{"DST"};

}

std::string_view name_of(asset_class a) {
    switch (a) {
        case asset_class::ir:
            return "ir";
        case asset_class::fx:
            return "fx";
        case asset_class::credit:
            return "credit";
        case asset_class::equity:
            return "equity";
        case asset_class::commodity:
            return "commodity";
        case asset_class::inflation:
            return "inflation";
        case asset_class::security:
            return "security";
        case asset_class::correlation:
            return "correlation";
        case asset_class::rating:
            return "rating";
        case asset_class::shape_profile:
            return "shape_profile";
    }
    return {};
}

std::optional<asset_class> asset_class_named(std::string_view name) {
    for (std::size_t i = 0; i < asset_class_count; ++i) {
        const auto a = static_cast<asset_class>(i);
        if (name_of(a) == name)
            return a;
    }
    return std::nullopt;
}

std::string_view name_of(field f) {
    return field_names[static_cast<std::size_t>(f)];
}

std::optional<field> field_named(std::string_view name) {
    for (std::size_t i = 0; i < field_names.size(); ++i) {
        if (field_names[i] == name)
            return static_cast<field>(i);
    }
    return std::nullopt;
}

std::span<const std::string_view> codes_of(field f) {
    switch (f) {
        case field::dimension:
            return dimensions;
        case field::payer_receiver:
            return payer_receiver;
        case field::atm:
        case field::relative:
            return booleans;
        case field::cap_floor:
            return cap_floor;
        case field::option_type:
            return option_types;
        case field::side:
            return sides;
        case field::time_unit:
            return time_units;
        case field::dst:
            return dst;
        default:
            return {};
    }
}

}
