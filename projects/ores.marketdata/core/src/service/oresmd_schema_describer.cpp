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
#include "ores.marketdata.core/service/oresmd_schema_describer.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/ore_types.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include <string>
#include <string_view>
#include <utility>

namespace ores::marketdata::service {

namespace {

using namespace ores::marketdata::datum;

std::string_view kind_name(value_kind kind) {
    switch (kind) {
        case value_kind::text:
            return "text";
        case value_kind::term:
            return "term";
        case value_kind::decimal:
            return "decimal";
        case value_kind::strike:
            return "strike";
        case value_kind::code:
            return "code";
        case value_kind::strike_label:
            return "strike_label";
    }
    std::unreachable();
}

std::string_view role_name(field_role role) {
    return role == field_role::identity ? "identity" : "coordinate";
}

messaging::oresmd_field_spec describe_field(const field_spec& spec) {
    messaging::oresmd_field_spec out;
    out.name = std::string(name_of(spec.name));
    out.role = std::string(role_name(spec.role));
    out.may_be_none = spec.may_be_none;
    out.kind = std::string(kind_name(kind_of(spec.name)));
    for (const auto code : codes_of(spec.name))
        out.codes.emplace_back(code);
    return out;
}

messaging::oresmd_instrument describe_instrument(instrument_type type) {
    const auto& row = schema_of(type);
    messaging::oresmd_instrument out;
    out.code = oresmd_uri_codec::instrument_spelling(type);
    out.ore_name = std::string(ore_name(type));
    out.asset = std::string(name_of(row.asset));
    out.subject = std::string(name_of(row.subject));
    for (const auto& spec : row.fields)
        out.fields.push_back(describe_field(spec));
    return out;
}

messaging::oresmd_index_family describe_index(const index_row& row) {
    messaging::oresmd_index_family out;
    out.code = std::string(name_of(row.family));
    out.asset = std::string(name_of(row.asset));
    out.subject = std::string(row.subject);
    for (const auto& spec : row.fields)
        out.fields.push_back({std::string(spec.name), spec.optional});
    return out;
}

messaging::describe_oresmd_schema_response build() {
    messaging::describe_oresmd_schema_response out;
    for (std::size_t i = 0; i < asset_class_count; ++i)
        out.assets.emplace_back(name_of(static_cast<asset_class>(i)));
    for (std::size_t i = 0; i < instrument_type_count; ++i)
        out.instruments.push_back(describe_instrument(static_cast<instrument_type>(i)));
    for (std::size_t i = 0; i < quote_type_count; ++i)
        out.quotes.push_back(oresmd_uri_codec::quote_spelling(static_cast<quote_type>(i)));
    for (const auto& row : index_rows)
        out.indexes.push_back(describe_index(row));
    out.success = true;
    return out;
}

}

messaging::describe_oresmd_schema_response oresmd_schema_describer::describe() {
    // The tables are constants, so the answer is built once and copied.
    static const auto answer = build();
    return answer;
}

}
