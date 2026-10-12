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
#include "ores.marketdata.core/service/oresmd_uri_service.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/ore_types.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include <algorithm>
#include <cctype>
#include <expected>
#include <format>
#include <map>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace ores::marketdata::service {

namespace {

using namespace ores::marketdata::datum;

constexpr std::string_view quote_kind = "quote";
constexpr std::string_view series_kind = "series";
constexpr std::string_view fixing_kind = "fixing";

std::string upper(std::string_view text) {
    std::string out(text);
    std::ranges::transform(out, out.begin(), [](unsigned char c) { return std::toupper(c); });
    return out;
}

std::string too_long(std::string_view what) {
    return std::format("{} is longer than {} characters", what, oresmd_uri_service::max_text);
}

messaging::parse_oresmd_uri_response parsed(const market_datum& datum) {
    const auto& row = schema_of(datum.type());
    messaging::parse_oresmd_uri_response out;
    out.kind = datum.is_series() ? series_kind : quote_kind;
    out.instrument = oresmd_uri_codec::instrument_spelling(datum.type());
    out.quote = oresmd_uri_codec::quote_spelling(datum.quote());
    out.asset = std::string(name_of(row.asset));
    for (const auto& held : datum.fields()) {
        if (held.name == row.subject)
            out.subject = text_of(held.held);
        else if (!std::holds_alternative<none_t>(held.held))
            out.fields.push_back({std::string(name_of(held.name)), text_of(held.held)});
    }
    out.success = true;
    return out;
}

messaging::parse_oresmd_uri_response parsed(const market_index& index) {
    messaging::parse_oresmd_uri_response out;
    out.kind = fixing_kind;
    out.instrument = std::string(name_of(index.family()));
    out.asset = std::string(name_of(index_row_of(index.family()).asset));
    out.subject = index.subject();
    for (const auto& held : index.fields())
        out.fields.push_back({held.name, held.text});
    out.success = true;
    return out;
}

/// Whether the query of @p uri has the key type with the value fixing.
bool states_fixing(std::string_view uri) {
    const auto query = uri.find('?');
    if (query == std::string_view::npos)
        return false;
    for (const std::string_view pair : {"?type=fixing", "&type=fixing"}) {
        for (auto at = uri.find(pair, query); at != std::string_view::npos;
             at = uri.find(pair, at + 1)) {
            const auto end = at + pair.size();
            if (end == uri.size() || uri[end] == '&' || uri[end] == '#')
                return true;
        }
    }
    return false;
}

messaging::parse_oresmd_uri_response refused_parse(std::string why) {
    messaging::parse_oresmd_uri_response out;
    out.message = std::move(why);
    return out;
}

messaging::compose_oresmd_uri_response refused_compose(std::string why) {
    messaging::compose_oresmd_uri_response out;
    out.message = std::move(why);
    return out;
}

messaging::compose_oresmd_uri_response written(std::string uri) {
    messaging::compose_oresmd_uri_response out;
    out.uri = std::move(uri);
    out.success = true;
    return out;
}

std::string limit_breach(const messaging::compose_oresmd_uri_request& request) {
    if (request.subject.size() > oresmd_uri_service::max_text)
        return too_long("the subject");
    if (request.fields.size() > oresmd_uri_service::max_fields)
        return std::format("a URI states at most {} fields", oresmd_uri_service::max_fields);
    for (const auto& field : request.fields) {
        if (field.name.size() > oresmd_uri_service::max_text ||
            field.text.size() > oresmd_uri_service::max_text)
            return too_long("a field");
    }
    return {};
}

messaging::compose_oresmd_uri_response
compose_fixing(const messaging::compose_oresmd_uri_request& request) {
    const auto family = index_family_named(request.instrument);
    if (!family)
        return refused_compose(std::format("'{}' is not an index family", request.instrument));
    // A blank field is one the index leaves out, as for a quote. The index
    // refuses a field given twice, so no duplicate check is made here.
    std::vector<market_index::field_text> fields;
    for (const auto& field : request.fields) {
        if (!field.text.empty())
            fields.push_back({field.name, field.text});
    }
    auto index = market_index::make(*family, request.subject, std::move(fields));
    if (!index)
        return refused_compose(index.error());
    auto uri = oresmd_uri_codec::write_index(*index);
    return uri ? written(std::move(*uri)) : refused_compose(uri.error());
}

messaging::compose_oresmd_uri_response
compose_datum(const messaging::compose_oresmd_uri_request& request, bool series) {
    const auto type = instrument_type_named(upper(request.instrument));
    if (!type || oresmd_uri_codec::instrument_spelling(*type) != request.instrument)
        return refused_compose(
            std::format("'{}' is not an ORE instrument type", request.instrument));
    const auto quote = quote_type_named(upper(request.quote));
    if (!quote || oresmd_uri_codec::quote_spelling(*quote) != request.quote)
        return refused_compose(std::format("'{}' is not an ORE quote type", request.quote));

    const auto& row = schema_of(*type);
    const auto held_by_row = [&](field f) {
        return std::ranges::any_of(row.fields, [&](const field_spec& spec) {
            return spec.name == f && !(series && spec.role == field_role::coordinate);
        });
    };
    std::map<field, std::string> texts;
    for (const auto& given : request.fields) {
        const auto name = field_named(given.name);
        if (!name || !held_by_row(*name) || *name == row.subject) {
            return refused_compose(std::format("'{}' is not a field of {} {}",
                                               given.name,
                                               series ? "a series of" : "a quote of",
                                               request.instrument));
        }
        if (!texts.emplace(*name, given.text).second)
            return refused_compose(std::format("'{}' is given twice", given.name));
    }

    std::vector<field_value> fields;
    for (const auto& spec : row.fields) {
        if (series && spec.role == field_role::coordinate)
            continue;
        const auto found = texts.find(spec.name);
        const std::string& text = spec.name == row.subject ?
                                      request.subject :
                                      (found == texts.end() ? std::string() : found->second);
        if (text.empty()) {
            fields.push_back({spec.name, none_t{}});
            continue;
        }
        auto held = parse_value(spec.name, text);
        if (!held)
            return refused_compose(std::format("{}: {}", name_of(spec.name), held.error()));
        fields.push_back({spec.name, std::move(*held)});
    }
    auto datum = series ? market_datum::make_series(*type, *quote, std::move(fields)) :
                          market_datum::make(*type, *quote, std::move(fields));
    if (!datum)
        return refused_compose(datum.error());
    auto uri = oresmd_uri_codec::write(*datum);
    return uri ? written(std::move(*uri)) : refused_compose(uri.error());
}

}

messaging::parse_oresmd_uri_response
oresmd_uri_service::parse(const messaging::parse_oresmd_uri_request& request) {
    if (request.uri.size() > max_text)
        return refused_parse(too_long("the URI"));
    const auto datum = oresmd_uri_codec::read(request.uri);
    if (datum)
        return parsed(*datum);
    const auto index = oresmd_uri_codec::read_index(request.uri);
    if (index)
        return parsed(*index);
    // A fixing is read by another reader, so its refusal is the one to show
    // when the URI says it is a fixing.
    if (states_fixing(request.uri))
        return refused_parse(index.error());
    return refused_parse(datum.error());
}

messaging::compose_oresmd_uri_response
oresmd_uri_service::compose(const messaging::compose_oresmd_uri_request& request) {
    if (const auto why = limit_breach(request); !why.empty())
        return refused_compose(why);
    if (request.kind == fixing_kind)
        return compose_fixing(request);
    if (request.kind == quote_kind || request.kind == series_kind)
        return compose_datum(request, request.kind == series_kind);
    return refused_compose(std::format("kind is quote, series or fixing, not '{}'", request.kind));
}

}
