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
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include <boost/url.hpp>
#include <algorithm>
#include <format>
#include <optional>
#include <utility>
#include <vector>

namespace ores::marketdata::datum {

namespace {

constexpr std::string_view scheme = "oresmd";
constexpr std::string_view quote_type_value = "quote";
constexpr std::string_view series_type_value = "series";

/// A plus sign is data in a key such as a strike, not an encoded space.
boost::urls::encoding_opts query_encoding() {
    boost::urls::encoding_opts opts;
    opts.space_as_plus = false;
    return opts;
}

std::string lower(std::string_view s) {
    std::string out(s);
    std::ranges::transform(
        out, out.begin(), [](char c) { return c >= 'A' && c <= 'Z' ? char(c - 'A' + 'a') : c; });
    return out;
}

std::string upper(std::string_view s) {
    std::string out(s);
    std::ranges::transform(
        out, out.begin(), [](char c) { return c >= 'a' && c <= 'z' ? char(c - 'a' + 'A') : c; });
    return out;
}

std::unexpected<std::string> refuse(std::string_view uri, std::string_view why) {
    return std::unexpected(std::format("'{}': {}", uri, why));
}

/// A field's value read from its URI text, by the value type the field holds.
std::expected<value, std::string> value_of(field f, std::string_view text) {
    switch (kind_of(f)) {
        case value_kind::text:
            return value(std::string(text));
        case value_kind::term:
            return term::parse(text).transform([](term t) { return value(std::move(t)); });
        case value_kind::decimal:
            return decimal::parse(text).transform([](decimal d) { return value(std::move(d)); });
        case value_kind::strike:
            return strike::parse(text).transform([](strike s) { return value(std::move(s)); });
        case value_kind::code:
            return code::parse(text).transform([](code c) { return value(std::move(c)); });
    }
    return std::unexpected(std::string("unknown value kind"));
}

struct query_entry {
    std::string key;
    std::string text;
};

/// The URI text of a datum the schema and, for a quote, ORE admit.
std::string spelling_of(const market_datum& datum) {
    const auto& row = schema_of(datum.type());

    boost::urls::url u;
    u.set_scheme(scheme);
    u.set_host_name(name_of(row.asset));
    u.set_path_absolute(true);
    for (const auto& fv : datum.fields()) {
        if (fv.name == row.subject)
            u.segments().push_back(text_of(fv.held));
    }

    auto params = u.params(query_encoding());
    params.append({"type", datum.is_series() ? series_type_value : quote_type_value});
    params.append({"instrument", lower(ore_name(datum.type()))});
    params.append({"quote", lower(ore_name(datum.quote()))});
    for (const auto& fv : datum.fields()) {
        if (fv.name == row.subject || std::holds_alternative<none_t>(fv.held))
            continue;
        params.append({name_of(fv.name), text_of(fv.held)});
    }
    return std::string(u.buffer());
}

}

std::expected<market_datum, std::string> oresmd_uri_codec::read(std::string_view uri) {
    const auto parsed = boost::urls::parse_uri(uri);
    if (!parsed)
        return refuse(uri, "not a URI");
    const auto& u = *parsed;
    if (u.scheme() != scheme)
        return refuse(uri, std::format("the scheme is not {}", scheme));
    if (u.has_userinfo() || u.has_port() || u.has_fragment())
        return refuse(uri, "an oresmd URI has no user, port or fragment");

    const auto segments = u.segments();
    if (!u.is_path_absolute() || segments.size() != 1 || segments.front().empty())
        return refuse(uri, "the path must be one segment, the subject");
    const std::string subject_text = *segments.begin();

    std::vector<query_entry> entries;
    for (const auto& param : u.params(query_encoding())) {
        if (param.key.empty() || !param.has_value || param.value.empty())
            return refuse(uri, std::format("the query key '{}' has no value", param.key));
        const bool repeated =
            std::ranges::any_of(entries, [&](const query_entry& e) { return e.key == param.key; });
        if (repeated)
            return refuse(uri, std::format("the query repeats '{}'", param.key));
        entries.push_back({param.key, param.value});
    }
    const auto take = [&](std::string_view key) -> std::optional<std::string> {
        const auto it =
            std::ranges::find_if(entries, [&](const query_entry& e) { return e.key == key; });
        if (it == entries.end())
            return std::nullopt;
        auto text = std::move(it->text);
        entries.erase(it);
        return text;
    };

    const auto type_text = take("type");
    const auto instrument_text = take("instrument");
    const auto quote_text = take("quote");
    if (!type_text || !instrument_text || !quote_text)
        return refuse(uri, "the query needs type, instrument and quote");
    if (*type_text != quote_type_value && *type_text != series_type_value)
        return refuse(uri, std::format("type is quote or series, not '{}'", *type_text));
    const bool series = *type_text == series_type_value;

    const auto instrument = instrument_type_named(upper(*instrument_text));
    if (!instrument || lower(ore_name(*instrument)) != *instrument_text)
        return refuse(uri, std::format("'{}' is not an ORE instrument type", *instrument_text));
    const auto quote = quote_type_named(upper(*quote_text));
    if (!quote || lower(ore_name(*quote)) != *quote_text)
        return refuse(uri, std::format("'{}' is not an ORE quote type", *quote_text));

    const auto& row = schema_of(*instrument);
    if (u.host() != name_of(row.asset))
        return refuse(uri,
                      std::format("a {} belongs to the asset class {}, not {}",
                                  *instrument_text,
                                  name_of(row.asset),
                                  std::string(u.host())));

    std::vector<field_value> fields;
    for (const auto& spec : row.fields) {
        if (series && spec.role == field_role::coordinate)
            continue;
        const auto name = name_of(spec.name);
        if (spec.name == row.subject) {
            if (take(name))
                return refuse(uri,
                              std::format("'{}' is the path; the query must not repeat it", name));
            auto held = value_of(spec.name, subject_text);
            if (!held)
                return refuse(uri, std::format("the path: {}", held.error()));
            fields.push_back({spec.name, std::move(*held)});
            continue;
        }
        const auto text = take(name);
        if (!text) {
            if (!spec.may_be_none)
                return refuse(uri, std::format("the query needs '{}'", name));
            fields.push_back({spec.name, none});
            continue;
        }
        auto held = value_of(spec.name, *text);
        if (!held)
            return refuse(uri, std::format("'{}': {}", name, held.error()));
        fields.push_back({spec.name, std::move(*held)});
    }
    if (!entries.empty()) {
        const auto& key = entries.front().key;
        const auto f = field_named(key);
        const bool coordinate = f && std::ranges::any_of(row.fields, [&](const field_spec& s) {
                                    return s.name == *f && s.role == field_role::coordinate;
                                });
        if (series && coordinate)
            return refuse(uri,
                          std::format("a series has no coordinate, but the query has '{}'", key));
        return refuse(uri, std::format("a {} has no field '{}'", *instrument_text, key));
    }

    auto datum = series ? market_datum::make_series(*instrument, *quote, std::move(fields)) :
                          market_datum::make(*instrument, *quote, std::move(fields));
    if (!datum)
        return refuse(uri, datum.error());
    if (!series) {
        if (const auto key = ore_key_codec::write(*datum); !key)
            return refuse(uri, std::format("no ORE key names this datum: {}", key.error()));
    }
    // One datum has one spelling, so a stored URI can be compared as text: a
    // reordered query or an encoding the writer would not use is refused.
    if (const auto spelling = spelling_of(*datum); spelling != uri)
        return refuse(uri, std::format("the datum is written {}", spelling));
    return std::move(*datum);
}

std::expected<std::string, std::string> oresmd_uri_codec::write(const market_datum& datum) {
    if (!datum.is_series()) {
        if (const auto key = ore_key_codec::write(datum); !key)
            return std::unexpected(std::format("no ORE key names this datum: {}", key.error()));
    }
    return spelling_of(datum);
}

}
