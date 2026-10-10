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
#ifndef ORES_TRADING_API_DOMAIN_ECONOMIC_DIGEST_HPP
#define ORES_TRADING_API_DOMAIN_ECONOMIC_DIGEST_HPP

#include "ores.utility/crypto/sha256.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <format>
#include <iterator>
#include <optional>
#include <rfl.hpp>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

namespace detail {

/*
 * The digest covers the economics the customer agreed to, and nothing else.
 * This module decides inclusion by name, which is a first cut: a name can
 * state what a field is called, not what it means. A later task declares the
 * economic fields in the model and replaces this rule.
 *
 * The names below are identity or audit. A name ending in _id also
 * identifies this row or another one, so the suffix rule excludes every _id
 * field except the external identifiers listed below it.
 */
inline constexpr std::string_view excluded_field_names[] = {
    "id",
    "version",
    "external_version",
    "economic_digest",
    "trade_type_code",
    "description",
    "modified_by",
    "performed_by",
    "change_reason_code",
    "change_commentary",
    "recorded_at",
    "valid_from",
    "valid_to",
};

/*
 * An external identifier is not a foreign key. security_id is the ISIN, and
 * a trade re-pointed to a different security is a change the customer agreed
 * to, so these names survive the _id rule below. A *_id name that is not
 * here stays excluded: an unknown one is far more likely to be a foreign key,
 * and wrongly including it would move the digest for a change nobody agreed
 * to.
 */
inline constexpr std::string_view economic_identifier_names[] = {
    "security_id",
    "credit_curve_id",
    "reference_curve_id",
    "income_curve_id",
    "volatility_curve_id",
};

constexpr bool is_economic_identifier(std::string_view name) {
    for (const auto economic : economic_identifier_names)
        if (name == economic)
            return true;
    return false;
}

constexpr bool is_identity_or_audit(std::string_view name) {
    for (const auto excluded : excluded_field_names)
        if (name == excluded)
            return true;
    if (is_economic_identifier(name))
        return false;
    return name.size() > 3u && name.ends_with("_id");
}

template <typename>
inline constexpr bool always_false = false;

/*
 * A frame states a value's length before its bytes, so a value never runs
 * into the one after it. Two different values cannot frame to the same text.
 */
inline std::string frame(std::string_view payload) {
    std::string out;
    out.append(std::to_string(payload.size()));
    out.push_back(':');
    out.append(payload);
    return out;
}

template <typename T>
std::string canonical_value(const T& v);

template <typename E>
std::string canonical_enum(const E& v) {
    if constexpr (requires { to_string(v); })
        return frame(to_string(v));
    else
        return frame(std::to_string(static_cast<std::underlying_type_t<E>>(v)));
}

/*
 * Walks the value's fields with rfl. An included field carries its own name,
 * as a plain identifier, then its canonical value, already framed, so the
 * name and the value never merge.
 */
template <typename T>
std::string canonical_struct(const T& v) {
    std::string out = "{";
    rfl::to_view(v).apply([&](const auto& field) {
        using field_type = std::remove_cvref_t<decltype(field)>;
        if constexpr (!is_identity_or_audit(field_type::name())) {
            out.append(field_type::name());
            out.push_back('=');
            out.append(canonical_value(*field.value()));
            out.push_back(';');
        }
    });
    out.push_back('}');
    return out;
}

/*
 * Renders one field canonically. The same value always gives the same
 * bytes: a decimal renders its own text and never passes through a double.
 */
template <typename T>
std::string canonical_value(const T& v) {
    using V = std::remove_cvref_t<T>;

    if constexpr (std::is_same_v<V, ores::utility::decimal::decimal>) {
        return frame(v.to_string());
    } else if constexpr (std::is_same_v<V, bool>) {
        return frame(v ? "true" : "false");
    } else if constexpr (std::is_integral_v<V>) {
        return frame(std::to_string(v));
    } else if constexpr (std::is_floating_point_v<V>) {
        return frame(std::format("{:.17g}", v));
    } else if constexpr (std::is_enum_v<V>) {
        return canonical_enum(v);
    } else if constexpr (std::is_same_v<V, std::string> || std::is_same_v<V, std::string_view>) {
        return frame(v);
    } else if constexpr (std::is_same_v<V, std::chrono::year_month_day>) {
        return frame(std::format("{:04d}-{:02d}-{:02d}",
                                 static_cast<int>(v.year()),
                                 static_cast<unsigned>(v.month()),
                                 static_cast<unsigned>(v.day())));
    } else if constexpr (std::is_same_v<V, std::chrono::system_clock::time_point>) {
        /*
         * A time is rendered by its own tick count, not by a calendar format,
         * so the rendering depends on the value and not on a locale, a
         * timezone or a format library's version.
         */
        return frame(std::to_string(static_cast<long long>(v.time_since_epoch().count())));
    } else if constexpr (requires {
                             v.has_value();
                             *v;
                         }) {
        if (!v.has_value())
            return std::string("none");
        return std::string("some") + canonical_value(*v);
    } else if constexpr (requires {
                             v.begin();
                             v.end();
                             typename V::value_type;
                         }) {
        std::string out = "seq";
        out.append(std::to_string(std::distance(v.begin(), v.end())));
        out.push_back('{');
        std::size_t index = 0;
        for (const auto& element : v) {
            out.append(std::to_string(index));
            out.push_back('=');
            out.append(canonical_value(element));
            out.push_back(';');
            ++index;
        }
        out.push_back('}');
        return out;
    } else if constexpr (std::is_class_v<V> && std::is_aggregate_v<V>) {
        return canonical_struct(v);
    } else {
        static_assert(always_false<V>,
                      "economic_digest cannot render this field type. Add a branch to "
                      "canonical_value.");
    }
}

}

/**
 * @brief The SHA-256 digest of a value's economic fields, lowercase hex.
 *
 * Walks the fields of @p v generically, skips the ones named identity or
 * audit, renders each remaining field canonically and hashes the result. Two
 * values with the same economics give the same digest; a change to a field
 * the customer did not agree to leaves the digest alone.
 *
 * The exclusion rule is a first cut and lives in @c excluded_field_names.
 *
 * @param v The instrument, trade or component value to digest.
 * @return The lowercase hex SHA-256 digest.
 */
template <typename T>
std::string economic_digest(const T& v) {
    return ores::utility::crypto::sha256::hex_digest(detail::canonical_value(v));
}

}

#endif
