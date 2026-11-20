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
#ifndef ORES_MARKETDATA_API_DOMAIN_TICK_SUBJECTS_HPP
#define ORES_MARKETDATA_API_DOMAIN_TICK_SUBJECTS_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include <algorithm>
#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace ores::marketdata::domain {

/**
 * @brief The subjects a live market_tick travels on.
 *
 * A producer publishes on "synthetic.v1.ops.tick.<source>", or under the sandbox
 * base, which the ingest loop never subscribes to. The ingest loop publishes each
 * tick once on the market_tick subject of the source's feed, feed_tick_subject(),
 * which names the provider, the feed, the instrument type, the series and the
 * coordinate.
 *
 * These name the base of each family, not a subject a producer uses and not
 * the wildcard a reader wants. A subscription passes the relative wildcard,
 * "<base>.>", because ores.nats::client::subscribe() adds the environment
 * prefix itself. A stream filter needs the prefixed subjects:
 * `covering_subjects({base})` provisions the stream that holds them.
 */
inline constexpr std::string_view synthetic_tick_subject = "synthetic.v1.ops.tick";
inline constexpr std::string_view synthetic_sandbox_tick_subject = "synthetic.v1.ops.sandbox_tick";

/// The subject a producer publishes @p source_name's ticks on.
inline std::string synthetic_tick_subject_for(std::string_view source_name) {
    return std::string(synthetic_tick_subject).append(".").append(source_name);
}

/**
 * @brief Text as one NATS subject token.
 *
 * A subject token may not hold a dot, a space, a wildcard or a control
 * character, and tools handle only letters, digits, hyphens and underscores
 * without trouble. Every other byte becomes a percent sign and its two upper
 * case hexadecimal digits, so the token holds nothing but those characters and
 * @c % and @c A-F, and @c subject_token_text() gives the text back.
 */
inline bool is_plain(unsigned char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' ||
           c == '_';
}

inline std::string subject_token(std::string_view text) {
    constexpr std::string_view hex = "0123456789ABCDEF";
    std::string token;
    token.reserve(text.size());
    for (const unsigned char c : text) {
        if (is_plain(c)) {
            token.push_back(static_cast<char>(c));
        } else {
            token.push_back('%');
            token.push_back(hex[c >> 4]);
            token.push_back(hex[c & 0x0F]);
        }
    }
    return token;
}

/// The text a @c subject_token() was made from, or nothing for a token it could not have made.
inline std::optional<std::string> subject_token_text(std::string_view token) {
    const auto digit = [](char c) -> int {
        if (c >= '0' && c <= '9')
            return c - '0';
        if (c >= 'A' && c <= 'F')
            return c - 'A' + 10;
        return -1;
    };
    std::string text;
    text.reserve(token.size());
    for (std::size_t i = 0; i < token.size(); ++i) {
        const char c = token[i];
        if (c != '%') {
            if (!is_plain(static_cast<unsigned char>(c)))
                return std::nullopt;
            text.push_back(c);
            continue;
        }
        if (i + 2 >= token.size())
            return std::nullopt;
        const int high = digit(token[i + 1]);
        const int low = digit(token[i + 2]);
        if (high < 0 || low < 0)
            return std::nullopt;
        const auto byte = static_cast<char>((high << 4) | low);
        // The encoder never escapes a plain byte, so a token that does has another maker.
        if (is_plain(static_cast<unsigned char>(byte)))
            return std::nullopt;
        text.push_back(byte);
        i += 2;
    }
    return text;
}

/// The coordinate token of a series, which has no coordinates.
inline constexpr std::string_view no_coordinates_token = "_";

/**
 * @brief The coordinates of @p datum as one subject token: each coordinate field
 * that holds a value, as "<name>=<value>" in the schema row's order, joined by
 * "&" and encoded by @c subject_token(). A datum with none gets
 * @c no_coordinates_token, which no encoded coordinate can equal because each
 * holds an encoded "=".
 */
inline std::string coordinates_token(const datum::market_datum& datum) {
    const auto& row = datum::schema_of(datum.type());
    std::string text;
    for (const auto& fv : datum.fields()) {
        const auto spec = std::ranges::find(row.fields, fv.name, &datum::field_spec::name);
        if (spec == row.fields.end() || spec->role != datum::field_role::coordinate ||
            std::holds_alternative<datum::none_t>(fv.held))
            continue;
        if (!text.empty())
            text.push_back('&');
        text.append(datum::name_of(fv.name)).append("=").append(datum::text_of(fv.held));
    }
    return text.empty() ? std::string(no_coordinates_token) : subject_token(text);
}

/// Every token of a feed's tick subject up to the series, which the coordinate token follows.
inline std::string feed_series_subject(std::string_view provider_code,
                                       std::string_view feed_id,
                                       datum::instrument_type type,
                                       std::string_view series_uri) {
    std::string subject(messaging::market_tick::nats_subject);
    subject.append(".").append(subject_token(provider_code));
    subject.append(".").append(feed_id);
    subject.append(".").append(datum::oresmd_uri_codec::instrument_spelling(type));
    return subject.append(".").append(subject_token(series_uri));
}

/**
 * @brief The subject a feed's ticks of one datum travel on:
 * "marketdata.v1.ops.market_tick.<provider>.<feed>.<instrument_type>.<series_token>.<coordinate_token>".
 *
 * The subject names the feed and no tenant or party, so the ingest publishes a tick
 * once, whatever the number of parties that may watch the feed. The series token is
 * the encoded @p series_uri, so one wildcard covers a series, such as a volatility
 * surface.
 */
inline std::string feed_tick_subject(std::string_view provider_code,
                                     std::string_view feed_id,
                                     const datum::market_datum& datum,
                                     std::string_view series_uri) {
    return feed_series_subject(provider_code, feed_id, datum.type(), series_uri)
        .append(".")
        .append(coordinates_token(datum));
}

/**
 * @brief The subject a watcher of @p datum subscribes to on one feed.
 *
 * A point watches its own subject. A series has no subject of its own, so it watches its
 * series token with a wildcard for the coordinate, and the broker sends every point of the
 * series and nothing else. The error is the codec's own message.
 */
inline std::expected<std::string, std::string> feed_tick_watch_subject(
    std::string_view provider_code, std::string_view feed_id, const datum::market_datum& datum) {
    const auto series = datum::oresmd_uri_codec::write(datum::series_of(datum));
    if (!series)
        return std::unexpected(series.error());
    if (!datum.is_series())
        return feed_tick_subject(provider_code, feed_id, datum, *series);
    return feed_series_subject(provider_code, feed_id, datum.type(), *series).append(".*");
}

/// The feed token that stands for every feed of a provider in a watch subject.
inline constexpr std::string_view any_feed = "*";

/**
 * @brief The subjects a watcher of @p series_uris subscribes to, on every feed of
 * @p provider_code: each series' token with a wildcard for the feed and for the
 * coordinate.
 *
 * A producer that publishes through a provider's adapter does not know the feed the
 * ingest delivers its ticks on, but it knows the provider and the series it quotes,
 * which is enough to name where its ticks land. A series URI the codec cannot read
 * is left out.
 */
inline std::vector<std::string>
provider_series_watch_subjects(std::string_view provider_code,
                               const std::vector<std::string>& series_uris) {
    std::vector<std::string> out;
    out.reserve(series_uris.size());
    for (const auto& uri : series_uris) {
        const auto series = datum::oresmd_uri_codec::read(uri);
        if (!series)
            continue;
        auto subject = feed_tick_watch_subject(provider_code, any_feed, *series);
        if (subject)
            out.push_back(std::move(*subject));
    }
    return out;
}

}

#endif
