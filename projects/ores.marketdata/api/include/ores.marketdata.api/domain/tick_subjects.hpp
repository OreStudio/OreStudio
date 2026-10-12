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

#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The subjects a live market_tick travels on.
 *
 * A producer publishes on "synthetic.v1.ops.tick.<source>", or under the sandbox
 * base, which the ingest loop never subscribes to. The ingest loop
 * republishes each tick to every consumer the source's feed bindings name, on
 * the market_tick subject extended with the consumer and the datum's oresmd
 * URI, encoded as one subject token.
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
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') ||
           c == '-' || c == '_';
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

/// The subject that covers every datum a party reads in one tenant.
inline std::string market_tick_subject_all(std::string_view tenant_id, std::string_view party_id) {
    std::string subject(messaging::market_tick::nats_subject);
    for (const auto part : {tenant_id, party_id})
        subject.append(".").append(part);
    return subject.append(".>");
}

/**
 * @brief The subject a consumer reads one datum's ticks on:
 * "marketdata.v1.ops.market_tick.<tenant>.<party>.<token>", where the token is
 * the datum's canonical oresmd URI as one subject token. Every valid datum has
 * a URI, so every valid quote has a subject, with or without an ORE key.
 */
inline std::string
market_tick_subject(std::string_view tenant_id, std::string_view party_id, std::string_view oresmd_uri) {
    std::string subject(messaging::market_tick::nats_subject);
    for (const auto part : {tenant_id, party_id})
        subject.append(".").append(part);
    return subject.append(".").append(subject_token(oresmd_uri));
}

}

#endif
