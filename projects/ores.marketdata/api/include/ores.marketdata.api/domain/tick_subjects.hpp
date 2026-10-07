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
#include <algorithm>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The subjects a live market_tick travels on.
 *
 * A producer publishes on "synthetic.v1.tick.<source>", or under the sandbox
 * prefix, which the ingest loop never subscribes to. The ingest loop
 * republishes each tick to every consumer the source's feed bindings name, on
 * the market_tick subject extended with the consumer and the datum's
 * canonical ORE key.
 */
inline constexpr std::string_view synthetic_tick_subject_prefix = "synthetic.v1.tick.";
inline constexpr std::string_view synthetic_sandbox_tick_subject_prefix =
    "synthetic.v1.sandbox.tick.";

/// Every producer's ticks; '>' because a source name may itself be dotted.
inline std::string synthetic_tick_wildcard() {
    return std::string(synthetic_tick_subject_prefix).append(">");
}

/// Every consumer's ticks.
inline std::string market_tick_wildcard() {
    return std::string(messaging::market_tick::nats_subject).append(".>");
}

/// The subject a producer publishes @p source_name's ticks on.
inline std::string synthetic_tick_subject(std::string_view source_name) {
    return std::string(synthetic_tick_subject_prefix).append(source_name);
}

/**
 * @brief The subject a consumer reads one datum's ticks on:
 * "marketdata.v1.tick.<tenant>.<party>.<key>", where the key is the
 * datum's canonical ORE key, lower-cased, with its slashes turned to dots.
 */
inline std::string
market_tick_subject(std::string_view tenant_id, std::string_view party_id, std::string ore_key) {
    std::ranges::transform(ore_key, ore_key.begin(), [](unsigned char c) {
        return c == '/' ? '.' : static_cast<char>(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
    });
    std::string subject(messaging::market_tick::nats_subject);
    for (const auto part : {tenant_id, party_id, std::string_view(ore_key)})
        subject.append(".").append(part);
    return subject;
}

}

#endif
