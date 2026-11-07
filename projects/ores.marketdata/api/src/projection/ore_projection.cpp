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
#include "ores.marketdata.api/projection/ore_projection.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/ore_index_codec.hpp"
#include "ores.marketdata.api/datum/ore_key_codec.hpp"
#include <format>
#include <utility>

namespace ores::marketdata::projection {

namespace {

using namespace ores::logging;

auto& lg() {
    static auto instance = make_logger("ores.marketdata.projection.ore_projection");
    return instance;
}

/// Whether the text is spelled as an oresmd address, whatever it names.
bool looks_like_an_address(std::string_view text) {
    return text.starts_with("oresmd://");
}

}

ore_projection_error::ore_projection_error(projection_refusal refusal)
    : std::runtime_error(std::format(
          "the ORE projection cannot write '{}': {}", refusal.reference, refusal.reason))
    , kind_(refusal.kind)
    , reference_(std::move(refusal.reference)) {}

ore_projection::ore_projection(std::unordered_map<std::string, std::string> curve_names)
    : curve_names_(std::move(curve_names)) {}

std::expected<std::string, projection_refusal>
ore_projection::ore_name_of(const datum::market_reference& reference) const {
    if (const auto* index = reference.index())
        return datum::ore_index_codec::write(*index);

    const auto* datum = reference.datum();
    if (!datum->is_series()) {
        auto key = datum::ore_key_codec::write(*datum);
        if (!key)
            return std::unexpected(projection_refusal{
                projection_refusal_kind::ore_has_no_name, reference.uri(), std::move(key.error())});
        return std::move(*key);
    }

    const auto named = curve_names_.find(reference.uri());
    if (named == curve_names_.end())
        return std::unexpected(projection_refusal{
            projection_refusal_kind::configuration_names_no_curve,
            reference.uri(),
            std::format("the market configuration gives the curve '{}' no ORE name",
                        reference.uri())});
    return named->second;
}

std::string ore_projection::ore_name_or_stored(std::string_view stored) const {
    const auto reference = datum::market_reference::parse(stored);
    if (!reference) {
        if (looks_like_an_address(stored)) {
            BOOST_LOG_SEV(lg(), error) << "A stored reference is spelled as an oresmd address and "
                                          "the grammar refuses it: '"
                                       << stored << "': " << reference.error();
            throw ore_projection_error(
                projection_refusal{projection_refusal_kind::address_the_grammar_refuses,
                                   std::string(stored),
                                   reference.error()});
        }
        BOOST_LOG_SEV(lg(), debug)
            << "A stored reference is not an oresmd address, so it is written as stored: '"
            << stored << "'";
        return std::string(stored);
    }

    auto name = ore_name_of(*reference);
    if (name)
        return std::move(*name);

    const auto& refusal = name.error();
    if (refusal.kind == projection_refusal_kind::configuration_names_no_curve) {
        BOOST_LOG_SEV(lg(), warn)
            << "A stored curve series is written as the trade holds it, because this run's market "
               "configuration gives it no ORE name: '"
            << refusal.reference << "'";
        return std::string(stored);
    }

    BOOST_LOG_SEV(lg(), error) << "A stored reference has no ORE name: '" << refusal.reference
                               << "': " << refusal.reason;
    throw ore_projection_error(refusal);
}

}
