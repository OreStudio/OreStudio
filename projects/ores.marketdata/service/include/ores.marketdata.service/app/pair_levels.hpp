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
#ifndef ORES_MARKETDATA_SERVICE_APP_PAIR_LEVELS_HPP
#define ORES_MARKETDATA_SERVICE_APP_PAIR_LEVELS_HPP

#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.service/app/fixing_sources.hpp"
#include "ores.marketdata.service/export.hpp"
#include <functional>
#include <string>

namespace ores::marketdata::service::app {

/**
 * @brief Reads the fixings of one item over the window a pair was asked for.
 *
 * The answer is the fixing window the service already serves, so a refusal it carries,
 * such as a party the caller cannot see, is the refusal the pair's caller receives.
 */
using fixing_window_reader = std::function<messaging::get_fixing_window_response(
    const std::string& provider_code, const std::string& item)>;

/**
 * @brief Composes a pair's daily levels from the fixings one party holds.
 *
 * The pair is read from the source @c sources_for_pairs chooses for it, through the base
 * that source quotes every currency against, so the base cancels and no two publishers
 * are mixed. Each side of the pair that is not the base is one fixing series of that
 * source; the levels are what @c replay_fixings reads from them, and a day the source
 * did not fix both sides is left out rather than carried forward.
 *
 * @param party What the party's fixing series say about the sources it holds.
 * @param request The pair, the party and the window.
 * @param read How to read one fixing series over the window.
 * @return The levels, the provider and the pivot; or the refusal, naming the pair.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT messaging::get_pair_levels_response
compose_pair_levels(const party_fixing_sources& party,
                    const messaging::get_pair_levels_request& request,
                    const fixing_window_reader& read);

}

#endif
