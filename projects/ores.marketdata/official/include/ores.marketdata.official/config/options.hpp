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
#ifndef ORES_MARKETDATA_OFFICIAL_CONFIG_OPTIONS_HPP
#define ORES_MARKETDATA_OFFICIAL_CONFIG_OPTIONS_HPP

#include "ores.logging/logging_options.hpp"
#include "ores.marketdata.official/domain/source_feed.hpp"
#include "ores.nats/config/nats_options.hpp"
#include <cstdint>
#include <iosfwd>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::official::config {

struct options final {
    std::optional<ores::logging::logging_options> logging;
    ores::nats::config::nats_options nats;
    /// The address of Frankfurter: the hosted service, or a team's own.
    std::string frankfurter_url;
    /// The sources the adapter hosts.
    std::vector<domain::hosted_source> sources;
    /// The days a source sends when a currency is first demanded.
    std::uint32_t backfill_days{400};
    /// The minutes between two reads of a source once its demand is served.
    std::uint32_t poll_minutes{60};
};

std::ostream& operator<<(std::ostream& s, const options& v);

/**
 * @brief Reads one source as =<source name>:<provider code>:<Frankfurter key>=, such as
 * =ecb-frankfurter:ecb:ECB=.
 */
std::optional<domain::hosted_source> parse_source(const std::string& text);

}

#endif
