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
#ifndef ORES_MARKETDATA_CLIENT_DETAIL_SUBJECT_HELPERS_HPP
#define ORES_MARKETDATA_CLIENT_DETAIL_SUBJECT_HELPERS_HPP

#include <algorithm>
#include <string>

namespace ores::marketdata::client::detail {

/**
 * @brief Convert an ORE canonical key to a per-party NATS fan-out subject.
 *
 * "marketdata.v1.tick.<tenant_id>.<workspace_id>.<party_id>.fx.rate.eur.usd"
 *
 * The marketdata ingest loop republishes every tick on exactly this shape
 * (see the simulated-market-data strategy: one stream per (tenant,
 * workspace, party, ore_key), so each party's realtime stream is its own).
 */
inline std::string ore_key_to_subject(std::string ore_key,
                                      const std::string& tenant_id,
                                      const std::string& workspace_id,
                                      const std::string& party_id) {
    std::transform(ore_key.begin(), ore_key.end(), ore_key.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    std::replace(ore_key.begin(), ore_key.end(), '/', '.');
    return "marketdata.v1.tick." + tenant_id + "." + workspace_id + "." + party_id + "." + ore_key;
}

}

#endif
