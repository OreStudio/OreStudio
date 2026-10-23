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
#ifndef ORES_MARKETDATA_SERVICE_APP_DELIVERY_METER_HPP
#define ORES_MARKETDATA_SERVICE_APP_DELIVERY_METER_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.service/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <map>
#include <mutex>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Whose ticks were delivered, and of which item, in which hour.
 */
struct delivery_key {
    ores::utility::uuid::tenant_id tenant_id = ores::utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;
    std::string provider_code;
    boost::uuids::uuid source_id;
    std::string oresmd_uri;
    std::chrono::system_clock::time_point period_start;

    bool operator<(const delivery_key& other) const {
        return std::make_tuple(tenant_id.to_string(),
                               party_id,
                               provider_code,
                               source_id,
                               oresmd_uri,
                               period_start) < std::make_tuple(other.tenant_id.to_string(),
                                                               other.party_id,
                                                               other.provider_code,
                                                               other.source_id,
                                                               other.oresmd_uri,
                                                               other.period_start);
    }
};

/// One count to write: the key and the deliveries counted since the last drain.
struct delivery_entry {
    delivery_key key;
    std::int64_t deliveries = 0;
};

/**
 * @brief Counts the ticks that the fan-out delivers to each party, for each item and hour.
 *
 * The only cost at tick time is one addition in memory. A caller adds the deliveries of a batch
 * with one call and a writer drains the counts now and then, so the counts reach the store in
 * batches and not one row for each tick. A mutex guards the counts.
 */
class ORES_MARKETDATA_SERVICE_EXPORT delivery_meter {
public:
    /// The hour that contains @p t.
    [[nodiscard]] static std::chrono::system_clock::time_point
    hour_of(std::chrono::system_clock::time_point t) {
        return std::chrono::floor<std::chrono::hours>(t);
    }

    /// Adds @p count deliveries to @p key.
    void add(const delivery_key& key, std::int64_t count = 1);

    /// Adds every count in @p counts under one lock.
    void add_many(const std::vector<delivery_entry>& counts);

    /// Returns the counts since the last drain, in key order, and starts again from zero.
    [[nodiscard]] std::vector<delivery_entry> drain();

    /// How many different keys are waiting.
    [[nodiscard]] std::size_t pending() const;

private:
    mutable std::mutex mu_;
    std::map<delivery_key, std::int64_t> counts_;
};

}

#endif
