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
#ifndef ORES_MARKETDATA_API_DOMAIN_FEED_BINDING_HPP
#define ORES_MARKETDATA_API_DOMAIN_FEED_BINDING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief Persisted mapping that binds an official ORE market series (ore_key) to one raw synthetic
 * producer channel (source_name); enables/disables the ingest loop subscription.
 *
 * A feed binding records which raw producer channel feeds an official market
 * series. The marketdata service reads all enabled bindings at startup, subscribes
 * to synthetic.v1.tick.<source_name>, persists each arriving tick as a
 * market_observation, and republishes on the official tenant-scoped stream
 * marketdata.v1.tick.<tenant_id>.<ore_key>.
 *
 * Rebinding (editing source_name) switches the ingest source without restarting
 * producers. Setting enabled  false= suspends the subscription without deleting
 * the binding.
 */
struct feed_binding final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this feed binding.
     */
    boost::uuids::uuid id;

    /**
     * @brief Official ORE market data key for the series being bound (e.g. FX/RATE/EUR/USD).
     */
    std::string ore_key;

    /**
     * @brief Unique producer identity; the ingest loop subscribes to
     * synthetic.v1.tick.<source_name>. Matches the source_name field in
     * start_market_feed_config_request.
     */
    std::string source_name;

    /**
     * @brief When true the marketdata service maintains an active NATS subscription for this
     * binding. Setting to false suspends ingestion without removing the binding record.
     */
    bool enabled = true;

    /**
     * @brief Username of the person who last modified this feed binding.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
