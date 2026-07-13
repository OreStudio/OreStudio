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
#ifndef ORES_MARKETDATA_SERVICE_APP_CRM_INGEST_BRIDGE_HPP
#define ORES_MARKETDATA_SERVICE_APP_CRM_INGEST_BRIDGE_HPP

#include "ores.analytics.quant/domain/derived_rate.hpp"
#include "ores.analytics.quant/service/rate_engine.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/export.hpp"
#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

namespace quant = ores::analytics::quant;

/**
 * @brief Owns one ores.analytics.quant::service::rate_engine per (tenant,
 * party), built from that party's crm_topology_config + enabled
 * crm_driver_pair rows, and feeds it driver ticks as they arrive.
 *
 * Not multi-threaded itself -- spawns no threads -- but is thread-safe:
 * refresh() (rare, config-change-triggered) atomically swaps in a whole
 * new engine map built from scratch, so update()/rate()/rates() (the hot
 * path, called from any number of threads) always see a consistent,
 * fully-built set of engines and never observe a partially-rebuilt one.
 * Losing accumulated rate state across a refresh() is expected and
 * acceptable: a refresh only happens when the topology/driver-pair
 * config itself changed, at which point prior rates are stale by
 * definition anyway -- this mirrors feed_ingest_loop's own
 * config-changes-are-rare, full-rebuild-is-fine design.
 *
 * update() is silently a no-op when no engine exists for the tick's
 * (tenant, party) (most ticks are not CRM-configured pairs), or when the
 * pair is not a driver edge of that party's topology (rate_engine
 * throws std::invalid_argument for a non-edge pair; caught and ignored
 * here rather than propagated, since feed_ingest_loop must not fail a
 * tick's persist+remap because of an unrelated CRM lookup).
 */
class ORES_MARKETDATA_SERVICE_EXPORT crm_ingest_bridge {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.crm_ingest_bridge");
        return instance;
    }

public:
    explicit crm_ingest_bridge(ores::database::context ctx);

    /// Rebuilds every (tenant, party) engine from the current
    /// crm_topology_config/crm_driver_pair rows. Call on startup and on
    /// crm_topology_config_changed_event/crm_driver_pair_changed_event.
    void refresh();

    /// Feeds one driver tick into the (tenant, party)'s engine, if one
    /// exists and the pair is one of its driver edges. See class doc for
    /// why this never throws.
    void update(const std::string& tenant_id_str,
                const std::string& party_id_str,
                const std::string& base_currency_code,
                const std::string& quote_currency_code,
                double rate,
                std::chrono::system_clock::time_point observed_at);

    /// A single rate for a party; std::nullopt if no engine exists for
    /// that (tenant, party) at all (CRM not configured), as opposed to
    /// domain::rate_status::unavailable (configured, but not yet seeded).
    [[nodiscard]] std::optional<quant::domain::derived_rate>
    rate(const std::string& tenant_id_str,
         const std::string& party_id_str,
         const std::string& base_currency_code,
         const std::string& quote_currency_code) const;

    /// Every currently-configured pair for a party (its driver edges +
    /// enabled derived pairs) in one batch -- one atomic snapshot load,
    /// not N single-pair calls. Empty if no engine exists for that
    /// (tenant, party).
    [[nodiscard]] std::vector<quant::domain::derived_rate>
    rates(const std::string& tenant_id_str, const std::string& party_id_str) const;

private:
    using pair_key = std::pair<std::string, std::string>; // (tenant_id_str, party_id_str)

    struct party_engine {
        std::shared_ptr<quant::service::rate_engine> engine;
        std::vector<std::pair<std::string, std::string>> configured_pairs;
    };

    using engines_map = std::map<pair_key, party_engine>;

    [[nodiscard]] std::shared_ptr<const engines_map> snapshot() const {
        return engines_.load();
    }

    ores::database::context ctx_;
    std::atomic<std::shared_ptr<const engines_map>> engines_;
};

} // namespace ores::marketdata::service::app

#endif
