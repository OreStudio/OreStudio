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

#include "ores.analytics.quant/domain/crm_rate_view.hpp"
#include "ores.analytics.quant/domain/derived_rate.hpp"
#include "ores.analytics.quant/service/rate_delta_tracker.hpp"
#include "ores.analytics.quant/service/rate_engine.hpp"
#include "ores.analytics.quant/service/rate_inverter.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/export.hpp"
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

namespace quant = ores::analytics::quant;

/**
 * @brief A rate tagged with the name of the CRM (crm_topology_config)
 * that produced it -- returned by the all-CRMs overload of @c rates(),
 * where the caller doesn't pre-select a single named CRM.
 */
struct named_rate {
    std::string crm_name;
    quant::domain::derived_rate rate;
};

/// A resolved (post-inversion, post-delta) view tagged with the name of
/// the CRM that produced it -- returned by the all-CRMs overload of
/// @c resolved_rates().
struct named_rate_view {
    std::string crm_name;
    quant::domain::crm_rate_view view;
};

/**
 * @brief Owns one ores.analytics.quant::service::rate_engine per
 * (tenant, party, CRM name), built from that party's
 * crm_topology_config rows (every *enabled* one, not just one) and each
 * config's enabled crm_driver_pair rows, and feeds driver ticks to every
 * matching named engine as they arrive.
 *
 * A party may run several concurrently-enabled, independently-named
 * CRMs at once -- e.g. a "majors" CRM with a dense direct-quote mesh for
 * liquid G10 pairs, and an "exotics" CRM that's a strict USD-pivot star
 * -- matching how real FX desks tier liquidity. There is deliberately no
 * single "the" CRM per party any more; every query is either scoped to
 * one CRM by name, or explicitly asks for all of them.
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
 * update() is silently a no-op for a given named engine when the pair is
 * not one of its driver edges (rate_engine throws std::invalid_argument
 * for a non-edge pair; caught and ignored here rather than propagated,
 * since feed_ingest_loop must not fail a tick's persist+remap because of
 * an unrelated CRM lookup) -- a single tick may feed several of a
 * party's named engines at once (e.g. EUR/USD is very likely a driver
 * edge of both a "majors" and an "exotics" CRM).
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

    /// Rebuilds every (tenant, party, CRM name) engine from the current
    /// crm_topology_config/crm_driver_pair rows. Call on startup and on
    /// crm_topology_config_changed_event/crm_driver_pair_changed_event.
    void refresh();

    /// Feeds one driver tick into every named engine for the (tenant,
    /// party) that has it as a driver edge. See class doc for why this
    /// never throws.
    void update(const std::string& tenant_id_str,
                const std::string& party_id_str,
                const std::string& base_currency_code,
                const std::string& quote_currency_code,
                double rate,
                std::chrono::system_clock::time_point observed_at);

    /// A single rate from one named CRM; std::nullopt if no engine
    /// exists for that (tenant, party, crm_name) at all (CRM not
    /// configured or not enabled), as opposed to
    /// domain::rate_status::unavailable (configured, but not yet
    /// seeded).
    [[nodiscard]] std::optional<quant::domain::derived_rate>
    rate(const std::string& tenant_id_str,
         const std::string& party_id_str,
         const std::string& crm_name,
         const std::string& base_currency_code,
         const std::string& quote_currency_code) const;

    /// Every currently-configured pair for one named CRM (its driver
    /// edges + enabled derived pairs) in one batch -- one atomic
    /// snapshot load, not N single-pair calls. Empty if no such engine
    /// exists for that (tenant, party, crm_name).
    [[nodiscard]] std::vector<quant::domain::derived_rate> rates(const std::string& tenant_id_str,
                                                                 const std::string& party_id_str,
                                                                 const std::string& crm_name) const;

    /// Every currently-configured pair across *every* enabled CRM for a
    /// party, each tagged with which CRM produced it. Empty if the
    /// party has no enabled CRM at all.
    [[nodiscard]] std::vector<named_rate> rates(const std::string& tenant_id_str,
                                                const std::string& party_id_str) const;

    /// Every configured pair for one named CRM, resolved: pairs with no
    /// direct quote are backfilled with the reverse pair's inverse when
    /// @p inverted is true (and the reverse itself isn't also a
    /// configured pair, in which case its own direct rate wins), and
    /// each view's delta_pct is filled in vs. the last value this
    /// specific (tenant, party, crm_name) scope served for that pair.
    /// Empty if no such engine exists.
    [[nodiscard]] std::vector<quant::domain::crm_rate_view>
    resolved_rates(const std::string& tenant_id_str,
                   const std::string& party_id_str,
                   const std::string& crm_name,
                   bool inverted) const;

    /// Resolved views (see above) across every enabled CRM for a party,
    /// each tagged with which CRM produced it.
    [[nodiscard]] std::vector<named_rate_view> resolved_rates(const std::string& tenant_id_str,
                                                              const std::string& party_id_str,
                                                              bool inverted) const;

private:
    using pair_key = std::pair<std::string, std::string>; // (tenant_id_str, party_id_str)

    struct named_engine {
        std::string name;
        std::shared_ptr<quant::service::rate_engine> engine;
        std::vector<std::pair<std::string, std::string>> configured_pairs;
        // shared_ptr, not by-value: rate_delta_tracker holds a mutex, so
        // it is neither copyable nor movable, and named_engine must
        // remain copyable to live in a std::vector the way engines_map
        // already requires. One instance per (tenant, party, crm_name)
        // scope, reset (like the engine itself) on every refresh() --
        // see the class doc's note on refresh() being a full rebuild.
        std::shared_ptr<quant::service::rate_delta_tracker> deltas =
            std::make_shared<quant::service::rate_delta_tracker>();
    };

    using engines_map = std::map<pair_key, std::vector<named_engine>>;

    /// Snapshot load: briefly locks only to copy the shared_ptr (an
    /// atomic refcount bump), never to touch the map itself -- readers
    /// never block on refresh() building the next map, and refresh()
    /// never blocks on a reader holding an old snapshot. A mutex here
    /// (rather than std::atomic<std::shared_ptr<T>>) is a deliberate,
    /// portable choice: some libc++ versions fall back to their generic
    /// atomic<T> primary template for non-trivially-copyable T, which
    /// static_asserts trivial copyability and fails to compile.
    [[nodiscard]] std::shared_ptr<const engines_map> snapshot() const {
        std::lock_guard lock(engines_mutex_);
        return engines_;
    }

    /// Resolves+deltas one named engine's configured pairs -- shared by
    /// both @c resolved_rates overloads.
    [[nodiscard]] static std::vector<quant::domain::crm_rate_view>
    resolve(const named_engine& engine, bool inverted);

    ores::database::context ctx_;
    mutable std::mutex engines_mutex_;
    std::shared_ptr<const engines_map> engines_;
};

} // namespace ores::marketdata::service::app

#endif
