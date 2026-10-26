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
#ifndef ORES_SYNTHETIC_API_FEEDS_FEED_FACTORY_HPP
#define ORES_SYNTHETIC_API_FEEDS_FEED_FACTORY_HPP

#include "fx_spot_feed.hpp"
#include "ir_curve_feed.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.api/domain/market_clock.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/export.hpp"
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace ores::synthetic::feed {

// Kind strings (fx_spot_feed_kind, ir_curve_feed_kind) live in their producer headers and are
// registered by make_default_feed_factory(). A producer's kind is the asset-class discriminator
// of the factory seam: the control-plane passes it (or derives it from the config table) to
// select the builder, and i_feed::kind() exposes it.

/**
 * @brief Shared inputs every producer builder needs, gathered once by the caller (application
 * auto-start, control-plane handlers) and passed to every factory::make() call.
 */
struct feed_build_context final {
    ores::nats::service::client& nats;
    /// The service's own identity. A vintage lookup runs as the system tenant through it.
    ores::nats::service::nats_client& auth_nats;
    /// The clock every feed asks for the date it starts from, so no two feeds start from two days.
    std::shared_ptr<const ores::marketdata::domain::market_clock> clock =
        std::make_shared<ores::marketdata::domain::system_market_clock>();
    /// The market every feed built from this context shares, or null for a run
    /// with no market. One run, one market: a state that fed only one feed
    /// would not be a market.
    ores::analytics::quant::domain::market_regime* market = nullptr;
    /// The heat every event in the run feeds, or null for a run with no
    /// endogenous factor.
    ores::analytics::quant::domain::shared_intensity* intensity = nullptr;
    /// The run's one clock, or null for a run where each feed keeps its own.
    ores::analytics::quant::service::tick_scheduler* scheduler = nullptr;
};

/**
 * @brief Build inputs for the FX spot producer: its persisted config plus the gmm_component
 * rows that define the mixture process.
 */

/**
 * @brief The arrival spec a config row names.
 *
 * The four arrival columns travel together from the row to the factory. One
 * helper per entity keeps the mapping in a single place: a new column reaches
 * every feed by changing this function, not each builder.
 */
ores::analytics::quant::domain::arrival_spec
to_arrival_spec(const ores::synthetic::domain::fx_spot_generation_config& config);
ores::analytics::quant::domain::arrival_spec
to_arrival_spec(const ores::synthetic::domain::ir_curve_generation_config& config);

struct fx_spot_feed_build_input final {
    ores::synthetic::domain::fx_spot_generation_config config;
    std::vector<ores::synthetic::domain::gmm_component> components;
    /// Set when the generator serves an adapter: the vendor id it quotes and the subject it uses.
    std::optional<adapter_target> adapter;
};

/**
 * @brief Build inputs for the IR curve producer: its persisted config, the template-entry and
 * parameter-value rows grouped by the caller, the parameter-definitions catalogue, the refdata
 * context resolved for the config's tenor convention, and the binding mode that decides its
 * publish subject.
 */
struct ir_curve_feed_build_input final {
    ores::synthetic::domain::ir_curve_generation_config config;
    std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
    std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value> values;
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition> definitions;
    ir_curve_refdata_context refctx;
    /**
     * @brief The adapter id each pillar quotes, or empty when the generator runs without an
     * adapter.
     *
     * One curve is one generator for every pillar, so the targets travel as a set and each names
     * the pillar it belongs to. A set that leaves a pillar without a target is refused when the
     * generator is built, since a pillar with no id has nowhere to publish.
     */
    std::vector<ir_pillar_quote_target> adapter_targets;
};

/**
 * @brief One of the per-kind build inputs, discriminated by the kind string passed to make().
 */
using feed_build_input = std::variant<fx_spot_feed_build_input, ir_curve_feed_build_input>;

/**
 * @brief Maps a feed kind to the producer builder that constructs that kind from its persisted
 * configuration -- the factory seam the original FX PoC architecture doc specified (open design
 * question 8): control-plane and lifecycle code request a producer by kind and never name a
 * concrete producer class. A new asset class is a config table, a producer behind i_feed, and
 * one registration.
 */
class ORES_SYNTHETIC_API_EXPORT feed_factory final {
public:
    using builder = std::function<std::shared_ptr<ores::marketdata::domain::i_feed>(
        const feed_build_context&, const feed_build_input&)>;

    /**
     * @brief Register a producer builder under its kind string. Registering an existing kind
     * replaces that kind's builder.
     */
    void register_kind(std::string kind, builder b);

    /**
     * @brief Construct a producer of @p kind from @p input under @p ctx.
     *
     * @throws std::invalid_argument if no builder is registered for @p kind, or @p input is not
     * the kind's own build-input variant member.
     */
    [[nodiscard]] std::shared_ptr<ores::marketdata::domain::i_feed>
    make(const std::string& kind,
         const feed_build_context& ctx,
         const feed_build_input& input) const;

    /**
     * @brief The registered kind strings, sorted alphabetically.
     */
    [[nodiscard]] std::vector<std::string> kinds() const;

private:
    std::map<std::string, builder> builders_;
};

/**
 * @brief The per-asset-class registrations as they stand today: FX spot and IR curves. A new
 * asset class registers its own builder here and its kind string alongside the constants above;
 * the lifecycle machinery never changes.
 */
ORES_SYNTHETIC_API_EXPORT feed_factory make_default_feed_factory();

/**
 * @brief The one default registry instance, built on first use and shared by every control-plane
 * handler and the auto-start walk. Callers register nothing into it, so a single instance is
 * enough and no call site rebuilds the map.
 */
ORES_SYNTHETIC_API_EXPORT const feed_factory& default_feed_factory();

}
#endif
