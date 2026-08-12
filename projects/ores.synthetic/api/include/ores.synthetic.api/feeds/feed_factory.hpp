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
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/export.hpp"
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace ores::synthetic::feed {

// Kind strings (fx_spot_feed_kind, ir_curve_feed_kind) live in their producer headers and are
// registered by make_default_feed_factory(). A producer's kind is the asset-class discriminator
// of the factory seam: the control-plane passes it (or derives it from the config table) to
// select the builder, and IFeed::kind() exposes it.

/**
 * @brief Shared inputs every producer builder needs, gathered once by the caller (application
 * auto-start, control-plane handlers) and passed to every factory::make() call.
 */
struct feed_build_context final {
    ores::nats::service::client& nats;
    ores::nats::service::nats_client& auth_nats;
    /**
     * @brief Bearer token of the requesting end-user, forwarded as X-Delegated-Authorization
     * on delegated service-to-service lookups (vintage resolution) so they run in the caller's
     * tenant/party context. Empty for paths without an end-user session (auto-start).
     */
    std::string caller_bearer_token;
};

/**
 * @brief Build inputs for the FX spot producer: its persisted config plus the gmm_component
 * rows that define the mixture process.
 */
struct fx_spot_feed_build_input final {
    ores::synthetic::domain::fx_spot_generation_config config;
    std::vector<ores::synthetic::domain::gmm_component> components;
    ores::synthetic::domain::binding_mode binding_mode =
        ores::synthetic::domain::binding_mode::bound;
};

/**
 * @brief Build inputs for the IR curve producer: its persisted config, the template-entry and
 * parameter-value rows grouped by the caller, the parameter-definitions catalogue, and the
 * refdata context resolved for the config's tenor convention.
 */
struct ir_curve_feed_build_input final {
    ores::synthetic::domain::ir_curve_generation_config config;
    std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
    std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value> values;
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition> definitions;
    ir_curve_refdata_context refctx;
};

/**
 * @brief One of the per-kind build inputs, discriminated by the kind string passed to make().
 */
using feed_build_input = std::variant<fx_spot_feed_build_input, ir_curve_feed_build_input>;

/**
 * @brief Maps a feed kind to the producer builder that constructs that kind from its persisted
 * configuration -- the factory seam the original FX PoC architecture doc specified (open design
 * question 8): control-plane and lifecycle code request a producer by kind and never name a
 * concrete producer class. A new asset class is a config table, a producer behind IFeed, and
 * one registration.
 */
class ORES_SYNTHETIC_API_EXPORT feed_factory final {
public:
    using builder = std::function<std::shared_ptr<ores::marketdata::domain::IFeed>(
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
    [[nodiscard]] std::shared_ptr<ores::marketdata::domain::IFeed>
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

}
#endif
