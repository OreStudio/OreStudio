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
#ifndef ORES_SYNTHETIC_SERVICE_SIMULATOR_OFFERS_HPP
#define ORES_SYNTHETIC_SERVICE_SIMULATOR_OFFERS_HPP

#include "feed_adapter_targets.hpp"
#include "feed_kind_registry.hpp"
#include "generator_runner.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <algorithm>
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The generators the simulator offers an adapter, from the system collections it can see.
 *
 * An offer is a startable config of a kind that has a vendor id in the notation of the provider,
 * in a collection that names the provider of the adapter. A collection of another provider quotes
 * that provider's ids, which this adapter does not publish. The offers come in the order of the
 * config ids, so when two configs quote one instrument the same one wins at each read. Each offer
 * builds its generator with the target of the adapter, so the generator publishes adapter ticks on
 * the ingress subject of its id.
 *
 * A feed that quotes one instrument is one offer. A curve is one generator for every pillar it
 * holds, so it is one offer for each of its pillars, all of them naming the curve as their group,
 * and each of them builds the same generator: the curve, with the adapter target of every pillar.
 *
 * @param ctx A context of the system tenant, which sees the system collections.
 */
inline std::vector<generator_offer>
system_generator_offers(const feed_kind_registry& registry,
                        const ores::database::context& ctx,
                        const feed::feed_build_context& build,
                        const std::string& adapter_id,
                        const std::string& provider_code,
                        std::shared_ptr<const feed::fault_state> faults = {}) {
    namespace vendor = ores::marketdata::vendor;
    namespace msg = ores::marketdata::messaging;
    using namespace ores::synthetic::feed;

    // The id a provider's own notation writes, and the subject its tick travels on. The simulator
    // never spells an id itself.
    const auto target_of = [&adapter_id, &faults, &provider_code](
                               const vendor::quote_description& description)
        -> std::optional<adapter_target> {
        return adapter_target_of(adapter_id, provider_code, description, faults);
    };

    auto targets = registry.targets(ctx);
    std::ranges::sort(targets, [](const auto& a, const auto& b) {
        return a.row.candidate.feed_config_id < b.row.candidate.feed_config_id;
    });
    const feed::feed_adapter adapter_for_build{.id = adapter_id, .provider_code = provider_code};
    std::vector<generator_offer> out;
    for (const auto& target : targets) {
        const auto& candidate = target.row.candidate;
        if (!target.startable() || !target.container_is_system ||
            target.container_provider_code != provider_code)
            continue;
        const auto* notation = vendor::notation_of(provider_code);
        if (!notation)
            continue;

        if (candidate.fx_pair && target.row.kind->kind == fx_spot_feed_kind) {
            const auto written = target_of(
                vendor::fx_spot_quote{candidate.fx_pair->first, candidate.fx_pair->second,
                                      notation->primary_side()});
            if (!written)
                continue;
            const auto adapter = *written;
            out.push_back(generator_offer{
                .id = adapter.id, .group = {}, .make = [&registry, &build, target, adapter, adapter_for_build] {
                    auto outcome = target.row.candidate.build_input(adapter_for_build);
                    if (!outcome.input)
                        throw std::runtime_error(outcome.skip_reason);
                    std::get<fx_spot_feed_build_input>(*outcome.input).adapter = adapter;
                    return registry.factory().make(target.row.kind->kind, build, *outcome.input);
                }});
            continue;
        }

        if (candidate.pillars.empty() || target.row.kind->kind != ir_curve_feed_kind)
            continue;

        // Each pillar gets the adapter target of its own id, and every offer of the curve builds
        // the one generator that publishes them all.
        std::vector<ir_pillar_quote_target> pillar_targets;
        std::vector<msg::vendor_id> ids;
        for (const auto& pillar : candidate.pillars) {
            const auto written = target_of(pillar);
            if (!written)
                continue;
            ids.push_back(written->id);
            pillar_targets.push_back(ir_pillar_quote_target{.start_tenor_code = pillar.start_tenor,
                                                            .end_tenor_code = pillar.end_tenor,
                                                            .target = *written});
        }
        if (pillar_targets.empty())
            continue;
        auto make = [&registry, &build, target, pillar_targets, adapter_for_build] {
            auto outcome = target.row.candidate.build_input(adapter_for_build);
            if (!outcome.input)
                throw std::runtime_error(outcome.skip_reason);
            std::get<ir_curve_feed_build_input>(*outcome.input).adapter_targets = pillar_targets;
            return registry.factory().make(target.row.kind->kind, build, *outcome.input);
        };
        for (const auto& id : ids)
            out.push_back(
                generator_offer{.id = id, .group = candidate.feed_config_id, .make = make});
    }
    return out;
}

}

#endif
