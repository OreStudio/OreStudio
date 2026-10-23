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

#include "feed_kind_registry.hpp"
#include "generator_runner.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include <algorithm>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The generators the simulator offers an adapter, from the system collections it can see.
 *
 * An offer is a startable config of a kind that has a vendor id, in a collection that names the
 * provider of the adapter. A collection of another provider quotes that provider's ids, which
 * this adapter does not publish. The offers come in the order of the config ids, so when two configs quote one
 * instrument the same one wins at each read. Each offer builds its generator with the target of
 * the adapter, so the generator publishes adapter ticks on the ingress subject of its id.
 *
 * @param ctx A context of the system tenant, which sees the system collections.
 */
inline std::vector<generator_offer> system_generator_offers(const feed_kind_registry& registry,
                                                            const ores::database::context& ctx,
                                                            const feed::feed_build_context& build,
                                                            const std::string& adapter_id,
                                                            const std::string& provider_code) {
    auto targets = registry.targets(ctx);
    std::ranges::sort(targets, [](const auto& a, const auto& b) {
        return a.row.candidate.feed_config_id < b.row.candidate.feed_config_id;
    });
    std::vector<generator_offer> out;
    for (const auto& target : targets) {
        const auto& candidate = target.row.candidate;
        if (!target.startable() || !candidate.vendor || !target.container_is_system ||
            target.container_provider_code != provider_code)
            continue;
        if (target.row.kind->kind != feed::fx_spot_feed_kind)
            continue;
        const auto vendor = *candidate.vendor;
        const auto subject =
            ores::marketdata::domain::adapter_ingress_subject(adapter_id, vendor.external_id);
        out.push_back(generator_offer{
            .id = vendor, .make = [&registry, &build, target, vendor, subject] {
                auto outcome = target.row.candidate.build_input();
                if (!outcome.input)
                    throw std::runtime_error(outcome.skip_reason);
                std::get<feed::fx_spot_feed_build_input>(*outcome.input).adapter =
                    feed::adapter_target{.id = vendor, .subject = subject};
                return registry.factory().make(target.row.kind->kind, build, *outcome.input);
            }});
    }
    return out;
}

}

#endif
