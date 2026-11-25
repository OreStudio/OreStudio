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
#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/simulated_fixing_notation.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include "ores.synthetic.api/feeds/index_fixing_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "shared_surfaces.hpp"
#include <algorithm>
#include <chrono>
#include <expected>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Whether the feed node of this name is halted: an operator stopped it, or a stop starves
 * it.
 */
using node_halted_fn = std::function<bool(const std::string&)>;

/// One pair's FX fixing: the source that fixes its default cut, and the cut's time and zone.
struct fx_fixing_schedule final {
    std::string source;
    feed::fixing_time when;
};

/// What the simulated fixing publisher's offers need beyond the configs.
struct fixing_offer_inputs final {
    /// Each pair's FX fixing, by base then quote currency.
    std::map<std::pair<std::string, std::string>, fx_fixing_schedule> fx;
    /// The days the market already holds of an item up to a date.
    std::function<std::expected<std::set<std::chrono::year_month_day>, std::string>(
        const std::string& item, std::chrono::year_month_day to)>
        held;
};

/**
 * @brief The fixing generators the simulated fixing publisher offers its adapter.
 *
 * One for the index of each curve that states an index rate, which it takes from the curve's
 * rate on the run's board, and one for each pair a spot config quotes and whose default cut
 * names a fixing source, which it takes from the first such spot feed's level on the board at
 * the cut. Each offer stands alone, and quotes the fixing's address in the simulated fixing
 * notation.
 */
inline std::vector<generator_offer> fixing_offers(const std::vector<feed_start_target>& targets,
                                                  const feed::feed_build_context& build,
                                                  const std::string& adapter_id,
                                                  const fixing_offer_inputs& inputs,
                                                  std::shared_ptr<const feed::fault_state> faults) {
    namespace vendor = ores::marketdata::vendor;
    namespace datum = ores::marketdata::datum;
    using namespace ores::synthetic::feed;

    std::vector<generator_offer> out;
    std::set<std::string> offered;
    const auto offer = [&](std::string item,
                           ores::analytics::quant::domain::market_member_kind kind,
                           std::vector<std::string> level_codes,
                           fixing_time when,
                           std::optional<derived_fixing> derived = std::nullopt) {
        if (!offered.insert(item).second)
            return;
        const auto target = adapter_target_of(adapter_id,
                                              vendor::simulated_fixings_provider,
                                              vendor::index_fixing{.item = item},
                                              faults);
        if (!target)
            return;
        auto make =
            [&build, target = *target, item, kind, level_codes, when, derived, held = inputs.held] {
                return std::shared_ptr<ores::marketdata::domain::i_feed>(
                    std::make_shared<index_fixing_feed>(
                        build.nats,
                        index_fixing_feed_input{.source_name = "synthetic.fixing." + item,
                                                .target = target,
                                                .item = item,
                                                .level_kind = kind,
                                                .level_codes = level_codes,
                                                .derived = derived,
                                                .when = when,
                                                .run_seed = build.market.loop().run_seed(),
                                                .held = held},
                        build.market));
            };
        out.push_back(generator_offer{.id = target->id, .group = {}, .make = std::move(make)});
    };

    // Every spot feed of a pair, whichever runs: a fixing reads the first that has a level.
    std::map<std::pair<std::string, std::string>, std::vector<std::string>> pair_spots;
    for (const auto& target : targets)
        if (target.startable() && target.container_is_system && target.row.candidate.fx_pair &&
            target.row.kind->kind == fx_spot_feed_kind)
            pair_spots[*target.row.candidate.fx_pair].push_back(
                spot_member_code(target.row.candidate.source_name));

    for (const auto& target : targets) {
        const auto& candidate = target.row.candidate;
        if (!target.startable() || !target.container_is_system)
            continue;
        // A derived index is its own node: the config builds the generator, so this
        // pass leaves it alone or it would be started twice.
        if (candidate.derived)
            continue;
        if (candidate.fixing_item)
            offer(*candidate.fixing_item,
                  ores::analytics::quant::domain::market_member_kind::index_rate,
                  {*candidate.fixing_item},
                  {});
        if (candidate.fx_pair && target.row.kind->kind == fx_spot_feed_kind) {
            const auto found = inputs.fx.find(*candidate.fx_pair);
            if (found == inputs.fx.end())
                continue;
            auto index = datum::market_index::make(
                datum::index_family::fx,
                candidate.fx_pair->first,
                {{"source", found->second.source}, {"ccy", candidate.fx_pair->second}});
            if (!index)
                continue;
            auto item = datum::oresmd_uri_codec::write_index(*index);
            if (!item)
                continue;
            offer(*item,
                  ores::analytics::quant::domain::market_member_kind::spot,
                  pair_spots[*candidate.fx_pair],
                  found->second.when);
        }
    }
    return out;
}

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
 * A halted node offers nothing: a stopped or starved upstream offers none of its ids, a latent
 * surface none of its views, and a vendor view none of its points, so its vendor stops quoting
 * that surface while the surface and the other vendors run on.
 *
 * @param ctx A context of the system tenant, which sees the system collections.
 */
inline std::vector<generator_offer>
system_generator_offers(const feed_kind_registry& registry,
                        const ores::database::context& ctx,
                        const feed::feed_build_context& build,
                        const std::string& adapter_id,
                        const std::string& provider_code,
                        std::shared_ptr<const feed::fault_state> faults = {},
                        shared_surfaces* surfaces = nullptr,
                        const node_halted_fn& node_halted = {},
                        const fixing_offer_inputs* fixings = nullptr) {
    namespace vendor = ores::marketdata::vendor;
    namespace msg = ores::marketdata::messaging;
    using namespace ores::synthetic::feed;

    // The id a provider's own notation writes, and the subject its tick travels on. The simulator
    // never spells an id itself.
    const auto target_of =
        [&adapter_id, &faults, &provider_code](
            const vendor::quote_description& description) -> std::optional<adapter_target> {
        return adapter_target_of(adapter_id, provider_code, description, faults);
    };

    auto targets = registry.targets(ctx);
    std::ranges::sort(targets, [](const auto& a, const auto& b) {
        return a.row.candidate.feed_config_id < b.row.candidate.feed_config_id;
    });
    if (provider_code == vendor::simulated_fixings_provider)
        return fixings ? fixing_offers(targets, build, adapter_id, *fixings, faults) :
                         std::vector<generator_offer>{};
    const feed::feed_adapter adapter_for_build{.id = adapter_id, .provider_code = provider_code};
    std::vector<generator_offer> out;
    for (const auto& target : targets) {
        const auto& candidate = target.row.candidate;
        if (!target.startable() || !target.container_is_system)
            continue;
        if (node_halted && node_halted(candidate.source_name))
            continue;
        const auto* notation = vendor::notation_of(provider_code);
        if (!notation)
            continue;

        // A derived index states the fixing of an index no publisher quotes, from the curve its
        // rule names. Its adapter is offered that one address, so the market data service that
        // demands it is answered; the graph withholds the offer while a stop holds the node or
        // starves it, which is what stops the fixings when their curve stops.
        if (candidate.derived) {
            if (!candidate.fixing_item || fixings == nullptr)
                continue;
            const auto written = target_of(vendor::index_fixing{.item = *candidate.fixing_item});
            if (!written)
                continue;
            feed::derived_fixing_feed_build_input in{.source_name = candidate.source_name,
                                                     .item = *candidate.fixing_item,
                                                     .target = *written,
                                                     .rule = *candidate.derived,
                                                     .held = fixings->held};
            if (node_halted)
                in.starved = [halted = node_halted, name = candidate.source_name] {
                    return halted(name);
                };
            out.push_back(generator_offer{
                .id = written->id,
                .group = candidate.feed_config_id,
                .make = [&registry, &build, kind = target.row.kind->kind, in] {
                    return registry.factory().make(kind, build, feed::feed_build_input{in});
                }});
            continue;
        }

        // A surface is offered to the adapter of each contributor's provider, for that
        // contributor's points, whichever collection holds it: the contributors observe one
        // surface, which the adapters share.
        if (target.row.kind->kind == fx_vol_surface_feed_kind) {
            if (surfaces == nullptr)
                continue;
            for (const auto& [contributor, view] : candidate.views) {
                if (view.provider_code != provider_code || !view.enabled)
                    continue;
                // The view's own row names it; a view staged before it had a name takes the
                // scheme's.
                const auto view_name = view.source_name.empty() ?
                                           fx_option_view_name(provider_code,
                                                               candidate.fx_pair->first,
                                                               candidate.fx_pair->second) :
                                           view.source_name;
                if (node_halted && node_halted(view_name))
                    continue;
                fx_vol_surface_feed::point_targets point_targets;
                std::vector<msg::vendor_id> ids;
                for (const auto& point : candidate.option_points) {
                    const auto written = target_of(point);
                    if (!written)
                        continue;
                    ids.push_back(written->id);
                    point_targets.emplace(std::pair{point.expiry, point.strike_label}, *written);
                }
                if (ids.empty())
                    continue;
                auto make = [&registry,
                             &build,
                             target,
                             adapter_for_build,
                             surfaces,
                             contributor,
                             point_targets,
                             adapter_id,
                             view_name] {
                    const auto feed_id = target.row.candidate.feed_config_id;
                    auto make_surface = [&registry, &build, target, adapter_for_build] {
                        auto outcome = target.row.candidate.build_input(adapter_for_build);
                        if (!outcome.input)
                            throw std::runtime_error(outcome.skip_reason);
                        return std::dynamic_pointer_cast<fx_vol_surface_feed>(
                            registry.factory().make(target.row.kind->kind, build, *outcome.input));
                    };
                    return std::shared_ptr<ores::marketdata::domain::i_feed>(
                        std::make_shared<shared_surface_hold>(*surfaces,
                                                              feed_id,
                                                              adapter_id,
                                                              contributor,
                                                              view_name,
                                                              point_targets,
                                                              make_surface));
                };
                for (const auto& id : ids)
                    out.push_back(
                        generator_offer{.id = id,
                                        .group = candidate.feed_config_id + "|" + contributor,
                                        .make = make});
            }
            continue;
        }

        if (target.container_provider_code != provider_code)
            continue;

        if (candidate.fx_pair && target.row.kind->kind == fx_spot_feed_kind) {
            const auto written = target_of(vendor::fx_spot_quote{
                candidate.fx_pair->first, candidate.fx_pair->second, notation->primary_side()});
            if (!written)
                continue;
            const auto adapter = *written;
            out.push_back(generator_offer{
                .id = adapter.id,
                .group = {},
                .make = [&registry, &build, target, adapter, adapter_for_build] {
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
            // A provider whose id does not name the curve's index tenor is given none, so the
            // description is one the notation writes.
            auto described = pillar;
            if (!notation->names_index_tenor())
                described.index_tenor.clear();
            const auto written = target_of(described);
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
