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
#include "feed_kind_registry.hpp"
#include "client_message.hpp"
#include "feed_adapter_targets.hpp"
#include "ir_curve_preview_process.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_paths_common.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_fixing_item_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <expected>
#include <ranges>
#include <stdexcept>
#include <utility>

namespace ores::synthetic::service {

namespace {

// One kind's IR refdata context, resolved from the config's own series qualifier. The context is
// per config: the FOMC grid resolves under RATES_SPOT_FOMC, everything else under
// RATES_SPOT_FORWARD.
std::optional<feed::ir_curve_refdata_context>
ir_refdata_context(const ores::database::context& ctx,
                   const ores::synthetic::domain::ir_curve_generation_config& cfg) {
    return feed::build_ir_curve_refdata_context(ctx, feed::ir_curve_tenor_convention_code(cfg));
}

// The system-tenant owned parameter-definition catalogue: the publish path resolves each value's
// parameter_definition_id from the system tenant, so a read scoped to the caller's tenant returns
// nothing for any real tenant.
std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>
read_definition_catalogue(const ores::database::context& ctx) {
    namespace repo = ores::synthetic::repository;
    repo::yield_curve_process_parameter_definition_repository definition_repo;
    const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(ctx);
    return definition_repo.read_latest(sys_ctx);
}

// The target a feed quotes one description through: the id the provider's notation writes and
// the ingress subject the tick travels on.
std::expected<feed::adapter_target, std::string>
target_for(const feed::feed_adapter& adapter,
           const ores::marketdata::vendor::quote_description& description) {
    auto target = adapter_target_of(adapter.id, adapter.provider_code, description);
    if (!target)
        return std::unexpected("The notation of provider '" + adapter.provider_code +
                               "' writes no vendor id for this quote, so the feed cannot say what "
                               "it quotes.");
    return std::move(*target);
}

// The target of one currency pair, whose side is the one the provider's notation prices.
std::expected<feed::adapter_target, std::string>
fx_target_for(const feed::feed_adapter& adapter,
              const ores::synthetic::domain::fx_spot_generation_config& fx) {
    const auto* notation = ores::marketdata::vendor::notation_of(adapter.provider_code);
    if (notation == nullptr)
        return std::unexpected("The catalogue holds no notation for provider '" +
                               adapter.provider_code + "'.");
    return target_for(adapter,
                      ores::marketdata::vendor::fx_spot_quote{
                          fx.base_currency_code, fx.quote_currency_code, notation->primary_side()});
}

feed_kind make_fx_spot_kind() {
    using ores::synthetic::feed::fx_spot_feed_build_input;
    using ores::synthetic::feed::fx_spot_feed_kind;
    namespace repo = ores::synthetic::repository;
    namespace datum = ores::marketdata::datum;
    namespace msg = ores::synthetic::messaging;

    feed_kind k;
    k.kind = std::string(fx_spot_feed_kind);
    k.config_permission = "synthetic::fx_spot_generation_configs:read";

    k.candidates = [](const ores::database::context& ctx, const std::string& id) {
        repo::fx_spot_generation_config_repository fx_repo;
        const auto rows = id.empty() ? fx_repo.read_latest(ctx) : fx_repo.read_latest(ctx, id);
        std::vector<feed_kind_candidate> out;
        out.reserve(rows.size());
        for (const auto& fx : rows) {
            feed_kind_candidate c;
            c.feed_config_id = boost::uuids::to_string(fx.id);
            c.container_id = fx.config_id;
            c.source_name = fx.source_name;
            c.display_name = fx.oresmd_uri;
            c.enabled = fx.enabled;
            c.auto_start = fx.auto_start;
            c.folder_id = fx.folder_id;
            c.fx_pair = std::make_pair(fx.base_currency_code, fx.quote_currency_code);
            c.build_input = [ctx, fx](const feed::feed_adapter& adapter) -> feed_build_outcome {
                repo::gmm_component_repository comp_repo;
                std::vector<ores::synthetic::domain::gmm_component> components;
                for (auto& g : comp_repo.read_latest(ctx))
                    if (g.fx_spot_config_id == fx.id)
                        components.push_back(std::move(g));
                if (components.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no GMM components: " +
                                           boost::uuids::to_string(fx.id)};
                std::vector<ores::synthetic::domain::fx_spot_fixing_item> fixing_items;
                if (fx.price_source == "fixing")
                    for (auto& i : repo::fx_spot_fixing_item_repository{}.read_latest(ctx))
                        if (i.fx_spot_config_id == fx.id)
                            fixing_items.push_back(std::move(i));

                // The feed quotes the pair in the notation of the adapter's provider, so a feed
                // started by hand publishes the same ids the demand set names.
                std::optional<feed::adapter_target> target;
                if (adapter.has_adapter()) {
                    auto written = fx_target_for(adapter, fx);
                    if (!written)
                        return {.input = std::nullopt, .skip_reason = written.error()};
                    target = std::move(*written);
                }
                return {.input = feed::feed_build_input{fx_spot_feed_build_input{
                            fx, std::move(components), std::move(target), std::move(fixing_items)}},
                        .skip_reason = {}};
            };
            out.push_back(std::move(c));
        }
        return out;
    };

    k.simulate_subject = std::string(msg::simulate_fx_spot_paths_request::nats_subject);
    k.decode_simulate = [](const ores::nats::message& m) -> std::optional<feed_simulate_envelope> {
        auto req = ores::service::messaging::decode<msg::simulate_fx_spot_paths_request>(m);
        if (!req)
            return std::nullopt;
        return feed_simulate_envelope{
            .num_ticks = req->num_ticks,
            .num_paths = req->num_paths,
            .seed = req->seed,
            .make_process = [req = *req](std::uint32_t seed) {
                if (req.gmm_means.empty())
                    throw std::invalid_argument("at least one GMM component is required");
                return std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process>(
                    ores::analytics::quant::service::process_factory::make_process(
                        req.process_type,
                        req.gmm_means,
                        req.gmm_stdevs,
                        req.gmm_weights,
                        req.initial_price,
                        ores::synthetic::feed::price_stream_for(seed, "simulate")));
            }};
    };
    k.reply_simulate = [](ores::nats::service::client& nats,
                          const ores::nats::message& m,
                          const feed_simulation_result& r) {
        ores::service::messaging::reply(nats,
                                        m,
                                        msg::simulate_fx_spot_paths_response{.success = r.success,
                                                                             .message = r.message,
                                                                             .paths = r.paths});
    };
    return k;
}

feed_kind make_ir_curve_kind() {
    using ores::synthetic::feed::ir_curve_feed_build_input;
    using ores::synthetic::feed::ir_curve_feed_kind;
    namespace repo = ores::synthetic::repository;
    namespace datum = ores::marketdata::datum;
    namespace msg = ores::synthetic::messaging;

    feed_kind k;
    k.kind = std::string(ir_curve_feed_kind);
    k.config_permission = "synthetic::ir_curve_generation_configs:read";

    k.candidates = [](const ores::database::context& ctx, const std::string& id) {
        repo::ir_curve_generation_config_repository config_repo;
        const auto rows =
            id.empty() ? config_repo.read_latest(ctx) : config_repo.read_latest(ctx, id);

        // A curve quotes one id for each of its template entries, so the candidates carry the
        // entries in the notation's own words and an offer writes an id from them. The entry
        // table is read once for every row of the request, since its repository reads it whole.
        namespace vendor = ores::marketdata::vendor;
        repo::ir_curve_template_entry_repository entry_repo;
        auto all_entries = entry_repo.read_latest(ctx);
        std::ranges::sort(all_entries, [](const auto& a, const auto& b) {
            return a.sequence_index < b.sequence_index;
        });

        std::vector<feed_kind_candidate> out;
        out.reserve(rows.size());
        for (const auto& cfg : rows) {
            feed_kind_candidate c;
            c.feed_config_id = boost::uuids::to_string(cfg.id);
            c.container_id = cfg.config_id;
            c.source_name = cfg.source_name;
            c.display_name = cfg.currency_code + "/" + cfg.index_family;
            c.enabled = cfg.enabled;
            c.auto_start = cfg.auto_start;
            c.folder_id = cfg.folder_id;
            for (const auto& e : all_entries)
                if (e.ir_curve_config_id == cfg.id)
                    c.pillars.push_back(vendor::ir_curve_pillar{cfg.currency_code,
                                                                cfg.index_family,
                                                                e.instrument_code,
                                                                e.start_tenor_code,
                                                                e.end_tenor_code});
            c.build_input = [ctx, cfg](const feed::feed_adapter& adapter) -> feed_build_outcome {
                repo::ir_curve_template_entry_repository entry_repo;
                std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
                for (auto& e : entry_repo.read_latest(ctx))
                    if (e.ir_curve_config_id == cfg.id)
                        entries.push_back(std::move(e));
                if (entries.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no Curve Template entries: " +
                                           boost::uuids::to_string(cfg.id)};

                // Row-based parameters: the config's own value rows (filtered to cfg.id -- the
                // generated repository has no parent-scoped read) plus the system-tenant
                // definitions catalogue; make_ir_curve_feed joins and validates the two.
                repo::ir_curve_generation_config_process_parameter_value_repository value_repo;
                std::vector<
                    ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>
                    values;
                for (auto& v : value_repo.read_latest(ctx))
                    if (v.config_id == cfg.id)
                        values.push_back(std::move(v));
                if (values.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no parameter value rows: " +
                                           boost::uuids::to_string(cfg.id)};

                const auto refctx = ir_refdata_context(ctx, cfg);
                if (!refctx)
                    return {.input = std::nullopt,
                            .skip_reason = "Tenor convention not found: " +
                                           feed::ir_curve_tenor_convention_code(cfg)};

                // One adapter target for each pillar, in the order the entries came in: the curve
                // publishes every pillar under its own vendor id, as the simulator's offer does.
                std::vector<feed::ir_pillar_quote_target> targets;
                if (adapter.has_adapter()) {
                    targets.reserve(entries.size());
                    for (const auto& e : entries) {
                        auto target = target_for(adapter,
                                                 vendor::ir_curve_pillar{cfg.currency_code,
                                                                         cfg.index_family,
                                                                         e.instrument_code,
                                                                         e.start_tenor_code,
                                                                         e.end_tenor_code});
                        if (!target)
                            return {.input = std::nullopt, .skip_reason = target.error()};
                        targets.push_back(
                            feed::ir_pillar_quote_target{.start_tenor_code = e.start_tenor_code,
                                                         .end_tenor_code = e.end_tenor_code,
                                                         .target = std::move(*target)});
                    }
                }

                return {.input = feed::feed_build_input{ir_curve_feed_build_input{
                            cfg,
                            std::move(entries),
                            std::move(values),
                            read_definition_catalogue(ctx),
                            *refctx,
                            std::move(targets)}},
                        .skip_reason = {}};
            };
            out.push_back(std::move(c));
        }
        return out;
    };

    k.simulate_subject = std::string(msg::simulate_ir_curve_paths_request::nats_subject);
    k.decode_simulate = [](const ores::nats::message& m) -> std::optional<feed_simulate_envelope> {
        auto req = ores::service::messaging::decode<msg::simulate_ir_curve_paths_request>(m);
        if (!req)
            return std::nullopt;
        return feed_simulate_envelope{
            .num_ticks = req->num_ticks,
            .num_paths = req->num_paths,
            .seed = req->seed,
            .make_process = [req = *req](std::uint32_t seed) {
                return std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process>(
                    make_preview_process(req.process_type, req.parameters, seed));
            }};
    };
    k.reply_simulate = [](ores::nats::service::client& nats,
                          const ores::nats::message& m,
                          const feed_simulation_result& r) {
        ores::service::messaging::reply(nats,
                                        m,
                                        msg::simulate_ir_curve_paths_response{.success = r.success,
                                                                              .message = r.message,
                                                                              .paths = r.paths});
    };
    return k;
}

}

feed_kind_registry::feed_kind_registry(adapter_lookup_fn adapter_of_provider)
    : deps_{.factory = &feed::default_feed_factory(),
            .folder_subtree =
                [](const ores::database::context& ctx, const boost::uuids::uuid& root_id) {
                    namespace repo = ores::synthetic::repository;
                    const auto rows = repo::folder_repository().get_hierarchy(ctx, root_id, false);
                    std::set<boost::uuids::uuid> ids;
                    for (const auto& row : rows)
                        ids.insert(row.id);
                    return ids;
                },
            .container_by_id =
                [](const ores::database::context& ctx, const boost::uuids::uuid& id) {
                    namespace repo = ores::synthetic::repository;
                    const auto rows = repo::market_data_generation_config_repository().read_latest(
                        ctx, boost::uuids::to_string(id));
                    return rows.empty() ?
                               std::nullopt :
                               std::optional<domain::market_data_generation_config>(rows.front());
                },
            .containers =
                [](const ores::database::context& ctx) {
                    namespace repo = ores::synthetic::repository;
                    return repo::market_data_generation_config_repository().read_latest(ctx);
                },
            .adapter_of_provider = std::move(adapter_of_provider)} {}

feed_kind_registry::feed_kind_registry(deps d)
    : deps_(std::move(d)) {}

void feed_kind_registry::register_kind(feed_kind k) {
    if (k.kind.empty())
        throw std::invalid_argument("feed kind name is empty");
    if (k.config_permission.empty())
        throw std::invalid_argument("feed kind '" + k.kind + "' has no config permission");
    if (!k.candidates)
        throw std::invalid_argument("feed kind '" + k.kind + "' has no candidates loader");
    if (!k.decode_simulate || !k.reply_simulate)
        throw std::invalid_argument("feed kind '" + k.kind + "' has no simulate closures");
    if (kinds_.contains(k.kind))
        throw std::invalid_argument("feed kind '" + k.kind + "' is already registered");
    const auto supported = deps_.factory->kinds();
    if (std::ranges::find(supported, k.kind) == supported.end())
        throw std::invalid_argument("feed kind '" + k.kind +
                                    "' has no builder in the feed factory");
    kinds_.emplace(k.kind, std::move(k));
}

const feed_kind* feed_kind_registry::find(std::string_view kind) const {
    const auto it = kinds_.find(std::string(kind));
    return it == kinds_.end() ? nullptr : &it->second;
}

std::vector<const feed_kind*> feed_kind_registry::all() const {
    std::vector<const feed_kind*> out;
    out.reserve(kinds_.size());
    for (const auto& [_, k] : kinds_)
        out.push_back(&k);
    return out;
}

bool feed_kind_registry::permits_all_configs(const ores::database::context& ctx) const {
    return std::ranges::all_of(kinds_, [&ctx](const auto& entry) {
        return ores::service::messaging::has_permission(ctx, entry.second.config_permission);
    });
}

bool feed_kind_registry::permits_run(const ores::database::context& ctx) {
    return ores::service::messaging::has_permission(ctx, std::string(run_permission));
}

bool feed_kind_registry::is_system_caller(const ores::database::context& ctx) {
    return ctx.tenant_id().to_string() == ores::database::service::tenant_context::system_tenant_id;
}

std::set<boost::uuids::uuid>
feed_kind_registry::system_containers(const ores::database::context& ctx) const {
    std::set<boost::uuids::uuid> out;
    for (const auto& c : deps_.containers(ctx))
        if (c.tenant_id.to_string() == ores::database::service::tenant_context::system_tenant_id)
            out.insert(c.id);
    return out;
}

std::vector<feed_kind_row> feed_kind_registry::rows(const ores::database::context& ctx) const {
    std::vector<feed_kind_row> out;
    for (const auto& [_, k] : kinds_)
        for (auto& c : k.candidates(ctx, {}))
            out.push_back(feed_kind_row{&k, std::move(c)});
    return out;
}

std::optional<feed_start_target> feed_kind_registry::make_target(
    const feed_kind& k,
    feed_kind_candidate c,
    const std::map<boost::uuids::uuid, domain::market_data_generation_config>* containers_by_id)
    const {
    feed_start_target target;
    target.row = feed_kind_row{&k, std::move(c)};
    const auto found = containers_by_id->find(target.row.candidate.container_id);
    if (found == containers_by_id->end())
        return target;
    target.container_found = true;
    target.container_enabled = found->second.enabled;
    target.container_provider_code = found->second.provider_code;
    target.container_is_system = found->second.tenant_id.to_string() ==
                                 ores::database::service::tenant_context::system_tenant_id;
    return target;
}

std::vector<feed_start_target>
feed_kind_registry::targets(const ores::database::context& ctx) const {
    std::map<boost::uuids::uuid, domain::market_data_generation_config> by_id;
    for (auto& c : deps_.containers(ctx))
        by_id.emplace(c.id, std::move(c));

    std::vector<feed_start_target> out;
    for (const auto& [_, k] : kinds_)
        for (auto& c : k.candidates(ctx, {})) {
            auto target = make_target(k, std::move(c), &by_id);
            // A config whose container is not visible is not a candidate for
            // any verb: the gate would reject it in every case.
            if (target && target->container_found)
                out.push_back(std::move(*target));
        }
    return out;
}

std::optional<feed_start_target>
feed_kind_registry::resolve(const ores::database::context& ctx,
                            const std::string& feed_config_id) const {
    for (const auto& [_, k] : kinds_) {
        auto rows_here = k.candidates(ctx, feed_config_id);
        if (rows_here.empty())
            continue;
        std::map<boost::uuids::uuid, domain::market_data_generation_config> by_id;
        const auto container = deps_.container_by_id(ctx, rows_here.front().container_id);
        if (container)
            by_id.emplace(container->id, *container);
        return make_target(k, std::move(rows_here.front()), &by_id);
    }
    return std::nullopt;
}

std::set<boost::uuids::uuid>
feed_kind_registry::folder_subtree(const ores::database::context& ctx,
                                   const boost::uuids::uuid& root_id) const {
    return deps_.folder_subtree(ctx, root_id);
}

feed_start_attempt feed_kind_registry::make_feed(const feed_start_target& target,
                                                 const feed::feed_build_context& bctx) const {
    if (!target.startable())
        return {.feed = {},
                .failure = "Feed config is not enabled: " + target.row.candidate.feed_config_id};

    // The adapter the feed publishes through. With a lookup installed a feed whose provider the
    // catalogue does not serve with a simulated source is refused: nothing would consume its
    // ticks, and a start that reported success would be a lie about the store.
    feed::feed_adapter adapter;
    if (deps_.adapter_of_provider) {
        const auto found = deps_.adapter_of_provider(target.container_provider_code);
        if (!found)
            return {.feed = {},
                    .failure = "The catalogue holds no simulated source of provider '" +
                               target.container_provider_code +
                               "', so this feed has no adapter to publish through."};
        adapter = *found;
    }

    const auto outcome = target.row.candidate.build_input(adapter);
    if (!outcome.input)
        return {.feed = {}, .failure = outcome.skip_reason};

    return {.feed = factory().make(target.row.kind->kind, bctx, *outcome.input), .failure = {}};
}

void feed_kind_registry::reply_unknown_kind(ores::nats::service::client& nats,
                                            const ores::nats::message& msg,
                                            const std::string& kind) const {
    if (kinds_.empty())
        return;
    kinds_.begin()->second.reply_simulate(
        nats,
        msg,
        feed_simulation_result{.success = false, .message = "Unknown feed kind: " + kind});
}

feed_kind_registry
make_default_feed_kind_registry(feed_kind_registry::adapter_lookup_fn adapter_of_provider) {
    feed_kind_registry registry(std::move(adapter_of_provider));
    registry.register_kind(make_fx_spot_kind());
    registry.register_kind(make_ir_curve_kind());
    return registry;
}

feed_simulation_result run_simulate_paths(const feed_simulate_envelope& env) {
    feed_simulation_result r;
    const int ticks =
        std::clamp(env.num_ticks, 1, ores::synthetic::messaging::max_simulate_num_ticks);
    const int paths =
        std::clamp(env.num_paths, 1, ores::synthetic::messaging::max_simulate_num_paths);
    try {
        for (int p = 0; p < paths; ++p) {
            auto process = env.make_process(env.seed + static_cast<std::uint32_t>(p));
            std::vector<double> path;
            path.reserve(static_cast<std::size_t>(ticks));
            for (int t = 0; t < ticks; ++t)
                path.push_back(process->next());
            r.paths.push_back(std::move(path));
        }
        r.success = true;
    } catch (const std::exception& e) {
        r.success = false;
        r.message = client_message(e);
    }
    return r;
}

}
