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
#include "ir_curve_preview_process.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.api/feeds/vintage_lookup.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_ir_curve_paths_protocol.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
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
    return feed::build_ir_curve_refdata_context(
        ctx, feed::ir_curve_tenor_convention_code(feed::ir_curve_qualifier(cfg)));
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
            c.display_name = fx.ore_key;
            c.enabled = fx.enabled;
            c.auto_start = fx.auto_start;
            c.folder_id = fx.folder_id;
            c.vintage_anchor = [fx] {
                feed_vintage_anchor a;
                a.applicable = fx.price_source == "vintage";
                if (!a.applicable)
                    return a;
                a.source = fx.vintage_source;
                a.date = fx.vintage_date;
                const auto d = datum::ore_key_codec::read(fx.ore_key);
                if (!d) {
                    a.error = "ORE key '" + fx.ore_key + "': " + d.error();
                    return a;
                }
                a.series_uri = datum::oresmd_uri_codec::write(datum::series_of(*d)).value();
                a.datum_uri = datum::oresmd_uri_codec::write(*d).value();
                return a;
            };
            c.build_input = [ctx, fx](domain::binding_mode mode) -> feed_build_outcome {
                repo::gmm_component_repository comp_repo;
                std::vector<ores::synthetic::domain::gmm_component> components;
                for (auto& g : comp_repo.read_latest(ctx))
                    if (g.fx_spot_config_id == fx.id)
                        components.push_back(std::move(g));
                if (components.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no GMM components: " +
                                           boost::uuids::to_string(fx.id)};
                return {.input = feed::feed_build_input{fx_spot_feed_build_input{
                            fx, std::move(components), mode}},
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
                return std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>(
                    ores::analytics::quant::service::process_factory::make_process(
                        req.process_type,
                        req.gmm_means,
                        req.gmm_stdevs,
                        req.gmm_weights,
                        req.initial_price,
                        seed));
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
            c.vintage_anchor = [ctx, cfg] {
                feed_vintage_anchor a;
                a.applicable = cfg.price_source == "vintage";
                if (!a.applicable)
                    return a;
                a.source = cfg.vintage_source;
                a.date = cfg.vintage_date;
                a.series_uri = cfg.vintage_series_uri;
                a.party_id = boost::uuids::to_string(cfg.party_id);

                repo::ir_curve_template_entry_repository entry_repo;
                std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
                for (auto& e : entry_repo.read_latest(ctx))
                    if (e.ir_curve_config_id == cfg.id)
                        entries.push_back(std::move(e));
                if (entries.empty()) {
                    a.error = "Feed config has no Curve Template entries: " +
                              boost::uuids::to_string(cfg.id);
                    return a;
                }
                const auto refctx = ir_refdata_context(ctx, cfg);
                if (!refctx) {
                    a.error = "Tenor convention not found: " +
                              feed::ir_curve_tenor_convention_code(feed::ir_curve_qualifier(cfg));
                    return a;
                }
                const auto resolved =
                    feed::resolve(entries, *refctx, cfg.fixed_leg_payment_frequency_code);
                const auto* anchor = feed::select_vintage_anchor_entry(resolved);
                if (!anchor) {
                    a.error = "Cannot resolve vintage initial_rate: config has no DEPOSIT "
                              "entry to anchor on.";
                    return a;
                }

                // The row the vintage is read from is the anchor pillar's own datum: the series
                // the config names at the pillar's point.
                const auto series_datum = datum::oresmd_uri_codec::read(cfg.vintage_series_uri);
                if (!series_datum || !series_datum->is_series()) {
                    a.error = "'" + cfg.vintage_series_uri + "' is not a series URI" +
                              (series_datum ? "" : ": " + series_datum.error());
                    return a;
                }
                const auto anchor_datum = datum::datum_at_point(*series_datum, anchor->point_id);
                if (!anchor_datum) {
                    a.error = "no datum for series '" + cfg.vintage_series_uri + "' at point '" +
                              anchor->point_id + "': " + anchor_datum.error();
                    return a;
                }
                const auto anchor_uri = datum::oresmd_uri_codec::write(*anchor_datum);
                if (!anchor_uri) {
                    a.error = "no URI for series '" + cfg.vintage_series_uri + "' at point '" +
                              anchor->point_id + "': " + anchor_uri.error();
                    return a;
                }
                a.datum_uri = *anchor_uri;
                return a;
            };
            c.build_input = [ctx, cfg](domain::binding_mode mode) -> feed_build_outcome {
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
                                           feed::ir_curve_tenor_convention_code(
                                               feed::ir_curve_qualifier(cfg))};
                return {.input = feed::feed_build_input{ir_curve_feed_build_input{
                            cfg,
                            std::move(entries),
                            std::move(values),
                            read_definition_catalogue(ctx),
                            *refctx,
                            mode}},
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
                return std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>(
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

feed_kind_registry::feed_kind_registry()
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
                }} {}

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
    target.binding_mode = found->second.binding_mode;
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

    const auto outcome = target.row.candidate.build_input(target.binding_mode);
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

ores::marketdata::messaging::vintage_validity_entry
feed_kind_registry::check_vintage(const feed_kind_row& row,
                                  ores::nats::service::nats_client& auth_nats,
                                  const std::string& caller_bearer_token) const {
    ores::marketdata::messaging::vintage_validity_entry e;
    e.config_id = row.candidate.feed_config_id;
    e.kind = row.kind->kind;
    const auto a = row.candidate.vintage_anchor();
    e.applicable = a.applicable;
    if (!a.applicable)
        return e;
    // A non-empty error means the anchor could not be derived: applicable, and invalid.
    if (!a.error.empty())
        return e;
    const auto found = ores::synthetic::feed::find_vintage_observation(
        auth_nats,
        caller_bearer_token,
        a.series_uri,
        a.datum_uri,
        a.source,
        a.date,
        "No vintage data found for source=" + a.source + ", date=" + a.date + ".",
        a.series_uri,
        a.party_id);
    e.valid = found.has_value();
    return e;
}

std::vector<ores::marketdata::messaging::vintage_validity_entry>
feed_kind_registry::vintage_validity(const ores::database::context& ctx,
                                     ores::nats::service::nats_client& auth_nats,
                                     const std::string& caller_bearer_token) const {
    std::vector<ores::marketdata::messaging::vintage_validity_entry> out;
    for (const auto& row : rows(ctx))
        out.push_back(check_vintage(row, auth_nats, caller_bearer_token));
    return out;
}

feed_kind_registry make_default_feed_kind_registry() {
    feed_kind_registry registry;
    registry.register_kind(make_fx_spot_kind());
    registry.register_kind(make_ir_curve_kind());
    return registry;
}

feed_simulation_result run_simulate_paths(const feed_simulate_envelope& env) {
    feed_simulation_result r;
    const int ticks = std::clamp(env.num_ticks, 1, messaging::max_simulate_num_ticks);
    const int paths = std::clamp(env.num_paths, 1, messaging::max_simulate_num_paths);
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
        r.message = e.what();
    }
    return r;
}

}
