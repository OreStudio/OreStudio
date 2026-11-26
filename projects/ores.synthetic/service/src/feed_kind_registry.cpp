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
#include "ores.analytics.quant/service/fx_vol_surface_generator.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/vendor/derived_notation.hpp"
#include "ores.refdata.core/mapping/quant_codes.hpp"
#include "ores.refdata.core/repository/fx_option_convention_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/service/pair_expiry_date_rule.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/derived_fx_vol.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/generator_outputs.hpp"
#include "ores.synthetic.api/feeds/index_fixings.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_paths_common.hpp"
#include "ores.synthetic.core/repository/calibration_policy_repository.hpp"
#include "ores.synthetic.core/repository/calibration_provider_order_repository.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_fixing_item_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/fx_vol_contributor_repository.hpp"
#include "ores.synthetic.core/repository/fx_vol_surface_feed_expiry_repository.hpp"
#include "ores.synthetic.core/repository/fx_vol_surface_feed_repository.hpp"
#include "ores.synthetic.core/repository/fx_vol_view_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/variance_calibration_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <expected>
#include <map>
#include <memory>
#include <mutex>
#include <ranges>
#include <stdexcept>
#include <utility>

namespace ores::synthetic::service {

std::optional<feed::calibration_policy>
read_calibration_policy(const ores::database::context& ctx, const std::string& asset_class_code) {
    namespace repo = ores::synthetic::repository;
    const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(ctx);
    const auto policies = repo::calibration_policy_repository{}.read_latest(sys_ctx);
    const auto found = std::ranges::find_if(
        policies, [&](const auto& p) { return p.asset_class_code == asset_class_code; });
    if (found == policies.end())
        return std::nullopt;

    feed::calibration_policy out;
    out.id = boost::uuids::to_string(found->id);
    out.version = found->version;
    out.window_days = found->window_days;
    out.variance_window_days = found->variance_window_days;
    out.allow_inverse = found->allow_inverse;
    out.fallback_code = found->fallback_code;
    out.variance_approval = {.by_policy = found->variance_approval_mode == "auto",
                             .min_returns = found->variance_min_returns,
                             .require_starts_agree = found->variance_require_starts_agree,
                             .min_persistence = found->variance_min_persistence,
                             .max_persistence = found->variance_max_persistence,
                             .min_reaction = found->variance_min_reaction};
    auto orders = repo::calibration_provider_order_repository{}.read_latest(sys_ctx);
    std::erase_if(orders, [&](const auto& o) { return o.policy_id != found->id; });
    std::ranges::sort(orders, {}, &ores::synthetic::domain::calibration_provider_order::rank);
    for (const auto& o : orders)
        out.provider_order.push_back(o.provider_code);
    return out;
}

namespace {

auto& registry_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_kind_registry");
    return instance;
}

/**
 * @brief The source name of the spot feed a surface names, or why it names none.
 *
 * A URI names what is observed and not who observes it, so two collections may quote one spot
 * under one URI; the provider is what makes it one feed. A spot feed's provider is its
 * collection's.
 */
std::expected<std::string, std::string>
spot_feed_named(const ores::database::context& ctx,
                const ores::synthetic::domain::fx_vol_surface_feed& cfg) {
    namespace repo = ores::synthetic::repository;
    std::map<boost::uuids::uuid, std::string> provider_of_collection;
    for (const auto& collection : repo::market_data_generation_config_repository().read_latest(ctx))
        provider_of_collection.emplace(collection.id, collection.provider_code);
    // A tenant reads its own rows and the system tenant's, so one URI and provider can name a
    // spot in each. The nearest wins: the surface's own collection, then its own tenant.
    const auto nearness = [&cfg](const ores::synthetic::domain::fx_spot_generation_config& spot) {
        if (spot.config_id == cfg.config_id)
            return 0;
        return spot.tenant_id == cfg.tenant_id ? 1 : 2;
    };
    std::optional<ores::synthetic::domain::fx_spot_generation_config> nearest;
    for (const auto& spot : repo::fx_spot_generation_config_repository().read_latest(ctx)) {
        if (spot.oresmd_uri != cfg.spot_oresmd_uri)
            continue;
        const auto provider = provider_of_collection.find(spot.config_id);
        if (provider == provider_of_collection.end() || provider->second != cfg.spot_provider_code)
            continue;
        if (!nearest || nearness(spot) < nearness(*nearest))
            nearest = spot;
    }
    if (nearest)
        return nearest->source_name;
    return std::unexpected("The surface " + cfg.base_currency_code + "/" + cfg.quote_currency_code +
                           " names no spot feed: no collection of provider '" +
                           cfg.spot_provider_code + "' has a current spot feed at " +
                           cfg.spot_oresmd_uri);
}

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

/**
 * The rows every FX feed of one candidates() call builds from, read once at the first build. A
 * feed's build reads whole tables and keeps its own rows, so without this a class of feeds read
 * each table once per feed.
 */
struct fx_build_rows final {
    std::mutex guard;
    bool read = false;
    std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::gmm_component>> components;
    std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::fx_spot_fixing_item>> items;
    std::map<std::string, std::optional<feed::calibration_policy>> policies;

    void read_once(const ores::database::context& ctx) {
        if (read)
            return;
        for (auto& g : repository::gmm_component_repository{}.read_latest(ctx))
            components[g.fx_spot_config_id].push_back(std::move(g));
        for (auto& i : repository::fx_spot_fixing_item_repository{}.read_latest(ctx))
            items[i.fx_spot_config_id].push_back(std::move(i));
        read = true;
    }

    std::optional<feed::calibration_policy> policy_of(const ores::database::context& ctx,
                                                      const std::string& asset_class_code) {
        const auto found = policies.find(asset_class_code);
        if (found != policies.end())
            return found->second;
        return policies.emplace(asset_class_code, read_calibration_policy(ctx, asset_class_code))
            .first->second;
    }
};

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
        const auto shared = std::make_shared<fx_build_rows>();
        for (const auto& fx : rows) {
            feed_kind_candidate c;
            c.feed_config_id = boost::uuids::to_string(fx.id);
            c.container_id = fx.config_id;
            c.source_name = fx.source_name;
            c.display_name = fx.oresmd_uri;
            c.enabled = fx.enabled;
            c.auto_start = fx.auto_start;
            c.folder_id = fx.folder_id;
            c.asset_class_code = fx.asset_class_code;
            c.fx_pair = std::make_pair(fx.base_currency_code, fx.quote_currency_code);
            c.build_input =
                [ctx, fx, shared](const feed::feed_adapter& adapter) -> feed_build_outcome {
                std::unique_lock lock(shared->guard);
                shared->read_once(ctx);
                auto components = shared->components[fx.id];
                if (components.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no GMM components: " +
                                           boost::uuids::to_string(fx.id)};
                std::vector<ores::synthetic::domain::fx_spot_fixing_item> fixing_items;
                if (fx.price_source == "fixing")
                    fixing_items = shared->items[fx.id];

                // A config that names no items calibrates under its class policy, or the
                // defaults when the class has none. The feed's own window overrides the
                // policy's for the items it derives; explicit items keep their own windows.
                auto policy = shared->policy_of(ctx, fx.asset_class_code)
                                  .value_or(feed::calibration_policy{});
                lock.unlock();
                if (fx.calibration_window_days)
                    policy.window_days = *fx.calibration_window_days;

                // The calibration a GJR-GARCH feed names is a system row, read as stored; the
                // feed decides at start whether it is approved.
                std::optional<ores::synthetic::domain::variance_calibration> calibration;
                if (fx.variance_calibration_id) {
                    const auto found =
                        ores::synthetic::repository::variance_calibration_repository{}.read_latest(
                            ores::database::service::tenant_context::with_system_tenant(ctx),
                            boost::uuids::to_string(*fx.variance_calibration_id));
                    if (!found.empty())
                        calibration = found.front();
                }

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
                            fx,
                            std::move(components),
                            std::move(target),
                            std::move(fixing_items),
                            std::move(policy),
                            std::move(calibration)}},
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
        // A derived index reads the overnight curve of its currency, which is a config
        // of its own: a request that names one config still needs the others to find
        // that curve, so the funding curves are read whole unless the request is whole.
        const auto other_rows = id.empty() ? decltype(rows){} : config_repo.read_latest(ctx);
        const auto& funding_rows = id.empty() ? rows : other_rows;

        // A curve quotes one id for each of its template entries, so the candidates carry the
        // entries in the notation's own words and an offer writes an id from them. The entry
        // table is read once for every row of the request, since its repository reads it whole.
        namespace vendor = ores::marketdata::vendor;
        repo::ir_curve_template_entry_repository entry_repo;
        auto all_entries = entry_repo.read_latest(ctx);
        std::ranges::sort(all_entries, [](const auto& a, const auto& b) {
            return a.sequence_index < b.sequence_index;
        });

        // A derived index reads its currency's overnight curve, so the funding
        // configs are read once for the whole request: the day-count convention
        // that annualises each currency's compounded rate is refdata's, not the
        // synthetic config's, and the row shape is what names it.
        namespace rrepo = ores::refdata::repository;
        namespace rmapping = ores::refdata::mapping;
        rrepo::ir_curve_bootstrap_config_repository bootstrap_repo;
        const auto bootstrap_configs = bootstrap_repo.read_latest(ctx);

        // The convention an index's term is measured in is its overnight curve's own: the
        // funding config of the currency that takes no tenor, which is the curve a derived
        // index reads.
        const auto convention_of = [&bootstrap_configs](const std::string& currency) {
            const auto funds = [&currency](const auto& config) {
                return config.currency_code == currency && config.curve_family_role == "FUNDING" &&
                       config.index_tenor.empty();
            };
            const auto funding = std::ranges::find_if(bootstrap_configs, funds);
            if (funding == bootstrap_configs.end())
                return ores::analytics::quant::service::day_count_convention_code::A360;
            try {
                return rmapping::parse_day_count_convention_code(funding->day_count_convention);
            } catch (const std::exception&) {
                // A convention the calculator does not implement leaves the compounded
                // rate on the most common overnight basis rather than refusing the whole
                // request.
                return ores::analytics::quant::service::day_count_convention_code::A360;
            }
        };

        // A derived index says why it is not offered rather than vanishing from the
        // run: the address its fixings take, the curve it reads and the term are all
        // things a person can fix, and silence would say none of it.

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
            const auto index_tenor = feed::ir_curve_index_tenor(cfg);
            if (auto item = feed::curve_fixing_uri(cfg))
                c.fixing_item = std::move(*item);
            if (cfg.rate_source != "process") {
                // The currency's funding config publishes the overnight curve the
                // rate is read from; its source name is the board member's code.
                // A currency may have more than one config that funds it, and only the
                // one that starts publishes its curve: a curve nobody runs states no
                // rate to derive from. Prefer a funding config that starts with the
                // service, and fall back to any funding config only when none does.
                const auto funds = [&cfg](const auto& other) {
                    return other.currency_code == cfg.currency_code &&
                           feed::ir_curve_discounts_its_currency(other);
                };
                auto funding = std::ranges::find_if(funding_rows, [&](const auto& other) {
                    return funds(other) && other.auto_start;
                });
                if (funding == funding_rows.end())
                    funding = std::ranges::find_if(funding_rows, funds);
                // A tenor is a count and a unit: 1M, 3M, 6M or 1Y. The rule needs the
                // term in months, so the digits are read up to the unit letter.
                int months = 0;
                const auto tenor = cfg.tenor;
                if (tenor.size() >= 2) {
                    int count = 0;
                    const auto digits_end = tenor.data() + tenor.size() - 1;
                    const auto [end, ec] = std::from_chars(tenor.data(), digits_end, count);
                    if (ec == std::errc{} && end == digits_end)
                        months = tenor.back() == 'Y' ? count * 12 : count;
                }
                if (funding == funding_rows.end())
                    ORES_LOG_SEV(registry_lg(), warn)
                        << "The derived index " << cfg.source_name
                        << " names no currency curve to read: no config of " << cfg.currency_code
                        << " funds and takes no tenor.";
                if (months <= 0)
                    ORES_LOG_SEV(registry_lg(), warn)
                        << "The derived index " << cfg.source_name << " has no term in its tenor '"
                        << cfg.tenor << "'.";
                if (!c.fixing_item)
                    ORES_LOG_SEV(registry_lg(), warn)
                        << "The derived index " << cfg.source_name
                        << " has no fixing address, so no fixing can be stated for it.";
                if (funding != funding_rows.end() && months > 0)
                    c.derived = feed::derived_fixing{
                        .curve_code = feed::curve_member_code(funding->source_name),
                        .rule = cfg.rate_source == "compounded_overnight" ?
                                    ores::analytics::quant::service::term_rate_rule::
                                        compounded_overnight :
                                    ores::analytics::quant::service::term_rate_rule::
                                        overnight_plus_spread,
                        .term_months = months,
                        .convention = convention_of(cfg.currency_code),
                        .spread_basis_points = cfg.derived_spread_basis_points};
                if (c.derived)
                    ORES_LOG_SEV(registry_lg(), info)
                        << "The derived index " << cfg.source_name << " is offered: it fixes "
                        << c.fixing_item.value_or("(no address)") << " as "
                        << (cfg.rate_source == "compounded_overnight" ? "the compounded" :
                                                                        "the spread")
                        << " rate of " << c.derived->curve_code << " over "
                        << c.derived->term_months << " month(s).";
            }
            for (const auto& e : all_entries)
                if (e.ir_curve_config_id == cfg.id)
                    c.pillars.push_back(vendor::ir_curve_pillar{cfg.currency_code,
                                                                cfg.index_family,
                                                                e.instrument_code,
                                                                e.start_tenor_code,
                                                                e.end_tenor_code,
                                                                index_tenor});
            c.build_input = [ctx, cfg, derived = c.derived, item = c.fixing_item](
                                const feed::feed_adapter& adapter) -> feed_build_outcome {
                if (cfg.rate_source != "process") {
                    // A derived index is a node of its own: the config builds the generator
                    // that states its fixings, so starting the config starts the node and
                    // stopping it stops the node. It reads the curve its rule names and
                    // publishes under its provider.
                    if (!derived || !item)
                        return {.input = std::nullopt,
                                .skip_reason = "Derived index " + cfg.source_name +
                                               " states no rule or no fixing address"};
                    const auto target =
                        adapter_target_of(adapter.id,
                                          ores::marketdata::vendor::derived_provider,
                                          ores::marketdata::vendor::index_fixing{.item = *item});
                    if (!target)
                        return {.input = std::nullopt,
                                .skip_reason = "Derived index " + cfg.source_name +
                                               " has no id under the derived provider"};
                    return {.input = feed::derived_fixing_feed_build_input{.source_name =
                                                                               cfg.source_name,
                                                                           .item = *item,
                                                                           .target = *target,
                                                                           .rule = *derived}};
                }

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
                    // Only a provider whose id names the curve's index tenor is given it.
                    const auto* notation = vendor::notation_of(adapter.provider_code);
                    const auto index_tenor = notation != nullptr && notation->names_index_tenor() ?
                                                 feed::ir_curve_index_tenor(cfg) :
                                                 std::string{};
                    targets.reserve(entries.size());
                    for (const auto& e : entries) {
                        auto target = target_for(adapter,
                                                 vendor::ir_curve_pillar{cfg.currency_code,
                                                                         cfg.index_family,
                                                                         e.instrument_code,
                                                                         e.start_tenor_code,
                                                                         e.end_tenor_code,
                                                                         index_tenor});
                        if (!target)
                            return {.input = std::nullopt, .skip_reason = target.error()};
                        targets.push_back(
                            feed::ir_pillar_quote_target{.start_tenor_code = e.start_tenor_code,
                                                         .end_tenor_code = e.end_tenor_code,
                                                         .target = std::move(*target)});
                    }
                }

                // A curve of price source "fixing" starts under its class policy, or the
                // defaults when the class has none.
                auto policy = read_calibration_policy(ctx, cfg.asset_class_code)
                                  .value_or(feed::calibration_policy{});
                return {.input = feed::feed_build_input{ir_curve_feed_build_input{
                            cfg,
                            std::move(entries),
                            std::move(values),
                            read_definition_catalogue(ctx),
                            *refctx,
                            std::move(targets),
                            std::move(policy)}},
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
    if (target.row.kind && target.row.kind->held_by_adapters)
        return {.feed = {},
                .failure = "Feed " + target.row.candidate.feed_config_id +
                           " runs while a provider's simulated source holds it, so it is not "
                           "started by hand."};
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

    auto outcome = target.row.candidate.build_input(adapter);
    if (!outcome.input)
        return {.feed = {}, .failure = outcome.skip_reason};

    // A derived index reads a curve another node publishes, so it states nothing while the
    // graph marks it starved: the caller that starts it is the one that reads the graph. It
    // also asks which days the market already holds, so a node started by hand states the
    // history the market lacks and not one it has.
    if (auto* derived = std::get_if<feed::derived_fixing_feed_build_input>(&*outcome.input);
        derived != nullptr) {
        if (!derived->held)
            derived->held = bctx.held_by_item;
        if (bctx.node_halted)
            derived->starved = [halted = bctx.node_halted,
                                name = target.row.candidate.source_name] {
                return halted(name);
            };
    }

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


/**
 * @brief The synthetic volatility surface kind.
 *
 * The surface has no path to simulate, so its simulate closures refuse and it answers no
 * subject. Its quotes are read from the surface it steps, which only a running feed has.
 */
feed_kind make_fx_vol_surface_kind() {
    using ores::synthetic::feed::fx_vol_surface_feed_build_input;
    using ores::synthetic::feed::fx_vol_surface_feed_kind;
    namespace repo = ores::synthetic::repository;

    feed_kind k;
    k.kind = std::string(fx_vol_surface_feed_kind);
    k.config_permission = "synthetic::fx_vol_surface_feeds:read";
    k.held_by_adapters = true;

    k.candidates = [](const ores::database::context& ctx, const std::string& id) {
        repo::fx_vol_surface_feed_repository feed_repo;
        const auto rows = id.empty() ? feed_repo.read_latest(ctx) : feed_repo.read_latest(ctx, id);

        std::vector<feed_kind_candidate> out;
        out.reserve(rows.size());
        for (const auto& cfg : rows) {
            feed_kind_candidate c;
            c.feed_config_id = boost::uuids::to_string(cfg.id);
            c.container_id = cfg.config_id;
            c.source_name = cfg.source_name;
            c.display_name = cfg.base_currency_code + "/" + cfg.quote_currency_code;
            c.enabled = cfg.enabled;
            c.auto_start = cfg.auto_start;
            c.folder_id = cfg.folder_id;
            c.asset_class_code = cfg.asset_class_code;
            c.fx_pair = std::pair{cfg.base_currency_code, cfg.quote_currency_code};
            const auto spot = spot_feed_named(ctx, cfg);
            if (spot)
                c.inputs.spot = *spot;
            c.inputs.base_curve = cfg.base_curve_name;
            c.inputs.quote_curve = cfg.quote_curve_name;
            // The block the run's market is built from, so it can be made before any feed
            // starts: the segments are the expiry rows and the numbers are the row's own.
            {
                repo::fx_vol_surface_feed_expiry_repository count_repo;
                std::size_t segments = 0;
                for (const auto& e : count_repo.read_latest(ctx))
                    if (e.feed_id == cfg.id)
                        ++segments;
                if (segments > 0)
                    c.block = feed_latent_block{.segments = segments,
                                                .spot_variance = cfg.spot_variance_correlation,
                                                .spot_skew = cfg.spot_skew_correlation,
                                                .neighbour = cfg.segment_correlation};
            }
            // The points the surface quotes, and who quotes them: each expiry at the labels the
            // feed quotes, and each contributor's provider, whose adapter is offered its points.
            {
                repo::fx_vol_surface_feed_expiry_repository point_repo;
                for (const auto& e : point_repo.read_latest(ctx))
                    if (e.feed_id == cfg.id)
                        for (const auto label : {"ATM", "25RR", "25BF", "10RR", "10BF"})
                            c.option_points.push_back(ores::marketdata::vendor::fx_option_quote{
                                cfg.base_currency_code, cfg.quote_currency_code, e.tenor, label});
                std::map<std::string, std::string> providers;
                repo::fx_vol_contributor_repository who_repo;
                for (const auto& who : who_repo.read_latest(ctx))
                    providers.emplace(boost::uuids::to_string(who.id), who.provider_code);
                repo::fx_vol_view_repository view_repo;
                for (const auto& view : view_repo.read_latest(ctx))
                    if (view.feed_id == cfg.id)
                        if (const auto found =
                                providers.find(boost::uuids::to_string(view.contributor_id));
                            found != providers.end())
                            c.views.emplace(
                                found->first,
                                surface_view{.provider_code = found->second,
                                             .view_id = boost::uuids::to_string(view.id),
                                             .source_name = view.source_name,
                                             .enabled = view.enabled});
            }
            c.build_input =
                [ctx, cfg, spot](const feed::feed_adapter& adapter) -> feed_build_outcome {
                static_cast<void>(adapter);
                if (!spot)
                    return {.input = std::nullopt, .skip_reason = spot.error()};

                repo::fx_vol_surface_feed_expiry_repository expiry_repo;
                std::vector<ores::synthetic::domain::fx_vol_surface_feed_expiry> expiries;
                for (auto& e : expiry_repo.read_latest(ctx))
                    if (e.feed_id == cfg.id)
                        expiries.push_back(std::move(e));
                if (expiries.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no Expiry rows: " +
                                           boost::uuids::to_string(cfg.id)};

                /*
                 * Each contributor observes the surface through its own noise, so its row's
                 * profile goes with its name. Without the profiles every contributor is the
                 * clean one, and the vendors would quote the same number.
                 */
                std::map<boost::uuids::uuid, ores::synthetic::domain::fx_vol_contributor>
                    contributor_rows;
                for (auto& row : repo::fx_vol_contributor_repository().read_latest(ctx))
                    contributor_rows.emplace(row.id, std::move(row));
                repo::fx_vol_view_repository contributor_repo;
                std::vector<std::string> contributors;
                std::vector<ores::analytics::quant::service::fx_vol_noise_profile> profiles;
                // Each contributor's noise stream is seeded by its place in the list, so the
                // list is in contributor id order and a restart keeps each vendor's stream.
                auto links = contributor_repo.read_latest(ctx);
                std::ranges::sort(links, {}, [](const auto& l) { return l.contributor_id; });
                for (auto& link : links) {
                    if (link.feed_id != cfg.id)
                        continue;
                    const auto found = contributor_rows.find(link.contributor_id);
                    if (found == contributor_rows.end())
                        return {.input = std::nullopt,
                                .skip_reason = "Feed config names a contributor with no row: " +
                                               boost::uuids::to_string(link.contributor_id)};
                    const auto& c = found->second;
                    contributors.push_back(boost::uuids::to_string(link.contributor_id));
                    profiles.push_back({.noise = c.noise,
                                        .wing_widening = c.wing_widening,
                                        .expiry_widening = c.expiry_widening,
                                        .quote_step = c.quote_step,
                                        .missing_probability = c.missing_probability,
                                        .stale_probability = c.stale_probability,
                                        .outlier_probability = c.outlier_probability,
                                        .outlier_size = c.outlier_size,
                                        .bias = c.bias});
                }
                if (contributors.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "Feed config has no Contributor rows: " +
                                           boost::uuids::to_string(cfg.id)};

                /*
                 * A pair's forward needs both currencies' curves, so a surface whose pair the
                 * run carries no curve feed for has nothing to be struck against. That is a
                 * configuration error and it is refused here, naming the currency, rather than
                 * discovered a tick at a time as a surface that publishes nothing.
                 */
                {
                    std::set<std::string> quoted;
                    repo::ir_curve_generation_config_repository curve_repo;
                    for (const auto& curve : curve_repo.read_latest(ctx))
                        quoted.insert(curve.currency_code);
                    for (const auto& ccy : {cfg.base_currency_code, cfg.quote_currency_code})
                        if (!quoted.contains(ccy))
                            return {.input = std::nullopt,
                                    .skip_reason = "No curve feed quotes " + ccy +
                                                   ", so the surface " + cfg.base_currency_code +
                                                   "/" + cfg.quote_currency_code +
                                                   " has no forward to be struck against"};
                }

                /*
                 * The date each expiry is struck at is the pair's, read the way the forward
                 * ladder reads it: one reader of the convention and the calendars, so the
                 * surface quotes at the dates the ladder publishes rather than its own.
                 */
                const auto pair_code = cfg.base_currency_code + "/" + cfg.quote_currency_code;
                const auto expiry_rule =
                    ores::refdata::service::read_pair_expiry_date_rule(ctx, pair_code);
                if (!expiry_rule)
                    return {.input = std::nullopt,
                            .skip_reason = "The surface " + pair_code +
                                           " cannot date its expiries: " + expiry_rule.error()};

                /*
                 * A quote means what the pair's market means by it, so each expiry is quoted in
                 * the convention the pair's own row states for it, the row the calibration reads.
                 * The id comes from the derived source's rule, so both feeds read the same row.
                 */
                const auto convention_id = feed::fx_option_convention_id(
                    {.base = cfg.base_currency_code, .quote = cfg.quote_currency_code});
                auto conventions =
                    ores::refdata::repository::fx_option_convention_repository().read_latest(
                        ctx, convention_id);
                if (conventions.empty())
                    return {.input = std::nullopt,
                            .skip_reason = "The surface " + pair_code +
                                           " has no FX option convention '" + convention_id + "'"};

                fx_vol_surface_feed_build_input in;
                in.config = cfg;
                in.spot_source_name = *spot;
                in.expiries = std::move(expiries);
                in.contributors = std::move(contributors);
                in.contributor_profiles = std::move(profiles);
                in.expiry_rule = expiry_rule->rule;
                in.convention = std::move(conventions.front());
                /*
                 * The market is not a fact of the row: the feed reads its pair's spot and its
                 * two currencies' curves from the run's values on every tick, and solves each
                 * expiry against the forward they imply.
                 */
                in.deltas.assign(feed::fx_vol_surface_deltas.begin(),
                                 feed::fx_vol_surface_deltas.end());
                return {.input = feed::feed_build_input{std::move(in)}, .skip_reason = {}};
            };
            out.push_back(std::move(c));
        }
        return out;
    };

    k.simulate_subject.clear();
    k.decode_simulate = [](const ores::nats::message&) -> std::optional<feed_simulate_envelope> {
        return std::nullopt;
    };
    k.reply_simulate = [](ores::nats::service::client&,
                          const ores::nats::message&,
                          const feed_simulation_result&) {
    };
    return k;
}

feed_kind_registry
make_default_feed_kind_registry(feed_kind_registry::adapter_lookup_fn adapter_of_provider) {
    feed_kind_registry registry(std::move(adapter_of_provider));
    registry.register_kind(make_fx_spot_kind());
    registry.register_kind(make_ir_curve_kind());
    registry.register_kind(make_fx_vol_surface_kind());
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
