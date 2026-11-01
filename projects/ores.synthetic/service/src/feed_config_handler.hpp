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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_CONFIG_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_CONFIG_HANDLER_HPP

#include "client_message.hpp"
#include "feed_kind_registry.hpp"
#include "generator_fleet.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/feed_start.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.core/service/fx_spot_generation_config_service.hpp"
#include "ores.synthetic.core/service/gmm_component_service.hpp"
#include <algorithm>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <variant>

namespace ores::synthetic::service {

namespace {
inline auto& feed_config_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_config_handler");
    return instance;
}

inline std::vector<ores::synthetic::messaging::provider_dispersion>
dispersion_of(const ores::marketdata::domain::feed_start& start) {
    std::vector<ores::synthetic::messaging::provider_dispersion> out;
    out.reserve(start.dispersion.size());
    for (const auto& d : start.dispersion)
        out.push_back(
            {.provider_code = d.provider_code, .item = d.item, .basis_points = d.basis_points});
    return out;
}

/// A run's start in the words of the wire, field for field.
inline ores::synthetic::messaging::feed_start
to_wire(const ores::marketdata::domain::feed_start& start) {
    return {.from_fixing = start.from_fixing,
            .item = start.item,
            .provider_code = start.provider_code,
            .vendor_id = start.vendor_id,
            .value = start.from_fixing ? start.value.to_string() : std::string{},
            .inverted = start.inverted,
            .fixing_date = start.fixing_date,
            .received_at = start.received_at,
            .window_days = start.window_days,
            .used = start.used,
            .hourly_spread = start.hourly_spread,
            .note = start.note,
            .age_days = start.age_days,
            .daily_spread = start.daily_spread,
            .annual_drift = start.annual_drift,
            .min_return = start.min_return,
            .max_return = start.max_return,
            .jumps = start.jumps,
            .dispersion = dispersion_of(start)};
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler for per-config feed start/stop/list control messages, one for every asset
 * class.
 *
 * Replaces the per-kind handlers (market_feed_config_handler and ir_curve_feed_config_handler):
 * the client sends only a config_id and the feed kind registry resolves the config — whichever
 * kind it is, its children, and the refdata context — checks the resolved kind's permission, and
 * builds the producer. The runner then runs it as a manual run, which no demand set reconciles
 * away. Missing and disabled outcomes use one message shape regardless of
 * kind, and a start of a feed that already runs answers the same success its kind does.
 */
class feed_config_handler {
public:
    feed_config_handler(ores::nats::service::client& nats,
                        ores::nats::service::nats_client& auth_nats,
                        generator_fleet& generators,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier,
                        const feed_kind_registry& registry,
                        ores::analytics::quant::service::tick_scheduler& scheduler)
        : nats_(nats)
        , auth_nats_(auth_nats)
        , generators_(generators)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry)
        , scheduler_(scheduler) {}

    void start(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("start", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        auto req = decode<start_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed start body; rejecting";
            reply(nats_,
                  msg,
                  start_feed_response{.success = false, .message = "Malformed start request"});
            return;
        }

        const auto target = registry_.resolve(req_ctx, req->config_id);
        if (!target) {
            reply(nats_,
                  msg,
                  start_feed_response{.success = false,
                                      .message = "Feed config not found: " + req->config_id});
            return;
        }

        if (!has_permission(req_ctx, target->row.kind->config_permission) ||
            !feed_kind_registry::permits_run(req_ctx)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting start request: missing permission "
                << target->row.kind->config_permission << " or " << run_permission << ".";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        if (target->container_is_system && !feed_kind_registry::is_system_caller(req_ctx)) {
            reply(
                nats_,
                msg,
                start_feed_response{.success = false,
                                    .message = "A system feed starts only in the system tenant."});
            return;
        }

        start_feed_response resp;
        try {
            const ores::synthetic::feed::feed_build_context bctx{
                .nats = nats_, .auth_nats = auth_nats_, .scheduler = scheduler_};
            auto attempt = registry_.make_feed(*target, bctx);
            if (!attempt.feed) {
                resp.success = false;
                resp.message = std::move(attempt.failure);
                reply(nats_, msg, resp);
                return;
            }
            reply_start_outcome(msg, resp, req->config_id, std::move(attempt.feed));
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = client_message(e);
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " — failed to start: " << e.what();
            reply(nats_, msg, resp);
        }
    }

    void stop(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("stop", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        if (!feed_kind_registry::permits_run(req_ctx)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting stop request: missing permission " << run_permission << ".";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<stop_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed stop body; rejecting";
            reply(nats_,
                  msg,
                  stop_feed_response{.success = false, .message = "Malformed stop request"});
            return;
        }

        // The registry reads the configured feeds and the runners stop them, so
        // either can throw. An exception leaving this handler reaches the
        // transport, which contains it but sends no reply, and the caller then
        // waits out its deadline for an answer this handler could have given.
        try {
            std::string source_name = req->source_name;
            if (!req->config_id.empty()) {
                // The registry resolves the config_id to its source_name server-side
                // so the client never needs the naming conventions.
                const auto target = registry_.resolve(req_ctx, req->config_id);
                if (!target) {
                    reply(
                        nats_,
                        msg,
                        stop_feed_response{.success = false,
                                           .message = "Feed config not found: " + req->config_id});
                    return;
                }
                source_name = target->row.candidate.source_name;
            }

            // Naming no feed stops every feed the service runs, those of other tenants included,
            // so only the system tenant may ask for it.
            if (source_name.empty() && !feed_kind_registry::is_system_caller(req_ctx)) {
                reply(nats_,
                      msg,
                      stop_feed_response{.success = false, .message = "Name the feed to stop."});
                return;
            }

            // A system feed runs once for every tenant, so only the system tenant stops it,
            // whether the caller names it by its config or by its source.
            if (!feed_kind_registry::is_system_caller(req_ctx)) {
                const auto system = registry_.system_containers(req_ctx);
                for (const auto& row : registry_.rows(req_ctx)) {
                    if (row.candidate.source_name == source_name &&
                        system.contains(row.candidate.container_id)) {
                        reply(nats_,
                              msg,
                              stop_feed_response{
                                  .success = false,
                                  .message = "A system feed stops only in the system tenant."});
                        return;
                    }
                }
            }

            const auto stopped =
                source_name.empty() ? generators_.stop_all() : generators_.stop_source(source_name);
            stop_feed_response resp;
            resp.success = true; // idempotent — 0 stopped means it was already stopped
            resp.message = std::to_string(stopped) + " feed(s) stopped";
            BOOST_LOG_SEV(feed_config_handler_lg(), info)
                << msg.subject << " — " << resp.message
                << (source_name.empty() ? " (all)" : " (" + source_name + ")");
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, stop_feed_response{.success = false, .message = e.what()});
        }
    }

    void list(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("list", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        // Every kind is listed, so the gate is the same uniform one the folder
        // cascade requires — the caller holds every registered kind's config
        // read permission.
        if (!registry_.permits_all_configs(req_ctx)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting list request: missing a registered kind's config read permission.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            list_feeds_response resp;
            for (const auto& feed : generators_.running_feeds()) {
                running_feed one;
                one.source_name = feed.source_name;
                if (feed.start)
                    one.start = to_wire(*feed.start);
                resp.running_feeds.push_back(std::move(one));
            }
            resp.success = true;
            BOOST_LOG_SEV(feed_config_handler_lg(), info)
                << msg.subject << " — " << resp.running_feeds.size() << " feed(s) running";
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            // This response carries no message field, so the reason is only in
            // the log; success=false is what the caller can act on.
            reply(nats_, msg, list_feeds_response{.success = false});
        }
    }

    void calibrate(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("calibrate", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        auto req = decode<calibrate_request>(msg);
        if (!req || req->asset_class_code.empty()) {
            reply(nats_,
                  msg,
                  calibrate_response{.success = false,
                                     .message = "A calibration names its asset class."});
            return;
        }

        const auto* kind = registry_.find(ores::synthetic::feed::fx_spot_feed_kind);
        if (kind == nullptr || !has_permission(req_ctx, kind->config_permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        // An applied calibration writes new versions of the configs and their components, and
        // the services that write them check no permission of their own.
        if (req->apply &&
            (!has_permission(req_ctx, "synthetic::fx_spot_generation_configs:write") ||
             !has_permission(req_ctx, "synthetic::gmm_components:write"))) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            reply(nats_, msg, calibrate_class(req_ctx, *kind, *req));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, calibrate_response{.success = false, .message = client_message(e)});
        }
    }

private:
    // Resolves every feed of the class as a start would, or the one feed the request names, and
    // starts none. A feed that cannot be built still appears, with the reason, so the class shows
    // every feed it holds. An applied calibration writes what moved as a new version.
    ores::synthetic::messaging::calibrate_response
    calibrate_class(const ores::database::context& req_ctx,
                    const feed_kind& kind,
                    const ores::synthetic::messaging::calibrate_request& req) {
        using namespace ores::synthetic::messaging;
        namespace feed = ores::synthetic::feed;
        const auto& asset_class_code = req.asset_class_code;

        std::vector<std::string> running;
        for (const auto& f : generators_.running_feeds())
            running.push_back(f.source_name);

        calibrate_response resp;
        const auto class_policy = read_calibration_policy(req_ctx, asset_class_code);
        const auto policy = class_policy.value_or(feed::calibration_policy{});
        resp.policy = {.from_policy_row = class_policy.has_value(),
                       .window_days = policy.window_days,
                       .allow_inverse = policy.allow_inverse,
                       .fallback_code = policy.fallback_code,
                       .provider_order = policy.provider_order};

        const ores::marketdata::domain::system_market_clock clock;
        // Read once, on the first feed that resolves from a fixing: each feed would otherwise
        // list every series again.
        std::optional<feed::series_listing> series;
        for (const auto& c : kind.candidates(req_ctx, {})) {
            if (c.asset_class_code != asset_class_code)
                continue;
            if (!req.config_id.empty() && c.feed_config_id != req.config_id)
                continue;
            calibrated_feed one;
            one.config_id = c.feed_config_id;
            one.source_name = c.source_name;
            one.running = std::ranges::contains(running, c.source_name);
            if (c.fx_pair) {
                one.base_currency_code = c.fx_pair->first;
                one.quote_currency_code = c.fx_pair->second;
            }
            const auto outcome = c.build_input(feed::feed_adapter{});
            if (!outcome.input) {
                one.start.note = outcome.skip_reason;
                if (req.apply) {
                    one.outcome = "failed";
                    one.outcome_message = outcome.skip_reason;
                }
                resp.feeds.push_back(std::move(one));
                continue;
            }
            const auto& in = std::get<feed::fx_spot_feed_build_input>(*outcome.input);
            one.explicit_items = !in.fixing_items.empty();
            one.window_days_override = in.config.calibration_window_days;
            if (in.config.price_source != "fixing") {
                one.start.note = "The config starts from its fixed level.";
                if (req.apply) {
                    one.outcome = "kept";
                    one.outcome_message = one.start.note;
                }
            } else {
                if (!series) {
                    auto delegated = auth_nats_.with_delegation({});
                    series = ores::marketdata::client::market_data_client(delegated).list_series();
                }
                const auto resolved = feed::resolve_fx_start(auth_nats_,
                                                             in.config,
                                                             in.components,
                                                             in.fixing_items,
                                                             in.policy,
                                                             &clock,
                                                             *series);
                one.refused = resolved.refused;
                one.start = to_wire(resolved.source);
                for (const auto& c : resolved.candidates)
                    one.candidates.push_back({.item = c.item,
                                              .provider_code = c.provider_code,
                                              .inverted = c.inverted,
                                              .rank = c.rank,
                                              .state = std::string(feed::to_string(c.state)),
                                              .reason = c.reason});
                if (req.apply)
                    apply_calibration(req_ctx, in, resolved, one);
            }
            resp.feeds.push_back(std::move(one));
        }
        std::ranges::sort(resp.feeds, {}, &calibrated_feed::source_name);
        resp.success = true;
        BOOST_LOG_SEV(feed_config_handler_lg(), info)
            << "Calibrated " << resp.feeds.size() << " feed(s) of class " << asset_class_code;
        return resp;
    }

    // Writes what the calibration moved: the components' deviations, then the config's level, each
    // claimed against the version the resolution read, so a write that raced it is refused rather
    // than overwritten. A feed whose values already match writes nothing.
    void apply_calibration(const ores::database::context& req_ctx,
                           const ores::synthetic::feed::fx_spot_feed_build_input& in,
                           const ores::synthetic::feed::resolved_start& resolved,
                           ores::synthetic::messaging::calibrated_feed& one) {
        using namespace ores::synthetic::messaging;
        using ores::utility::domain::outcome;
        using ores::utility::domain::precondition;
        using ores::utility::domain::precondition_kind;
        namespace feed = ores::synthetic::feed;

        if (resolved.refused) {
            one.outcome = "refused";
            one.outcome_message = resolved.source.note;
            return;
        }
        const auto& src = resolved.source;
        if (!src.from_fixing) {
            one.outcome = "kept";
            one.outcome_message = src.note;
            return;
        }

        std::vector<double> stored_stdevs;
        for (const auto& c : in.components)
            stored_stdevs.push_back(c.stdev);
        const auto change = feed::change_of(
            in.config.gmm_initial_price, stored_stdevs, resolved.level, resolved.stdevs);
        const auto fixing = std::format("the {} fixing of {}{}",
                                        src.provider_code,
                                        src.fixing_date ? std::format("{:%F}", *src.fixing_date) :
                                                          std::string("an unknown date"),
                                        src.inverted ? ", inverted" : "");
        if (change.empty()) {
            one.outcome = "unchanged";
            one.outcome_message = "The level and the mixture already match " + fixing + ".";
            return;
        }

        std::string words = "Calibrated from " + fixing + ":";
        if (change.level)
            words +=
                std::format(" level {:.6g} to {:.6g};", in.config.gmm_initial_price, *change.level);
        const auto moved =
            std::ranges::count_if(change.stdevs, [](const auto& s) { return s.has_value(); });
        if (moved > 0)
            words +=
                std::format(" {} deviation(s) scaled to the volatility of {} fixing(s) over {} "
                            "day(s);",
                            moved,
                            src.used,
                            src.window_days);
        words.pop_back();
        words += ".";
        const ores::utility::domain::change_intent intent{.reason_code = "system.calibration",
                                                          .commentary = words};
        const auto claimed = [](int version) {
            return precondition{.kind = precondition_kind::must_match_version,
                                .version = static_cast<std::uint32_t>(version)};
        };

        try {
            if (moved > 0) {
                put_many_gmm_components_request request{.intent = intent};
                for (std::size_t i = 0; i < in.components.size(); ++i) {
                    if (!change.stdevs[i])
                        continue;
                    const auto& c = in.components[i];
                    request.changes.push_back({.write = {.id = c.id,
                                                         .fx_spot_config_id = c.fx_spot_config_id,
                                                         .component_index = c.component_index,
                                                         .description = c.description,
                                                         .mean = c.mean,
                                                         .stdev = *change.stdevs[i],
                                                         .weight = c.weight},
                                               .precondition = claimed(c.version)});
                }
                const auto written = ores::synthetic::service::gmm_component_service(req_ctx)
                                         .put_many_gmm_components(request);
                if (written.result.outcome != outcome::ok) {
                    one.outcome = "failed";
                    one.outcome_message = "The mixture was not written: " + written.result.message;
                    return;
                }
            }
            if (change.level) {
                const auto& cfg = in.config;
                put_fx_spot_generation_config_request request{
                    .change = {.write = {.id = cfg.id,
                                         .config_id = cfg.config_id,
                                         .base_currency_code = cfg.base_currency_code,
                                         .quote_currency_code = cfg.quote_currency_code,
                                         .source_name = cfg.source_name,
                                         .oresmd_uri = cfg.oresmd_uri,
                                         .price_source = cfg.price_source,
                                         .calibration_window_days = cfg.calibration_window_days,
                                         .gmm_initial_price = *change.level,
                                         .ticks_per_hour = cfg.ticks_per_hour,
                                         .process_type = cfg.process_type,
                                         .enabled = cfg.enabled,
                                         .auto_start = cfg.auto_start,
                                         .folder_id = cfg.folder_id,
                                         .asset_class_code = cfg.asset_class_code,
                                         .arrival_mode = cfg.arrival_mode,
                                         .branching_ratio = cfg.branching_ratio,
                                         .decay = cfg.decay,
                                         .sensitivity = cfg.sensitivity},
                               .precondition = claimed(cfg.version)},
                    .intent = intent};
                const auto written =
                    ores::synthetic::service::fx_spot_generation_config_service(req_ctx)
                        .put_fx_spot_generation_config(request);
                if (written.result.outcome != outcome::ok) {
                    one.outcome = "failed";
                    one.outcome_message = "The level was not written: " + written.result.message;
                    return;
                }
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Calibration of " << one.source_name << " was not written: " << e.what();
            one.outcome = "failed";
            one.outcome_message = "The calibration was not written: " + client_message(e);
            return;
        }
        one.outcome = "updated";
        one.outcome_message = words;
        BOOST_LOG_SEV(feed_config_handler_lg(), info) << one.source_name << ": " << words;
    }

    // Auth gate shared by every verb; the per-kind permission check happens
    // after resolution, since the caller's kind is not known until the
    // config_id is resolved.
    std::optional<ores::database::context> authenticated_context(std::string_view verb,
                                                                 const ores::nats::message& msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting " << verb
                << " request: auth failed: " << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        return std::optional<ores::database::context>(std::move(*ctx_expected));
    }

    // One start outcome for every kind: a started feed and a feed that already runs
    // are both a success, so the dispatch needs no per-kind branching.
    void reply_start_outcome(const ores::nats::message& msg,
                             ores::synthetic::messaging::start_feed_response& resp,
                             const std::string& feed_config_id,
                             std::shared_ptr<ores::marketdata::domain::i_feed> feed) {
        const auto source_name = feed->source_name();
        const auto started = generators_.start_manual(feed_config_id, std::move(feed));
        resp.success = true;
        resp.message =
            started ? "Feed started: " + source_name : "Feed already running: " + source_name;
        BOOST_LOG_SEV(feed_config_handler_lg(), info) << msg.subject << " — " << resp.message;
        reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& auth_nats_;
    generator_fleet& generators_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
    ores::analytics::quant::service::tick_scheduler& scheduler_;
};

}

#endif
