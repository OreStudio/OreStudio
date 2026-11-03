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
#include "ores.synthetic.api/feeds/variance_calibration.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/variance_calibration_input_point_repository.hpp"
#include "ores.synthetic.core/repository/variance_calibration_input_repository.hpp"
#include "ores.synthetic.core/repository/variance_calibration_repository.hpp"
#include "ores.synthetic.core/service/fx_spot_generation_config_service.hpp"
#include "ores.synthetic.core/service/gmm_component_service.hpp"
#include "ores.synthetic.core/service/variance_calibration_input_point_service.hpp"
#include "ores.synthetic.core/service/variance_calibration_input_service.hpp"
#include "ores.synthetic.core/service/variance_calibration_service.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
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

/// How a run's daily variance started, in the words of the wire.
inline std::optional<ores::synthetic::messaging::variance_start>
variance_of(const ores::marketdata::domain::feed_start& start) {
    if (!start.variance)
        return std::nullopt;
    const auto& v = *start.variance;
    return ores::synthetic::messaging::variance_start{.model = v.model,
                                                      .calibration_id = v.calibration_id,
                                                      .calibration_version = v.calibration_version,
                                                      .fitted = v.fitted,
                                                      .alpha = v.alpha,
                                                      .gamma = v.gamma,
                                                      .beta = v.beta,
                                                      .persistence = v.persistence,
                                                      .initial_ratio = v.initial_ratio,
                                                      .returns = v.returns,
                                                      .note = v.note};
}

/// A stored FX spot config as a write that keeps every field it holds.
inline ores::synthetic::messaging::fx_spot_generation_config_write
write_of(const ores::synthetic::domain::fx_spot_generation_config& cfg) {
    return {.id = cfg.id,
            .config_id = cfg.config_id,
            .base_currency_code = cfg.base_currency_code,
            .quote_currency_code = cfg.quote_currency_code,
            .source_name = cfg.source_name,
            .oresmd_uri = cfg.oresmd_uri,
            .price_source = cfg.price_source,
            .calibration_window_days = cfg.calibration_window_days,
            .gmm_initial_price = cfg.gmm_initial_price,
            .ticks_per_hour = cfg.ticks_per_hour,
            .process_type = cfg.process_type,
            .enabled = cfg.enabled,
            .auto_start = cfg.auto_start,
            .folder_id = cfg.folder_id,
            .asset_class_code = cfg.asset_class_code,
            .arrival_mode = cfg.arrival_mode,
            .branching_ratio = cfg.branching_ratio,
            .decay = cfg.decay,
            .sensitivity = cfg.sensitivity,
            .variance_model = cfg.variance_model,
            .variance_calibration_id = cfg.variance_calibration_id};
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
            .dispersion = dispersion_of(start),
            .variance = variance_of(start)};
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
        // the services that write them check no permission of their own. The feeds it writes run
        // for every tenant, so, like the policy, only the system tenant may apply it.
        if (req->apply &&
            (!feed_kind_registry::is_system_caller(req_ctx) ||
             !has_permission(req_ctx, "synthetic::fx_spot_generation_configs:write") ||
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

    void review_variance_calibration(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("review", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        auto req = decode<review_variance_calibration_request>(msg);
        if (!req || req->calibration_id.empty() ||
            (req->decision != "approve" && req->decision != "reject") || req->reason.empty()) {
            reply(nats_,
                  msg,
                  review_variance_calibration_response{
                      .success = false,
                      .message = "A review names the calibration, a decision of approve or "
                                 "reject, and the reason for it.",
                      .refusal = "invalid"});
            return;
        }
        // The calibrations and the feeds an approval re-points are system rows that every tenant
        // runs, so only the system tenant reviews them, under a permission of its own.
        if (!feed_kind_registry::is_system_caller(req_ctx) ||
            !has_permission(req_ctx, "synthetic::variance_calibrations:review")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            reply(nats_, msg, review(req_ctx, *req));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  review_variance_calibration_response{.success = false,
                                                       .message = client_message(e)});
        }
    }

private:
    // Each write claims the version it read, so a review that raced another is refused rather
    // than overwriting it. Approving supersedes before it approves, because the partial unique
    // index admits one approval of a pair, and then points the pair's feeds at it. Each step
    // reads what is stored before it writes, so a review that stopped part way converges when it
    // is sent again: an approval of a calibration already approved only points the feeds that
    // still name another one.
    ores::synthetic::messaging::review_variance_calibration_response
    review(const ores::database::context& req_ctx,
           const ores::synthetic::messaging::review_variance_calibration_request& req) {
        using namespace ores::synthetic::messaging;
        using ores::utility::domain::outcome;
        using ores::utility::domain::precondition;
        using ores::utility::domain::precondition_kind;
        namespace repo = ores::synthetic::repository;
        namespace svc = ores::synthetic::service;
        review_variance_calibration_response resp;

        const auto found =
            repo::variance_calibration_repository{}.read_latest(req_ctx, req.calibration_id);
        if (found.empty()) {
            resp.refusal = "not_found";
            resp.message = "No variance calibration " + req.calibration_id + " exists.";
            return resp;
        }
        const auto& c = found.front();
        const bool repoint_only = c.state == "approved" && req.decision == "approve";
        if (c.state != "proposed" && !repoint_only) {
            resp.refusal = "decided";
            resp.message = "Only a proposed calibration is reviewed; " + req.calibration_id +
                           " is " + c.state + ".";
            resp.state = c.state;
            return resp;
        }
        const auto claimed = [](int version) {
            return precondition{.kind = precondition_kind::must_match_version,
                                .version = static_cast<std::uint32_t>(version)};
        };
        const auto write_state = [&](const ores::synthetic::domain::variance_calibration& row,
                                     const std::string& state,
                                     const std::string& reason) {
            const ores::utility::domain::change_intent intent{
                .reason_code = "system.review",
                .commentary = std::format("Variance calibration {} {}: {}",
                                          boost::uuids::to_string(row.id),
                                          state,
                                          reason)};
            const auto written =
                svc::variance_calibration_service(req_ctx).put_variance_calibration(
                    {.change = {.write = {.id = row.id,
                                          .base_currency_code = row.base_currency_code,
                                          .quote_currency_code = row.quote_currency_code,
                                          .input_id = row.input_id,
                                          .method = row.method,
                                          .method_version = row.method_version,
                                          .alpha = row.alpha,
                                          .gamma = row.gamma,
                                          .beta = row.beta,
                                          .persistence = row.persistence,
                                          .long_run_variance = row.long_run_variance,
                                          .starting_variance_ratio = row.starting_variance_ratio,
                                          .log_likelihood = row.log_likelihood,
                                          .returns_used = row.returns_used,
                                          .starts = row.starts,
                                          .starts_agreed = row.starts_agreed,
                                          .state = state,
                                          .state_reason = reason},
                                .precondition = claimed(row.version)},
                     .intent = intent});
            if (written.result.outcome != outcome::ok)
                throw std::runtime_error("the calibration " + boost::uuids::to_string(row.id) +
                                         " was not written: " + written.result.message);
        };

        if (req.decision == "reject") {
            write_state(c, "rejected", req.reason);
            resp.success = true;
            resp.state = "rejected";
            resp.message = "Rejected: " + req.reason;
            return resp;
        }

        int closed = 0;
        // Approving again completes an approval that stopped part way, so it also closes what
        // the first approval would have closed.
        for (const auto& other : repo::variance_calibration_repository{}.read_latest(req_ctx)) {
            if (other.id == c.id || other.base_currency_code != c.base_currency_code ||
                other.quote_currency_code != c.quote_currency_code)
                continue;
            if (other.state == "approved") {
                write_state(other, "superseded", "superseded by " + req.calibration_id);
                resp.superseded_id = boost::uuids::to_string(other.id);
            } else if (other.state == "proposed" && other.recorded_at < c.recorded_at) {
                // An older fit of the pair can no longer be the one approved: approving it would
                // replace a fit of later fixings. It is closed, so it does not wait for review.
                write_state(other,
                            "superseded",
                            "superseded unreviewed by " + req.calibration_id +
                                ", a later fit of the pair that was approved");
                ++closed;
            }
        }
        if (!repoint_only)
            write_state(c, "approved", req.reason);
        resp.state = "approved";

        const ores::utility::domain::change_intent repoint{
            .reason_code = "system.review",
            .commentary = std::format("Runs the approved variance calibration {}{}",
                                      req.calibration_id,
                                      resp.superseded_id.empty() ?
                                          std::string() :
                                          ", which superseded " + resp.superseded_id)};
        for (const auto& cfg : repo::fx_spot_generation_config_repository{}.read_latest(req_ctx)) {
            if (cfg.variance_model != "gjr_garch" ||
                cfg.base_currency_code != c.base_currency_code ||
                cfg.quote_currency_code != c.quote_currency_code ||
                cfg.variance_calibration_id == c.id)
                continue;
            auto write = write_of(cfg);
            write.variance_calibration_id = c.id;
            const auto written =
                svc::fx_spot_generation_config_service(req_ctx).put_fx_spot_generation_config(
                    {.change = {.write = std::move(write), .precondition = claimed(cfg.version)},
                     .intent = repoint});
            if (written.result.outcome != outcome::ok)
                throw std::runtime_error(
                    "the feed " + cfg.source_name +
                    " was not pointed at the approved calibration: " + written.result.message);
            resp.repointed_feeds.push_back(cfg.source_name);
        }
        resp.success = true;
        resp.message = std::format("Approved; {} feed(s) now run it.", resp.repointed_feeds.size());
        if (closed > 0)
            resp.message +=
                std::format(" {} older proposed fit(s) of the pair were superseded.", closed);
        BOOST_LOG_SEV(feed_config_handler_lg(), info)
            << "Variance calibration " << req.calibration_id << " approved; "
            << resp.repointed_feeds.size() << " feed(s) re-pointed";
        return resp;
    }

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
                       .variance_window_days = policy.variance_window_days,
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
            one.current_variance = current_variance_of(req_ctx, in.config);
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
                for (const auto& tried : resolved.candidates)
                    one.candidates.push_back({.item = tried.item,
                                              .provider_code = tried.provider_code,
                                              .inverted = tried.inverted,
                                              .rank = tried.rank,
                                              .state = std::string(feed::to_string(tried.state)),
                                              .reason = tried.reason});
                // The variance is fitted first so the level's commentary can name its outcome.
                // Both read the mixture as the resolution read it, so the order moves no number.
                calibrate_variance(req_ctx, in, resolved, policy, req.apply, one);
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
        if (one.variance)
            words += " " + variance_words(*one.variance);
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
                auto write = write_of(cfg);
                write.gmm_initial_price = *change.level;
                put_fx_spot_generation_config_request request{
                    .change = {.write = std::move(write), .precondition = claimed(cfg.version)},
                    .intent = intent};
                const auto written =
                    ores::synthetic::service::fx_spot_generation_config_service(req_ctx)
                        .put_fx_spot_generation_config(request);
                if (written.result.outcome != outcome::ok) {
                    one.outcome = "failed";
                    one.outcome_message = "The level was not written: " + written.result.message;
                    if (moved > 0)
                        one.outcome_message += ". The scaled deviations are stored; calibrating "
                                               "again writes the level alone.";
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

    // The daily variance of a GJR-GARCH feed is calibrated from the item the feed resolved to:
    // its window of fixings becomes an input, and the input a fitted or rejected calibration.
    // A preview reports both and stores nothing. An applied run stores each record once, under
    // an id named by what identifies it, so a repeat converges on the same records and never
    // reopens a calibration that a person has decided.
    void calibrate_variance(const ores::database::context& req_ctx,
                            const ores::synthetic::feed::fx_spot_feed_build_input& in,
                            const ores::synthetic::feed::resolved_start& resolved,
                            const ores::synthetic::feed::calibration_policy& policy,
                            bool apply,
                            ores::synthetic::messaging::calibrated_feed& one) {
        using namespace ores::synthetic::messaging;
        namespace feed = ores::synthetic::feed;
        const auto& src = resolved.source;
        if (in.config.variance_model != "gjr_garch" || resolved.refused || !src.from_fixing ||
            !src.fixing_date)
            return;

        variance_calibration_summary v{.item = src.item,
                                       .provider_code = src.provider_code,
                                       .inverted = src.inverted,
                                       .window_days = policy.variance_window_days,
                                       .window_end = std::format("{:%F}", *src.fixing_date)};
        auto delegated = auth_nats_.with_delegation({});
        const auto window =
            ores::marketdata::client::market_data_client(delegated).get_fixing_window(
                {.item = src.item,
                 .provider_code = src.provider_code,
                 .party_id = boost::uuids::to_string(in.config.party_id),
                 .to = *src.fixing_date,
                 .days = policy.variance_window_days});
        if (!window) {
            v.state = "rejected";
            v.state_reason = "the fixings could not be read: " + window.error();
            one.variance = std::move(v);
            return;
        }
        if (window->dates.size() != window->values.size()) {
            v.state = "rejected";
            v.state_reason = std::format("the fixing window holds {} dates and {} values",
                                         window->dates.size(),
                                         window->values.size());
            one.variance = std::move(v);
            return;
        }
        const auto points =
            feed::capture_variance_points(window->dates, window->values, src.inverted);
        std::vector<feed::mixture_component> components;
        for (const auto& c : in.components)
            components.push_back({.mean = c.mean, .stdev = c.stdev, .weight = c.weight});
        const double p_minus = feed::daily_p_minus(components, in.config.ticks_per_hour);
        const auto fit = feed::fit_variance(points, p_minus);

        v.point_count = static_cast<int>(points.size());
        v.used_count = static_cast<int>(std::ranges::count_if(points, &feed::variance_point::used));
        v.state = fit.fitted ? "proposed" : "rejected";
        v.state_reason = fit.reason;
        v.alpha = fit.alpha;
        v.gamma = fit.gamma;
        v.beta = fit.beta;
        v.persistence = fit.persistence;
        v.starting_variance_ratio = fit.starting_variance_ratio;
        v.starts = fit.starts;
        v.starts_agreed = fit.starts_agreed;
        if (apply)
            store_variance_calibration(
                req_ctx, in, policy, *src.fixing_date, points, fit, p_minus, v);
        else
            report_stored_variance_calibration(req_ctx, in, policy, *src.fixing_date, p_minus, v);
        one.variance = std::move(v);
    }

    /// What a calibration run made of the feed's daily variance, as the feed's commentary says it.
    static std::string
    variance_words(const ores::synthetic::messaging::variance_calibration_summary& v) {
        const auto id = v.calibration_id.substr(0, 8);
        if (v.state == "proposed")
            return std::format("GJR-GARCH daily variance fitted from {} of {} return(s), "
                               "persistence {:.4f}: calibration {} waits for review.",
                               v.used_count,
                               v.point_count,
                               v.persistence.value_or(0.0),
                               id);
        if (v.state == "rejected")
            return "GJR-GARCH daily variance rejected: " + v.state_reason;
        if (v.state == "failed")
            return v.state_reason;
        return std::format("GJR-GARCH calibration {} is {}.", id, v.state);
    }

    /// The ids a feed's variance records are stored under, named by what identifies each.
    struct variance_ids final {
        boost::uuids::uuid input;
        boost::uuids::uuid calibration;
    };

    // The fit depends on the feed's mixture through p_minus as well as on the input, so two
    // feeds that read one item under different mixtures fit two calibrations.
    // The text of each name is part of the stored id, the format of p_minus included: changing it
    // names new ids, so a preview no longer finds the records an earlier run stored.
    static variance_ids
    name_variance_ids(const ores::synthetic::domain::fx_spot_generation_config& cfg,
                      const std::string& item,
                      int window_days,
                      std::chrono::year_month_day window_end,
                      double p_minus) {
        const boost::uuids::name_generator_sha1 name(variance_calibration_namespace());
        const auto input = name(std::format("input|{}|{}|{:%F}|{}",
                                            boost::uuids::to_string(cfg.party_id),
                                            item,
                                            window_end,
                                            window_days));
        const auto calibration =
            name(std::format("calibration|{}|gjr_garch_qmle|{}|{:.12f}",
                             boost::uuids::to_string(input),
                             ores::synthetic::feed::variance_fit_method_version,
                             p_minus));
        return {.input = input, .calibration = calibration};
    }

    // The calibration a feed runs is the one its config names, read as stored with the input it
    // was fitted from, so a reader sees what the feed is on beside any new fit of the run.
    static std::optional<ores::synthetic::messaging::variance_calibration_summary>
    current_variance_of(const ores::database::context& req_ctx,
                        const ores::synthetic::domain::fx_spot_generation_config& cfg) {
        namespace repo = ores::synthetic::repository;
        if (cfg.variance_model != "gjr_garch" || !cfg.variance_calibration_id)
            return std::nullopt;
        const auto id = boost::uuids::to_string(*cfg.variance_calibration_id);
        const auto found = repo::variance_calibration_repository{}.read_latest(req_ctx, id);
        if (found.empty())
            return std::nullopt;
        const auto& c = found.front();
        ores::synthetic::messaging::variance_calibration_summary v;
        report_stored(c, v);
        v.calibration_id = id;
        if (c.input_id) {
            v.input_id = boost::uuids::to_string(*c.input_id);
            const auto inputs =
                repo::variance_calibration_input_repository{}.read_latest(req_ctx, v.input_id);
            if (!inputs.empty()) {
                const auto& in = inputs.front();
                v.item = in.fixing_uri;
                v.provider_code = in.provider_code;
                v.inverted = in.inverted;
                v.window_days = in.window_days;
                v.window_end = std::format("{:%F}", in.window_end);
                v.point_count = in.point_count;
                v.used_count = in.used_count;
            }
        }
        return v;
    }

    // A preview stores nothing, but a calibration an earlier applied run stored is the one a
    // reviewer decides, so the preview reports it as stored, with its ids.
    void report_stored_variance_calibration(
        const ores::database::context& req_ctx,
        const ores::synthetic::feed::fx_spot_feed_build_input& in,
        const ores::synthetic::feed::calibration_policy& policy,
        std::chrono::year_month_day window_end,
        double p_minus,
        ores::synthetic::messaging::variance_calibration_summary& v) {
        const auto ids =
            name_variance_ids(in.config, v.item, policy.variance_window_days, window_end, p_minus);
        const auto existing =
            ores::synthetic::repository::variance_calibration_repository{}.read_latest(
                req_ctx, boost::uuids::to_string(ids.calibration));
        if (existing.empty())
            return;
        report_stored(existing.front(), v);
        v.input_id = boost::uuids::to_string(ids.input);
        v.calibration_id = boost::uuids::to_string(ids.calibration);
    }

    static void report_stored(const ores::synthetic::domain::variance_calibration& stored,
                              ores::synthetic::messaging::variance_calibration_summary& v) {
        v.method = stored.method;
        v.state = stored.state;
        v.state_reason = stored.state_reason;
        v.alpha = stored.alpha;
        v.gamma = stored.gamma;
        v.beta = stored.beta;
        v.persistence = stored.persistence;
        v.starting_variance_ratio = stored.starting_variance_ratio;
        v.starts = stored.starts;
        v.starts_agreed = stored.starts_agreed;
    }

    // The namespace the ids of variance calibration records are named under.
    static const boost::uuids::uuid& variance_calibration_namespace() {
        static const auto ns =
            boost::uuids::string_generator{}("5d4c7f0e-2a31-4f6b-9c1e-8b7a6f3d2e10");
        return ns;
    }

    void
    store_variance_calibration(const ores::database::context& req_ctx,
                               const ores::synthetic::feed::fx_spot_feed_build_input& in,
                               const ores::synthetic::feed::calibration_policy& policy,
                               std::chrono::year_month_day window_end,
                               const std::vector<ores::synthetic::feed::variance_point>& points,
                               const ores::synthetic::feed::variance_fit& fit,
                               double p_minus,
                               ores::synthetic::messaging::variance_calibration_summary& v) {
        using namespace ores::synthetic::messaging;
        using ores::utility::domain::outcome;
        using ores::utility::domain::precondition;
        using ores::utility::domain::precondition_kind;
        namespace repo = ores::synthetic::repository;
        namespace svc = ores::synthetic::service;
        namespace feed = ores::synthetic::feed;
        const boost::uuids::name_generator_sha1 name(variance_calibration_namespace());
        const auto& cfg = in.config;
        const auto ids =
            name_variance_ids(cfg, v.item, policy.variance_window_days, window_end, p_minus);
        const auto input_id = ids.input;
        const auto calibration_id = ids.calibration;
        const auto point_id = [&](std::chrono::year_month_day date) {
            return name(std::format("point|{}|{:%F}", boost::uuids::to_string(input_id), date));
        };
        const precondition absent{.kind = precondition_kind::must_not_exist};
        const ores::utility::domain::change_intent intent{
            .reason_code = "system.calibration",
            .commentary = std::format(
                "Captured {} {} fixing(s) of {} over {} day(s) to {:%F}, {} usable as returns, "
                "for the GJR-GARCH fit of {}.",
                v.point_count,
                v.provider_code,
                v.item,
                policy.variance_window_days,
                window_end,
                v.used_count,
                cfg.source_name)};
        const ores::utility::domain::change_intent calibration_intent{
            .reason_code = "system.calibration",
            .commentary =
                fit.fitted ?
                    std::format(
                        "GJR-GARCH fit of {} from {} return(s): alpha {:.4f}, gamma {:.4f}, "
                        "beta {:.4f}, persistence {:.4f}. Proposed; it waits for review.",
                        cfg.source_name,
                        fit.returns_used,
                        fit.alpha.value_or(0.0),
                        fit.gamma.value_or(0.0),
                        fit.beta.value_or(0.0),
                        fit.persistence.value_or(0.0)) :
                    std::format("GJR-GARCH fit of {} rejected: {}", cfg.source_name, fit.reason)};
        try {
            const bool input_stored = !repo::variance_calibration_input_repository{}
                                           .read_latest(req_ctx, boost::uuids::to_string(input_id))
                                           .empty();
            if (!input_stored) {
                const auto written =
                    svc::variance_calibration_input_service(req_ctx).put_variance_calibration_input(
                        {.change = {.write = {.id = input_id,
                                              .fixing_uri = v.item,
                                              .window_end = window_end,
                                              .window_days = policy.variance_window_days,
                                              .base_currency_code = cfg.base_currency_code,
                                              .quote_currency_code = cfg.quote_currency_code,
                                              .provider_code = v.provider_code,
                                              .inverted = v.inverted,
                                              .policy_id =
                                                  policy.id.empty() ?
                                                      boost::uuids::uuid{} :
                                                      boost::uuids::string_generator{}(policy.id),
                                              .policy_version = policy.version,
                                              .point_count = v.point_count,
                                              .used_count = v.used_count},
                                    .precondition = absent},
                         .intent = intent});
                if (written.result.outcome != outcome::ok)
                    throw std::runtime_error("the input was not stored: " + written.result.message);
            }
            // The points name their input, so they are written after it, in one call. A run that
            // stopped between the two left an input without its points; the last point's id
            // tells, and the points are written again.
            const bool points_stored =
                points.empty() ||
                (input_stored &&
                 !repo::variance_calibration_input_point_repository{}
                      .read_latest(req_ctx, boost::uuids::to_string(point_id(points.back().date)))
                      .empty());
            if (!points_stored) {
                put_many_variance_calibration_input_points_request request{.intent = intent};
                for (const auto& p : points)
                    request.changes.push_back({.write = {.id = point_id(p.date),
                                                         .input_id = input_id,
                                                         .fixing_date = p.date,
                                                         .fixing_text = p.text,
                                                         .fixing_value = p.value,
                                                         .log_return = p.log_return,
                                                         .used = p.used,
                                                         .exclusion_reason = p.exclusion_reason},
                                               .precondition = {.kind = precondition_kind::any}});
                const auto stored = svc::variance_calibration_input_point_service(req_ctx)
                                        .put_many_variance_calibration_input_points(request);
                if (stored.result.outcome != outcome::ok)
                    throw std::runtime_error("the points were not stored: " +
                                             stored.result.message);
            }
            v.input_id = boost::uuids::to_string(input_id);

            const auto existing = repo::variance_calibration_repository{}.read_latest(
                req_ctx, boost::uuids::to_string(calibration_id));
            if (!existing.empty()) {
                // A stored calibration is reported as stored, so the summary never mixes a
                // decision with the numbers of a fit made since.
                report_stored(existing.front(), v);
            } else {
                const auto written =
                    svc::variance_calibration_service(req_ctx).put_variance_calibration(
                        {.change = {.write = {.id = calibration_id,
                                              .base_currency_code = cfg.base_currency_code,
                                              .quote_currency_code = cfg.quote_currency_code,
                                              .input_id = input_id,
                                              .method = "gjr_garch_qmle",
                                              .method_version = feed::variance_fit_method_version,
                                              .alpha = fit.alpha,
                                              .gamma = fit.gamma,
                                              .beta = fit.beta,
                                              .persistence = fit.persistence,
                                              .long_run_variance = fit.long_run_variance,
                                              .starting_variance_ratio =
                                                  fit.starting_variance_ratio,
                                              .log_likelihood = fit.log_likelihood,
                                              .returns_used = fit.returns_used,
                                              .starts = fit.starts,
                                              .starts_agreed = fit.starts_agreed,
                                              .state = v.state,
                                              .state_reason = v.state_reason},
                                    .precondition = absent},
                         .intent = calibration_intent});
                if (written.result.outcome != outcome::ok)
                    throw std::runtime_error("the calibration was not stored: " +
                                             written.result.message);
            }
            v.calibration_id = boost::uuids::to_string(calibration_id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Variance calibration of " << cfg.source_name << " was not stored: " << e.what();
            v.state = "failed";
            v.input_id.clear();
            v.calibration_id.clear();
            v.state_reason =
                std::string("The variance calibration was not stored: ") + client_message(e);
        }
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
