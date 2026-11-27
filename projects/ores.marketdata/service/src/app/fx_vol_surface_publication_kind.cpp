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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.service/app/fx_vol_surface_publication_kind.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_calibration_repository.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.service/app/catalogue_page.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_calibration_plan.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_calibration_service.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_grid.hpp"
#include "ores.marketdata.service/app/publication_kind.hpp"
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.marketdata.service/app/staleness_policy.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_blueprint_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <optional>
#include <set>
#include <stdexcept>

namespace ores::marketdata::service::app {

namespace {

namespace msg = ores::marketdata::messaging;
using ores::database::context;
using ores::platform::time::datetime;
using time_point = std::chrono::system_clock::time_point;

auto& publish_log() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.service.app.fx_vol_surface_publish_service");
    return instance;
}

ores::refdata::domain::fx_vol_surface_blueprint blueprint_of(const context& ctx,
                                                             const std::string& blueprint_id) {
    const auto rows = ores::refdata::repository::fx_vol_surface_blueprint_repository{}.read_latest(
        ctx, blueprint_id);
    if (rows.empty())
        throw std::invalid_argument("No FX volatility surface blueprint " + blueprint_id +
                                    " is visible to your party.");
    return rows.front();
}

std::optional<domain::fx_vol_surface_calibration>
calibration_of(const context& ctx, const boost::uuids::uuid& blueprint_id, time_point as_of) {
    const auto rows = repository::fx_vol_surface_calibration_repository{}.read_latest(
        ctx, boost::uuids::to_string(calibration_id_of(blueprint_id, as_of)));
    if (rows.empty())
        return std::nullopt;
    return rows.front();
}

int violation_count_of(const domain::fx_vol_surface_calibration& calibration) {
    const auto violations =
        ores::utility::codec::json::read_expected<std::vector<msg::fx_vol_violation>>(
            calibration.violations);
    return violations ? static_cast<int>(violations->size()) : 0;
}

}

std::optional<std::string>
blocking_failure_of(const domain::fx_vol_surface_calibration& calibration) {
    const auto violations =
        ores::utility::codec::json::read_expected<std::vector<msg::fx_vol_violation>>(
            calibration.violations);
    if (!violations)
        return "Its arbitrage checks cannot be read, so it cannot be published under any rule.";
    if (violations->empty())
        return std::nullopt;
    std::string out = "It breaks " + std::to_string(violations->size()) +
                      " arbitrage condition(s), so it cannot be published under any rule:";
    for (const auto& v : *violations)
        out += " " + v.kind + " at " + v.expiry + ";";
    return out;
}

namespace {

/// Refuses a subject a request of this kind already asks to publish.
///
/// One raise at a time: a second request for the same calibration would publish
/// it twice, so the second is not raised.
std::optional<std::string>
open_raise_refusal(const context& ctx, const boost::uuids::uuid& blueprint_id, time_point as_of) {
    msg::fx_vol_surface_publishes_filter raised_here;
    raised_here.blueprint_id = blueprint_id;
    const auto raises = read_all_pages([&](std::uint32_t offset, std::uint32_t limit) {
        return repository::fx_vol_surface_publish_repository{}.read_latest(
            ctx, offset, limit, {}, raised_here);
    });
    for (const auto& pending : raises) {
        if (pending.as_of != as_of || pending.published_at)
            continue;
        const auto open = ores::inbox::service::approval_lifecycle(ctx).request(
            boost::uuids::to_string(pending.request_id));
        if (open && (open->state_code == "waiting" || open->state_code == "approved"))
            return "Request " + boost::uuids::to_string(pending.request_id) +
                   " already asks to publish this calibration and is " + open->state_code + ".";
    }
    return std::nullopt;
}

}

msg::get_fx_vol_surface_publish_subject_response
fx_vol_surface_published_reader::read(const context& ctx,
                                      const msg::get_fx_vol_surface_publish_subject_request& req) {
    msg::get_fx_vol_surface_publish_subject_response out;
    out.approval_request_id = req.approval_request_id;
    try {
        // A request names a set. This read answers about one surface, so it
        // answers only when the request names one: a set is read through the
        // publish list filtered by the request, which is what a screen showing a
        // set uses.
        msg::fx_vol_surface_publishes_filter by_request;
        by_request.request_id = boost::uuids::string_generator{}(req.approval_request_id);
        const auto records = read_all_pages([&](std::uint32_t offset, std::uint32_t limit) {
            return repository::fx_vol_surface_publish_repository{}.read_latest(
                ctx, offset, limit, {}, by_request);
        });
        if (records.empty()) {
            out.message = "No surface publish request has the id " + req.approval_request_id + ".";
            return out;
        }
        if (records.size() > 1) {
            out.message = "The request " + req.approval_request_id + " authorises " +
                          std::to_string(records.size()) +
                          " surfaces, so its subject cannot be read one at a time. Read them "
                          "through the publish list, which names the request.";
            return out;
        }
        const auto& record = records.front();
        const auto request =
            ores::inbox::service::approval_lifecycle(ctx).request(req.approval_request_id);
        if (!request) {
            out.message = "The approval request " + req.approval_request_id +
                          " is gone, so its subject authorises nothing.";
            return out;
        }
        out.success = true;
        out.state_code = request->state_code;
        const auto blueprints =
            ores::refdata::repository::fx_vol_surface_blueprint_repository{}.read_latest(
                ctx, boost::uuids::to_string(record.blueprint_id));
        out.blueprint_name = blueprints.empty() ? boost::uuids::to_string(record.blueprint_id) :
                                                  blueprints.front().name;
        out.party_id = boost::uuids::to_string(record.party_id);
        out.as_of = record.as_of;
        out.calibration_id = boost::uuids::to_string(record.calibration_id);
        out.calibration_version = record.calibration_version;
        out.converged = record.converged;
        out.max_abs_error = record.max_abs_error;
        out.pillars_beyond_tolerance = record.pillars_beyond_tolerance;
        out.violation_count = record.violation_count;
        out.published_at = record.published_at;
        out.published_by = record.published_by;
        out.superseded_at = record.superseded_at;
        if (record.superseded_by_request_id)
            out.superseded_by_request_id =
                boost::uuids::to_string(*record.superseded_by_request_id);
    } catch (const std::exception& e) {
        out.success = false;
        out.message = e.what();
    }
    return out;
}

msg::get_published_fx_vol_surface_response fx_vol_surface_published_reader::read_published(
    const context& ctx, const msg::get_published_fx_vol_surface_request& req) {
    msg::get_published_fx_vol_surface_response out;
    try {
        const auto party = ores::service::messaging::session_party(ctx, "A surface");
        const auto blueprint = blueprint_of(ctx, req.blueprint_id);
        if (blueprint.party_id != party)
            throw std::invalid_argument("The surface " + blueprint.name +
                                        " belongs to another party.");
        out.blueprint_name = blueprint.name;
        const auto sets =
            repository::fx_vol_surface_publish_repository{}.read_published_for_blueprint(
                ctx, blueprint.id);
        const auto found =
            std::ranges::find_if(sets, [&](const auto& s) { return s.as_of <= req.as_of; });
        if (found == sets.end()) {
            out.message = "No surface of " + blueprint.name + " is published at or before " +
                          datetime::to_iso8601_utc(req.as_of) + ".";
            return out;
        }
        std::vector<surface_grid_point> points;
        for (const auto& obs : repository::market_observation_repository{}.read_as_of(
                 ctx, found->output_series_id, found->as_of)) {
            if (obs.observation_datetime != found->as_of)
                continue;
            const auto named = datum::oresmd_uri_codec::read(obs.oresmd_uri);
            const auto value = ores::platform::numeric::parse_double(obs.value);
            if (!named || !value) {
                ORES_LOG_SEV(publish_log(), ores::logging::warn)
                    << "Skipping a point of the published surface of " << blueprint.name
                    << " that does not read: " << obs.oresmd_uri;
                continue;
            }
            const auto* expiry = named->get<datum::field::expiry>();
            const auto* strike = named->get<datum::field::strike>();
            if (expiry && strike)
                points.push_back({expiry->text(), strike->text(), *value});
        }
        auto grid = surface_grid_of(std::move(points),
                                    split_blueprint_list(blueprint.output_strikes),
                                    req.expiry,
                                    req.strike);
        if (!grid)
            throw std::invalid_argument("The published surface of " + blueprint.name + ": " +
                                        grid.error() + ".");
        out.success = true;
        out.series_id = boost::uuids::to_string(found->output_series_id);
        out.publish_request_id = boost::uuids::to_string(found->request_id);
        out.as_of = found->as_of;
        out.published_at = found->published_at;
        out.published_by = found->published_by;
        out.superseded_at = found->superseded_at;
        // The model and the delta convention are what make a strike column a
        // number a person may price from, so they travel with the grid.
        out.model = blueprint.smile_model;
        if (!blueprint.fx_option_convention_id.empty()) {
            const auto conventions =
                ores::refdata::repository::fx_option_convention_repository{}.read_latest(
                    ctx, blueprint.fx_option_convention_id);
            if (!conventions.empty())
                out.delta_type = conventions.front().delta_type;
        }
        out.strike_convention = out.delta_type.empty() ? "absolute" : "delta";
        // The verdict is the server's, asked of the one policy under the FX threshold,
        // so every surface ages by the same rule and no client applies its own.
        out.stale = staleness_policy::is_stale(
            "fx", out.published_at.value_or(time_point{}), std::chrono::system_clock::now());
        out.expiries = std::move(grid->expiries);
        out.strikes = std::move(grid->strikes);
        for (auto& p : grid->points)
            out.points.push_back({.expiry = std::move(p.expiry),
                                  .strike = std::move(p.strike),
                                  .volatility = p.volatility});
    } catch (const std::exception& e) {
        out.success = false;
        out.message = e.what();
    }
    return out;
}

namespace {

/**
 * @brief The volatility surface, reached only through the interface the lifecycle calls.
 *
 * The subject is a blueprint, the candidate is its calibration at the as-of, and
 * the checks are the calibration's freedom from arbitrage. The acts are the
 * lifecycle's, in publication_lifecycle.cpp.
 */
class fx_vol_surface_publication_kind_impl final : public publication_kind {
public:
    derived_kind kind() const override {
        return derived_kind::fx_vol_surface;
    }

    publication_subject
    resolve_subject(const ores::database::context& ctx,
                    const boost::uuids::uuid& party_id,
                    const std::string& subject_key,
                    std::chrono::system_clock::time_point as_of) const override {
        if (as_of == time_point{})
            throw std::invalid_argument("Name the as-of of the calibration to publish.");
        const auto blueprint = blueprint_of(ctx, subject_key);
        if (!party_id.is_nil() && blueprint.party_id != party_id)
            throw std::invalid_argument("The surface " + blueprint.name +
                                        " belongs to another party, and only a person acting "
                                        "for that party publishes it.");
        return publication_subject{.kind = derived_kind::fx_vol_surface,
                                   .party_id = blueprint.party_id,
                                   .subject_id = blueprint.id,
                                   .subject_key = subject_key,
                                   .subject_name = blueprint.name,
                                   .output_series_id = blueprint.output_series_id,
                                   .as_of = as_of};
    }

    std::optional<std::string> refusal_of_raise(const ores::database::context& ctx,
                                                const publication_subject& subject) const override {
        const auto calibration = calibration_of(ctx, subject.subject_id, subject.as_of);
        if (!calibration)
            return "The blueprint " + subject.subject_name + " has no calibration as of " +
                   datetime::to_iso8601_utc(subject.as_of) + ". Calibrate it first.";
        // A rule closes a raise without a person, so a blocking failure is
        // refused here, before anything is raised, and never left to a decider
        // who may not exist.
        if (const auto failure = blocking_failure_of(*calibration))
            return "The calibration of " + subject.subject_name + " as of " +
                   datetime::to_iso8601_utc(subject.as_of) + " is not publishable. " + *failure;
        return open_raise_refusal(ctx, subject.subject_id, subject.as_of);
    }

    void write_publish(const ores::database::context& tx,
                       const boost::uuids::uuid& request_id,
                       const publication_subject& subject) const override {
        const auto calibration = calibration_of(tx, subject.subject_id, subject.as_of);
        if (!calibration)
            throw std::invalid_argument(
                "The blueprint " + subject.subject_name + " has no calibration as of " +
                datetime::to_iso8601_utc(subject.as_of) + ", so the request names nothing.");

        domain::fx_vol_surface_publish record;
        record.tenant_id = tx.tenant_id();
        record.id = boost::uuids::random_generator{}();
        record.request_id = request_id;
        record.party_id = subject.party_id;
        record.blueprint_id = subject.subject_id;
        record.as_of = subject.as_of;
        record.output_series_id = subject.output_series_id;
        record.calibration_id = calibration->id;
        record.calibration_version = calibration->version;
        record.converged = calibration->converged;
        record.max_abs_error = calibration->max_abs_error;
        record.pillars_beyond_tolerance = calibration->pillars_beyond_tolerance;
        record.violation_count = violation_count_of(*calibration);
        record.modified_by = ores::service::messaging::delegated_actor(tx);
        record.performed_by = tx.service_account();
        record.change_reason_code = "system.new_record";
        record.change_commentary =
            "Subject of approval request " + boost::uuids::to_string(request_id);
        repository::fx_vol_surface_publish_repository{}.write(tx, record);
    }

    publication_verdict verdict_on_publish(const ores::database::context& ctx,
                                           const std::string& request_id,
                                           const publication_subject& attempted) const override {
        publication_verdict out;
        const auto request = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
        if (!request) {
            out.message = "No approval request has the id " + request_id + ".";
            return out;
        }
        out.state_code = request->state_code;
        if (request->kind_code != fx_vol_surface_publish_kind_code) {
            out.message = "The request is a " + request->kind_code +
                          " request, and a surface is published by a " +
                          std::string(fx_vol_surface_publish_kind_code) + " request.";
            return out;
        }
        // The request names a set, and this is the member the publish asks
        // about: a request that does not name this blueprint authorises nothing
        // here, which is the same answer a request that names no surface at all
        // gets.
        const auto record =
            repository::fx_vol_surface_publish_repository{}.read_latest_by_request_and_blueprint(
                ctx, request_id, attempted.subject_id);
        if (!record) {
            out.message = "The request does not name the surface this publish is about, so it "
                          "authorises no publish of it.";
            return out;
        }
        if (record->party_id != attempted.party_id || record->as_of != attempted.as_of) {
            out.message = "The request authorises blueprint " +
                          boost::uuids::to_string(record->blueprint_id) + " as of " +
                          datetime::to_iso8601_utc(record->as_of) + " for party " +
                          boost::uuids::to_string(record->party_id) +
                          ", and this publish names another.";
            return out;
        }
        if (request->state_code != "approved") {
            out.message = "The request is " + request->state_code +
                          ", and a publish waits until it is approved.";
            return out;
        }
        if (record->published_at) {
            out.message = "The request already published its surface at " +
                          datetime::to_iso8601_utc(*record->published_at) + ", by " +
                          record->published_by + ". Raise a request for the next publish.";
            return out;
        }
        // The decision covers the version it was raised for: a calibration run
        // again after the raise leaves the request covering numbers nobody saw.
        const auto calibration = calibration_of(ctx, record->blueprint_id, record->as_of);
        if (!calibration || calibration->version != record->calibration_version) {
            out.message = "The calibration the request authorises was run again after the raise, "
                          "so the decision does not cover the surface it now holds. Raise a "
                          "request for it.";
            return out;
        }

        publication_facts facts;
        facts.subject = attempted;
        facts.publish_id = record->id;
        facts.request_id = record->request_id;
        facts.version = record->version;
        out.approved = true;
        out.facts = std::move(facts);
        return out;
    }

    std::optional<std::string> prepare(const ores::database::context&,
                                       const publication_subject&) const override {
        // The calibration is stored before the raise, so the apply computes
        // nothing and can never defer.
        return std::nullopt;
    }

    publication_facts publish(const ores::database::context& tx,
                              publication_facts set) const override {
        repository::fx_vol_surface_publish_repository repo;
        const auto stored = repo.read_latest(tx, boost::uuids::to_string(set.publish_id));
        if (stored.empty())
            throw std::runtime_error("The surface the request authorises is gone, so nothing is "
                                     "published under it.");
        auto record = stored.front();

        // A series promises one live surface: the successor and the end of what
        // it replaces are one transaction, so neither none nor two is ever live.
        const auto live = repo.read_live_for_series(tx, record.output_series_id);
        for (const auto& previous : live)
            if (previous.id != record.id && previous.as_of > record.as_of)
                throw std::invalid_argument("The surface as of " +
                                            datetime::to_iso8601_utc(previous.as_of) +
                                            " is live, and this one is older, as of " +
                                            datetime::to_iso8601_utc(record.as_of) +
                                            ". Publish a calibration at or after the live one.");

        std::optional<time_point> replaced;
        for (auto previous : live) {
            if (previous.id == record.id)
                continue;
            if (!replaced || previous.as_of > *replaced)
                replaced = previous.as_of;
            previous.superseded_at = set.published_at;
            previous.superseded_by_request_id = record.request_id;
            previous.superseded_as_of = record.as_of;
            previous.modified_by = set.published_by;
            previous.performed_by = tx.service_account();
            previous.change_reason_code = "system.surface_superseded";
            previous.change_commentary = "Superseded by the surface published under request " +
                                         boost::uuids::to_string(record.request_id) + " as of " +
                                         datetime::to_iso8601_utc(record.as_of);
            repo.write(tx,
                       previous,
                       {ores::utility::domain::precondition_kind::must_match_version,
                        static_cast<std::uint32_t>(previous.version)});
        }

        record.published_at = set.published_at;
        record.published_by = set.published_by;
        record.modified_by = set.published_by;
        record.performed_by = tx.service_account();
        record.change_reason_code = "system.surface_published";
        record.change_commentary = "Published under approval request " +
                                   boost::uuids::to_string(record.request_id) + " by " +
                                   set.published_by;
        repo.write(tx,
                   record,
                   {ores::utility::domain::precondition_kind::must_match_version,
                    static_cast<std::uint32_t>(record.version)});

        set.replaced_as_of = replaced;
        set.version = record.version + 1;
        ORES_LOG_SEV(publish_log(), ores::logging::info)
            << "Published the surface of series " << record.output_series_id << " as of "
            << datetime::to_iso8601_utc(record.as_of) << " under request " << record.request_id;
        return set;
    }

    void announce_published(const ores::database::context& ctx,
                            const publication_facts& set) const override {
        domain::fx_vol_surface_publish record;
        record.id = set.publish_id;
        record.request_id = set.request_id;
        record.party_id = set.subject.party_id;
        record.blueprint_id = set.subject.subject_id;
        record.output_series_id = set.subject.output_series_id;
        record.as_of = set.subject.as_of;
        record.published_at = set.published_at;
        record.published_by = set.published_by;
        announce_surface_publish(ctx, record, set.replaced_as_of);
    }
};

}

const publication_kind& fx_vol_surface_publication_kind() {
    static const fx_vol_surface_publication_kind_impl instance;
    return instance;
}

}
