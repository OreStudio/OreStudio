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
#include "ores.marketdata.service/app/fx_vol_surface_publish_service.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_calibration_repository.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_publish_repository.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_calibration_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_blueprint_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
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

/// Publishes the record and ends every live surface of its series, in one act, then tells. Refuses
/// a surface older than the one live, which would put older data in its place.
domain::fx_vol_surface_publish record_publication(const context& ctx,
                                                  domain::fx_vol_surface_publish record) {
    const auto publisher = ores::service::messaging::delegated_actor(ctx);
    record.published_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    record.published_by = publisher;
    record.modified_by = publisher;
    record.performed_by = ctx.service_account();
    record.change_reason_code = "system.surface_published";
    record.change_commentary = "Published under approval request " +
                               boost::uuids::to_string(record.request_id) + " by " + publisher;

    // A series promises one live surface: the successor and the end of what it replaces are one
    // transaction, so neither none nor two is ever live.
    std::optional<time_point> replaced;
    ores::database::repository::unit_of_work uow(ctx);
    const auto& tx = uow.ctx();
    repository::fx_vol_surface_publish_repository repo;
    const auto live = repo.read_live_for_series(tx, record.output_series_id);
    for (const auto& previous : live)
        if (previous.id != record.id && previous.as_of > record.as_of)
            throw std::invalid_argument(
                "The surface as of " + datetime::to_iso8601_utc(previous.as_of) +
                " is live, and this one is older, as of " + datetime::to_iso8601_utc(record.as_of) +
                ". Publish a calibration at or after the live one.");
    for (auto previous : live) {
        if (previous.id == record.id)
            continue;
        if (!replaced || previous.as_of > *replaced)
            replaced = previous.as_of;
        previous.superseded_at = record.published_at;
        previous.superseded_by_request_id = record.request_id;
        previous.superseded_as_of = record.as_of;
        previous.modified_by = publisher;
        previous.performed_by = ctx.service_account();
        previous.change_reason_code = "system.surface_superseded";
        previous.change_commentary = "Superseded by the surface published under request " +
                                     boost::uuids::to_string(record.request_id) + " as of " +
                                     datetime::to_iso8601_utc(record.as_of);
        repo.write(tx,
                   previous,
                   {ores::utility::domain::precondition_kind::must_match_version,
                    static_cast<std::uint32_t>(previous.version)});
    }
    repo.write(tx,
               record,
               {ores::utility::domain::precondition_kind::must_match_version,
                static_cast<std::uint32_t>(record.version)});
    uow.commit();
    record.version += 1;
    BOOST_LOG_SEV(publish_log(), ores::logging::info)
        << "Published the surface of series " << record.output_series_id << " as of "
        << datetime::to_iso8601_utc(record.as_of) << " under request " << record.request_id;

    // Telling never unmoves the surface: a notice that fails is logged by the announcer.
    announce_surface_publish(ctx, record, replaced);
    return record;
}

/// Publishes the calibration an approved request authorises, or says why it may not.
void apply_surface_publish(const context& ctx,
                           const std::string& request_id,
                           const boost::uuids::uuid& party,
                           const boost::uuids::uuid& blueprint_id,
                           time_point as_of,
                           msg::publish_fx_vol_surface_response& resp) {
    resp.approval_request_id = request_id;
    const auto request = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
    if (!request) {
        resp.message = "No approval request has the id " + request_id + ".";
        return;
    }
    resp.state_code = request->state_code;
    if (request->kind_code != fx_vol_surface_publish_kind_code) {
        resp.message = "The request is a " + request->kind_code +
                       " request, and a surface is published by a " +
                       std::string(fx_vol_surface_publish_kind_code) + " request.";
        return;
    }
    const auto record =
        repository::fx_vol_surface_publish_repository{}.read_latest_by_request(ctx, request_id);
    if (!record) {
        resp.message = "The request names no surface, so it authorises no publish.";
        return;
    }
    if (record->party_id != party || record->blueprint_id != blueprint_id ||
        record->as_of != as_of) {
        resp.message =
            "The request authorises blueprint " + boost::uuids::to_string(record->blueprint_id) +
            " as of " + datetime::to_iso8601_utc(record->as_of) + " for party " +
            boost::uuids::to_string(record->party_id) + ", and this publish names another.";
        return;
    }
    if (request->state_code != "approved") {
        resp.message =
            "The request is " + request->state_code + ", and a publish waits until it is approved.";
        return;
    }
    if (record->published_at) {
        resp.message = "The request already published its surface at " +
                       datetime::to_iso8601_utc(*record->published_at) + ", by " +
                       record->published_by + ". Raise a request for the next publish.";
        return;
    }
    const auto calibration = calibration_of(ctx, record->blueprint_id, record->as_of);
    if (!calibration || calibration->version != record->calibration_version) {
        resp.message =
            "The calibration the request authorises was run again after the raise, so the "
            "decision does not cover the surface it now holds. Raise a request for it.";
        return;
    }
    const auto published = record_publication(ctx, *record);
    resp.success = true;
    resp.message = "Published.";
    resp.published_at = published.published_at;
    resp.published_by = published.published_by;
}

}

msg::publish_fx_vol_surface_response
fx_vol_surface_publish_service::publish(context ctx,
                                        const msg::publish_fx_vol_surface_request& req) {
    msg::publish_fx_vol_surface_response resp;
    try {
        const auto party = ores::service::messaging::session_party(ctx, "A surface");
        const auto as_of = std::chrono::floor<std::chrono::microseconds>(req.as_of);
        if (as_of == time_point{})
            throw std::invalid_argument("Name the as-of of the calibration to publish.");
        const auto blueprint = blueprint_of(ctx, req.blueprint_id);
        if (blueprint.party_id != party)
            throw std::invalid_argument("The surface " + blueprint.name +
                                        " belongs to another party, and only a person acting for "
                                        "that party publishes it.");

        if (!req.approval_request_id.empty()) {
            apply_surface_publish(ctx, req.approval_request_id, party, blueprint.id, as_of, resp);
            return resp;
        }
        if (req.reason.empty())
            throw std::invalid_argument(
                "Say why the surface should be published, so the decider reads a reason.");
        const auto calibration = calibration_of(ctx, blueprint.id, as_of);
        if (!calibration)
            throw std::invalid_argument("The blueprint " + blueprint.name +
                                        " has no calibration as of " +
                                        datetime::to_iso8601_utc(as_of) + ". Calibrate it first.");

        // One raise at a time: a second request for the same calibration would publish it twice.
        for (const auto& pending :
             repository::fx_vol_surface_publish_repository{}.read_latest(ctx)) {
            if (pending.blueprint_id != blueprint.id || pending.as_of != as_of ||
                pending.published_at)
                continue;
            const auto open = ores::inbox::service::approval_lifecycle(ctx).request(
                boost::uuids::to_string(pending.request_id));
            if (open && (open->state_code == "waiting" || open->state_code == "approved"))
                throw std::invalid_argument(
                    "Request " + boost::uuids::to_string(pending.request_id) +
                    " already asks to publish this calibration and is " + open->state_code + ".");
        }

        // The request and its subject are one act, so no request ever names no surface.
        ores::database::repository::unit_of_work uow(ctx);
        const auto& tx = uow.ctx();
        ores::inbox::service::approval_lifecycle lifecycle(tx);
        const auto kind = lifecycle.kind(std::string(fx_vol_surface_publish_kind_code));
        if (!kind)
            throw std::runtime_error(
                "No such kind of request: " + std::string(fx_vol_surface_publish_kind_code) + ".");
        const auto me = lifecycle.actor_account_id();
        if (!me)
            throw std::runtime_error("The signed-in account was not found.");
        const auto raised = lifecycle.raise(*kind, req.reason, *me);

        domain::fx_vol_surface_publish record;
        record.tenant_id = ctx.tenant_id();
        record.id = boost::uuids::random_generator{}();
        record.request_id = raised.id;
        record.party_id = party;
        record.blueprint_id = blueprint.id;
        record.as_of = as_of;
        record.output_series_id = blueprint.output_series_id;
        record.calibration_id = calibration->id;
        record.calibration_version = calibration->version;
        record.converged = calibration->converged;
        record.max_abs_error = calibration->max_abs_error;
        record.pillars_beyond_tolerance = calibration->pillars_beyond_tolerance;
        record.violation_count = violation_count_of(*calibration);
        record.modified_by = ores::service::messaging::delegated_actor(ctx);
        record.performed_by = ctx.service_account();
        record.change_reason_code = "system.new_record";
        record.change_commentary =
            "Subject of approval request " + boost::uuids::to_string(raised.id);
        repository::fx_vol_surface_publish_repository{}.write(tx, record);
        uow.commit();

        resp.approval_request_id = boost::uuids::to_string(raised.id);
        resp.state_code = raised.state_code;
        if (raised.state_code != "approved") {
            resp.success = true;
            resp.message = "Raised. The publish waits on the decision.";
            return resp;
        }
        apply_surface_publish(ctx, resp.approval_request_id, party, blueprint.id, as_of, resp);
    } catch (const std::exception& e) {
        resp.success = false;
        resp.message = e.what();
    }
    return resp;
}

msg::get_fx_vol_surface_publish_subject_response
fx_vol_surface_publish_service::read(context ctx,
                                     const msg::get_fx_vol_surface_publish_subject_request& req) {
    msg::get_fx_vol_surface_publish_subject_response out;
    out.approval_request_id = req.approval_request_id;
    try {
        const auto record = repository::fx_vol_surface_publish_repository{}.read_latest_by_request(
            ctx, req.approval_request_id);
        if (!record) {
            out.message = "No surface publish request has the id " + req.approval_request_id + ".";
            return out;
        }
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
                ctx, boost::uuids::to_string(record->blueprint_id));
        out.blueprint_name = blueprints.empty() ? boost::uuids::to_string(record->blueprint_id) :
                                                  blueprints.front().name;
        out.party_id = boost::uuids::to_string(record->party_id);
        out.as_of = record->as_of;
        out.calibration_id = boost::uuids::to_string(record->calibration_id);
        out.calibration_version = record->calibration_version;
        out.converged = record->converged;
        out.max_abs_error = record->max_abs_error;
        out.pillars_beyond_tolerance = record->pillars_beyond_tolerance;
        out.violation_count = record->violation_count;
        out.published_at = record->published_at;
        out.published_by = record->published_by;
        out.superseded_at = record->superseded_at;
        if (record->superseded_by_request_id)
            out.superseded_by_request_id =
                boost::uuids::to_string(*record->superseded_by_request_id);
    } catch (const std::exception& e) {
        out.success = false;
        out.message = e.what();
    }
    return out;
}

}
