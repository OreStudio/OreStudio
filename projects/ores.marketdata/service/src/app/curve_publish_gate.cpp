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
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>

namespace ores::marketdata::service::app {

namespace {

using ores::platform::time::datetime;

/**
 * @brief The subject as the person reads it: the curve, the as-of and the party.
 *
 * The curve is named by its output series' oresmd URI, because that is the only
 * name a bootstrap configuration has: it carries a currency, an index tenor and
 * a convention, and no code. A refusal and the record a decider reads therefore
 * name the subject the same way.
 */
struct subject_names {
    std::string curve;
    std::string party;
    std::string as_of;
};

subject_names names_of(const ores::database::context& ctx,
                       const boost::uuids::uuid& party_id,
                       const boost::uuids::uuid& series_id,
                       const std::chrono::system_clock::time_point& as_of) {
    subject_names out;
    out.as_of = datetime::to_iso8601_utc(as_of);

    if (series_id.is_nil()) {
        out.curve = "no curve";
    } else {
        repository::market_series_repository repo;
        const auto series = repo.read_latest(ctx, boost::uuids::to_string(series_id));
        out.curve = series.empty() ? "series " + boost::uuids::to_string(series_id) :
                                     series.front().oresmd_uri;
    }

    if (party_id.is_nil()) {
        out.party = "no party";
    } else {
        ores::refdata::repository::party_repository repo;
        const auto parties = repo.read_latest(ctx, boost::uuids::to_string(party_id));
        out.party = parties.empty() ? "party " + boost::uuids::to_string(party_id) :
                                      "party " + parties.front().short_code;
    }
    return out;
}

/// @brief The subject as one sentence, for a message a person reads.
std::string describe(const subject_names& n) {
    return n.curve + " as of " + n.as_of + " of " + n.party;
}

/**
 * @brief The instant at the precision the store keeps.
 *
 * The wire carries a time point at whatever precision the client's clock has
 * and the column keeps microseconds, so two statements of one instant are equal
 * only after both are read at the column's precision.
 */
std::chrono::system_clock::time_point
at_store_precision(const std::chrono::system_clock::time_point& t) {
    return std::chrono::floor<std::chrono::microseconds>(t);
}

/**
 * @brief Ends every live set of the series the successor is published into.
 *
 * A supersession is not a second act: a corrected set is a publish whose subject
 * is the successor, so the successor's own apply writes the link. Every live set
 * of the series is ended and not only the newest, because the promise a reader
 * is given is one live set -- a series that carries two is a set that never
 * ended, and the next publish is where that is put right.
 */
void end_previous_sets(const ores::database::context& ctx, const domain::curve_publish& successor) {
    if (successor.output_series_id.is_nil() || !successor.published_at)
        return;

    repository::curve_publish_repository repo;
    for (auto previous : repo.read_live_for_series(ctx, successor.output_series_id)) {
        if (previous.id == successor.id)
            continue;
        previous.superseded_at = successor.published_at;
        previous.superseded_by_request_id = successor.request_id;
        previous.superseded_as_of = successor.as_of;
        previous.modified_by = successor.published_by;
        previous.performed_by = ctx.service_account();
        previous.change_reason_code = "system.curve_superseded";
        previous.change_commentary = "Superseded by the set published under request " +
                                     boost::uuids::to_string(successor.request_id) + " as of " +
                                     datetime::to_iso8601_utc(successor.as_of);
        repo.write(ctx,
                   previous,
                   {ores::utility::domain::precondition_kind::must_match_version,
                    static_cast<std::uint32_t>(previous.version)});
    }
}

}

std::string raise_permission_of(const ores::database::context& ctx, std::string_view kind_code) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx);
    const auto kind = lifecycle.kind(std::string(kind_code));
    if (!kind)
        throw std::runtime_error("No such kind of request: " + std::string(kind_code) + ".");
    return kind->raise_permission_code.value_or(std::string{});
}

publish_subject resolve_publish_subject(const ores::database::context& ctx,
                                        const std::string& bootstrap_config_id,
                                        std::chrono::system_clock::time_point as_of) {
    if (bootstrap_config_id.empty())
        throw std::invalid_argument(
            "Name the curve configuration the publish is about, so the request authorises a "
            "subject and not whatever comes next.");
    if (at_store_precision(as_of) == std::chrono::system_clock::time_point{})
        throw std::invalid_argument(
            "Name the as-of instant the curve is bootstrapped against, so the request authorises "
            "one published set and not any of them.");

    ores::refdata::repository::ir_curve_bootstrap_config_repository config_repo;
    const auto configs = config_repo.read_latest(ctx, bootstrap_config_id);
    if (configs.empty())
        throw std::invalid_argument("No curve configuration has the id " + bootstrap_config_id +
                                    ", so there is no subject to publish.");
    const auto& config = configs.front();

    publish_subject subject;
    subject.party_id = config.party_id;
    subject.bootstrap_config_id = config.id;
    subject.output_series_id = config.output_series_id;
    subject.as_of = at_store_precision(as_of);
    return subject;
}

ores::inbox::domain::approval_request raise_publish_request(const ores::database::context& ctx,
                                                            const publish_subject& subject,
                                                            const std::string& reason) {
    if (reason.empty())
        throw std::invalid_argument(
            "Say why the curve should be published, so the decider reads a reason.");

    ores::inbox::service::approval_lifecycle lifecycle(ctx);
    const auto kind = lifecycle.kind(std::string(curve_publish_kind_code));
    if (!kind)
        throw std::runtime_error(
            "No such kind of request: " + std::string(curve_publish_kind_code) + ".");
    const auto me = lifecycle.actor_account_id();
    if (!me)
        throw std::runtime_error("The signed-in account was not found.");
    const auto raised = lifecycle.raise(*kind, reason, *me);

    domain::curve_publish record;
    record.id = boost::uuids::random_generator{}();
    record.tenant_id = ctx.tenant_id();
    record.request_id = raised.id;
    record.party_id = subject.party_id;
    record.bootstrap_config_id = subject.bootstrap_config_id;
    record.output_series_id = subject.output_series_id;
    record.as_of = subject.as_of;
    record.published_by = "";
    record.published_at = std::nullopt;
    record.modified_by = ctx.actor().empty() ? ctx.service_account() : ctx.actor();
    record.performed_by = ctx.service_account();
    record.change_reason_code = "system.new_record";
    record.change_commentary =
        "The subject that publish request " + boost::uuids::to_string(raised.id) + " authorises";
    repository::curve_publish_repository repo;
    repo.write(ctx, record);
    return raised;
}

std::optional<std::chrono::system_clock::time_point>
live_as_of(const ores::database::context& ctx, const boost::uuids::uuid& output_series_id) {
    if (output_series_id.is_nil())
        return std::nullopt;
    repository::curve_publish_repository repo;
    std::optional<std::chrono::system_clock::time_point> newest;
    for (const auto& live : repo.read_live_for_series(ctx, output_series_id)) {
        if (!live.published_at)
            continue;
        if (!newest || live.as_of > *newest)
            newest = live.as_of;
    }
    return newest;
}

publish_verdict verdict_on_publish(const ores::database::context& ctx,
                                   const std::string& request_id,
                                   const publish_subject& attempted) {
    if (request_id.empty())
        return {.approved = false,
                .state_code = {},
                .message = "A publish needs an approved request of the kind " +
                           std::string(curve_publish_kind_code) +
                           ". Raise one, have it decided, then publish with its id."};

    std::optional<ores::inbox::domain::approval_request> request;
    try {
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        request = lifecycle.request(request_id);
    } catch (const std::exception&) {
        // An id that is not a uuid is not a request. The person named
        // something the record cannot hold, which is a refusal and not a
        // failure of the service.
        request = std::nullopt;
    }

    if (!request)
        return {.approved = false,
                .state_code = {},
                .message = "No approval request has the id " + request_id + "."};

    if (request->kind_code != curve_publish_kind_code)
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request is a " + request->kind_code +
                           " request, and a publish is permitted by a " +
                           std::string(curve_publish_kind_code) + " request."};

    repository::curve_publish_repository repo;
    auto record = repo.read_latest_by_request(ctx, request_id);
    if (!record)
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request is a " + std::string(curve_publish_kind_code) +
                           " request that names no subject, so it authorises no publish."};

    if (record->bootstrap_config_id != attempted.bootstrap_config_id ||
        at_store_precision(record->as_of) != at_store_precision(attempted.as_of) ||
        record->party_id != attempted.party_id) {
        const auto authorised =
            describe(names_of(ctx, record->party_id, record->output_series_id, record->as_of));
        const auto tried = describe(
            names_of(ctx, attempted.party_id, attempted.output_series_id, attempted.as_of));
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request authorises " + authorised + ", and this publish is " +
                           tried + "."};
    }

    if (request->state_code != "approved")
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request is " + request->state_code +
                           ", and a publish waits until it is approved."};

    if (record->published_at)
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request already published " +
                           describe(names_of(
                               ctx, record->party_id, record->output_series_id, record->as_of)) +
                           ", at " + datetime::to_iso8601_utc(*record->published_at) + ", by " +
                           record->published_by + ". Raise a request for the next publish."};

    return {.approved = true, .state_code = request->state_code, .record = std::move(record)};
}

domain::curve_publish record_publication(const ores::database::context& ctx,
                                         domain::curve_publish record,
                                         const std::string& publisher) {
    if (publisher.empty())
        throw std::invalid_argument(
            "A publication names the account that applied it, and the call names none.");
    // Rounded to the precision the column keeps -- whole seconds, which is what
    // every nullable timestamp in this schema keeps -- so the instant this
    // returns is the instant a reader holds rather than a finer one the store
    // truncated.
    record.published_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    record.published_by = publisher;
    record.modified_by = publisher;
    record.performed_by = ctx.service_account();
    record.change_reason_code = "system.curve_published";
    record.change_commentary = "Published under approval request " +
                               boost::uuids::to_string(record.request_id) + " by " + publisher;

    // The set this one replaces ends in the same act, and both writes are one
    // transaction: a series promises one live set, so an apply that ended the
    // previous set but failed to write its successor would leave a series with
    // none, and one that wrote the successor first and then failed would leave
    // two.
    ores::database::repository::unit_of_work uow(ctx);
    const auto& tx = uow.ctx();
    // The set in use is read before the apply ends it, so the notice names the
    // version this one replaced and not the one it wrote.
    const auto replaced = live_as_of(tx, record.output_series_id);
    end_previous_sets(tx, record);

    repository::curve_publish_repository repo;
    repo.write(tx, record);
    uow.commit();

    // One notice, at the apply, once: the record is written once, so the notice
    // is raised once. Telling never unmoves the set, so a failure is logged in
    // the announcer and the record stands.
    announce_publish(ctx, record, replaced);
    return record;
}

}
