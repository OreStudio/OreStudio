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
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.marketdata.service/app/publication_kind.hpp"
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <set>
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

ores::inbox::domain::approval_request
raise_publish_request_set(const ores::database::context& ctx,
                          const std::vector<publish_subject>& subjects,
                          const std::string& reason) {
    std::vector<publication_request> requests;
    requests.reserve(subjects.size());
    for (const auto& subject : subjects)
        requests.push_back({.subject_key = boost::uuids::to_string(subject.bootstrap_config_id),
                            .as_of = subject.as_of});
    if (requests.empty())
        throw std::invalid_argument("A set of publishes names at least one curve to publish.");

    // The curve's contract is the stricter of the two the lifecycle offers: a set
    // is raised whole or not at all, so a caller never reads a request that
    // authorises a subset of what it asked for.
    const auto raised =
        raise_publication(ctx, derived_kind::curve, requests, reason, {}, set_raise::whole_set);

    const auto request = ores::inbox::service::approval_lifecycle(ctx).request(raised.request_id);
    if (!request)
        throw std::runtime_error("The request that was just raised cannot be read back.");
    return *request;
}

ores::inbox::domain::approval_request raise_publish_request(const ores::database::context& ctx,
                                                            const publish_subject& subject,
                                                            const std::string& reason) {
    return raise_publish_request_set(ctx, {subject}, reason);
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

    // The request names a set, and this is the member the publish asks about: a
    // request that does not name this configuration authorises nothing here,
    // which is the same answer a request that names no configuration at all
    // gets.
    repository::curve_publish_repository repo;
    auto record = repo.read_latest_by_request_and_config(
        ctx, request_id, boost::uuids::to_string(attempted.bootstrap_config_id));
    if (!record) {
        // The request names a set and this curve is not one of its members: the
        // refusal says what the request does authorise and what was attempted, so
        // a person reads both subjects rather than being told they differ.
        ores::marketdata::messaging::curve_publishes_filter by_request;
        by_request.request_id = boost::uuids::string_generator{}(request_id);
        std::string authorised;
        for (std::uint32_t offset = 0;; offset += 1000) {
            const auto page = repo.read_latest(ctx, offset, 1000, {}, by_request);
            for (const auto& member : page)
                authorised +=
                    (authorised.empty() ? "" : "; ") +
                    describe(names_of(ctx, member.party_id, member.output_series_id, member.as_of));
            if (page.size() < 1000)
                break;
        }
        if (authorised.empty())
            return {.approved = false,
                    .state_code = request->state_code,
                    .message = "The request is a " + std::string(curve_publish_kind_code) +
                               " request that names no subject, so it authorises no publish."};
        const auto tried = describe(
            names_of(ctx, attempted.party_id, attempted.output_series_id, attempted.as_of));
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request authorises " + authorised + ", and this publish is " +
                           tried + "."};
    }

    if (at_store_precision(record->as_of) != at_store_precision(attempted.as_of) ||
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

    // The apply is the lifecycle's, so a caller that has already read the
    // verdict still goes through the one act: the verdict, the deferred build,
    // the publication, the supersession and the notice are the same five steps
    // whichever way a publish arrives.
    const publication_subject attempted{
        .kind = derived_kind::curve,
        .party_id = record.party_id,
        .subject_id = record.bootstrap_config_id,
        .subject_key = boost::uuids::to_string(record.bootstrap_config_id),
        .subject_name = names_of(ctx, record.party_id, record.output_series_id, record.as_of).curve,
        .output_series_id = record.output_series_id,
        .as_of = record.as_of};
    const auto applied = apply_publication(
        ctx, derived_kind::curve, boost::uuids::to_string(record.request_id), attempted, publisher);
    if (applied.status != publication_status::published)
        throw std::invalid_argument(applied.message);

    // The record as the store holds it, so the caller answers with the instant
    // the store keeps rather than with its own clock.
    repository::curve_publish_repository repo;
    const auto stored = repo.read_latest(ctx, boost::uuids::to_string(record.id));
    if (stored.empty())
        throw std::runtime_error("The published set was written but cannot be read back.");
    return stored.front();
}

namespace {

/// The facts of a curve publish, in the shape every kind shares.
publication_facts facts_of(const ores::database::context& ctx,
                           const domain::curve_publish& record) {
    const auto named = names_of(ctx, record.party_id, record.output_series_id, record.as_of);
    publication_facts out;
    out.subject.kind = derived_kind::curve;
    out.subject.party_id = record.party_id;
    out.subject.subject_id = record.bootstrap_config_id;
    out.subject.subject_key = boost::uuids::to_string(record.bootstrap_config_id);
    out.subject.subject_name = named.curve;
    out.subject.output_series_id = record.output_series_id;
    out.subject.as_of = record.as_of;
    out.publish_id = record.id;
    out.request_id = record.request_id;
    out.version = record.version;
    out.published_at = record.published_at.value_or(std::chrono::system_clock::time_point{});
    out.published_by = record.published_by;
    return out;
}

/**
 * @brief The curve, reached only through the interface the lifecycle calls.
 *
 * The words are the gate's own, because a curve is named by its output series'
 * oresmd URI and a set is named by its party, its as-of and that URI. The acts
 * are the lifecycle's, in publication_lifecycle.cpp.
 */
class curve_publication_kind_impl final : public publication_kind {
public:
    derived_kind kind() const override {
        return derived_kind::curve;
    }

    publication_subject
    resolve_subject(const ores::database::context& ctx,
                    const boost::uuids::uuid& party_id,
                    const std::string& subject_key,
                    std::chrono::system_clock::time_point as_of) const override {
        const auto subject = resolve_publish_subject(ctx, subject_key, as_of);
        if (!party_id.is_nil() && subject.party_id != party_id)
            throw std::invalid_argument(
                "The curve " +
                describe(names_of(ctx, subject.party_id, subject.output_series_id, subject.as_of)) +
                " belongs to another party, and only a person acting for that party publishes "
                "it.");
        return publication_subject{
            .kind = derived_kind::curve,
            .party_id = subject.party_id,
            .subject_id = subject.bootstrap_config_id,
            .subject_key = subject_key,
            .subject_name =
                names_of(ctx, subject.party_id, subject.output_series_id, subject.as_of).curve,
            .output_series_id = subject.output_series_id,
            .as_of = subject.as_of};
    }

    /**
     * @brief Nothing, because a curve has no candidate before its apply.
     *
     * The bootstrap runs at the apply, and a request whose inputs have not
     * arrived is deferred rather than refused, so there is no candidate version
     * to check at the raise and no second open request to guard against. This
     * is the one kind that does not state the raise's checks, and the departures
     * are recorded against the page that states them.
     */
    std::optional<std::string> refusal_of_raise(const ores::database::context&,
                                                const publication_subject&) const override {
        return std::nullopt;
    }

    void write_publish(const ores::database::context& tx,
                       const boost::uuids::uuid& request_id,
                       const publication_subject& subject) const override {
        domain::curve_publish record;
        record.id = boost::uuids::random_generator{}();
        record.tenant_id = tx.tenant_id();
        record.request_id = request_id;
        record.party_id = subject.party_id;
        record.bootstrap_config_id = subject.subject_id;
        record.output_series_id = subject.output_series_id;
        record.as_of = subject.as_of;
        record.published_by = "";
        record.published_at = std::nullopt;
        record.modified_by = ores::service::messaging::delegated_actor(tx);
        record.performed_by = tx.service_account();
        record.change_reason_code = "system.new_record";
        record.change_commentary = "The subject that publish request " +
                                   boost::uuids::to_string(request_id) + " authorises";
        repository::curve_publish_repository{}.write(tx, record);
    }

    publication_verdict verdict_on_publish(const ores::database::context& ctx,
                                           const std::string& request_id,
                                           const publication_subject& attempted) const override {
        const auto verdict = ores::marketdata::service::app::verdict_on_publish(
            ctx,
            request_id,
            {.party_id = attempted.party_id,
             .bootstrap_config_id = attempted.subject_id,
             .output_series_id = attempted.output_series_id,
             .as_of = attempted.as_of});
        publication_verdict out;
        out.approved = verdict.approved;
        out.state_code = verdict.state_code;
        out.message = verdict.message;
        if (verdict.record)
            out.facts = facts_of(ctx, *verdict.record);
        return out;
    }

    /**
     * @brief The bootstrap, which the curve runs at its apply and not before its raise.
     *
     * A deferred build writes nothing and stamps nothing, so the request is not
     * spent on a curve whose inputs have not arrived: the next attempt applies
     * it.
     */
    std::optional<std::string> prepare(const ores::database::context& ctx,
                                       const publication_subject& subject) const override {
        const auto built =
            curve_republish_service::republish(ctx, subject.subject_id, subject.as_of);
        if (!built)
            return built.error().reason;
        return std::nullopt;
    }

    publication_facts publish(const ores::database::context& tx,
                              publication_facts set) const override {
        repository::curve_publish_repository repo;
        const auto stored = repo.read_latest(tx, boost::uuids::to_string(set.publish_id));
        if (stored.empty())
            throw std::runtime_error("The set the request authorises is gone, so nothing is "
                                     "published under it.");
        auto record = stored.front();

        // The set in use is read before the apply ends it, so the notice names
        // the version this one replaced and not the one it wrote.
        set.replaced_as_of = live_as_of(tx, record.output_series_id);

        record.published_at = set.published_at;
        record.published_by = set.published_by;
        record.modified_by = set.published_by;
        record.performed_by = tx.service_account();
        record.change_reason_code = "system.curve_published";
        record.change_commentary = "Published under approval request " +
                                   boost::uuids::to_string(record.request_id) + " by " +
                                   set.published_by;
        end_previous_sets(tx, record);
        repo.write(tx, record);

        set.subject = facts_of(tx, record).subject;
        set.version = record.version;
        return set;
    }

    void announce_published(const ores::database::context& ctx,
                            const publication_facts& set) const override {
        domain::curve_publish record;
        record.id = set.publish_id;
        record.request_id = set.request_id;
        record.party_id = set.subject.party_id;
        record.bootstrap_config_id = set.subject.subject_id;
        record.output_series_id = set.subject.output_series_id;
        record.as_of = set.subject.as_of;
        record.published_at = set.published_at;
        record.published_by = set.published_by;
        announce_publish(ctx, record, set.replaced_as_of);
    }
};

}

const publication_kind& curve_publication_kind() {
    static const curve_publication_kind_impl instance;
    return instance;
}

}
