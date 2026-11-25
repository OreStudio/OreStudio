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
#include "ores.marketdata.service/app/fx_forward_publish_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/repository/approval_decision_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_point_repository.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_repository.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_spot_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/fx_forward_ladder_service.hpp"
#include "ores.marketdata.service/app/publication_kind.hpp"
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>

namespace ores::marketdata::service::app {

namespace {

auto& lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.service.app.fx_forward_publish_service");
    return instance;
}

using ores::database::context;
using ores::platform::time::datetime;
namespace msg = ores::marketdata::messaging;
using time_point = std::chrono::system_clock::time_point;

/// The instant at the precision the store keeps, so a raise and its apply compare equal.
time_point at_store_precision(time_point t) {
    return std::chrono::floor<std::chrono::microseconds>(t);
}

/// Applies the approved request to the forward set it names, or says why the set did not move.
void apply_forward(const context& ctx,
                   const std::string& request_id,
                   const publication_subject& attempted,
                   msg::publish_fx_forward_response& resp) {
    const auto applied = apply_publication(ctx, derived_kind::fx_forward, request_id, attempted);
    resp.approval_request_id = request_id;
    resp.state_code = applied.state_code;
    switch (applied.status) {
        case publication_status::published:
            resp.success = true;
            resp.published_at = applied.facts.published_at;
            resp.published_by = applied.facts.published_by;
            break;
        case publication_status::deferred:
            // A forward set has no compute of its own to defer: its ladder is
            // computed at the raise, and a missing input refuses the set there.
            resp.message = applied.message;
            break;
        case publication_status::refused:
            resp.message = applied.message;
            break;
    }
}

/// The funding curve one side of a set was built from, and its state now.
msg::fx_forward_curve_source curve_source(const context& ctx,
                                          const boost::uuids::uuid& publish_id) {
    msg::fx_forward_curve_source out;
    out.publish_id = boost::uuids::to_string(publish_id);
    const auto publishes = repository::curve_publish_repository{}.read_latest(ctx, out.publish_id);
    if (publishes.empty()) {
        out.state_code = "missing";
        return out;
    }
    const auto& set = publishes.front();
    out.as_of = set.as_of;
    out.published_by = set.published_by;
    out.output_series_id = boost::uuids::to_string(set.output_series_id);
    out.state_code = set.withdrawn_at  ? "withdrawn" :
                     set.superseded_at ? "superseded" :
                                         "published";
    const auto configs =
        ores::refdata::repository::ir_curve_bootstrap_config_repository{}.read_latest(
            ctx, boost::uuids::to_string(set.bootstrap_config_id));
    if (!configs.empty()) {
        out.currency = configs.front().currency_code;
        out.curve_name = configs.front().name;
    }
    return out;
}

/// The figures the pair is quoted with: its pip and decimals from its convention, and its spot
/// lag, which is the longer of its two currencies' lags, the rule the ladder compute settles on.
void quote_figures(const context& ctx, msg::get_published_fx_forward_response& out) {
    const auto conventions =
        ores::refdata::repository::currency_pair_convention_repository{}.read_latest(ctx,
                                                                                     out.pair_code);
    if (!conventions.empty()) {
        out.pip_factor = conventions.front().pip_factor;
        out.decimal_places = conventions.front().decimal_places;
    }
    const auto slash = out.pair_code.find('/');
    if (slash == std::string::npos)
        return;
    ores::refdata::repository::currency_repository currencies;
    for (const auto& code : {out.pair_code.substr(0, slash), out.pair_code.substr(slash + 1)})
        for (const auto& c : currencies.read_latest(ctx, code))
            out.spot_days = std::max(out.spot_days, c.spot_days);
}

/// Who raised the publish, and how its request was decided. The raise writes version 1 of the
/// request as the person raising it, and the newest decision is the one that closed it.
void approval_of(const context& ctx, msg::get_published_fx_forward_response& out) {
    const auto versions =
        ores::inbox::repository::approval_request_repository{}.read_all(ctx, out.request_id);
    if (versions.empty())
        return;
    const auto first =
        std::ranges::min_element(versions, {}, &ores::inbox::domain::approval_request::version);
    const auto last =
        std::ranges::max_element(versions, {}, &ores::inbox::domain::approval_request::version);
    out.requested_by = first->modified_by;
    out.approval_state_code = last->state_code;
    out.auto_approval_code = last->auto_approval_code;
    const auto decisions =
        ores::inbox::repository::approval_decision_repository{}.read_latest_by_request_id(
            ctx, out.request_id, 0, 100);
    const auto decision = std::ranges::max_element(
        decisions, {}, &ores::inbox::domain::approval_decision::decided_at);
    if (decision != decisions.end()) {
        out.decided_by = decision->modified_by;
        out.decision_comment = decision->comment;
    }
}

}

msg::publish_fx_forward_response
fx_forward_publish_service::publish(const context& ctx,
                                    const msg::publish_fx_forward_request& req) {
    msg::publish_fx_forward_response resp;
    try {
        const auto party = ores::service::messaging::session_party(ctx, "A forward set");
        const auto as_of = at_store_precision(req.as_of);
        // The subject is the set's pair, and the party is the session's: the
        // set is computed by the raise, so a missing input refuses it there and
        // writes nothing.
        const auto attempted =
            resolve_publication_subject(ctx, derived_kind::fx_forward, req.pair_code, as_of);

        if (!req.approval_request_id.empty()) {
            apply_forward(ctx, req.approval_request_id, attempted, resp);
            return resp;
        }

        const auto raised = raise_publication(ctx,
                                              derived_kind::fx_forward,
                                              {{.subject_key = req.pair_code, .as_of = as_of}},
                                              req.reason,
                                              party);
        resp.approval_request_id = raised.request_id;
        resp.state_code = raised.state_code;
        if (raised.state_code != "approved") {
            resp.success = true;
            resp.message = "Raised. The publish waits on the decision.";
            return resp;
        }
        apply_forward(ctx, resp.approval_request_id, raised.raised.front(), resp);
    } catch (const std::exception& e) {
        resp.success = false;
        resp.message = e.what();
    }
    return resp;
}

msg::get_published_fx_forward_response
fx_forward_publish_service::read(const context& ctx,
                                 const msg::get_published_fx_forward_request& req) {
    msg::get_published_fx_forward_response out;
    out.pair_code = req.pair_code;
    try {
        const auto party = ores::service::messaging::session_party(ctx, "A forward set");
        const auto sets = repository::fx_forward_publish_repository{}.read_published_for_pair(
            ctx, party, req.pair_code);
        const auto found =
            std::ranges::find_if(sets, [&](const auto& s) { return s.as_of <= req.as_of; });
        if (found == sets.end()) {
            out.message = "Your party has no published forward set of " + req.pair_code +
                          " at or before the as-of.";
            return out;
        }
        const auto& set = *found;
        out.success = true;
        out.publish_id = boost::uuids::to_string(set.id);
        out.request_id = boost::uuids::to_string(set.request_id);
        out.party_id = boost::uuids::to_string(set.party_id);
        out.as_of = set.as_of;
        out.spot_date = set.spot_date;
        out.spot_rate = set.spot_rate;
        out.published_by = set.published_by;
        out.published_at = set.published_at;
        out.convention_version = set.convention_version;
        out.calendar_codes = set.calendar_codes;
        out.method_code = set.method_code;
        quote_figures(ctx, out);
        approval_of(ctx, out);
        out.base_curve = curve_source(ctx, set.base_curve_publish_id);
        out.quote_curve = curve_source(ctx, set.quote_curve_publish_id);

        auto points =
            repository::fx_forward_publish_point_repository{}.read_latest_by_publish(ctx, set.id);
        std::ranges::sort(points, {}, &domain::fx_forward_publish_point::sequence_index);
        for (const auto& p : points)
            out.rows.push_back({.tenor = p.tenor_code,
                                .near_date = p.near_date,
                                .value_date = p.value_date,
                                .base_discount = p.base_discount,
                                .quote_discount = p.quote_discount,
                                .forward_rate = p.forward_rate,
                                .forward_points = p.forward_points,
                                .swap_points = p.swap_points});
        for (const auto& s :
             repository::fx_forward_publish_spot_repository{}.read_latest_by_publish(ctx, set.id))
            out.spots.push_back({.leg_code = s.leg_code,
                                 .pair_code = s.pair_code,
                                 .series_id = boost::uuids::to_string(s.series_id),
                                 .observed_at = s.observed_at,
                                 .value = s.value});
    } catch (const std::exception& e) {
        out.success = false;
        out.message = e.what();
    }
    return out;
}

namespace {

/**
 * @brief The FX forward set, reached only through the interface the lifecycle calls.
 *
 * The subject is a currency pair, the candidate is the ladder the raise computes
 * by covered interest parity from two published curves, and the checks are the
 * inputs' state. The acts are the lifecycle's, in publication_lifecycle.cpp.
 */
class fx_forward_publication_kind_impl final : public publication_kind {
public:
    derived_kind kind() const override {
        return derived_kind::fx_forward;
    }

    publication_subject
    resolve_subject(const ores::database::context&,
                    const boost::uuids::uuid& party_id,
                    const std::string& subject_key,
                    std::chrono::system_clock::time_point as_of) const override {
        if (subject_key.empty())
            throw std::invalid_argument(
                "Name the currency pair the set is a set of, so the request authorises one set.");
        const auto stored_as_of = at_store_precision(as_of);
        if (stored_as_of == time_point{})
            throw std::invalid_argument(
                "Name the as-of instant the set reads the market at, so the request authorises "
                "one set.");
        return publication_subject{.kind = derived_kind::fx_forward,
                                   .party_id = party_id,
                                   .subject_id = {},
                                   .subject_key = subject_key,
                                   .subject_name = subject_key,
                                   .output_series_id = {},
                                   .as_of = stored_as_of};
    }

    /**
     * @brief Nothing, because the raise computes the ladder and a missing input refuses it.
     *
     * The compute writes the candidate in the raise's own transaction, so a set
     * whose input curve is not there is refused by the store's work and writes
     * nothing: there is no stored candidate to check first.
     */
    std::optional<std::string> refusal_of_raise(const ores::database::context&,
                                                const publication_subject&) const override {
        return std::nullopt;
    }

    void write_publish(const ores::database::context& tx,
                       const boost::uuids::uuid& request_id,
                       const publication_subject& subject) const override {
        // The set is computed before anything is raised, so a missing input refuses
        // the publish and writes nothing.
        const auto computed = fx_forward_ladder_service::compute(
            tx,
            fx_forward_inputs{.pair_code = subject.subject_key,
                              .as_of = subject.as_of,
                              .party_id = boost::uuids::to_string(subject.party_id)});
        const auto& ladder = computed.ladder;
        boost::uuids::random_generator gen;
        const auto commentary = "The FX forward set that publish request " +
                                boost::uuids::to_string(request_id) + " authorises";

        domain::fx_forward_publish record;
        record.id = gen();
        record.tenant_id = tx.tenant_id();
        record.request_id = request_id;
        record.party_id = subject.party_id;
        record.pair_code = ladder.pair_code;
        record.as_of = subject.as_of;
        record.spot_date = ladder.spot_date;
        record.spot_rate = ladder.spot;
        record.base_curve_publish_id =
            boost::lexical_cast<boost::uuids::uuid>(ladder.base_curve.publish_id);
        record.quote_curve_publish_id =
            boost::lexical_cast<boost::uuids::uuid>(ladder.quote_curve.publish_id);
        record.convention_version = computed.convention_version;
        record.calendar_codes = computed.calendar_codes;
        record.method_code = "covered_interest_parity";
        record.published_by = "";
        record.published_at = std::nullopt;
        record.modified_by = ores::service::messaging::delegated_actor(tx);
        record.performed_by = tx.service_account();
        record.change_reason_code = "system.new_record";
        record.change_commentary = commentary;

        std::vector<domain::fx_forward_publish_point> points;
        int index = 0;
        for (const auto& row : ladder.rows) {
            domain::fx_forward_publish_point p;
            p.id = gen();
            p.tenant_id = tx.tenant_id();
            p.publish_id = record.id;
            p.sequence_index = index++;
            p.tenor_code = row.tenor;
            p.near_date = row.near_date;
            p.value_date = row.value_date;
            p.base_discount = row.base_discount;
            p.quote_discount = row.quote_discount;
            p.forward_rate = row.forward_rate;
            p.forward_points = row.forward_points;
            p.swap_points = row.swap_points;
            p.modified_by = record.modified_by;
            p.performed_by = record.performed_by;
            p.change_reason_code = record.change_reason_code;
            p.change_commentary = commentary;
            points.push_back(std::move(p));
        }

        std::vector<domain::fx_forward_publish_spot> spots;
        for (const auto& leg : computed.spots) {
            domain::fx_forward_publish_spot spot;
            spot.id = gen();
            spot.tenant_id = tx.tenant_id();
            spot.publish_id = record.id;
            spot.leg_code = leg.leg_code;
            spot.pair_code = leg.pair_code;
            spot.series_id = boost::lexical_cast<boost::uuids::uuid>(leg.series_id);
            spot.observed_at = leg.observed_at;
            spot.value = leg.value;
            spot.modified_by = record.modified_by;
            spot.performed_by = record.performed_by;
            spot.change_reason_code = record.change_reason_code;
            spot.change_commentary = commentary;
            spots.push_back(std::move(spot));
        }

        // The subject and its numbers are one act: a request whose set was half written
        // would authorise numbers nobody computed.
        repository::fx_forward_publish_repository{}.write(tx, record);
        repository::fx_forward_publish_point_repository{}.write(tx, points);
        repository::fx_forward_publish_spot_repository{}.write(tx, spots);
    }

    publication_verdict verdict_on_publish(const ores::database::context& ctx,
                                           const std::string& request_id,
                                           const publication_subject& attempted) const override {
        publication_verdict out;
        std::optional<ores::inbox::domain::approval_request> request;
        try {
            request = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
        } catch (const std::exception& e) {
            // An id that is not a uuid is not a request; anything else is logged so a
            // store failure is not read as a missing request.
            ORES_LOG_SEV(lg(), ores::logging::warn)
                << "Reading approval request " << request_id << ": " << e.what();
            request = std::nullopt;
        }
        if (!request) {
            out.message = "No approval request has the id " + request_id + ".";
            return out;
        }
        out.state_code = request->state_code;
        if (request->kind_code != fx_forward_publish_kind_code) {
            out.message = "The request is a " + request->kind_code +
                          " request, and a forward set is published by a " +
                          std::string(fx_forward_publish_kind_code) + " request.";
            return out;
        }

        repository::fx_forward_publish_repository repo;
        const auto record = repo.read_latest_by_request(ctx, request_id);
        if (!record) {
            out.message = "The request names no forward set, so it authorises no publish.";
            return out;
        }
        if (record->party_id != attempted.party_id || record->pair_code != attempted.subject_key ||
            record->as_of != attempted.as_of) {
            out.message = "The request authorises " + record->pair_code + " as of " +
                          datetime::to_iso8601_utc(record->as_of) + " for party " +
                          boost::uuids::to_string(record->party_id) + ", and this publish is " +
                          attempted.subject_key + " as of " +
                          datetime::to_iso8601_utc(attempted.as_of) + " for party " +
                          boost::uuids::to_string(attempted.party_id) + ".";
            return out;
        }
        if (request->state_code != "approved") {
            out.message = "The request is " + request->state_code +
                          ", and a publish waits until it is approved.";
            return out;
        }
        if (record->published_at) {
            out.message = "The request already published " + record->pair_code + " at " +
                          datetime::to_iso8601_utc(*record->published_at) + ", by " +
                          record->published_by + ". Raise a request for the next publish.";
            return out;
        }

        // The forward set has no output series yet, so its facts carry the pair
        // and the as-of, which is what names it to a reader.
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
        // The ladder is computed at the raise, so the apply computes nothing and
        // can never defer.
        return std::nullopt;
    }

    publication_facts publish(const ores::database::context& tx,
                              publication_facts set) const override {
        repository::fx_forward_publish_repository repo;
        const auto stored = repo.read_latest(tx, boost::uuids::to_string(set.publish_id));
        if (stored.empty())
            throw std::runtime_error("The set the request authorises is gone, so nothing is "
                                     "published under it.");
        auto record = stored.front();
        record.published_at = set.published_at;
        record.published_by = set.published_by;
        record.modified_by = set.published_by;
        record.performed_by = tx.service_account();
        record.change_reason_code = "system.fx_forward_published";
        record.change_commentary = "Published under approval request " +
                                   boost::uuids::to_string(record.request_id) + " by " +
                                   set.published_by;
        // A supersession is the record's to state and its columns do not exist
        // yet, so the set ends nothing and no reader is told: the story's third
        // task adds both.
        repo.write(tx,
                   record,
                   {ores::utility::domain::precondition_kind::must_match_version,
                    static_cast<std::uint32_t>(record.version)});
        set.version = record.version + 1;
        return set;
    }

    void announce_published(const ores::database::context&,
                            const publication_facts&) const override {
        // The forward set tells nobody yet: the seed carries no notice kind for
        // it, so announcing would raise a kind that does not exist. The story's
        // third task seeds the kind and the notice.
    }
};

}

const publication_kind& fx_forward_publication_kind() {
    static const fx_forward_publication_kind_impl instance;
    return instance;
}

}
