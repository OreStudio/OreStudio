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
#include "ores.trading.core/service/trade_operations_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_repository.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_repository.hpp"
#include "ores.trading.core/repository/bond_issue_leg_amortization_repository.hpp"
#include "ores.trading.core/repository/bond_issue_leg_amount_repository.hpp"
#include "ores.trading.core/repository/bond_issue_leg_rate_repository.hpp"
#include "ores.trading.core/repository/bond_issue_leg_repository.hpp"
#include "ores.trading.core/repository/bond_issue_leg_schedule_date_repository.hpp"
#include "ores.trading.core/repository/bond_issue_leg_schedule_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/structure_member_repository.hpp"
#include "ores.trading.core/repository/structure_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include "ores.trading.core/service/structure_service.hpp"
#include "ores.trading.core/service/instrument_batch_writer.hpp"
#include "ores.trading.core/service/operational_authorisation_service.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <exception>
#include <optional>
#include <rfl.hpp>
#include <string>
#include <string_view>

namespace ores::trading::service {

using namespace ores::logging;

trade_operations_service::trade_operations_service(context ctx, const book_status_port& books)
    : ctx_(std::move(ctx))
    , books_(&books) {}

namespace {

// The audit members are the store's to set, so two rows hold the same terms
// when they are equal once the incoming row takes the stored row's audit. A row
// type that gains another member the store sets must be listed here, or a
// repeat write reads as other terms.
template <typename Row>
bool same_terms(const Row& stored, Row incoming) {
    incoming.version = stored.version;
    incoming.modified_by = stored.modified_by;
    incoming.performed_by = stored.performed_by;
    incoming.change_reason_code = stored.change_reason_code;
    incoming.change_commentary = stored.change_commentary;
    incoming.recorded_at = stored.recorded_at;
    return incoming == stored;
}

// Each incoming row must match a different stored row.
template <typename Row>
bool same_rows(std::vector<Row> remaining, const std::vector<Row>& incoming) {
    if (remaining.size() != incoming.size())
        return false;
    for (const auto& row : incoming) {
        const auto match = std::ranges::find_if(
            remaining, [&](const Row& other) { return same_terms(other, row); });
        if (match == remaining.end())
            return false;
        remaining.erase(match);
    }
    return true;
}

template <typename Repository, typename Row>
void create_rows(Repository& repository,
                 const ores::database::context& ctx,
                 std::vector<Row>& rows,
                 const ores::utility::domain::precondition& claim) {
    for (auto& row : rows) {
        row.version = 0;
        repository.write(ctx, row, claim);
    }
}

template <typename Row>
void stamp_rows(std::vector<Row>& rows, const ores::database::context& ctx) {
    for (auto& row : rows)
        ores::service::messaging::stamp(row, ctx);
}

/**
 * @brief True when a member of a bond row holds data: an engaged optional or a
 * string or a vector that is not empty.
 */
template <typename T>
bool holds_data(const T& value) {
    if constexpr (requires { value.has_value(); })
        return value.has_value();
    else if constexpr (requires { value.empty(); })
        return !value.empty();
    else
        return false;
}

// Booking writes the header of a bond row. Every other member of the row that
// can hold data, apart from the issue, which the caller has stored already,
// belongs to a writer that does not exist yet, so the row names the member in a
// refusal before anything is written.
std::vector<std::string> unsupported_bond_parts(const domain::instrument_batch& batch) {
    std::vector<std::string> names;
    for (const auto& bond : batch.bond_instruments) {
        bool refused = false;
        rfl::to_view(bond).apply([&](const auto& field) {
            const std::string_view name = field.name();
            if (name != "instrument" && name != "issue" && holds_data(*field.value())) {
                names.emplace_back("bond_instruments." + std::string(name));
                refused = true;
            }
        });
        if (refused)
            break;
    }
    return names;
}

std::string join(const std::vector<std::string>& names) {
    std::string joined;
    for (const auto& name : names)
        joined += (joined.empty() ? "" : ", ") + name;
    return joined;
}

}

messaging::book_trade_response
trade_operations_service::book_trade(const messaging::book_trade_request& request,
                                     std::string_view actor_token) {
    using ores::database::repository::unit_of_work;
    using ores::service::messaging::stamp;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition;
    using ores::utility::domain::precondition_kind;

    auto anchor = request.anchor;
    auto booking = request.booking;
    stamp(anchor, ctx_);
    stamp(booking, ctx_);
    BOOST_LOG_SEV(lg(), debug) << "Booking trade " << anchor.id;

    messaging::book_trade_response response;

    if (const auto misrouted = service::misrouted_arrays(request.instruments, anchor.trade_type);
        !misrouted.empty()) {
        response.result.outcome = outcome::failed;
        response.result.code = "instrument_not_routed";
        response.result.message = "The trade type " + anchor.trade_type +
                                  " does not hold the instrument arrays: " + join(misrouted) + ".";
        return response;
    }
    if (const auto orphans = service::orphan_arrays(request.instruments); !orphans.empty()) {
        response.result.outcome = outcome::failed;
        response.result.code = "instrument_child_without_header";
        response.result.message =
            "The batch holds rows for " + join(orphans) + " and no instrument header.";
        return response;
    }
    if (const auto unsupported = unsupported_bond_parts(request.instruments);
        !unsupported.empty()) {
        response.result.outcome = outcome::failed;
        response.result.code = "instrument_not_supported";
        response.result.message =
            "Booking writes no instrument yet for: " + join(unsupported) + ".";
        return response;
    }

    /*
     * A booking lands in a book, and a book that is not open takes no booking.
     * The status is reference data, so it is asked for rather than assumed; a
     * status that cannot be had is logged and does not stop the booking, because
     * refusing every booking while refdata is busy would be a worse fault.
     */
    switch (books_->of(booking.book_id, actor_token)) {
        case book_status_port::state::not_open:
            response.result.outcome = outcome::failed;
            response.result.code = "book_not_open";
            response.result.message =
                "Book " + boost::uuids::to_string(booking.book_id) + " is not open for booking.";
            return response;
        case book_status_port::state::unknown:
            BOOST_LOG_SEV(lg(), warn)
                << "Could not read the status of book " << boost::uuids::to_string(booking.book_id)
                << "; booking continues.";
            break;
        case book_status_port::state::open:
            break;
    }

    // One transaction writes the anchor, the activity, the booking, the state,
    // the identifiers and the instrument, so a failure in any of them leaves
    // none of them. A read made through the transaction joins it without
    // ending it.
    unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    repository::trade_repository trades;
    if (!trades.read_latest(ctx, boost::uuids::to_string(anchor.id)).empty()) {
        response.result.outcome = outcome::conflict;
        response.result.code = "already_exists";
        response.result.message =
            "Trade " + boost::uuids::to_string(anchor.id) + " is already booked.";
        return response;
    }

    domain::trade_activity activity;
    activity.id = boost::uuids::random_generator()();
    activity.activity_type_code = request.activity_type_code;
    activity.actor = booking.modified_by;
    activity.occurred_at = booking.execution_timestamp.value_or(std::chrono::system_clock::now());
    activity.comment = booking.change_commentary;
    stamp(activity, ctx_);
    activity.party_id = anchor.party_id;

    booking.trade_id = anchor.id;
    booking.trade_activity_id = activity.id;
    booking.version = 0;

    domain::trade_state state;
    state.trade_id = anchor.id;
    state.trade_activity_id = activity.id;
    state.version = 0;
    state.modified_by = booking.modified_by;
    state.performed_by = booking.modified_by;
    state.change_reason_code = booking.change_reason_code;
    state.change_commentary = booking.change_commentary;
    stamp(state, ctx_);
    state.status_id = boost::uuids::uuid{};

    const precondition must_not_exist{precondition_kind::must_not_exist, std::nullopt};
    trades.write(ctx, anchor, must_not_exist);
    repository::trade_activity_repository{}.write(ctx, activity, must_not_exist);
    repository::trade_booking_repository{}.write(ctx, booking, must_not_exist);
    repository::trade_state_repository{}.write(ctx, state, must_not_exist);

    // The identifiers and the instrument are children of the trade: each write
    // bumps the trade's version, so they follow the anchor, and the request
    // below records the version they leave. The stamp takes the actor from the
    // caller's context, so the modified_by and performed_by of a request do not stand.
    auto identifiers = request.identifiers;
    repository::trade_identifier_repository identifier_repository;
    for (auto& identifier : identifiers) {
        identifier.trade_id = anchor.id;
        identifier.trade_activity_id = activity.id;
        identifier.version = 0;
        stamp(identifier, ctx_);
        identifier_repository.write(ctx, identifier, must_not_exist);
    }

    auto instruments = request.instruments;
    const service::instrument_keys keys{anchor.id, anchor.trade_type, activity.id};
    service::write_instrument_batch(
        ctx, instruments, keys, [&](auto& row) { stamp(row, ctx_); }, must_not_exist);

    /*
     * A draft states why it is not live. The reason is what a report of aged
     * drafts groups by, so a draft without one is a deal nobody can pick up; and
     * a trade that goes live when it is booked waits on nothing, so a reason
     * there is a mistake the caller should hear about.
     */
    const auto books_a_draft = request.activity_type_code == "draft_capture";
    if (books_a_draft && request.draft_reason_code.empty()) {
        response.result.outcome = outcome::failed;
        response.result.code = "draft_reason_missing";
        response.result.message = "A draft states why it is not live.";
        return response;
    }
    if (!books_a_draft && !request.draft_reason_code.empty()) {
        response.result.outcome = outcome::failed;
        response.result.code = "draft_reason_on_live_trade";
        response.result.message =
            "A trade that goes live when it is booked waits on nothing, so it states no draft reason.";
        return response;
    }
    anchor.draft_reason_code = books_a_draft ? request.draft_reason_code : std::string{};

    // A trade booked live in a real book awaits Operations. The request joins
    // this transaction, so a trade is never live without one.
    operational_authorisation_service authorisation(ctx);
    const auto raised = authorisation.raise_if_awaiting(anchor.id);

    uow.commit();

    // The request stands whether or not anybody is told, so the telling
    // happens after the commit and a failure there never fails the booking.
    if (raised) {
        try {
            operational_authorisation_service(ctx_).announce(*raised);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Trade " << anchor.id
                                      << " was booked but its deciders were not told: " << e.what();
        }
    }

    response.activity_id = activity.id;
    return response;
}

std::optional<raised_authorisation> trade_operations_service::execute_within(
    context ctx,
    const boost::uuids::uuid& trade_id,
    const std::string& change_reason_code,
    const std::string& change_commentary,
    boost::uuids::uuid* activity_id) {
    using ores::service::messaging::stamp;
    using ores::utility::domain::precondition;
    using ores::utility::domain::precondition_kind;

    const auto id = boost::uuids::to_string(trade_id);
    const auto trades = repository::trade_repository().read_latest(ctx, id);
    if (trades.empty())
        throw std::runtime_error("Trade " + id + " is not booked.");
    const auto& trade = trades.front();

    domain::trade_activity activity;
    activity.id = boost::uuids::random_generator()();
    activity.activity_type_code = "execution";
    activity.actor = ctx_.actor();
    activity.occurred_at = std::chrono::system_clock::now();
    activity.comment = change_commentary;
    stamp(activity, ctx);
    activity.party_id = trade.party_id;

    repository::trade_activity_repository{}.write(
        ctx, activity, {precondition_kind::must_not_exist, std::nullopt});

    // The status is the transition's: the store reads the activity and moves
    // the machine. The claim is the state it replaces, which is the draft.
    domain::trade_state state;
    state.trade_id = trade_id;
    state.trade_activity_id = activity.id;
    state.status_id = boost::uuids::uuid{};
    state.change_reason_code = change_reason_code;
    state.change_commentary = change_commentary;
    stamp(state, ctx);
    repository::trade_state_repository{}.write(ctx, state);

    if (activity_id != nullptr)
        *activity_id = activity.id;

    operational_authorisation_service authorisation(ctx);
    return authorisation.raise_if_awaiting(trade_id);
}

messaging::execute_trade_response
trade_operations_service::execute_trade(const messaging::execute_trade_request& request) {
    using ores::database::repository::unit_of_work;
    using ores::service::messaging::stamp;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition;
    using ores::utility::domain::precondition_kind;

    messaging::execute_trade_response response;
    const auto id = boost::uuids::to_string(request.trade_id);
    BOOST_LOG_SEV(lg(), debug) << "Executing trade " << id;

    // The activity, the state and the request are one decision, so they are one
    // transaction: a trade that goes live without its request is exactly the
    // hole this operation closes.
    unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    repository::trade_repository trades;
    const auto found = trades.read_latest(ctx, id);
    if (found.empty()) {
        response.result.outcome = outcome::missing;
        response.result.code = "not_found";
        response.result.message = "Trade " + id + " is not booked.";
        return response;
    }
    const auto& trade = found.front();
    if (trade.version != request.expected_version) {
        response.result.outcome = outcome::conflict;
        response.result.code = "version_conflict";
        response.result.message = "Trade " + id + " is at version " +
                                  std::to_string(trade.version) + ", not " +
                                  std::to_string(request.expected_version) + ".";
        return response;
    }
    if (trade.booking_nature != domain::booking_nature::actual) {
        response.result.outcome = outcome::conflict;
        response.result.code = "not_actual";
        response.result.message = "Trade " + id + " is not an actual trade, so it executes nothing.";
        return response;
    }

    // Only a draft executes, and the state says which one it is: the transition
    // the activity names is checked by the store, so a trade that is not a draft
    // is refused here before anything is written.
    repository::trade_state_repository states;
    const auto held = states.read_latest(ctx, id);
    if (held.empty()) {
        response.result.outcome = outcome::missing;
        response.result.code = "not_found";
        response.result.message = "Trade " + id + " has no state to execute.";
        return response;
    }
    const auto draft = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "select ores_trading_status_is_draft_fn($1::uuid)::text",
        {boost::uuids::to_string(held.front().status_id)},
        lg(),
        "Checking whether a trade state is a draft");
    if (draft.empty() || draft.front() != "true") {
        response.result.outcome = outcome::conflict;
        response.result.code = "not_draft";
        response.result.message = "Trade " + id + " is not a draft, so it cannot be executed.";
        return response;
    }

    auto activity_id = boost::uuids::uuid{};
    const auto raised = execute_within(ctx, request.trade_id, request.change_reason_code,
                                       request.change_commentary, &activity_id);

    uow.commit();

    if (raised) {
        try {
            operational_authorisation_service(ctx_).announce(*raised);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Trade " << id << " was executed but its deciders were not told: " << e.what();
        }
    }

    response.activity_id = activity_id;
    return response;
}

messaging::complete_structure_response
trade_operations_service::complete_structure(const messaging::complete_structure_request& request) {
    using ores::database::repository::unit_of_work;
    using ores::service::messaging::stamp;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;

    messaging::complete_structure_response response;

    // The count is the caller's claim about the list. A truncated list is the
    // failure this checks for: a structure completed with a leg missing is
    // worse than one that was not completed.
    if (request.leg_count <= 0 ||
        static_cast<int>(request.legs.size()) != request.leg_count) {
        response.result.outcome = outcome::failed;
        response.result.code = "leg_count_mismatch";
        response.result.message = "A completion states " + std::to_string(request.leg_count) +
                                  " legs and the request holds " +
                                  std::to_string(request.legs.size()) + ".";
        return response;
    }

    unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    auto structure = request.structure;
    stamp(structure, ctx);
    const auto structure_id = boost::uuids::to_string(structure.id);

    repository::structure_repository structures;
    if (!structures.read_latest(ctx, structure_id).empty()) {
        response.result.outcome = outcome::conflict;
        response.result.code = "already_exists";
        response.result.message = "Structure " + structure_id + " is already made.";
        return response;
    }

    /*
     * Every leg must be a draft in a real book and free of another structure.
     * The checks run before anything is written, and the transaction is what
     * makes them hold afterwards: a leg that somebody groups elsewhere while the
     * completion runs makes its member write fail, and the whole unit rolls back.
     */
    repository::trade_repository trades;
    repository::trade_state_repository states;
    repository::structure_member_repository members;
    for (const auto& leg : request.legs) {
        const auto leg_id = boost::uuids::to_string(leg.trade_id);
        const auto held = trades.read_latest(ctx, leg_id);
        if (held.empty()) {
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            response.result.message = "Trade " + leg_id + " is not booked.";
            return response;
        }
        if (held.front().booking_nature != domain::booking_nature::actual) {
            response.result.outcome = outcome::conflict;
            response.result.code = "not_actual";
            response.result.message = "Trade " + leg_id +
                                      " is not an actual trade, so it completes no structure.";
            return response;
        }
        const auto state = states.read_latest(ctx, leg_id);
        if (state.empty()) {
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            response.result.message = "Trade " + leg_id + " has no state to complete.";
            return response;
        }
        const auto draft = ores::database::repository::execute_parameterized_string_query(
            ctx,
            "select ores_trading_status_is_draft_fn($1::uuid)::text",
            {boost::uuids::to_string(state.front().status_id)},
            lg(),
            "Checking whether a leg is a draft");
        if (draft.empty() || draft.front() != "true") {
            response.result.outcome = outcome::conflict;
            response.result.code = "not_draft";
            response.result.message = "Trade " + leg_id + " is not a draft, so it completes nothing.";
            return response;
        }
        if (!members.read_latest(ctx, leg_id).empty()) {
            response.result.outcome = outcome::conflict;
            response.result.code = "already_grouped";
            response.result.message = "Trade " + leg_id + " is already a leg of a structure.";
            return response;
        }
    }

    // The structure is written through its own service, which is what names it:
    // a deal that a person then reads carries a reference rather than a uuid.
    messaging::put_structure_request written_request;
    written_request.change.write = structure;
    written_request.change.precondition = {precondition_kind::must_not_exist, std::nullopt};
    written_request.intent.reason_code = request.change_reason_code;
    written_request.intent.commentary = request.change_commentary;
    const auto written = structure_service(ctx).put_structure(written_request);
    if (written.result.outcome != outcome::ok || !written.structure) {
        response.result = written.result;
        return response;
    }

    int sequence = 0;
    for (const auto& leg : request.legs) {
        auto member = leg;
        member.structure_id = structure.id;
        member.sequence_number = ++sequence;
        member.change_reason_code = request.change_reason_code;
        member.change_commentary = request.change_commentary;
        stamp(member, ctx);
        members.write(ctx, member, {precondition_kind::must_not_exist, std::nullopt});
    }

    // Each leg goes live in this same unit, so a leg is never live without its
    // structure and the structure is never made of drafts.
    std::vector<raised_authorisation> raised;
    for (const auto& leg : request.legs) {
        auto one = execute_within(ctx, leg.trade_id, request.change_reason_code,
                                  request.change_commentary);
        if (one)
            raised.push_back(std::move(*one));
    }

    uow.commit();

    for (const auto& one : raised) {
        try {
            operational_authorisation_service(ctx_).announce(one);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "A leg of structure " << structure_id
                                      << " was completed but its deciders were not told: "
                                      << e.what();
        }
    }

    response.structure_id = structure.id;
    response.structure_version = written.structure->version;
    return response;
}

messaging::put_bond_issue_composite_response trade_operations_service::put_bond_issue_composite(
    const messaging::put_bond_issue_composite_request& request) {
    using ores::database::repository::unit_of_work;
    using ores::service::messaging::stamp;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition;
    using ores::utility::domain::precondition_kind;

    auto issue = request.issue;
    auto legs = request.legs;
    auto amounts = request.amounts;
    auto rates = request.rates;
    auto schedules = request.schedules;
    auto schedule_dates = request.schedule_dates;
    auto amortizations = request.amortizations;
    auto call_dates = request.call_dates;
    auto conversion_targets = request.conversion_targets;

    stamp(issue, ctx_);
    stamp_rows(legs, ctx_);
    stamp_rows(amounts, ctx_);
    stamp_rows(rates, ctx_);
    stamp_rows(schedules, ctx_);
    stamp_rows(schedule_dates, ctx_);
    stamp_rows(amortizations, ctx_);
    stamp_rows(call_dates, ctx_);
    stamp_rows(conversion_targets, ctx_);
    BOOST_LOG_SEV(lg(), debug) << "Creating bond issue " << issue.issue_id;

    messaging::put_bond_issue_composite_response response;
    const auto issue_key = boost::uuids::to_string(issue.issue_id);

    // The answer for an issue the store already holds: equal terms are
    // already_exists, and other terms are content_differs. Neither writes.
    const auto answer_if_stored = [&](const ores::database::context& ctx)
        -> std::optional<messaging::put_bond_issue_composite_response> {
        repository::bond_issue_repository issues;
        const auto stored = issues.read_latest(ctx, issue_key);
        if (stored.empty())
            return std::nullopt;

        const std::vector<std::string> keys{issue_key};
        const bool same =
            same_terms(stored.front(), issue) &&
            same_rows(repository::read_issue_legs_by_issue_ids(ctx, keys), legs) &&
            same_rows(repository::read_issue_leg_amounts_by_issue_ids(ctx, keys), amounts) &&
            same_rows(repository::read_issue_leg_rates_by_issue_ids(ctx, keys), rates) &&
            same_rows(repository::read_issue_schedules_by_issue_ids(ctx, keys), schedules) &&
            same_rows(repository::read_issue_schedule_dates_by_issue_ids(ctx, keys),
                      schedule_dates) &&
            same_rows(repository::read_issue_leg_amortizations_by_issue_ids(ctx, keys),
                      amortizations) &&
            same_rows(repository::read_call_dates_by_issue_ids(ctx, keys), call_dates) &&
            same_rows(repository::read_conversion_targets_by_issue_ids(ctx, keys),
                      conversion_targets);
        messaging::put_bond_issue_composite_response answer;
        answer.result.outcome = outcome::conflict;
        answer.result.code = same ? "already_exists" : "content_differs";
        answer.result.message = "Bond issue " + issue_key +
                                (same ? " is already stored." : " is stored with other terms.");
        BOOST_LOG_SEV(lg(), info) << "Bond issue " << issue_key << " answered "
                                  << answer.result.code << ".";
        return answer;
    };

    try {
        // One transaction writes the issue and every child row, so a refusal
        // in any of them leaves no issue. A read made through the transaction
        // joins it without ending it.
        unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();

        if (auto answer = answer_if_stored(ctx))
            return *answer;

        const precondition must_not_exist{precondition_kind::must_not_exist, std::nullopt};
        issue.version = 0;
        repository::bond_issue_repository issues;
        issues.write(ctx, issue, must_not_exist);

        repository::bond_issue_leg_repository leg_repository;
        create_rows(leg_repository, ctx, legs, must_not_exist);
        repository::bond_issue_leg_amount_repository amount_repository;
        create_rows(amount_repository, ctx, amounts, must_not_exist);
        repository::bond_issue_leg_rate_repository rate_repository;
        create_rows(rate_repository, ctx, rates, must_not_exist);
        repository::bond_issue_leg_schedule_repository schedule_repository;
        create_rows(schedule_repository, ctx, schedules, must_not_exist);
        repository::bond_issue_leg_schedule_date_repository schedule_date_repository;
        create_rows(schedule_date_repository, ctx, schedule_dates, must_not_exist);
        repository::bond_issue_leg_amortization_repository amortization_repository;
        create_rows(amortization_repository, ctx, amortizations, must_not_exist);
        repository::bond_issue_call_date_repository call_date_repository;
        create_rows(call_date_repository, ctx, call_dates, must_not_exist);
        repository::bond_issue_conversion_target_repository conversion_target_repository;
        create_rows(conversion_target_repository, ctx, conversion_targets, must_not_exist);

        uow.commit();
        BOOST_LOG_SEV(lg(), info) << "Created bond issue " << issue_key << " with " << legs.size()
                                  << " legs, " << amounts.size() << " amounts, " << rates.size()
                                  << " rates, " << schedules.size() << " schedules, "
                                  << schedule_dates.size() << " schedule dates, "
                                  << amortizations.size() << " amortisations, " << call_dates.size()
                                  << " call dates and " << conversion_targets.size()
                                  << " conversion targets.";
        return response;
    } catch (const std::exception&) {
        // A parallel write of the same issue can pass the read above and fail
        // at the store. The transaction is gone by now, so a new one reads what
        // the other write left. A failure with nothing stored is the caller's.
        unit_of_work retry(ctx_);
        if (auto answer = answer_if_stored(retry.ctx()))
            return *answer;
        throw;
    }
}

}
