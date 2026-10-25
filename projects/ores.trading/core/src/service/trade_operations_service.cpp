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
#include "ores.trading.core/repository/trade_state_repository.hpp"
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

trade_operations_service::trade_operations_service(context ctx)
    : ctx_(std::move(ctx)) {}

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
trade_operations_service::book_trade(const messaging::book_trade_request& request) {
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
