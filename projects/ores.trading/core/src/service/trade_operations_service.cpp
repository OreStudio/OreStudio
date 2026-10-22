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
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <optional>
#include <string>

namespace ores::trading::service {

using namespace ores::logging;

trade_operations_service::trade_operations_service(context ctx)
    : ctx_(std::move(ctx)) {}

namespace {

// The audit members are the store's to set, so two rows hold the same terms
// when they are equal once the incoming row takes the stored row's audit.
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

template <typename Row>
bool same_rows(const std::vector<Row>& stored, const std::vector<Row>& incoming) {
    if (stored.size() != incoming.size())
        return false;
    return std::ranges::all_of(incoming, [&](const Row& row) {
        return std::ranges::any_of(stored,
                                   [&](const Row& other) { return same_terms(other, row); });
    });
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

    // One transaction writes the anchor, the activity, the booking and the
    // state, so a failure in any of them leaves none of them. A read made
    // through the transaction joins it without ending it.
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

    uow.commit();

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

    // One transaction writes the issue and every child row, so a refusal in
    // any of them leaves no issue. A read made through the transaction joins
    // it without ending it.
    unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    repository::bond_issue_repository issues;
    const auto issue_key = boost::uuids::to_string(issue.issue_id);
    const auto stored = issues.read_latest(ctx, issue_key);
    if (!stored.empty()) {
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
        response.result.outcome = outcome::conflict;
        response.result.code = same ? "already_exists" : "content_differs";
        response.result.message = "Bond issue " + issue_key +
                                  (same ? " is already stored." :
                                          " is stored with other terms.");
        return response;
    }

    const precondition must_not_exist{precondition_kind::must_not_exist, std::nullopt};
    issue.version = 0;
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
    return response;
}

}
