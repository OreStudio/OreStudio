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
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/account_role_protocol.hpp"
#include "ores.iam.api/messaging/role_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/netting_set_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_counterparty_protocol.hpp"
#include "ores.trading.api/messaging/bond_instrument_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <format>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

namespace {

namespace domain = ores::synthetic::domain;
using ores::utility::domain::outcome;

constexpr std::uint32_t page = 10000;

/*
 * The reason a generated trade states. It is a system reason and not a
 * customer's: the digest fold reads it to decide whether an amendment moved
 * the external version, and a population that claimed an agreement would
 * report every generated trade as customer-amended.
 */
constexpr std::string_view population_reason = "system.initial_load";

/*
 * The scheme the population's own identifier travels under. UTI and USI are
 * the regimes' identifiers and ORE is the engine's; the firm's own name for a
 * trade is the Internal one.
 */
constexpr std::string_view internal_id_type = "Internal";

/*
 * The coupon a generated bond states. A fixed rate is what a bond's terms
 * carry, and the engine refuses a fixed leg that states none.
 */
constexpr double coupon_rate = 0.05;

/*
 * The face value of a generated security. A trade's size is the instrument's
 * notional, which the engine reads as the bond notional, so the security
 * itself states a unit face and carries no trade's size.
 */
constexpr double security_face_value = 1.0;

}

trading_trade_population_writer::trading_trade_population_writer(
    ores::nats::service::nats_client& nats)
    : nats_(nats) {}

template <typename Request>
bool trading_trade_population_writer::call_ok(std::string_view subject,
                                              const Request& request,
                                              const std::string& what) {
    const auto& codec = ores::nats::default_wire_codec();
    const auto reply = nats_.authenticated_request(subject, codec.encode(request));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(
            std::format("Writing {} was refused: {}", what, error->second));

    auto decoded = codec.decode<typename Request::response_type>(reply.data);
    if (!decoded)
        throw trade_population_write_error(std::format(
            "Writing {} answered something unreadable: {}", what, decoded.error().what()));
    if (decoded->result.outcome != outcome::ok)
        throw trade_population_write_error(
            std::format("Writing {} failed: {}", what, decoded->result.message));
    return true;
}

domain::trade_generation_scope
trading_trade_population_writer::resolve_scope(const boost::uuids::uuid& config_id) {
    const auto& codec = ores::nats::default_wire_codec();
    /*
     * One read helper for every reference this run resolves. A refusal names
     * the subject, because the caller's next move is to ask why the tenant's
     * own data is not readable rather than to change the config.
     */
    const auto read = [&](const auto& request) {
        using request_type = std::decay_t<decltype(request)>;
        const auto reply =
            nats_.authenticated_request(request_type::nats_subject, codec.encode(request));
        const auto error = reply.headers.find("X-Error");
        if (error != reply.headers.end())
            throw trade_population_write_error(
                std::format("{} was refused: {}", request_type::nats_subject, error->second));
        auto decoded = codec.decode<typename request_type::response_type>(reply.data);
        if (!decoded)
            throw trade_population_write_error(std::format("{} answered something unreadable: {}",
                                                           request_type::nats_subject,
                                                           decoded.error().what()));
        return *decoded;
    };

    namespace synthetic = ores::synthetic::messaging;

    synthetic::list_trade_generation_configs_request config_req;
    config_req.limit = page;
    synthetic::trade_generation_configs_filter config_filter;
    config_filter.id_one_of = std::vector<boost::uuids::uuid>{config_id};
    config_req.filter = config_filter;
    const auto configs = read(config_req);
    if (configs.trade_generation_configs.empty())
        throw trade_population_write_error("No trade generation config " +
                                           boost::uuids::to_string(config_id) +
                                           " is on the books.");
    const auto& config = configs.trade_generation_configs.front();

    domain::trade_generation_scope scope;
    scope.config_id = config.id;
    scope.party_id = config.party_id;
    scope.asset_class_code = config.asset_class_code;
    scope.min_tenor_code = config.min_tenor_code;
    scope.max_tenor_code = config.max_tenor_code;
    scope.min_notional = config.min_notional.to_double();
    scope.max_notional = config.max_notional.to_double();
    scope.as_of_date = config.as_of_date;
    scope.seed = config.seed;
    scope.reference_curve_id = config.reference_curve_id;

    synthetic::list_trade_generation_books_request book_scope_req;
    book_scope_req.limit = page;
    synthetic::trade_generation_books_filter book_scope_filter;
    book_scope_filter.config_id = config_id;
    book_scope_req.filter = book_scope_filter;
    for (const auto& row : read(book_scope_req).trade_generation_books)
        scope.book_ids.push_back(row.book_id);

    synthetic::list_trade_generation_counterparties_request cp_scope_req;
    cp_scope_req.limit = page;
    synthetic::trade_generation_counterparties_filter cp_scope_filter;
    cp_scope_filter.config_id = config_id;
    cp_scope_req.filter = cp_scope_filter;
    for (const auto& row : read(cp_scope_req).trade_generation_counterparties)
        scope.counterparty_ids.push_back(row.counterparty_id);

    /*
     * The books are checked against the tenant's own, so a scope naming a book
     * that does not exist fails here rather than at the booking trigger, which
     * would report it as the trade's fault.
     */
    ores::refdata::messaging::list_books_request books_req;
    books_req.limit = page;
    const auto books = read(books_req);
    for (const auto& book_id : scope.book_ids) {
        const auto known = std::ranges::any_of(
            books.books, [&](const auto& candidate) { return candidate.id == book_id; });
        if (!known)
            throw trade_population_write_error("Book " + boost::uuids::to_string(book_id) +
                                               " is in the config's scope and is not a book of "
                                               "this tenant.");
    }
    /*
     * The currency the population is kept in is the first book's own, because a
     * book is where a currency is decided and the config does not state one.
     */
    for (const auto& book : books.books) {
        if (book.id == scope.book_ids.front() && !book.functional_currency.empty()) {
            scope.currency_code = book.functional_currency;
            break;
        }
    }

    ores::refdata::messaging::list_counterparties_request cp_req;
    cp_req.limit = page;
    const auto counterparties = read(cp_req);
    for (const auto& counterparty : counterparties.counterparties)
        scope.issuer_ids.push_back(counterparty.id);
    for (const auto& counterparty_id : scope.counterparty_ids) {
        const auto known =
            std::ranges::any_of(counterparties.counterparties, [&](const auto& candidate) {
                return candidate.id == counterparty_id;
            });
        if (!known)
            throw trade_population_write_error(
                "Counterparty " + boost::uuids::to_string(counterparty_id) +
                " is in the config's scope and is not a counterparty of this tenant.");
    }

    /*
     * The netting set is not a fact of the config: it belongs to the party and
     * the counterparty together, which is what the booking trigger checks. So
     * it is read from the sets the party actually clears against and a
     * counterparty with none simply books without one.
     */
    ores::refdata::messaging::list_netting_sets_request sets_req;
    sets_req.limit = page;
    const auto sets = read(sets_req);
    for (const auto& set : sets.netting_sets) {
        if (set.party_id != scope.party_id || !set.counterparty_id)
            continue;
        scope.netting_sets.emplace_back(*set.counterparty_id, set.id);
    }

    /*
     * The person who may enter a trade comes from the tenant's own staff: the
     * party's accounts that hold the Trading role. A party with none is a party
     * whose trades cannot name an author, so the run stops rather than writing
     * an anonymous one.
     */
    ores::iam::messaging::list_accounts_request accounts_req;
    accounts_req.limit = page;
    const auto accounts = read(accounts_req);

    /*
     * The person who may enter a trade comes from the tenant's own staff: the
     * accounts the tenant holds that carry the booking party as their own. A
     * party with none is a party whose trades cannot name an author, so the run
     * stops rather than writing an anonymous one.
     */
    for (const auto& account : accounts.accounts) {
        if (account.account_type != "user")
            continue;
        if (!account.default_party_id || *account.default_party_id != scope.party_id)
            continue;
        scope.trader_usernames.push_back(account.username);
    }

    return scope;
}

std::optional<trade_population_stored>
trading_trade_population_writer::read_trade(const boost::uuids::uuid& trade_id) {
    const auto& codec = ores::nats::default_wire_codec();
    ores::trading::messaging::list_trades_request req;
    req.limit = 1;
    ores::trading::messaging::trades_filter filter;
    filter.id_one_of = std::vector<boost::uuids::uuid>{trade_id};
    req.filter = filter;

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::list_trades_request::nats_subject, codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(std::format(
            "Reading trade {} was refused: {}", boost::uuids::to_string(trade_id), error->second));
    auto decoded = codec.decode<ores::trading::messaging::list_trades_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Reading trade {} answered something unreadable: {}",
                        boost::uuids::to_string(trade_id),
                        decoded.error().what()));
    if (decoded->trades.empty())
        return std::nullopt;
    return trade_population_stored{
        .version = static_cast<std::uint32_t>(decoded->trades.front().version),
        .external_version = static_cast<std::uint32_t>(decoded->trades.front().external_version)};
}

void trading_trade_population_writer::ensure_issue(const trade_population_write& write) {
    if (issues_written_.contains(write.issue_id))
        return;

    const auto& slot = write.slot;
    const auto& codec = ores::nats::default_wire_codec();
    const std::string commentary = "Generated trade population.";
    const auto reason = std::string(population_reason);

    const auto face_value =
        ores::utility::decimal::decimal::from_double(security_face_value).value();

    ores::trading::messaging::put_bond_issue_composite_request req;
    auto& issue = req.issue;
    issue.issue_id = write.issue_id;
    issue.security_id = write.security_id;
    issue.issuer = boost::uuids::to_string(slot.issuer_id);
    issue.face_value = face_value;
    issue.issue_date = slot.issue_date;
    issue.settlement_days = 2;
    issue.calendar = slot.calendar;
    issue.sub_type = "Fixed";
    issue.price_type = "Clean";
    /*
     * The curve the bond is discounted on. The engine resolves the name
     * the trade states through the market's own forwarding, so a bond that
     * states none is refused with a curve it could not find rather than
     * priced.
     */
    if (!slot.reference_curve_id.empty())
        issue.reference_curve_id = slot.reference_curve_id;

    ores::trading::domain::bond_issue_leg leg;
    leg.issue_id = write.issue_id;
    leg.leg_number = 1;
    leg.payer = false;
    leg.leg_type = "Fixed";
    leg.currency = slot.currency;
    leg.payment_convention = "F";
    leg.day_counter = "ACT/ACT";
    req.legs.push_back(leg);

    ores::trading::domain::bond_issue_leg_amount notional;
    notional.issue_id = write.issue_id;
    notional.leg_number = 1;
    notional.amount_role = "notional";
    notional.sequence_number = 1;
    notional.value = face_value;
    req.amounts.push_back(notional);

    /*
     * The coupon: a fixed leg states a rate and no index, and the reader takes
     * the rate itself from the amount list under the "rate" role. Without both,
     * the leg the engine rebuilds has no FixedLegData and refuses to price.
     */
    ores::trading::domain::bond_issue_leg_rate rate;
    rate.issue_id = write.issue_id;
    rate.leg_number = 1;
    rate.rate_kind = "fixed";
    req.rates.push_back(rate);

    ores::trading::domain::bond_issue_leg_amount coupon;
    coupon.issue_id = write.issue_id;
    coupon.leg_number = 1;
    coupon.amount_role = "rate";
    coupon.sequence_number = 2;
    coupon.value = ores::utility::decimal::decimal::from_double(coupon_rate).value();
    req.amounts.push_back(coupon);

    /*
     * The schedule is a rule and not a date list, so the terms stay relative
     * to the anchor: the tenor and the convention are what the document
     * states, and the dates follow from them.
     */
    ores::trading::domain::bond_issue_leg_schedule schedule;
    schedule.issue_id = write.issue_id;
    schedule.leg_number = 1;
    schedule.schedule_role = "schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = slot.issue_date;
    schedule.end_date = slot.maturity_date;
    schedule.tenor = "1Y";
    schedule.calendar = slot.calendar;
    schedule.convention = "F";
    schedule.term_convention = "F";
    schedule.rule = "Forward";
    req.schedules.push_back(schedule);

    const auto mark = [&](auto& row) {
        row.change_reason_code = reason;
        row.change_commentary = commentary;
    };
    mark(req.issue);
    for (auto& row : req.legs)
        mark(row);
    for (auto& row : req.amounts)
        mark(row);
    for (auto& row : req.rates)
        mark(row);
    for (auto& row : req.schedules)
        mark(row);

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::put_bond_issue_composite_request::nats_subject,
        codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(
            std::format("Writing bond issue {} was refused: {}", write.security_id, error->second));
    auto decoded =
        codec.decode<ores::trading::messaging::put_bond_issue_composite_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Writing bond issue {} answered something unreadable: {}",
                        write.security_id,
                        decoded.error().what()));

    /*
     * The issue is whole or absent, so a stored issue with equal terms is the
     * issue this run needs. Stored with other terms, it is a different
     * security under the same id, and the run stops.
     */
    const auto& result = decoded->result;
    const bool present = result.outcome == outcome::conflict && result.code == "already_exists";
    if (result.outcome != outcome::ok && !present)
        throw trade_population_write_error(
            std::format("Writing bond issue {} failed: {}", write.security_id, result.message));
    issues_written_.insert(write.issue_id);
}

void trading_trade_population_writer::write_trade(const trade_population_write& write,
                                                  const domain::trade_generation_scope& scope) {
    const auto& codec = ores::nats::default_wire_codec();
    const auto& slot = write.slot;

    /*
     * The security is in place before the trade is booked. A run cut short in
     * the issue then leaves no trade, so the next run writes the issue again
     * and not a booked trade that no run revisits.
     */
    ensure_issue(write);

    ores::trading::messaging::book_trade_request book_req;
    book_req.anchor.id = slot.trade_id;
    book_req.anchor.party_id = scope.party_id;
    book_req.anchor.counterparty_id = slot.counterparty_id;
    book_req.anchor.trade_type = slot.trade_type_code;
    book_req.anchor.counterparty_scope = ores::trading::domain::counterparty_scope::external;
    book_req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    book_req.anchor.entry_channel = ores::trading::domain::entry_channel::stp;
    book_req.anchor.modified_by = slot.trader_username;
    book_req.anchor.performed_by = slot.trader_username;
    book_req.anchor.change_reason_code = std::string(population_reason);
    book_req.anchor.change_commentary = "Generated trade population.";
    book_req.booking.book_id = slot.book_id;
    book_req.booking.netting_set_id = slot.netting_set_id;
    book_req.booking.trade_date = slot.trade_date;
    book_req.booking.modified_by = slot.trader_username;
    book_req.booking.performed_by = slot.trader_username;
    book_req.booking.change_reason_code = std::string(population_reason);
    book_req.booking.change_commentary = "Generated trade population.";
    book_req.activity_type_code = "new_booking";

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::book_trade_request::nats_subject, codec.encode(book_req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(std::format("Booking trade {} was refused: {}",
                                                       boost::uuids::to_string(slot.trade_id),
                                                       error->second));
    auto booked = codec.decode<ores::trading::messaging::book_trade_response>(reply.data);
    if (!booked)
        throw trade_population_write_error(
            std::format("Booking trade {} answered something unreadable.",
                        boost::uuids::to_string(slot.trade_id)));
    if (booked->result.outcome != outcome::ok)
        throw trade_population_write_error(std::format("Booking trade {} failed: {}",
                                                       boost::uuids::to_string(slot.trade_id),
                                                       booked->result.message));
    if (!booked->activity_id)
        throw trade_population_write_error(
            std::format("Booking trade {} named no activity, so nothing can be stamped with it.",
                        boost::uuids::to_string(slot.trade_id)));
    activities_[slot.trade_id] = *booked->activity_id;

    /*
     * The identifier is written beside the trade and not after it, because the
     * identifier is a composite child: its write bumps its parent, so a run
     * that booked every trade first and identified them afterwards would leave
     * every trade standing at a version its own identifier had moved.
     */
    ores::trading::messaging::put_trade_identifier_request identifier_req;
    identifier_req.change.write.trade_id = slot.trade_id;
    identifier_req.change.write.id_type = std::string(internal_id_type);
    identifier_req.change.write.trade_activity_id = *booked->activity_id;
    identifier_req.change.write.id_value = slot.internal_identifier;
    identifier_req.change.write.issuing_party_id = scope.party_id;
    identifier_req.change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                          std::nullopt};
    identifier_req.intent.reason_code = std::string(population_reason);
    identifier_req.intent.commentary = "Generated trade population.";

    call_ok(ores::trading::messaging::put_trade_identifier_request::nats_subject,
            identifier_req,
            "the Internal identifier of trade " + boost::uuids::to_string(slot.trade_id));
}

void trading_trade_population_writer::write_instrument(const trade_population_write& write,
                                                       const domain::trade_generation_scope&) {
    const auto& slot = write.slot;
    const auto activity = activities_.find(slot.trade_id);
    if (activity == activities_.end())
        throw trade_population_write_error("Trade " + boost::uuids::to_string(slot.trade_id) +
                                           " has no booking activity to stamp its instrument "
                                           "with.");

    ores::trading::messaging::put_bond_instrument_request instrument_req;
    instrument_req.change.write.trade_id = slot.trade_id;
    instrument_req.change.write.trade_activity_id = activity->second;
    instrument_req.change.write.trade_type_code = slot.trade_type_code;
    instrument_req.change.write.issue_id = write.issue_id;
    instrument_req.change.write.notional =
        ores::utility::decimal::decimal::from_double(slot.notional).value();
    instrument_req.change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                          std::nullopt};
    instrument_req.intent.reason_code = std::string(population_reason);
    instrument_req.intent.commentary = "Generated trade population.";

    call_ok(ores::trading::messaging::put_bond_instrument_request::nats_subject,
            instrument_req,
            "the bond instrument of trade " + boost::uuids::to_string(slot.trade_id));
}

}
