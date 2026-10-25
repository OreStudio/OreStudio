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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/domain/origin.hpp"
#include "ores.trading.api/generators/bond_issue_call_date_generator.hpp"
#include "ores.trading.api/generators/bond_issue_conversion_target_generator.hpp"
#include "ores.trading.api/generators/bond_issue_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_amortization_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_amount_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_schedule_date_generator.hpp"
#include "ores.trading.api/generators/fx_forward_instrument_generator.hpp"
#include "ores.trading.api/generators/rate_instrument_generator.hpp"
#include "ores.trading.api/generators/swap_leg_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.trading.core/repository/trade_authorisation_request_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include "ores.trading.core/service/trade_export_service.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string tags("[service]");

using ores::trading::messaging::book_trade_request;
using ores::trading::messaging::put_bond_issue_composite_request;
using ores::trading::service::book_status_port;
using ores::trading::service::trade_operations_service;
using ores::utility::domain::outcome;

namespace {

/** A port that answers the same way for every book, so a case states its rule. */
class stated_book_status final : public book_status_port {
public:
    explicit stated_book_status(state answer)
        : answer_(answer) {}
    [[nodiscard]] state of(const boost::uuids::uuid&) const override {
        return answer_;
    }

private:
    state answer_;
};

const stated_book_status every_book_open{book_status_port::state::open};

}

/**
 * @brief A tenant with a party in session and a real book of that party.
 */
struct fixture final {
    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gen = ores::testing::make_generation_context(h);
    ores::database::context ctx = h.context();
    boost::uuids::uuid book_id{};

    fixture() {
        namespace gen_rd = ores::refdata::generators;
        namespace repo = ores::refdata::repository;

        // The context acts as an account of the tenant, so a raise finds the
        // account it raises for. A service account leaves the tenant's
        // bootstrap alone.
        auto maker = ores::iam::generators::generate_synthetic_account(gen);
        maker.account_type = "service";
        maker.change_reason_code = "system.test";
        ores::iam::repository::account_repository{}.write(h.context(), maker);

        auto party = gen_rd::generate_synthetic_party(gen);
        party.change_reason_code = "system.test";
        for (const auto& e : repo::party_repository().read_latest(h.context())) {
            if (e.tenant_id == party.tenant_id) {
                party.parent_party_id = e.id;
                break;
            }
        }
        repo::party_repository().write(h.context(), party);
        ctx = h.context().with_party(h.tenant_id(), party.id, {party.id}, maker.username);

        auto currency = gen_rd::generate_synthetic_currency(gen);
        repo::currency_repository().write(ctx, {currency});

        auto portfolio = gen_rd::generate_synthetic_portfolio(gen);
        portfolio.party_id = party.id;
        portfolio.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, portfolio);

        auto book = gen_rd::generate_synthetic_book(gen);
        book.party_id = party.id;
        book.parent_portfolio_id = portfolio.id;
        book.functional_currency = currency.iso_code;
        book.change_reason_code = "system.test";
        repo::book_repository().write(ctx, book);
        book_id = book.id;
    }

    put_bond_issue_composite_request issue_request() {
        namespace gen_tr = ores::trading::generators;
        put_bond_issue_composite_request r;
        r.issue = gen_tr::generate_synthetic_bond_issue(gen);
        r.issue.change_reason_code = "system.test";
        auto leg = gen_tr::generate_synthetic_bond_issue_leg(gen);
        leg.issue_id = r.issue.issue_id;
        leg.leg_number = 1;
        leg.change_reason_code = "system.test";
        r.legs.push_back(leg);
        auto amount = gen_tr::generate_synthetic_bond_issue_leg_amount(gen);
        amount.issue_id = r.issue.issue_id;
        amount.leg_number = 1;
        amount.sequence_number = 1;
        amount.change_reason_code = "system.test";
        r.amounts.push_back(amount);
        return r;
    }

    // The request with a row of each optional child type, so a comparison that
    // skipped one of them would pass the same-terms test and fail the other.
    put_bond_issue_composite_request full_issue_request() {
        namespace gen_tr = ores::trading::generators;
        auto r = issue_request();

        auto schedule_date = gen_tr::generate_synthetic_bond_issue_leg_schedule_date(gen);
        schedule_date.issue_id = r.issue.issue_id;
        schedule_date.leg_number = 1;
        schedule_date.schedule_role = "schedule";
        schedule_date.schedule_sequence_number = 1;
        schedule_date.sequence_number = 1;
        schedule_date.change_reason_code = "system.test";
        r.schedule_dates.push_back(schedule_date);

        auto amortization = gen_tr::generate_synthetic_bond_issue_leg_amortization(gen);
        amortization.issue_id = r.issue.issue_id;
        amortization.leg_number = 1;
        amortization.sequence_number = 1;
        amortization.change_reason_code = "system.test";
        r.amortizations.push_back(amortization);

        auto call_date = gen_tr::generate_synthetic_bond_issue_call_date(gen);
        call_date.issue_id = r.issue.issue_id;
        call_date.sequence_number = 1;
        call_date.change_reason_code = "system.test";
        r.call_dates.push_back(call_date);

        auto conversion_target = gen_tr::generate_synthetic_bond_issue_conversion_target(gen);
        conversion_target.issue_id = r.issue.issue_id;
        conversion_target.sequence_number = 1;
        conversion_target.change_reason_code = "system.test";
        r.conversion_targets.push_back(conversion_target);
        return r;
    }

    // A bond trade of an issue the store holds, with one identifier.
    book_trade_request bond_request(const boost::uuids::uuid& issue_id) {
        auto r = request();
        r.anchor.trade_type = "Bond";

        ores::trading::domain::trade_identifier identifier;
        identifier.id_type = "Internal";
        identifier.id_value = "TEST-" + boost::uuids::to_string(r.anchor.id);
        identifier.change_reason_code = "system.test";
        identifier.modified_by = r.booking.modified_by;
        identifier.performed_by = r.booking.modified_by;
        r.identifiers.push_back(identifier);

        ores::trading::domain::bond_instrument_data bond;
        bond.instrument.issue_id = issue_id;
        bond.instrument.audit.change_reason_code = "system.test";
        r.instruments.bond_instruments.push_back(bond);
        return r;
    }

    boost::uuids::uuid stored_issue() {
        const auto issue = issue_request();
        trade_operations_service(ctx, every_book_open).put_bond_issue_composite(issue);
        return issue.issue.issue_id;
    }

    book_trade_request request() {
        book_trade_request r;
        r.anchor = ores::trading::generators::generate_synthetic_trade(gen);
        r.booking = ores::trading::generators::generate_synthetic_trade_booking(gen);
        r.booking.book_id = book_id;
        r.booking.change_reason_code = "system.test";
        r.activity_type_code = "new_booking";
        return r;
    }
};

}

TEST_CASE("book_trade_writes_the_anchor_the_booking_and_the_state", tags) {
    fixture f;
    auto req = f.request();
    const auto executed_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    req.booking.execution_timestamp = executed_at;
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    REQUIRE(response.result.outcome == outcome::ok);
    REQUIRE(ores::trading::repository::trade_repository().read_latest(f.ctx, id).size() == 1);
    const auto bookings =
        ores::trading::repository::trade_booking_repository().read_latest(f.ctx, id);
    REQUIRE(bookings.size() == 1);
    CHECK(bookings.front().book_id == f.book_id);
    REQUIRE(bookings.front().execution_timestamp.has_value());
    CHECK(*bookings.front().execution_timestamp == executed_at);
    const auto states = ores::trading::repository::trade_state_repository().read_latest(f.ctx, id);
    REQUIRE(states.size() == 1);
    REQUIRE(response.activity_id.has_value());
    CHECK(bookings.front().trade_activity_id == *response.activity_id);
    CHECK(states.front().trade_activity_id == *response.activity_id);
}

TEST_CASE("a_booking_into_a_book_that_is_not_open_is_refused", tags) {
    fixture f;
    const stated_book_status frozen{book_status_port::state::not_open};
    auto request = f.request();
    const auto response = trade_operations_service(f.ctx, frozen).book_trade(request);
    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "book_not_open");
}

TEST_CASE("a_second_booking_of_the_same_trade_is_a_conflict", tags) {
    fixture f;
    const auto req = f.request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req).result.outcome ==
            outcome::ok);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "already_exists");
    const auto bookings = ores::trading::repository::trade_booking_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));
    REQUIRE(bookings.size() == 1);
    CHECK(bookings.front().version == 1);
}

TEST_CASE("a_booking_that_fails_writes_no_anchor", tags) {
    fixture f;
    auto req = f.request();
    req.booking.book_id = f.gen.generate_uuid();

    CHECK_THROWS(trade_operations_service(f.ctx, every_book_open).book_trade(req));

    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("put_bond_issue_composite_writes_the_issue_and_its_children", tags) {
    fixture f;
    const auto req = f.issue_request();

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    REQUIRE(response.result.outcome == outcome::ok);
    CHECK(ores::trading::repository::bond_issue_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.issue.issue_id))
              .size() == 1);
}

TEST_CASE("a_second_write_of_the_same_issue_is_already_exists", tags) {
    fixture f;
    const auto req = f.issue_request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open)
                .put_bond_issue_composite(req)
                .result.outcome == outcome::ok);

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "already_exists");
}

TEST_CASE("a_second_write_with_other_terms_is_content_differs", tags) {
    fixture f;
    auto req = f.issue_request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open)
                .put_bond_issue_composite(req)
                .result.outcome == outcome::ok);
    req.issue.issuer = req.issue.issuer + "-other";

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "content_differs");
    const auto stored = ores::trading::repository::bond_issue_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.issue.issue_id));
    REQUIRE(stored.size() == 1);
    CHECK(stored.front().issuer != req.issue.issuer);
}

TEST_CASE("a_refused_child_write_leaves_no_issue", tags) {
    fixture f;
    auto req = f.issue_request();
    req.amounts.front().sequence_number = 0;

    CHECK_THROWS(trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req));

    CHECK(ores::trading::repository::bond_issue_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.issue.issue_id))
              .empty());
}

TEST_CASE("a_second_write_of_an_issue_with_every_child_type_is_already_exists", tags) {
    fixture f;
    const auto req = f.full_issue_request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open)
                .put_bond_issue_composite(req)
                .result.outcome == outcome::ok);

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "already_exists");
}

TEST_CASE("a_change_in_any_optional_child_is_content_differs", tags) {
    const auto change = GENERATE(0, 1, 2, 3);
    fixture f;
    auto req = f.full_issue_request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open)
                .put_bond_issue_composite(req)
                .result.outcome == outcome::ok);

    switch (change) {
        case 0:
            req.schedule_dates.front().schedule_date += std::chrono::months(1);
            break;
        case 1:
            req.amortizations.front().amortization_type += "-other";
            break;
        case 2:
            req.call_dates.front().call_date += std::chrono::months(1);
            break;
        default:
            req.conversion_targets.front().underlying_id += "-other";
            break;
    }

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "content_differs");
}

TEST_CASE("a_write_with_a_child_less_than_the_stored_issue_is_content_differs", tags) {
    fixture f;
    auto req = f.full_issue_request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open)
                .put_bond_issue_composite(req)
                .result.outcome == outcome::ok);
    req.call_dates.clear();

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "content_differs");
}

namespace {

/**
 * @brief The requests that cover one trade. Tests share a tenant, so the tenant's
 * whole list holds the requests of other tests too.
 */
std::vector<ores::trading::domain::trade_authorisation_request>
requests_for(const ores::database::context& ctx, const boost::uuids::uuid& trade_id) {
    std::vector<ores::trading::domain::trade_authorisation_request> out;
    for (auto& r :
         ores::trading::repository::trade_authorisation_request_repository().read_latest(ctx)) {
        if (r.trade_id == trade_id)
            out.push_back(std::move(r));
    }
    return out;
}

}

TEST_CASE("an_actual_trade_booked_live_raises_one_authorisation_request", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;

    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req).result.outcome ==
            outcome::ok);

    const auto raised = requests_for(f.ctx, req.anchor.id);
    REQUIRE(raised.size() == 1);
    CHECK(raised.front().trade_version >= 1);
}

TEST_CASE("a_test_trade_booked_live_raises_no_authorisation_request", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::test;

    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req).result.outcome ==
            outcome::ok);

    CHECK(requests_for(f.ctx, req.anchor.id).empty());
}

TEST_CASE("the_summary_of_a_booked_actual_trade_names_its_state_book_and_waiting_request", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req).result.outcome ==
            outcome::ok);

    const auto rows = ores::trading::repository::trade_summary_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));

    REQUIRE(rows.size() == 1);
    CHECK(rows.front().row_kind == "trade");
    CHECK(rows.front().status == std::optional<std::string>{"live"});
    CHECK(rows.front().book_id == std::optional<boost::uuids::uuid>{f.book_id});
    CHECK(rows.front().authorisation_status == "waiting");
}

TEST_CASE("the_summary_of_a_booked_test_trade_needs_no_authorisation", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::test;
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req).result.outcome ==
            outcome::ok);

    const auto rows = ores::trading::repository::trade_summary_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));

    REQUIRE(rows.size() == 1);
    CHECK(rows.front().authorisation_status == "not_required");
}

TEST_CASE("a_bond_trade_is_booked_with_its_identifier_and_instrument", tags) {
    fixture f;
    const auto issue_id = f.stored_issue();
    const auto req = f.bond_request(issue_id);
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    REQUIRE(response.result.outcome == outcome::ok);
    REQUIRE(response.activity_id.has_value());
    const auto identifiers =
        ores::trading::repository::trade_identifier_repository().read_latest(f.ctx, id, "Internal");
    REQUIRE(identifiers.size() == 1);
    CHECK(identifiers.front().trade_activity_id == *response.activity_id);
    CHECK(identifiers.front().id_value == req.identifiers.front().id_value);
    const auto instruments =
        ores::trading::repository::bond_instrument_repository().read_latest(f.ctx, id);
    REQUIRE(instruments.size() == 1);
    CHECK(instruments.front().identity.trade_activity_id == *response.activity_id);
    CHECK(instruments.front().identity.trade_type_code == "Bond");
    CHECK(instruments.front().issue_id == issue_id);
}

TEST_CASE("a_bond_trade_of_an_unknown_issue_leaves_no_trade", tags) {
    fixture f;
    const auto req = f.bond_request(f.gen.generate_uuid());

    CHECK_THROWS(trade_operations_service(f.ctx, every_book_open).book_trade(req));

    const auto id = boost::uuids::to_string(req.anchor.id);
    CHECK(ores::trading::repository::trade_repository().read_latest(f.ctx, id).empty());
    CHECK(ores::trading::repository::trade_identifier_repository()
              .read_latest(f.ctx, id, "Internal")
              .empty());
}

TEST_CASE("an_array_the_trade_type_does_not_hold_is_refused_by_name", tags) {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.instruments.fx_forward_instruments.emplace_back();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_not_routed");
    CHECK(response.result.message.contains("fx_forward_instruments"));
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("an_fx_forward_trade_is_booked_with_its_instrument", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.trade_type = "FxForward";
    req.instruments.fx_forward_instruments.push_back(
        ores::trading::generators::generate_synthetic_fx_forward_instrument(f.gen));
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    REQUIRE(response.result.outcome == outcome::ok);
    const auto stored =
        ores::trading::repository::fx_forward_instrument_repository().read_latest(f.ctx, id);
    REQUIRE(stored.size() == 1);
    CHECK(stored.front().identity.trade_id == req.anchor.id);
    CHECK(stored.front().identity.trade_activity_id == *response.activity_id);
}

TEST_CASE("a_swap_trade_is_booked_with_its_header_and_its_legs", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.trade_type = "Swap";
    req.instruments.rate_instruments.push_back(
        ores::trading::generators::generate_synthetic_rate_instrument(f.gen));
    for (int leg_number = 1; leg_number <= 2; ++leg_number) {
        auto leg = ores::trading::generators::generate_synthetic_swap_leg(f.gen);
        leg.identity.leg_number = leg_number;
        req.instruments.swap_legs.push_back(leg);
    }
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    REQUIRE(response.result.outcome == outcome::ok);
    CHECK(ores::trading::repository::rate_instrument_repository().read_latest(f.ctx, id).size() ==
          1);
    const auto legs =
        ores::trading::repository::swap_leg_repository().read_latest_by_trade_id(f.ctx, id, 0, 10);
    REQUIRE(legs.size() == 2);
    CHECK(legs.front().identity.trade_id == req.anchor.id);
}

TEST_CASE("a_bond_row_with_a_fact_row_is_refused_by_name", tags) {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.instruments.bond_instruments.front().option.emplace();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_not_supported");
    CHECK(response.result.message.contains("bond_instruments"));
}

TEST_CASE("a_bond_row_with_any_optional_member_is_refused_by_the_name_of_the_member", tags) {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.instruments.bond_instruments.front().option_data.emplace();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_not_supported");
    CHECK(response.result.message.contains("bond_instruments.option_data"));
}

TEST_CASE("a_bond_row_on_a_trade_of_another_type_is_refused_by_name", tags) {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.anchor.trade_type = "Swap";

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_not_routed");
    CHECK(response.result.message.contains("bond_instruments"));
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("an_exported_swap_trade_books_back_to_the_same_rows", tags) {
    fixture f;
    auto first = f.request();
    first.anchor.trade_type = "Swap";
    first.instruments.rate_instruments.push_back(
        ores::trading::generators::generate_synthetic_rate_instrument(f.gen));
    for (int leg_number = 1; leg_number <= 2; ++leg_number) {
        auto leg = ores::trading::generators::generate_synthetic_swap_leg(f.gen);
        leg.identity.leg_number = leg_number;
        first.instruments.swap_legs.push_back(leg);
    }
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(first).result.outcome ==
            outcome::ok);

    ores::trading::domain::instrument_batch exported;
    ores::trading::service::trade_export_service(f.ctx).export_books(
        {boost::uuids::to_string(f.book_id)}, 0, 100, exported);
    REQUIRE(ores::trading::domain::find(exported.rate_instruments, first.anchor.id) != nullptr);

    auto second = f.request();
    second.anchor.trade_type = "Swap";
    second.instruments.rate_instruments.push_back(
        *ores::trading::domain::find(exported.rate_instruments, first.anchor.id));
    second.instruments.swap_legs =
        ores::trading::domain::find_all(exported.swap_legs, first.anchor.id);
    REQUIRE(second.instruments.swap_legs.size() == 2);
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(second).result.outcome ==
            outcome::ok);

    ores::trading::domain::instrument_batch again;
    ores::trading::service::trade_export_service(f.ctx).export_books(
        {boost::uuids::to_string(f.book_id)}, 0, 100, again);
    const auto legs = ores::trading::domain::find_all(again.swap_legs, second.anchor.id);
    const auto original = ores::trading::domain::find_all(again.swap_legs, first.anchor.id);
    REQUIRE(legs.size() == original.size());
    for (std::size_t i = 0; i < legs.size(); ++i) {
        CHECK(legs[i].identity.leg_number == original[i].identity.leg_number);
        CHECK(legs[i].leg_type_code == original[i].leg_type_code);
        CHECK(legs[i].currency == original[i].currency);
        CHECK(legs[i].day_count_fraction_code == original[i].day_count_fraction_code);
    }
    CHECK(ores::trading::domain::find(again.rate_instruments, second.anchor.id) != nullptr);
}

TEST_CASE("a_child_array_with_no_header_is_refused_by_name", tags) {
    fixture f;
    auto req = f.request();
    req.anchor.trade_type = "Swap";
    req.instruments.swap_legs.push_back(
        ores::trading::generators::generate_synthetic_swap_leg(f.gen));

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_child_without_header");
    CHECK(response.result.message.contains("swap_legs"));
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("every_array_the_trade_type_does_not_hold_is_named_once", tags) {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.instruments.fx_forward_instruments.emplace_back();
    req.instruments.rate_instruments.emplace_back();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req);

    CHECK(response.result.code == "instrument_not_routed");
    CHECK(response.result.message.contains("fx_forward_instruments"));
    CHECK(response.result.message.contains("rate_instruments"));
}
