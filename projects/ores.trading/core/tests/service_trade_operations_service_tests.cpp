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
#include "ores.database/query_builder/execute.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.logging/make_logger.hpp"
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
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/instrument_underlying.hpp"
#include "ores.trading.api/domain/origin.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include "ores.trading.api/domain/scripted_parameter.hpp"
#include "ores.trading.api/domain/scripted_parameter_value.hpp"
#include "ores.trading.api/generators/bond_issue_call_date_generator.hpp"
#include "ores.trading.api/generators/bond_issue_conversion_target_generator.hpp"
#include "ores.trading.api/generators/bond_issue_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_amortization_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_amount_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_generator.hpp"
#include "ores.trading.api/generators/bond_issue_leg_schedule_date_generator.hpp"
#include "ores.trading.api/generators/fx_forward_instrument_generator.hpp"
#include "ores.trading.api/generators/rate_instrument_generator.hpp"
#include "ores.trading.api/generators/structure_generator.hpp"
#include "ores.trading.api/generators/swap_leg_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/bond_option_repository.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/instrument_option_repository.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/script_input_repository.hpp"
#include "ores.trading.core/repository/structure_member_repository.hpp"
#include "ores.trading.core/repository/structure_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.trading.core/repository/trade_authorisation_request_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include "ores.trading.core/service/operational_authorisation_service.hpp"
#include "ores.trading.core/service/trade_export_service.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace {

using ores::trading::messaging::book_trade_request;
using ores::trading::messaging::put_bond_issue_composite_request;
using ores::trading::service::book_rules_port;
using ores::trading::service::trade_operations_service;
using ores::utility::domain::outcome;

namespace {

/** A port that answers the same way for every book, so a case states its rule. */
class stated_book_rules final : public book_rules_port {
public:
    explicit stated_book_rules(answer rules)
        : rules_(rules) {}
    [[nodiscard]] answer
    of(const boost::uuids::uuid&, std::string_view, std::string_view) const override {
        return rules_;
    }

private:
    answer rules_;
};

const book_rules_port::answer open_book_allows_every_type{book_rules_port::status::open,
                                                          book_rules_port::product_type::allows};
const stated_book_rules every_book_open{open_book_allows_every_type};

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

TEST_CASE("book_trade_writes_the_anchor_the_booking_and_the_state", "[service]") {
    fixture f;
    auto req = f.request();
    const auto executed_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    req.booking.execution_timestamp = executed_at;
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

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

TEST_CASE("a_booking_is_stamped_with_the_sessions_actor_not_the_requests", "[service]") {
    fixture f;
    auto req = f.request();
    // A caller may state any author. The store keeps the session's, so a
    // service that books cannot put a person's name on the trade.
    req.booking.modified_by = "trader.one";
    req.anchor.modified_by = "trader.one";
    const auto session_actor = f.ctx.actor();
    REQUIRE_FALSE(session_actor.empty());
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    REQUIRE(response.result.outcome == outcome::ok);
    const auto trade = ores::trading::repository::trade_repository().read_latest(f.ctx, id);
    REQUIRE(trade.size() == 1);
    CHECK(trade.front().modified_by == session_actor);
    const auto bookings =
        ores::trading::repository::trade_booking_repository().read_latest(f.ctx, id);
    REQUIRE(bookings.size() == 1);
    CHECK(bookings.front().modified_by == session_actor);
    REQUIRE(response.activity_id.has_value());
    const auto activity = ores::trading::repository::trade_activity_repository{}.read_latest(
        f.ctx, boost::uuids::to_string(*response.activity_id));
    REQUIRE(activity.size() == 1);
    CHECK(activity.front().actor == session_actor);
}

TEST_CASE("a_booking_into_a_book_that_is_not_open_is_refused", "[service]") {
    fixture f;
    const stated_book_rules frozen{
        book_rules_port::answer{.book_status = book_rules_port::status::not_open,
                                .allowed = book_rules_port::product_type::allows}};
    auto request = f.request();
    const auto response = trade_operations_service(f.ctx, frozen).book_trade(request, {});
    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "book_not_open");
}

TEST_CASE("a_booking_of_a_product_type_the_book_does_not_allow_is_refused", "[service]") {
    fixture f;
    const stated_book_rules narrow{
        book_rules_port::answer{.book_status = book_rules_port::status::open,
                                .allowed = book_rules_port::product_type::refuses}};
    const auto req = f.request();

    const auto response = trade_operations_service(f.ctx, narrow).book_trade(req, {});

    // The refusal names the rule and the type it refused, so a caller can tell
    // it from a book that is closed.
    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "product_type_not_allowed");
    CHECK(response.result.message.find(req.anchor.trade_type) != std::string::npos);
    CHECK(response.result.message.find("restricts the product types") != std::string::npos);
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("a_booking_of_a_type_the_engine_cannot_carry_is_refused_with_its_reason", "[service]") {
    for (const std::string type : {"CommodityPairwiseVarianceSwap", "ForwardVolatilityAgreement"}) {
        INFO("Trade type " << type);
        fixture f;
        auto req = f.request();
        req.anchor.trade_type = type;

        const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

        CHECK(response.result.outcome == outcome::failed);
        CHECK(response.result.code == "trade_type_not_supported");
        CHECK(response.result.message.find("ORE has no trade builder for " + type) !=
              std::string::npos);
        CHECK(ores::trading::repository::trade_repository()
                  .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
                  .empty());
    }
}

TEST_CASE("a_second_booking_of_the_same_trade_is_a_conflict", "[service]") {
    fixture f;
    const auto req = f.request();
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "already_exists");
    const auto bookings = ores::trading::repository::trade_booking_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));
    REQUIRE(bookings.size() == 1);
    CHECK(bookings.front().version == 1);
}

TEST_CASE("a_booking_that_fails_writes_no_anchor", "[service]") {
    fixture f;
    auto req = f.request();
    req.booking.book_id = f.gen.generate_uuid();

    CHECK_THROWS(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}));

    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("put_bond_issue_composite_writes_the_issue_and_its_children", "[service]") {
    fixture f;
    const auto req = f.issue_request();

    const auto response =
        trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req);

    REQUIRE(response.result.outcome == outcome::ok);
    CHECK(ores::trading::repository::bond_issue_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.issue.issue_id))
              .size() == 1);
}

TEST_CASE("a_second_write_of_the_same_issue_is_already_exists", "[service]") {
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

TEST_CASE("a_second_write_with_other_terms_is_content_differs", "[service]") {
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

TEST_CASE("a_refused_child_write_leaves_no_issue", "[service]") {
    fixture f;
    auto req = f.issue_request();
    req.amounts.front().sequence_number = 0;

    CHECK_THROWS(trade_operations_service(f.ctx, every_book_open).put_bond_issue_composite(req));

    CHECK(ores::trading::repository::bond_issue_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.issue.issue_id))
              .empty());
}

TEST_CASE("a_second_write_of_an_issue_with_every_child_type_is_already_exists", "[service]") {
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

TEST_CASE("a_change_in_any_optional_child_is_content_differs", "[service]") {
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

TEST_CASE("a_write_with_a_child_less_than_the_stored_issue_is_content_differs", "[service]") {
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

TEST_CASE("an_actual_trade_booked_live_raises_one_authorisation_request", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;

    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    const auto raised = requests_for(f.ctx, req.anchor.id);
    REQUIRE(raised.size() == 1);
    CHECK(raised.front().trade_version >= 1);
}

TEST_CASE("a_test_trade_booked_live_raises_no_authorisation_request", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::test;

    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    CHECK(requests_for(f.ctx, req.anchor.id).empty());
}

namespace {

/** Books an actual trade as a draft, which is where an execution starts. */
ores::trading::messaging::book_trade_request draft(fixture& f) {
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    req.activity_type_code = "draft_capture";
    req.draft_reason_code = "structure_incomplete";
    return req;
}

/** The message that executes a draft, against the version the caller read. */
ores::trading::messaging::execute_trade_request execute(const boost::uuids::uuid& trade_id,
                                                        int version) {
    ores::trading::messaging::execute_trade_request req;
    req.trade_id = trade_id;
    req.expected_version = version;
    req.change_reason_code = "system.test";
    req.change_commentary = "Executed by a service test.";
    return req;
}

int version_of(const ores::database::context& ctx, const boost::uuids::uuid& trade_id) {
    const auto rows = ores::trading::repository::trade_repository().read_latest(
        ctx, boost::uuids::to_string(trade_id));
    return rows.empty() ? -1 : rows.front().version;
}

}

TEST_CASE("a_draft_that_is_not_executed_raises_no_authorisation_request", "[service]") {
    fixture f;
    const auto req = draft(f);
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    CHECK(requests_for(f.ctx, req.anchor.id).empty());
}

TEST_CASE("a_draft_that_states_no_reason_is_refused", "[service]") {
    fixture f;
    auto req = draft(f);
    req.draft_reason_code.clear();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});
    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "draft_reason_missing");
}

TEST_CASE("a_trade_that_goes_live_states_no_draft_reason", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    // A concluded deal waits on nothing, so a reason here is a mistake.
    req.draft_reason_code = "structure_incomplete";

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});
    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "draft_reason_on_live_trade");
}

TEST_CASE("a_draft_keeps_the_reason_it_states", "[service]") {
    fixture f;
    const auto req = draft(f);
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    const auto stored = ores::trading::repository::trade_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));
    REQUIRE(stored.size() == 1);
    CHECK(stored.front().draft_reason_code == "structure_incomplete");
}

TEST_CASE("an_actual_draft_executed_raises_one_authorisation_request", "[service]") {
    fixture f;
    const auto booked = draft(f);
    REQUIRE(
        trade_operations_service(f.ctx, every_book_open).book_trade(booked, {}).result.outcome ==
        outcome::ok);

    const auto response =
        trade_operations_service(f.ctx, every_book_open)
            .execute_trade(execute(booked.anchor.id, version_of(f.ctx, booked.anchor.id)));
    REQUIRE(response.result.outcome == outcome::ok);

    const auto raised = requests_for(f.ctx, booked.anchor.id);
    REQUIRE(raised.size() == 1);

    // The execution took the trade live, which is what the request is about.
    const auto summaries = ores::trading::repository::trade_summary_repository().read_latest(
        f.ctx, boost::uuids::to_string(booked.anchor.id));
    REQUIRE(summaries.size() == 1);
    CHECK(summaries.front().status == std::optional<std::string>{"live"});
}

TEST_CASE("an_execution_does_not_ask_the_same_question_twice", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;

    // A live booking raises its request, and the trade now waits.
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);
    REQUIRE(requests_for(f.ctx, req.anchor.id).size() == 1);

    // A second write of the same kind of fact must not raise a second one.
    ores::trading::service::operational_authorisation_service authorisation(f.ctx);
    CHECK_FALSE(
        authorisation.raise_if_awaiting(req.anchor.id, authorisation.prepare()).has_value());
    CHECK(requests_for(f.ctx, req.anchor.id).size() == 1);
}

TEST_CASE("an_amendment_raises_its_own_request_after_the_last_was_answered", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);
    REQUIRE(requests_for(f.ctx, req.anchor.id).size() == 1);

    // A decider answers the request, so the trade no longer waits on it.
    const auto answered = requests_for(f.ctx, req.anchor.id).front();
    const auto stored = ores::inbox::repository::approval_request_repository().read_latest(
        f.ctx, boost::uuids::to_string(answered.request_id));
    REQUIRE(stored.size() == 1);
    auto approved = stored.front();
    approved.state_code = "approved";
    ores::inbox::repository::approval_request_repository().write(f.ctx, approved);

    ores::trading::service::operational_authorisation_service authorisation(f.ctx);
    REQUIRE(authorisation.raise_if_awaiting(req.anchor.id, authorisation.prepare()).has_value());
    CHECK(requests_for(f.ctx, req.anchor.id).size() == 2);
}

namespace {

/** The logger the store calls of this file share. */
ores::logging::logger_t& lg() {
    static auto instance = ores::logging::make_logger("service_trade_operations_service_tests");
    return instance;
}

/** The request that completes a structure over these legs. */
ores::trading::messaging::complete_structure_request
completion(fixture& f, const std::vector<boost::uuids::uuid>& legs) {
    ores::trading::messaging::complete_structure_request req;
    req.structure = ores::trading::generators::generate_synthetic_structure(f.gen);
    // The fixture's generator is seeded, so two cases would name one structure
    // and the reference would collide. A structure is a new deal each time.
    req.structure.id = boost::uuids::random_generator()();
    req.structure.kind = "Package";
    req.structure.template_code = "";
    req.structure.parent_structure_id = boost::uuids::uuid{};
    req.structure.change_reason_code = "system.test";
    req.leg_count = static_cast<int>(legs.size());
    int sequence = 0;
    for (const auto& id : legs) {
        ores::trading::domain::structure_member member;
        member.trade_id = id;
        member.role = "leg";
        member.sequence_number = ++sequence;
        member.change_reason_code = "system.test";
        req.legs.push_back(member);
    }
    req.change_reason_code = "system.test";
    req.change_commentary = "Completed by a service test.";
    return req;
}

/** Books two actual drafts and answers their ids. */
std::vector<boost::uuids::uuid> two_drafts(fixture& f) {
    std::vector<boost::uuids::uuid> ids;
    for (int i = 0; i < 2; ++i) {
        const auto req = draft(f);
        REQUIRE(
            trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);
        ids.push_back(req.anchor.id);
    }
    return ids;
}

}

TEST_CASE("a_completed_structure_holds_every_leg_live_and_waiting", "[service]") {
    fixture f;
    const auto legs = two_drafts(f);

    const auto response =
        trade_operations_service(f.ctx, every_book_open).complete_structure(completion(f, legs));
    REQUIRE(response.result.outcome == outcome::ok);
    REQUIRE(response.structure_id.has_value());
    CHECK(response.structure_version >= 0);

    for (const auto& id : legs) {
        const auto summaries = ores::trading::repository::trade_summary_repository().read_latest(
            f.ctx, boost::uuids::to_string(id));
        REQUIRE(summaries.size() == 1);
        // Live, and waiting on Operations rather than authorised by the grouping.
        CHECK(summaries.front().status == std::optional<std::string>{"live"});
        CHECK(summaries.front().authorisation_status == "waiting");
        CHECK(requests_for(f.ctx, id).size() == 1);
        // And each one is a member of the structure just made.
        const auto members = ores::trading::repository::structure_member_repository().read_latest(
            f.ctx, boost::uuids::to_string(id));
        REQUIRE(members.size() == 1);
        CHECK(members.front().structure_id == *response.structure_id);
    }
}

TEST_CASE("a_second_completion_of_the_same_structure_is_already_exists", "[service]") {
    fixture f;
    const auto legs = two_drafts(f);
    auto req = completion(f, legs);
    REQUIRE(
        trade_operations_service(f.ctx, every_book_open).complete_structure(req).result.outcome ==
        outcome::ok);

    // The same structure id, against legs that are no longer drafts: the
    // structure's own existence is what the answer names.
    const auto again = trade_operations_service(f.ctx, every_book_open).complete_structure(req);
    CHECK(again.result.outcome == outcome::conflict);
    CHECK(again.result.code == "already_exists");
}

TEST_CASE("a_refused_completion_leaves_every_leg_a_draft_and_writes_no_member", "[service]") {
    fixture f;
    const auto legs = two_drafts(f);
    auto req = completion(f, legs);
    // A completion that states fewer legs than it holds is refused before the
    // transaction opens, so nothing is written at all.
    req.leg_count = 1;

    const auto response = trade_operations_service(f.ctx, every_book_open).complete_structure(req);
    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "leg_count_mismatch");

    for (const auto& id : legs) {
        const auto states = ores::trading::repository::trade_state_repository().read_latest(
            f.ctx, boost::uuids::to_string(id));
        REQUIRE(states.size() == 1);
        const auto draft = ores::database::query_builder::execute_parameterized_string_query(
            f.ctx,
            "select ores_trading_status_is_draft_fn($1::uuid)::text",
            {boost::uuids::to_string(states.front().status_id)},
            lg(),
            "Checking whether a leg is still a draft");
        CHECK(draft.front() == "true");
        CHECK(ores::trading::repository::structure_member_repository()
                  .read_latest(f.ctx, boost::uuids::to_string(id))
                  .empty());
        CHECK(requests_for(f.ctx, id).empty());
    }
}

TEST_CASE("a_completion_takes_a_live_leg_as_it_stands_and_executes_the_drafts", "[service]") {
    fixture f;
    const auto legs = two_drafts(f);
    // One leg is already live, so it has asked its own question and a grouping
    // must not ask it again. The other is still a draft.
    REQUIRE(trade_operations_service(f.ctx, every_book_open)
                .execute_trade(execute(legs[0], version_of(f.ctx, legs[0])))
                .result.outcome == outcome::ok);
    REQUIRE(requests_for(f.ctx, legs[0]).size() == 1);

    const auto response =
        trade_operations_service(f.ctx, every_book_open).complete_structure(completion(f, legs));
    REQUIRE(response.result.outcome == outcome::ok);

    for (const auto& id : legs) {
        const auto members = ores::trading::repository::structure_member_repository().read_latest(
            f.ctx, boost::uuids::to_string(id));
        REQUIRE(members.size() == 1);
        CHECK(members.front().structure_id == *response.structure_id);
    }
    // The live leg keeps its one request; the draft gets its own.
    CHECK(requests_for(f.ctx, legs[0]).size() == 1);
    CHECK(requests_for(f.ctx, legs[1]).size() == 1);
    const auto summaries = ores::trading::repository::trade_summary_repository().read_latest(
        f.ctx, boost::uuids::to_string(legs[1]));
    REQUIRE(summaries.size() == 1);
    CHECK(summaries.front().status == std::optional<std::string>{"live"});
}

TEST_CASE("an_execution_of_a_trade_that_moved_since_it_was_read_is_refused", "[service]") {
    fixture f;
    const auto booked = draft(f);
    REQUIRE(
        trade_operations_service(f.ctx, every_book_open).book_trade(booked, {}).result.outcome ==
        outcome::ok);

    const auto response =
        trade_operations_service(f.ctx, every_book_open)
            .execute_trade(execute(booked.anchor.id, version_of(f.ctx, booked.anchor.id) + 7));
    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "version_conflict");
    CHECK(requests_for(f.ctx, booked.anchor.id).empty());
}

TEST_CASE("a_trade_that_is_not_a_draft_cannot_be_executed", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    const auto response =
        trade_operations_service(f.ctx, every_book_open)
            .execute_trade(execute(req.anchor.id, version_of(f.ctx, req.anchor.id)));
    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "not_draft");
}

TEST_CASE("the_summary_of_a_booked_actual_trade_names_its_state_book_and_waiting_request",
          "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    const auto rows = ores::trading::repository::trade_summary_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));

    REQUIRE(rows.size() == 1);
    CHECK(rows.front().row_kind == "trade");
    CHECK(rows.front().status == std::optional<std::string>{"live"});
    CHECK(rows.front().book_id == std::optional<boost::uuids::uuid>{f.book_id});
    CHECK(rows.front().authorisation_status == "waiting");
}

TEST_CASE("the_summary_of_a_booked_test_trade_needs_no_authorisation", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::test;
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}).result.outcome ==
            outcome::ok);

    const auto rows = ores::trading::repository::trade_summary_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));

    REQUIRE(rows.size() == 1);
    CHECK(rows.front().authorisation_status == "not_required");
}

TEST_CASE("a_booked_variance_swap_keeps_its_dates_in_order", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.booking_nature = ores::trading::domain::booking_nature::test;

    const auto start = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{10}, std::chrono::day{31}};
    const auto end = std::chrono::year_month_day{
        std::chrono::year{2027}, std::chrono::month{10}, std::chrono::day{31}};
    ores::trading::domain::fx_variance_swap_instrument row;
    row.identity.trade_type_code = "FxVarianceSwap";
    row.start_date = start;
    row.end_date = end;
    row.currency = "USD";
    row.underlying_code = "AAPL";
    row.long_short = "Long";
    row.strike = 16.0;
    row.notional = ores::utility::decimal::decimal::from_double(1000.0).value();
    row.moment_type = "Variance";
    req.instruments.fx_variance_swap_instruments.push_back(row);
    req.anchor.trade_type = "FxVarianceSwap";

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    REQUIRE(response.result.outcome == outcome::ok);
    const auto rows =
        ores::trading::repository::fx_variance_swap_instrument_repository().read_latest(
            f.ctx, boost::uuids::to_string(req.anchor.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().start_date == start);
    CHECK(rows.front().end_date == end);
}

TEST_CASE("a_bond_trade_is_booked_with_its_identifier_and_instrument", "[service]") {
    fixture f;
    const auto issue_id = f.stored_issue();
    const auto req = f.bond_request(issue_id);
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

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

TEST_CASE("a_bond_trade_of_an_unknown_issue_leaves_no_trade", "[service]") {
    fixture f;
    const auto req = f.bond_request(f.gen.generate_uuid());

    CHECK_THROWS(trade_operations_service(f.ctx, every_book_open).book_trade(req, {}));

    const auto id = boost::uuids::to_string(req.anchor.id);
    CHECK(ores::trading::repository::trade_repository().read_latest(f.ctx, id).empty());
    CHECK(ores::trading::repository::trade_identifier_repository()
              .read_latest(f.ctx, id, "Internal")
              .empty());
}

TEST_CASE("an_array_the_trade_type_does_not_hold_is_refused_by_name", "[service]") {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.instruments.fx_forward_instruments.emplace_back();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_not_routed");
    CHECK(response.result.message.contains("fx_forward_instruments"));
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("an_fx_forward_trade_is_booked_with_its_instrument", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.trade_type = "FxForward";
    req.instruments.fx_forward_instruments.push_back(
        ores::trading::generators::generate_synthetic_fx_forward_instrument(f.gen));
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    REQUIRE(response.result.outcome == outcome::ok);
    const auto stored =
        ores::trading::repository::fx_forward_instrument_repository().read_latest(f.ctx, id);
    REQUIRE(stored.size() == 1);
    CHECK(stored.front().identity.trade_id == req.anchor.id);
    CHECK(stored.front().identity.trade_activity_id == *response.activity_id);
}

TEST_CASE("a_swap_trade_is_booked_with_its_header_and_its_legs", "[service]") {
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

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    REQUIRE(response.result.outcome == outcome::ok);
    CHECK(ores::trading::repository::rate_instrument_repository().read_latest(f.ctx, id).size() ==
          1);
    const auto legs =
        ores::trading::repository::swap_leg_repository().read_latest_by_trade_id(f.ctx, id, 0, 10);
    REQUIRE(legs.size() == 2);
    CHECK(legs.front().identity.trade_id == req.anchor.id);
}

TEST_CASE("a_bond_row_is_booked_with_the_rows_its_product_states", "[service]") {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.anchor.trade_type = "BondOption";
    auto& bond = req.instruments.bond_instruments.front();
    bond.instrument.identity.trade_type_code = "BondOption";
    ores::trading::domain::bond_option fact;
    fact.option_type = "Call";
    fact.option_strike = ores::utility::decimal::decimal::from_string("0.98").value();
    fact.change_reason_code = "system.test";
    bond.option = fact;
    bond.option_redemption = "1";
    ores::trading::domain::bond_option_data block;
    block.long_short = "Long";
    block.option_type = "Call";
    block.style = "European";
    bond.option_data = block;
    bond.option_exercise_dates = {"2030-06-30"};
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    // The container's fact row, its option block and its exercise date are the
    // trade's own rows, so booking writes each of them beside the header.
    REQUIRE(response.result.outcome == outcome::ok);
    const auto facts = ores::trading::repository::bond_option_repository().read_latest(f.ctx, id);
    REQUIRE(facts.size() == 1);
    CHECK(facts.front().option_type == "Call");
    CHECK(facts.front().redemption == std::optional<std::string>("1"));
    const auto options =
        ores::trading::repository::instrument_option_repository().read_latest(f.ctx, id);
    REQUIRE(options.size() == 1);
    CHECK(options.front().long_short == "Long");
}

TEST_CASE("a_bond_row_on_a_trade_of_another_type_is_refused_by_name", "[service]") {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.anchor.trade_type = "Swap";

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_not_routed");
    CHECK(response.result.message.contains("bond_instruments"));
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("an_exported_swap_trade_books_back_to_the_same_rows", "[service]") {
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
    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(first, {}).result.outcome ==
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
    REQUIRE(
        trade_operations_service(f.ctx, every_book_open).book_trade(second, {}).result.outcome ==
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

TEST_CASE("a_scripted_trade_books_its_variables_and_underlyings_back", "[service]") {
    fixture f;
    auto first = f.request();
    first.anchor.trade_type = "ScriptedTrade";

    ores::trading::domain::scripted_instrument row;
    row.identity.trade_type_code = "ScriptedTrade";
    row.script_name = "AsianBasketOption";
    first.instruments.scripted_instruments.push_back(row);

    const auto add_parameter = [](ores::trading::domain::instrument_batch& batch,
                                  std::string name,
                                  std::string declared_type,
                                  std::string value) {
        ores::trading::domain::scripted_parameter parameter;
        parameter.sequence_number = static_cast<int>(batch.scripted_parameters.size()) + 1;
        parameter.name = std::move(name);
        parameter.declared_type = std::move(declared_type);
        parameter.shape = "scalar";
        parameter.value = std::move(value);
        batch.scripted_parameters.push_back(std::move(parameter));
    };
    const auto add_list = [](ores::trading::domain::instrument_batch& batch,
                             std::string name,
                             std::string declared_type,
                             const std::vector<std::string>& values) {
        ores::trading::domain::scripted_parameter parameter;
        parameter.sequence_number = static_cast<int>(batch.scripted_parameters.size()) + 1;
        parameter.name = std::move(name);
        parameter.declared_type = std::move(declared_type);
        parameter.shape = "vector";
        const auto parameter_sequence_number = parameter.sequence_number;
        batch.scripted_parameters.push_back(std::move(parameter));

        int sequence_number = 0;
        for (const auto& value : values) {
            ores::trading::domain::scripted_parameter_value member;
            member.parameter_sequence_number = parameter_sequence_number;
            member.sequence_number = ++sequence_number;
            member.value = value;
            batch.scripted_parameter_values.push_back(std::move(member));
        }
    };
    const auto add_underlying = [](ores::trading::domain::instrument_batch& batch,
                                   std::string role,
                                   std::string underlying_type,
                                   std::string address) {
        ores::trading::domain::instrument_underlying underlying;
        underlying.underlying_role = std::move(role);
        underlying.sequence_number = 1;
        underlying.underlying_type = std::move(underlying_type);
        underlying.name = std::move(address);
        batch.instrument_underlyings.push_back(std::move(underlying));
    };

    add_parameter(first.instruments, "Notional", "Number", "1000000");
    add_parameter(first.instruments,
                  "Underlying",
                  "Index",
                  "oresmd://equity/RIC:.SPX?type=fixing&index=equity");
    add_list(first.instruments, "Weights", "Number", {"0.6", "0.4"});
    add_underlying(first.instruments,
                   "underlying",
                   "Equity",
                   "oresmd://equity/RIC:.STOXX50E?type=fixing&index=equity");

    REQUIRE(trade_operations_service(f.ctx, every_book_open).book_trade(first, {}).result.outcome ==
            outcome::ok);

    ores::trading::domain::instrument_batch exported;
    ores::trading::service::trade_export_service(f.ctx).export_books(
        {boost::uuids::to_string(f.book_id)}, 0, 100, exported);

    const auto parameters =
        ores::trading::domain::find_all(exported.scripted_parameters, first.anchor.id);
    REQUIRE(parameters.size() == 3);
    CHECK(parameters[0].name == "Notional");
    CHECK(parameters[0].declared_type == "Number");
    CHECK(parameters[0].value == std::optional<std::string>("1000000"));
    CHECK(parameters[1].name == "Underlying");
    CHECK(parameters[1].value ==
          std::optional<std::string>("oresmd://equity/RIC:.SPX?type=fixing&index=equity"));
    CHECK(parameters[2].name == "Weights");
    CHECK(parameters[2].shape == "vector");

    const auto values =
        ores::trading::domain::find_all(exported.scripted_parameter_values, first.anchor.id);
    REQUIRE(values.size() == 2);
    CHECK(values[0].parameter_sequence_number == 3);
    CHECK(values[0].value == "0.6");
    CHECK(values[1].sequence_number == 2);
    CHECK(values[1].value == "0.4");

    const auto underlyings =
        ores::trading::domain::find_all(exported.instrument_underlyings, first.anchor.id);
    REQUIRE(underlyings.size() == 1);
    CHECK(underlyings[0].underlying_role == "underlying");
    CHECK(underlyings[0].underlying_type == "Equity");
    CHECK(underlyings[0].name == "oresmd://equity/RIC:.STOXX50E?type=fixing&index=equity");
}

TEST_CASE("a_child_array_with_no_header_is_refused_by_name", "[service]") {
    fixture f;
    auto req = f.request();
    req.anchor.trade_type = "Swap";
    req.instruments.swap_legs.push_back(
        ores::trading::generators::generate_synthetic_swap_leg(f.gen));

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "instrument_child_without_header");
    CHECK(response.result.message.contains("swap_legs"));
    CHECK(ores::trading::repository::trade_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}

TEST_CASE("every_array_the_trade_type_does_not_hold_is_named_once", "[service]") {
    fixture f;
    auto req = f.bond_request(f.stored_issue());
    req.instruments.fx_forward_instruments.emplace_back();
    req.instruments.rate_instruments.emplace_back();

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.code == "instrument_not_routed");
    CHECK(response.result.message.contains("fx_forward_instruments"));
    CHECK(response.result.message.contains("rate_instruments"));
}

namespace {

/*
 * A generic scripted trade of the library's window barrier option, stating
 * every input the library holds for it as the script reads it. The library
 * is the system tenant's, so the inputs are read there.
 */
book_trade_request window_barrier_request(fixture& f) {
    auto req = f.request();
    req.anchor.trade_type = "ScriptedTrade";
    ores::trading::domain::scripted_instrument row;
    row.identity.trade_id = req.anchor.id;
    row.identity.trade_type_code = "ScriptedTrade";
    row.script_name = "WindowBarrierOption";
    row.audit.change_reason_code = "system.test";
    req.instruments.scripted_instruments.push_back(row);

    const auto system = f.ctx.with_tenant(ores::utility::uuid::tenant_id::system(), "test");
    int sequence = 0;
    for (const auto& input :
         ores::trading::repository::script_input_repository().read_latest(system)) {
        if (input.script_name != "WindowBarrierOption")
            continue;
        ores::trading::domain::scripted_parameter p;
        p.trade_id = req.anchor.id;
        p.sequence_number = ++sequence;
        p.name = input.input_name;
        p.declared_type = input.data_type;
        p.shape = input.is_array ? "vector" : "scalar";
        p.value = std::string("1");
        p.change_reason_code = "system.test";
        req.instruments.scripted_parameters.push_back(p);
    }
    return req;
}

}

TEST_CASE("a_scripted_trade_that_leaves_out_an_input_of_its_script_is_refused_by_name",
          "[service]") {
    fixture f;
    auto req = window_barrier_request(f);
    REQUIRE(req.instruments.scripted_parameters.size() == 15u);
    std::erase_if(req.instruments.scripted_parameters,
                  [](const auto& p) { return p.name == "BarrierLevel"; });

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.outcome == outcome::failed);
    CHECK(response.result.code == "script_input_mismatch");
    CHECK(response.result.message.contains("BarrierLevel"));
}

TEST_CASE("a_scripted_trade_that_states_an_input_as_another_type_is_refused_by_name", "[service]") {
    fixture f;
    auto req = window_barrier_request(f);
    for (auto& p : req.instruments.scripted_parameters)
        if (p.name == "Expiry")
            p.declared_type = "Number";

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    CHECK(response.result.code == "script_input_mismatch");
    CHECK(response.result.message.contains("Expiry"));
    CHECK(response.result.message.contains("Event"));
}

TEST_CASE("a_scripted_trade_that_states_every_input_of_its_script_passes_the_check", "[service]") {
    fixture f;
    const auto req = window_barrier_request(f);

    const auto response = trade_operations_service(f.ctx, every_book_open).book_trade(req, {});

    INFO(response.result.message);
    CHECK(response.result.code != "script_input_mismatch");
}
