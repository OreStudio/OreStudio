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
#include "ores.refdata.api/messaging/commodity_future_convention_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_convention_protocol.hpp"
#include "ores.refdata.core/service/commodity_future_convention_service.hpp"
#include "ores.refdata.core/service/tenor_basis_swap_convention_service.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::refdata::messaging::put_commodity_future_convention_request;
using ores::refdata::messaging::put_tenor_basis_swap_convention_request;
using ores::refdata::messaging::tenor_basis_swap_pay_receive_change;
using ores::refdata::messaging::tenor_basis_swap_long_short_change;
using ores::utility::domain::outcome;

const ores::utility::domain::change_intent test_intent{.reason_code = "system.test",
                                                       .commentary = "Part cardinality test"};

put_tenor_basis_swap_convention_request tenor_basis_request(const boost::uuids::uuid& id) {
    put_tenor_basis_swap_convention_request request;
    request.item.change.write.id = id;
    request.item.change.write.kind_code = "TENOR_BASIS_SWAP";
    request.item.change.write.name = "EUR-EURIBOR-3M-6M-CARDINALITY";
    request.intent = test_intent;
    return request;
}

tenor_basis_swap_pay_receive_change pay_receive_of(const boost::uuids::uuid& convention_id) {
    tenor_basis_swap_pay_receive_change change;
    change.write.id = boost::uuids::random_generator()();
    change.write.convention_id = convention_id;
    change.write.pay_index = std::string("EUR-EURIBOR-6M");
    change.write.receive_index = std::string("EUR-EURIBOR-3M");
    return change;
}

}

TEST_CASE("a tenor basis swap that states neither form is refused, naming the parts",
          "[refdata][conventions][family][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    ores::refdata::service::tenor_basis_swap_convention_service service(ctx);

    const auto response = service.put_tenor_basis_swap_convention(
        tenor_basis_request(boost::uuids::random_generator()()));

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "part_cardinality");
    CHECK(response.result.message.find("pay_receive or long_short") != std::string::npos);
    CHECK_FALSE(response.item.has_value());
}

TEST_CASE("a tenor basis swap that states both forms is refused",
          "[refdata][conventions][family][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    ores::refdata::service::tenor_basis_swap_convention_service service(ctx);

    const auto id = boost::uuids::random_generator()();
    auto request = tenor_basis_request(id);
    request.item.pay_receive.push_back(pay_receive_of(id));
    tenor_basis_swap_long_short_change long_short;
    long_short.write.id = boost::uuids::random_generator()();
    long_short.write.convention_id = id;
    request.item.long_short.push_back(long_short);

    const auto response = service.put_tenor_basis_swap_convention(request);

    CHECK(response.result.code == "part_cardinality");
}

TEST_CASE("a tenor basis swap that states one form is written",
          "[refdata][conventions][family][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    ores::refdata::service::tenor_basis_swap_convention_service service(ctx);

    const auto id = boost::uuids::random_generator()();
    auto request = tenor_basis_request(id);
    request.item.pay_receive.push_back(pay_receive_of(id));

    const auto response = service.put_tenor_basis_swap_convention(request);

    INFO(response.result.message);
    REQUIRE(response.result.outcome == outcome::ok);
    REQUIRE(response.item.has_value());
    CHECK(response.item->pay_receive.size() == 1u);
    CHECK(response.item->long_short.empty());
}

TEST_CASE("a commodity future with no contract terms is refused, naming the part",
          "[refdata][conventions][family][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    ores::refdata::service::commodity_future_convention_service service(ctx);

    put_commodity_future_convention_request request;
    request.item.change.write.id = boost::uuids::random_generator()();
    request.item.change.write.kind_code = "COMMODITY_FUTURE";
    request.item.change.write.name = "NYMEX:CL-CARDINALITY";
    request.intent = test_intent;

    const auto response = service.put_commodity_future_convention(request);

    CHECK(response.result.code == "part_cardinality");
    CHECK(response.result.message.find("contract_terms") != std::string::npos);
}
