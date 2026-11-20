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
#include "ores.dq.api/domain/methodology.hpp"
#include "ores.dq.api/generators/methodology_generator.hpp"
#include "ores.dq.api/messaging/methodology_protocol.hpp"
#include "ores.dq.core/service/methodology_service.hpp"
#include "ores.logging/logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

using ores::dq::domain::methodology;
using ores::dq::messaging::methodology_change;
using ores::dq::messaging::methodology_key;
using ores::utility::domain::outcome;

/// Methodologies whose names no other test uses, so a read by name finds only these.
std::vector<methodology> unique_methodologies(std::size_t n) {
    ores::utility::generation::generation_context ctx;
    auto all = ores::dq::generators::generate_synthetic_methodologies(n, ctx);
    for (auto& m : all)
        m.name = m.name + "_" + std::string(faker::string::alphanumeric(12));
    return all;
}

methodology_change change_for(const methodology& m) {
    methodology_change c;
    c.write = {.id = m.id,
               .name = m.name,
               .description = m.description,
               .logic_reference = m.logic_reference,
               .implementation_details = m.implementation_details};
    return c;
}

}

TEST_CASE("get_many_by_name_answers_each_key_in_the_order_asked", "[service][batch]") {
    auto lg(make_logger(test_suite));
    ores::testing::database_helper h;
    ores::dq::service::methodology_service svc(h.context());
    const auto methodologies = unique_methodologies(3);

    ores::dq::messaging::put_many_methodologies_request put;
    for (const auto& m : methodologies)
        put.changes.push_back(change_for(m));
    ORES_LOG_SEV(lg, info) << "Putting " << put.changes.size() << " methodologies.";
    REQUIRE(svc.put_many_methodologies(put).result.outcome == outcome::ok);

    ores::dq::messaging::get_many_methodologies_request get;
    get.keys = {methodology_key{.name = "no_such_methodology_" +
                                        std::string(faker::string::alphanumeric(12))},
                methodology_key{.name = methodologies[2].name},
                methodology_key{.name = methodologies[0].name}};
    const auto read = svc.get_many_methodologies(get);

    REQUIRE(read.entries.size() == 3);
    CHECK(!read.entries[0].methodology);
    REQUIRE(read.entries[1].methodology);
    CHECK(read.entries[1].methodology->id == methodologies[2].id);
    REQUIRE(read.entries[2].methodology);
    CHECK(read.entries[2].methodology->id == methodologies[0].id);
}

TEST_CASE("put_many_over_an_existing_name_refuses_the_set_and_writes_nothing", "[service][batch]") {
    auto lg(make_logger(test_suite));
    ores::testing::database_helper h;
    ores::dq::service::methodology_service svc(h.context());
    const auto methodologies = unique_methodologies(3);

    ores::dq::messaging::put_many_methodologies_request first;
    first.changes.push_back(change_for(methodologies.back()));
    REQUIRE(svc.put_many_methodologies(first).result.outcome == outcome::ok);

    ores::dq::messaging::put_many_methodologies_request put;
    for (const auto& m : methodologies)
        put.changes.push_back(change_for(m));
    ORES_LOG_SEV(lg, info) << "Putting " << put.changes.size()
                           << " methodologies, one of which exists.";
    const auto refused = svc.put_many_methodologies(put);

    CHECK(refused.result.outcome != outcome::ok);
    CHECK(refused.result.code == "already_exists");

    ores::dq::messaging::get_many_methodologies_request get;
    for (std::size_t i = 0; i + 1 < methodologies.size(); ++i)
        get.keys.push_back(methodology_key{.name = methodologies[i].name});
    const auto read = svc.get_many_methodologies(get);

    REQUIRE(read.entries.size() == get.keys.size());
    for (const auto& entry : read.entries)
        CHECK(!entry.methodology);
}
