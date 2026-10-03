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
#include "ores.ore.core/domain/party_scope.hpp"
#include "ores.reporting.api/domain/report_analytic.hpp"
#include "ores.reporting.api/domain/report_run_setup.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string tags("[ore][party]");

struct mapped_document {
    ores::reporting::domain::report_run_setup setup;
    std::vector<ores::reporting::domain::report_analytic> analytics;
    std::optional<ores::reporting::domain::report_analytic> extra;
    std::optional<ores::reporting::domain::report_analytic> absent;
    std::string name;
    std::map<std::string, int> unmodelled;
};

}

TEST_CASE("assigning a party stamps every row of a mapped document", tags) {
    mapped_document d;
    d.analytics.resize(3);
    d.extra = ores::reporting::domain::report_analytic{};
    d.name = "unchanged";
    d.unmodelled["Element"] = 2;

    const auto party = boost::uuids::random_generator()();
    ores::ore::domain::assign_party(d, party);

    CHECK(d.setup.party_id == party);
    for (const auto& a : d.analytics)
        CHECK(a.party_id == party);
    REQUIRE(d.extra);
    CHECK(d.extra->party_id == party);
    CHECK_FALSE(d.absent);
    CHECK(d.name == "unchanged");
    CHECK(d.unmodelled.at("Element") == 2);
}
