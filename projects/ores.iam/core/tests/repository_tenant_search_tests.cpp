/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/domain/tenant.hpp"
#include "ores.iam.api/generators/tenant_generator.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using row_t = std::vector<std::optional<std::string>>;

/**
 * @brief A marker no other test's tenant code carries, so a search for it
 * answers only the tenants this case wrote.
 */
std::string unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "srch" + text.substr(0, 10);
}

/**
 * @brief Writes one tenant whose code, name and hostname carry the marker.
 */
ores::iam::domain::tenant write_tenant(ores::testing::database_helper& h,
                                       const std::string& marker,
                                       const std::string& suffix,
                                       const std::string& type,
                                       const std::string& status) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto t = ores::iam::generators::generate_synthetic_tenant(gen_ctx);
    t.code = marker + "_" + suffix;
    t.name = "Search Fixture " + suffix + " " + marker;
    t.hostname = marker + "-" + suffix + ".example.com";
    t.type = type;
    t.status = status;
    const auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    ores::iam::repository::tenant_repository{}.write(sys_ctx, t);
    return t;
}

/**
 * @brief Calls the search function as the roster's handler does.
 */
std::vector<row_t> search(ores::testing::database_helper& h,
                          const std::string& text,
                          const std::string& type = "",
                          const std::string& status = "",
                          int limit = 100,
                          int offset = 0) {
    static auto lg(ores::logging::make_logger(test_suite));
    const auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    return ores::database::repository::execute_parameterized_multi_column_query(
        sys_ctx,
        "SELECT * FROM ores_iam_tenants_search_fn($1, $2, $3, $4, $5)",
        {text, type, status, std::to_string(limit), std::to_string(offset)},
        lg,
        "searching tenants");
}

std::vector<std::string> codes(const std::vector<row_t>& rows) {
    std::vector<std::string> result;
    for (const auto& row : rows)
        result.push_back(row[2].value_or(""));
    return result;
}

}

using namespace ores::logging;
using ores::testing::database_helper;

TEST_CASE("tenant_search_matches_code_name_and_hostname_in_code_order", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const auto marker = unique_marker();
    write_tenant(h, marker, "c", "automation", "active");
    write_tenant(h, marker, "a", "automation", "active");
    write_tenant(h, marker, "b", "evaluation", "suspended");

    const auto rows = search(h, marker);
    CHECK(codes(rows) == std::vector<std::string>{marker + "_a", marker + "_b", marker + "_c"});
    REQUIRE_FALSE(rows.empty());
    CHECK(rows.front()[14].value_or("") == "3");

    // The search ignores case, and reaches the name and the hostname as well
    // as the code.
    CHECK(codes(search(h, "SEARCH FIXTURE A " + marker)) ==
          std::vector<std::string>{marker + "_a"});
    CHECK(codes(search(h, marker + "-b.example")) == std::vector<std::string>{marker + "_b"});
    BOOST_LOG_SEV(lg, debug) << "Searched for " << marker;
}

TEST_CASE("tenant_search_keeps_the_type_and_status_asked_for", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const auto marker = unique_marker();
    write_tenant(h, marker, "a", "automation", "active");
    write_tenant(h, marker, "b", "evaluation", "suspended");
    write_tenant(h, marker, "c", "evaluation", "active");

    CHECK(codes(search(h, marker, "evaluation")) ==
          std::vector<std::string>{marker + "_b", marker + "_c"});
    CHECK(codes(search(h, marker, "", "active")) ==
          std::vector<std::string>{marker + "_a", marker + "_c"});
    CHECK(codes(search(h, marker, "evaluation", "active")) ==
          std::vector<std::string>{marker + "_c"});
    BOOST_LOG_SEV(lg, debug) << "Filtered " << marker;
}

TEST_CASE("tenant_search_pages_and_counts_every_match", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const auto marker = unique_marker();
    for (const auto* suffix : {"a", "b", "c", "d", "e"})
        write_tenant(h, marker, suffix, "automation", "active");

    const auto page = search(h, marker, "", "", 2, 2);
    CHECK(codes(page) == std::vector<std::string>{marker + "_c", marker + "_d"});
    // Each row carries the total over every page, not the size of this one.
    for (const auto& row : page)
        CHECK(row[14].value_or("") == "5");
    BOOST_LOG_SEV(lg, debug) << "Paged " << marker;
}

TEST_CASE("tenant_search_never_answers_the_system_tenant", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const auto system_id = ores::utility::uuid::tenant_id::system().to_string();

    // The system tenant's code is "system", so this search would find it if
    // the rule did not hold; an empty search would too.
    for (const auto& text : {std::string("system"), std::string()}) {
        for (const auto& row : search(h, text, "", "", 1000)) {
            CHECK(row[0].value_or("") != system_id);
            CHECK(row[2].value_or("") != "system");
        }
    }
    BOOST_LOG_SEV(lg, debug) << "The system tenant stayed out.";
}
