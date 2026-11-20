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
#include "ores.http/routes/compute/grid_views.hpp"
#include "ores.logging/logger.hpp"
#include "test_suite.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>
#include <vector>

using ores::compute::domain::batch;
using ores::compute::domain::result;
using ores::compute::domain::workunit;
using ores::http::routes::compute::grid::batch_json;
using ores::http::routes::compute::grid::page;
using ores::http::routes::compute::grid::page_json;
using ores::http::routes::compute::grid::result_json;
using ores::http::routes::compute::grid::stated_id;
using ores::http::routes::compute::grid::workunit_json;
using namespace ores::logging;

namespace {

const std::string run_id = "22222222-2222-2222-2222-222222222222";
const std::string party_id = "33333333-3333-3333-3333-333333333333";
const std::string host_id = "44444444-4444-4444-4444-444444444444";

boost::uuids::uuid uuid_of(const std::string& text) {
    return boost::uuids::string_generator()(text);
}

batch a_batch() {
    batch row;
    row.id = uuid_of("11111111-1111-1111-1111-111111111111");
    row.external_ref = run_id;
    row.status = "closed";
    row.modified_by = "marco";
    row.performed_by = "marco";
    return row;
}

bool states(const std::string& json, const std::string& member) {
    return json.find(member) != std::string::npos;
}

}

TEST_CASE("a_batch_names_the_run_that_made_it_and_the_party_that_owns_it", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the run and the party a batch names";

    auto row = a_batch();
    row.party_id = uuid_of(party_id);
    const auto written = batch_json(row, {{run_id, "Nightly risk"}}, {{party_id, "ACME"}});
    CHECK(states(written, R"("id":"11111111-1111-1111-1111-111111111111")"));
    CHECK(states(written, R"("external_ref":")" + run_id + R"(")"));
    CHECK(states(written, R"("report_run_name":"Nightly risk")"));
    CHECK(states(written, R"("party_id":")" + party_id + R"(")"));
    CHECK(states(written, R"("party_name":"ACME")"));
    CHECK(states(written, R"("status":"closed")"));
    CHECK(states(written, R"("modified_by":"marco")"));
}

TEST_CASE("a_batch_names_its_run_whatever_the_case_of_the_reference", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the case a reference is read by";

    auto row = a_batch();
    row.external_ref = "AAAA2222-2222-2222-2222-222222222222";
    const auto written =
        batch_json(row, {{"aaaa2222-2222-2222-2222-222222222222", "Nightly risk"}}, {});
    CHECK(states(written, R"("report_run_name":"Nightly risk")"));
}

TEST_CASE("a_batch_that_names_no_run_and_no_party_states_neither", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a batch with nothing to name";

    auto row = a_batch();
    row.external_ref = "adhoc";
    const auto written = batch_json(row, {}, {});
    CHECK(states(written, R"("report_run_name":null)"));
    CHECK(states(written, R"("party_id":null)"));
    CHECK(states(written, R"("party_name":null)"));
}

TEST_CASE("an_identifier_that_is_the_nil_uuid_is_stated_as_none", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the nil identifier";

    CHECK_FALSE(stated_id(boost::uuids::nil_uuid()).has_value());
    CHECK_FALSE(stated_id(std::optional<boost::uuids::uuid>(boost::uuids::nil_uuid())).has_value());
    CHECK_FALSE(stated_id(std::optional<boost::uuids::uuid>()).has_value());
    CHECK(stated_id(std::optional<boost::uuids::uuid>(uuid_of(party_id))) == party_id);
}

TEST_CASE("a_work_unit_states_an_unset_config_and_canonical_run_as_none", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the parts of a work unit a screen reads as unknown";

    workunit row;
    row.id = uuid_of("55555555-5555-5555-5555-555555555555");
    row.batch_id = uuid_of("11111111-1111-1111-1111-111111111111");
    row.app_version_id = uuid_of("66666666-6666-6666-6666-666666666666");
    row.input_uri = "/api/v1/storage/ores/in.zip";
    row.priority = 3;
    row.target_redundancy = 2;
    row.modified_by = "marco";

    const auto written = workunit_json(row);
    CHECK(states(written, R"("id":"55555555-5555-5555-5555-555555555555")"));
    CHECK(states(written, R"("input_uri":"/api/v1/storage/ores/in.zip")"));
    CHECK(states(written, R"("config_uri":null)"));
    CHECK(states(written, R"("priority":3)"));
    CHECK(states(written, R"("target_redundancy":2)"));
    CHECK(states(written, R"("canonical_result_id":null)"));

    row.config_uri = "/api/v1/storage/ores/config.xml";
    row.canonical_result_id = uuid_of("77777777-7777-7777-7777-777777777777");
    const auto settled = workunit_json(row);
    CHECK(states(settled, R"("config_uri":"/api/v1/storage/ores/config.xml")"));
    CHECK(states(settled, R"("canonical_result_id":"77777777-7777-7777-7777-777777777777")"));
}

TEST_CASE("a_run_hides_the_node_from_a_reader_that_may_not_see_it", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the node a reader may not read";

    result row;
    row.id = uuid_of("88888888-8888-8888-8888-888888888888");
    row.workunit_id = uuid_of("55555555-5555-5555-5555-555555555555");
    row.host_id = uuid_of(host_id);
    row.server_state = 5;
    row.outcome = 1;
    row.output_uri = "/api/v1/storage/ores/out.zip";

    const auto hidden = result_json(row, false);
    CHECK(states(hidden, R"("host_id":null)"));
    CHECK(states(hidden, R"("host_hidden":true)"));
    CHECK(states(hidden, R"("outcome":1)"));
    CHECK(states(hidden, R"("output_uri":"/api/v1/storage/ores/out.zip")"));

    const auto shown = result_json(row, true);
    CHECK(states(shown, R"("host_id":")" + host_id + R"(")"));
    CHECK(states(shown, R"("host_hidden":false)"));
}

TEST_CASE("a_run_that_has_no_node_and_has_not_started_states_none_of_them", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a run in flight";

    result row;
    row.id = uuid_of("88888888-8888-8888-8888-888888888888");
    row.workunit_id = uuid_of("55555555-5555-5555-5555-555555555555");

    const auto written = result_json(row, true);
    CHECK(states(written, R"("host_id":null)"));
    CHECK(states(written, R"("host_hidden":false)"));
    CHECK(states(written, R"("output_uri":null)"));
    CHECK(states(written, R"("error_message":null)"));
    CHECK(states(written, R"("received_at":null)"));
    CHECK(states(written, R"("started_at":null)"));
    CHECK(states(written, R"("duration_ms":null)"));
}

TEST_CASE("a_page_states_the_page_it_holds_and_the_count_it_matches", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the answer a page of rows states";

    page asked;
    asked.offset = 50;
    asked.limit = 10;
    const auto written = page_json({R"({"id":"one"})", R"({"id":"two"})"}, 99, asked);
    CHECK(states(written, R"("rows":[{"id":"one"},{"id":"two"}])"));
    CHECK(states(written, R"("total":99)"));
    CHECK(states(written, R"("offset":50)"));
    CHECK(states(written, R"("limit":10)"));
    CHECK(states(written, R"("read_at":")"));
}

TEST_CASE("a_page_states_the_default_page_when_the_caller_states_none", "[grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the default page a list answers with";

    ores::http::domain::http_request req;
    const auto asked = ores::http::routes::compute::grid::page_of(
        req, ores::http::routes::compute::grid::page_refused);
    CHECK(asked.offset == 0);
    CHECK(asked.limit == 25);
}
