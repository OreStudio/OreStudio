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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http/routes/compute/compute_routes.hpp"
#include "ores.http/routes/compute/download_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::compute::tests::answer_at;
using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::compute::disposition_for;
using ores::http::routes::compute::download_name;
using ores::http::routes::compute::compute_routes;
using ores::http::routes::compute::content_type_of;
using ores::http::routes::compute::stored_key;
using namespace ores::logging;

namespace {

const std::string input_pattern = "/api/operations/grid/jobs/workunits/{id}/input";
const std::string output_pattern = "/api/operations/grid/jobs/results/{id}/output";

const std::string an_object_id = "11111111-1111-1111-1111-111111111111";
const std::string a_workunit_input =
    "/api/operations/grid/jobs/workunits/" + an_object_id + "/input";
const std::string a_result_output = "/api/operations/grid/jobs/results/" + an_object_id + "/output";

const std::string a_stored_input =
    "/api/v1/storage/ores/compute/input/11111111-1111-1111-1111-111111111111.tar.gz";

/// The answer a download gives at an address, on a client that is not connected.
ores::http::domain::http_response answer_of(const std::string& path, const std::string& pattern) {
    return answer_at<compute_routes>(http_method::get, path, http_request{}, pattern);
}

bool response_holds(const std::string& json, const std::string& part) {
    return json.find(part) != std::string::npos;
}

}

TEST_CASE("a_download_refuses_an_object_identifier_that_is_not_one", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifier a download names";

    const auto input = answer_at<compute_routes>(http_method::get,
                                                 "/api/operations/grid/jobs/workunits/x/input",
                                                 http_request{},
                                                 input_pattern);
    CHECK(input.status == http_status::bad_request);
    CHECK(response_holds(input.body, "not a uuid"));

    const auto output = answer_at<compute_routes>(http_method::get,
                                                  "/api/operations/grid/jobs/results/x/output",
                                                  http_request{},
                                                  output_pattern);
    CHECK(output.status == http_status::bad_request);
    CHECK(response_holds(output.body, "not a uuid"));
}

TEST_CASE("a_download_answers_the_service_is_absent", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a download with no service behind it";

    CHECK(answer_of(a_workunit_input, input_pattern).status == http_status::bad_gateway);
    CHECK(answer_of(a_result_output, output_pattern).status == http_status::bad_gateway);
}

TEST_CASE("a_download_refuses_a_parameter_it_does_not_take", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parameter a download does not name";

    http_request req;
    req.query_params["name"] = "input.tar.gz";
    const auto input =
        answer_at<compute_routes>(http_method::get, a_workunit_input, req, input_pattern);
    CHECK(input.status == http_status::bad_request);
    CHECK(response_holds(input.body, "name"));
}

TEST_CASE("a_stored_address_names_the_key_under_the_platform_bucket", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the object a stored address names";

    CHECK(stored_key(a_stored_input) ==
          "compute/input/11111111-1111-1111-1111-111111111111.tar.gz");
    CHECK_FALSE(
        stored_key("compute/input/11111111-1111-1111-1111-111111111111.tar.gz").has_value());
    CHECK_FALSE(stored_key("").has_value());
    CHECK_FALSE(stored_key("/api/v1/storage/another/object.tar.gz").has_value());
    CHECK_FALSE(stored_key("/api/v1/storage/ores").has_value());
    CHECK_FALSE(stored_key("/api/v1/storage/ores/").has_value());
    CHECK_FALSE(stored_key("/somewhere/else/object.tar.gz").has_value());
}

TEST_CASE("a_download_is_saved_under_the_name_of_its_object", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the name a download is saved as";

    CHECK(download_name("compute/output/abc.tar.gz") == "abc.tar.gz");
    CHECK(download_name("compute/input/abc") == "abc");
    CHECK(download_name("abc") == "abc");
    CHECK(download_name("compute/output/") == "");
}

TEST_CASE("a_download_name_reaches_the_header_as_printable_ascii", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the header a download name reaches";

    CHECK(disposition_for("report.tar.gz") == R"(attachment; filename="report.tar.gz")");
    CHECK(disposition_for("a\"b\\c") == R"(attachment; filename="a_b_c")");
    CHECK(disposition_for(std::string("a\nb\tc")) == R"(attachment; filename="a_b_c")");
    CHECK(disposition_for("caf\u00e9.tar.gz") == R"(attachment; filename="caf__.tar.gz")");
}

TEST_CASE("a_download_states_the_type_its_object_is_stored_as", "[grid_downloads]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the type a download states";

    CHECK(content_type_of("compute/output/abc.tar.gz") == "application/gzip");
    CHECK(content_type_of("compute/input/abc.tgz") == "application/gzip");
    CHECK(content_type_of("compute/config/abc.xml") == "application/octet-stream");
    CHECK(content_type_of("compute/input/abc") == "application/octet-stream");
}
