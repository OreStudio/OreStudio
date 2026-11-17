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
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::compute::tests::answer_at;
using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::compute::compute_routes;
using namespace ores::logging;

namespace {

const std::string batches_pattern = "/api/operations/grid/jobs/batches";
const std::string batch_pattern = "/api/operations/grid/jobs/batches/{id}";
const std::string batch_workunits_pattern = "/api/operations/grid/jobs/batches/{id}/workunits";
const std::string workunit_pattern = "/api/operations/grid/jobs/workunits/{id}";
const std::string workunit_results_pattern = "/api/operations/grid/jobs/workunits/{id}/results";
const std::string results_pattern = "/api/operations/grid/jobs/results";
const std::string result_pattern = "/api/operations/grid/jobs/results/{id}";

const std::string an_id = "11111111-1111-1111-1111-111111111111";
const std::string a_batch_path = "/api/operations/grid/jobs/batches/" + an_id;
const std::string a_workunit = "/api/operations/grid/jobs/workunits/" + an_id;
const std::string a_result = "/api/operations/grid/jobs/results/" + an_id;

const std::string page_refused =
    "The jobs read takes an offset of 0 or more and a limit of 1 to 200.";
const std::string batch_refused =
    "The batch list takes a page, a status of up to 32 characters, a party id and up to 256 "
    "characters of the reference.";
const std::string runs_refused =
    "The runs read takes an outcome code of 0, 1, 3 or 4, an offset of 0 or more and a limit of "
    "1 to 200.";
const std::string id_refused = "The address names an id that is not a uuid.";

/// Every jobs address, with the pattern it reaches and the query it takes.
struct address final {
    std::string path;
    std::string pattern;
    std::vector<std::pair<std::string, std::string>> query;
};

std::vector<address> addresses() {
    return {{batches_pattern, batches_pattern, {}},
            {a_batch_path, batch_pattern, {}},
            {a_batch_path + "/workunits", batch_workunits_pattern, {}},
            {a_workunit, workunit_pattern, {}},
            {a_workunit + "/results", workunit_results_pattern, {}},
            {results_pattern, results_pattern, {{"outcome", "1"}}},
            {a_result, result_pattern, {}}};
}

http_request asking(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

bool body_holds(const std::string& json, const std::string& member) {
    return json.find(member) != std::string::npos;
}

}

TEST_CASE("every_jobs_route_is_served_and_requires_a_session", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the addresses the jobs routes are served at";

    for (const auto& stated : addresses()) {
        const auto answer = answer_at<compute_routes>(
            http_method::get, stated.path, asking(stated.query), stated.pattern);
        CHECK(answer.status == http_status::bad_gateway);
    }
}

TEST_CASE("the_batch_list_refuses_a_page_that_is_not_one", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page the batch list refuses";

    const std::vector<std::pair<std::string, std::string>> pages = {
        {"offset", "-1"}, {"offset", "ten"}, {"limit", "0"}, {"limit", "201"}, {"limit", "ten"}};
    for (const auto& page : pages) {
        const auto answer = answer_at<compute_routes>(
            http_method::get, batches_pattern, asking({page}), batches_pattern);
        CHECK(answer.status == http_status::bad_request);
        CHECK(body_holds(answer.body, batch_refused));
    }
}

TEST_CASE("the_batch_list_refuses_a_status_and_a_reference_that_are_not_ones", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the narrowing the batch list refuses";

    for (const auto& query : std::vector<std::vector<std::pair<std::string, std::string>>>{
             {{"status", std::string(33, 'a')}},
             {{"status", "   "}},
             {{"party", "acme"}},
             {{"find", std::string(257, 'a')}}}) {
        const auto answer = answer_at<compute_routes>(
            http_method::get, batches_pattern, asking(query), batches_pattern);
        CHECK(answer.status == http_status::bad_request);
        CHECK(body_holds(answer.body, batch_refused));
    }
}

TEST_CASE("the_batch_list_takes_a_status_a_party_and_part_of_a_reference", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the narrowing the batch list takes";

    const auto answer = answer_at<compute_routes>(http_method::get,
                                                  batches_pattern,
                                                  asking({{"status", "closed"},
                                                          {"party", an_id},
                                                          {"find", "nightly"},
                                                          {"offset", "0"},
                                                          {"limit", "200"}}),
                                                  batches_pattern);
    CHECK(answer.status == http_status::bad_gateway);
}

TEST_CASE("a_jobs_route_refuses_a_parameter_it_does_not_take", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parameter the jobs routes do not name";

    const auto answer = answer_at<compute_routes>(
        http_method::get, batches_pattern, asking({{"sort", "status"}}), batches_pattern);
    CHECK(answer.status == http_status::bad_request);
    CHECK(body_holds(answer.body, "sort"));

    const auto page = answer_at<compute_routes>(http_method::get,
                                                a_batch_path + "/workunits",
                                                asking({{"status", "closed"}}),
                                                batch_workunits_pattern);
    CHECK(page.status == http_status::bad_request);
}

TEST_CASE("a_job_is_named_by_an_identifier", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifier a job address names";

    const auto not_an_id = std::pair{"/api/operations/grid/jobs/batches/not-an-id", batch_pattern};
    const auto workunit_id =
        std::pair{"/api/operations/grid/jobs/workunits/not-an-id", workunit_pattern};
    const auto result_id = std::pair{"/api/operations/grid/jobs/results/not-an-id", result_pattern};
    const auto workunits_of =
        std::pair{"/api/operations/grid/jobs/batches/not-an-id/workunits", batch_workunits_pattern};
    const auto results_of = std::pair{"/api/operations/grid/jobs/workunits/not-an-id/results",
                                      workunit_results_pattern};

    for (const auto& [path, pattern] :
         {not_an_id, workunit_id, result_id, workunits_of, results_of}) {
        const auto answer =
            answer_at<compute_routes>(http_method::get, path, http_request{}, pattern);
        CHECK(answer.status == http_status::bad_request);
        CHECK(body_holds(answer.body, id_refused));
    }
}

TEST_CASE("the_runs_read_refuses_an_outcome_it_does_not_name", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the outcome codes the runs read names";

    const auto absent = answer_at<compute_routes>(
        http_method::get, results_pattern, http_request{}, results_pattern);
    CHECK(absent.status == http_status::bad_request);
    CHECK(body_holds(absent.body, runs_refused));

    for (const auto& code : {"2", "5", "x", "10"}) {
        const auto answer = answer_at<compute_routes>(
            http_method::get, results_pattern, asking({{"outcome", code}}), results_pattern);
        CHECK(answer.status == http_status::bad_request);
        CHECK(body_holds(answer.body, runs_refused));
    }

    for (const auto& code : {"0", "1", "3", "4"}) {
        const auto answer = answer_at<compute_routes>(
            http_method::get, results_pattern, asking({{"outcome", code}}), results_pattern);
        CHECK(answer.status == http_status::bad_gateway);
    }
}

TEST_CASE("the_runs_read_refuses_a_page_that_is_not_one", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page the runs read refuses";

    const std::vector<std::pair<std::string, std::string>> pages = {{"offset", "-1"},
                                                                    {"limit", "1000"}};
    for (const auto& page : pages) {
        auto req = asking({{"outcome", "1"}});
        req.query_params[page.first] = page.second;
        const auto answer =
            answer_at<compute_routes>(http_method::get, results_pattern, req, results_pattern);
        CHECK(answer.status == http_status::bad_request);
        CHECK(body_holds(answer.body, runs_refused));
    }
}

TEST_CASE("the_children_of_a_job_refuse_a_page_that_is_not_one", "[grid_jobs]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page the children of a job refuse";

    const auto workunits = answer_at<compute_routes>(http_method::get,
                                                     a_batch_path + "/workunits",
                                                     asking({{"limit", "201"}}),
                                                     batch_workunits_pattern);
    CHECK(workunits.status == http_status::bad_request);
    CHECK(body_holds(workunits.body, page_refused));

    const auto results = answer_at<compute_routes>(http_method::get,
                                                   a_workunit + "/results",
                                                   asking({{"offset", "less"}}),
                                                   workunit_results_pattern);
    CHECK(results.status == http_status::bad_request);
    CHECK(body_holds(results.body, page_refused));
}
