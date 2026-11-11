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
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/synthetic/process_preview_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::synthetic::process_preview_routes;
using ores::http::synthetic::tests::answer_at;
using namespace ores::logging;

namespace {

http_status preview_status(const std::string& path, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<process_preview_routes>(http_method::post, path, req, path).status;
}

const std::string fx_paths = "/api/synthetic/simulate/fx";
const std::string ir_paths = "/api/synthetic/simulate/ir";
const std::string fx_convert = "/api/synthetic/convert/fx";
const std::string fx_simple = "/api/synthetic/convert/fx-simple";
const std::string ir_convert = "/api/synthetic/convert/ir";
const std::string ir_shape = "/api/synthetic/preview/ir-shape";

std::string fx_run(const std::string& means, const std::string& ticks) {
    return R"({"gmm_means":)" + means +
           R"(,"gmm_stdevs":[0.1],"gmm_weights":[1],)"
           R"("process_type":"geometric","initial_price":1.1,"num_ticks":)" +
           ticks + R"(,"num_paths":5,"seed":7})";
}

std::string fx_change(const std::string& to, const std::string& price) {
    return R"({"gmm_means":[0],"gmm_stdevs":[0.1],"gmm_weights":[1],)"
           R"("from_process_type":"geometric","to_process_type":")" +
           to + R"(","initial_price":)" + price + R"(,"ticks_per_hour":3600})";
}

std::string shape_of(const std::string& entries) {
    return R"({"process_type":"vasicek","parameters":[{"parameter_name":"kappa",)"
           R"("parameter_value":0.1}],"fixed_leg_payment_frequency_code":"Annual",)"
           R"("entries":)" +
           entries + "}";
}

const std::string ladder_row =
    R"({"sequence_index":0,"start_tenor_code":"0D","end_tenor_code":"1Y","instrument_code":"OIS"})";

}

TEST_CASE("process_previews_refuse_a_malformed_request", "[process_preview]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing each preview's refusals";

    CHECK(preview_status(fx_paths, fx_run("[]", "10")) == http_status::bad_request);
    CHECK(preview_status(fx_paths, fx_run("[0,1]", "10")) == http_status::bad_request);
    CHECK(preview_status(fx_paths, fx_run("[0]", "0")) == http_status::bad_request);
    CHECK(preview_status(fx_paths, "{}") == http_status::bad_request);

    CHECK(preview_status(fx_convert, fx_change("brownian", "1.1")) == http_status::bad_request);
    CHECK(preview_status(fx_convert, fx_change("arithmetic", "0")) == http_status::bad_request);

    CHECK(preview_status(fx_simple, R"({"process_type":"geometric","to_simple":true})") ==
          http_status::bad_request);
    CHECK(preview_status(fx_simple,
                         R"({"process_type":"levy","to_simple":true,"initial_price":1,)"
                         R"("ticks_per_hour":60})") == http_status::bad_request);

    CHECK(preview_status(ir_convert, R"({"process_type":" ","to_simple":true,"values":{}})") ==
          http_status::bad_request);
    CHECK(preview_status(ir_convert,
                         R"({"process_type":"vasicek","to_simple":true,"values":{"a":"x"}})") ==
          http_status::bad_request);

    CHECK(preview_status(ir_paths,
                         R"({"process_type":"vasicek","parameters":[{"parameter_name":"",)"
                         R"("parameter_value":1}],"num_ticks":10,"num_paths":2,"seed":1})") ==
          http_status::bad_request);

    CHECK(preview_status(ir_shape, shape_of("[]")) == http_status::bad_request);
    CHECK(preview_status(ir_shape,
                         shape_of(R"([{"sequence_index":-1,"start_tenor_code":"0D",)"
                                  R"("end_tenor_code":"1Y","instrument_code":"OIS"}])")) ==
          http_status::bad_request);
}

TEST_CASE("process_previews_forward_a_well_formed_request", "[process_preview]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an accepted preview reaches the service";

    // The client is not connected, so an accepted request fails at the forward step.
    CHECK(preview_status(fx_paths, fx_run("[0]", "10")) == http_status::bad_gateway);
    CHECK(preview_status(fx_convert, fx_change("ornstein_uhlenbeck", "1.1")) ==
          http_status::bad_gateway);
    CHECK(preview_status(fx_simple,
                         R"({"process_type":"arithmetic","to_simple":false,"initial_price":1.1,)"
                         R"("ticks_per_hour":60,"trend_pct_per_hour":0.5})") ==
          http_status::bad_gateway);
    CHECK(preview_status(ir_convert,
                         R"({"process_type":"vasicek","to_simple":true,)"
                         R"("values":{"kappa":0.1,"theta":0.03}})") == http_status::bad_gateway);
    CHECK(preview_status(ir_paths,
                         R"({"process_type":"vasicek","parameters":[{"parameter_name":"kappa",)"
                         R"("parameter_value":0.1}],"num_ticks":10,"num_paths":2,"seed":1})") ==
          http_status::bad_gateway);
    CHECK(preview_status(ir_shape, shape_of("[" + ladder_row + "]")) == http_status::bad_gateway);
}
