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
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http/routes/synthetic/calibration_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::synthetic::calibration_routes;
using ores::http::synthetic::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string calibrated_feed_id = "00000000-0000-0000-0000-0000000000f1";
const std::string other_tenant = "00000000-0000-0000-0000-0000000000a1";

http_request as_tenant(const std::string& tenant, const std::string& body) {
    http_request req;
    req.body = body;
    ores::security::jwt::jwt_claims claims;
    claims.subject = "admin";
    claims.tenant_id = tenant;
    req.authenticated_user = claims;
    return req;
}

http_status answer_of(
    http_method method,
    const std::string& path,
    const std::string& pattern,
    const std::string& body,
    const std::string& tenant = std::string(ores::http::net::record_routes::system_tenant_id)) {
    return answer_at<calibration_routes>(method, path, as_tenant(tenant, body), pattern).status;
}

const std::string run_pattern = "/api/synthetic/calibration/{assetClass}/run";
const std::string review_pattern = "/api/synthetic/calibration/variance/{id}/review";
const std::string policy_pattern = "/api/synthetic/calibration/{assetClass}/policy";
const std::string pin_pattern = "/api/synthetic/calibration/feeds/{id}/pin";

http_status run(const std::string& asset_class, const std::string& body) {
    return answer_of(
        http_method::post, "/api/synthetic/calibration/" + asset_class + "/run", run_pattern, body);
}

http_status
review(const std::string& body,
       const std::string& tenant = std::string(ores::http::net::record_routes::system_tenant_id)) {
    return answer_of(http_method::post,
                     "/api/synthetic/calibration/variance/" + calibrated_feed_id + "/review",
                     review_pattern,
                     body,
                     tenant);
}

http_status
policy(http_method method,
       const std::string& body,
       const std::string& tenant = std::string(ores::http::net::record_routes::system_tenant_id)) {
    return answer_of(method, "/api/synthetic/calibration/fx/policy", policy_pattern, body, tenant);
}

http_status pin(http_method method, const std::string& body, const std::string& tenant) {
    return answer_of(method,
                     "/api/synthetic/calibration/feeds/" + calibrated_feed_id + "/pin",
                     pin_pattern,
                     body,
                     tenant);
}

const std::string intent = R"("intent":{"reason_code":"system.calibration"})";

std::string policy_body(const std::string& window, const std::string& order) {
    return "{" + intent + R"(,"version":null,"window_days":)" + window +
           R"(,"allow_inverse":true,"fallback_code":"keep_fixed","provider_order":)" + order + "}";
}

std::string pin_body(const std::string& item, const std::string& provider) {
    return "{" + intent + R"(,"item":")" + item + R"(","provider_code":")" + provider +
           R"(","window_days":30})";
}

const std::string fixing = "oresmd://fx/EUR?quote=USD&type=fixing";

}

TEST_CASE("calibration_runs_refuse_a_malformed_request", "[calibration]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a calibration run's refusals";

    CHECK(run("FX", "") == http_status::bad_request);
    CHECK(run("fx", R"({"configId":"feed"})") == http_status::bad_request);
    CHECK(run("fx", R"({"configId":")" + calibrated_feed_id + R"(","apply":false})") ==
          http_status::bad_request);
    CHECK(run("fx", "") == http_status::bad_gateway);
    CHECK(run("fx", R"({"configId":")" + calibrated_feed_id + R"("})") == http_status::bad_gateway);
}

TEST_CASE("calibration_reviews_belong_to_the_system_tenant", "[calibration]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a review's refusals";

    const std::string approve = R"({"decision":"approve","reason":"Fits the history."})";
    CHECK(review(approve, other_tenant) == http_status::forbidden);
    CHECK(review(R"({"decision":"maybe","reason":"Fits."})") == http_status::bad_request);
    CHECK(review(R"({"decision":"reject","reason":"  "})") == http_status::bad_request);
    CHECK(review(R"({"decision":"reject","reason":")" + std::string(2001, 'r') + R"("})") ==
          http_status::bad_request);
    CHECK(review(R"({"decision":"reject","reason":"No.","by":"me"})") == http_status::bad_request);
    CHECK(review(approve) == http_status::bad_gateway);
}

TEST_CASE("calibration_policies_refuse_a_malformed_policy", "[calibration]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a policy write's refusals";

    CHECK(policy(http_method::put, policy_body("30", "[]"), other_tenant) ==
          http_status::forbidden);
    CHECK(policy(http_method::delete_, "{" + intent + "}", other_tenant) == http_status::forbidden);
    CHECK(policy(http_method::put, policy_body("1", "[]")) == http_status::bad_request);
    CHECK(policy(http_method::put, policy_body("181", "[]")) == http_status::bad_request);
    CHECK(policy(http_method::put, policy_body("30", R"(["bbg"," bbg "])")) ==
          http_status::bad_request);
    CHECK(policy(http_method::put, policy_body("30", R"([" "])")) == http_status::bad_request);
    CHECK(policy(http_method::put,
                 R"({"intent":{"reason_code":" "},"version":null,"window_days":30,)"
                 R"("allow_inverse":true,"fallback_code":"keep_fixed","provider_order":[]})") ==
          http_status::bad_request);
    CHECK(policy(http_method::put,
                 "{" + intent +
                     R"(,"version":null,"window_days":30,"allow_inverse":true,)"
                     R"("fallback_code":"guess","provider_order":[]})") ==
          http_status::bad_request);
    CHECK(policy(http_method::put,
                 "{" + intent +
                     R"(,"version":-1,"window_days":30,"allow_inverse":true,)"
                     R"("fallback_code":"refuse","provider_order":[]})") ==
          http_status::bad_request);
    CHECK(policy(http_method::delete_, "{}") == http_status::bad_request);
    CHECK(policy(http_method::put, policy_body("30", R"(["bbg","ecb"])")) ==
          http_status::bad_gateway);
    CHECK(policy(http_method::delete_, "{" + intent + "}") == http_status::bad_gateway);
}

TEST_CASE("calibration_pins_refuse_a_malformed_candidate", "[calibration]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a pin's refusals";

    CHECK(pin(http_method::put,
              pin_body("oresmd://fx/EUR?quote=USD&type=spot", "ecb"),
              other_tenant) == http_status::bad_request);
    CHECK(pin(http_method::put, pin_body("oresmd://ir/EUR?type=fixing", "ecb"), other_tenant) ==
          http_status::bad_request);
    CHECK(pin(http_method::put, pin_body(fixing, "ECB"), other_tenant) == http_status::bad_request);
    CHECK(pin(http_method::delete_, "{}", other_tenant) == http_status::bad_request);
    CHECK(answer_of(http_method::put,
                    "/api/synthetic/calibration/feeds/feed/pin",
                    pin_pattern,
                    pin_body(fixing, "ecb"),
                    other_tenant) == http_status::bad_request);
    // A feed belongs to its tenant, so any tenant reaches the service.
    CHECK(pin(http_method::put, pin_body(fixing, "ecb"), other_tenant) == http_status::bad_gateway);
    CHECK(pin(http_method::delete_, "{" + intent + "}", other_tenant) == http_status::bad_gateway);
}
