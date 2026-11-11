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
#include "ores.http.core/routes/site_routes.hpp"
#include "ores.http.core/site/site_configuration.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <string>

using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::site_routes;
using namespace ores::http::site;

namespace {

const std::string two_environments = R"({
  "active": "local",
  "environments": [
    {"id": "local", "displayName": "Local", "host": "localhost", "port": 4222,
     "subjectPrefix": "ores.local", "nonProduction": true},
    {"id": "prod", "displayName": "Production", "description": "Live",
     "host": "nats.prod", "port": 4223, "subjectPrefix": "ores.prod",
     "nonProduction": false, "tls": {"ca": "/etc/ca.pem"}}
  ],
  "tls": {},
  "developerTools": true,
  "developerAccounts": [{"username": "alice", "label": "Trader"}]
})";

std::string site_file(const std::string& text) {
    const auto path = std::filesystem::temp_directory_path() / "ores_site_config_tests.json";
    std::ofstream(path) << text;
    return path.string();
}

variable_lookup variables(std::map<std::string, std::string> values) {
    return [values](std::string_view name) -> std::optional<std::string> {
        const auto found = values.find(std::string(name));
        return found == values.end() ? std::nullopt : std::optional(found->second);
    };
}

bool refused(const std::string& text) {
    try {
        (void)parse_site(text, "test.json");
        return false;
    } catch (const site_configuration_error&) {
        return true;
    }
}

http_request with_body(std::string body) {
    http_request req;
    req.body = std::move(body);
    return req;
}

std::string entry(const std::string& logger, const std::string& extra = "") {
    return R"({"entries":[{"at":"2026-11-11T00:00:00Z","level":"info","logger":")" + logger +
           R"(","message":"opened the blotter")" + extra + "}]}";
}

}

TEST_CASE("site_configuration_fills_the_defaults_the_schema_states", "[site]") {
    const auto site = parse_site(two_environments, "test.json");
    REQUIRE(site.environments.size() == 2);
    CHECK(site.environments[0].description.empty());
    CHECK(site.environments[0].httpBaseUrl.empty());
    CHECK_FALSE(site.environments[0].tls.ca);
    CHECK(site.environments[1].tls.ca == "/etc/ca.pem");
    CHECK(site.developerAccounts[0].description.empty());
}

TEST_CASE("site_configuration_refuses_a_file_it_cannot_serve", "[site]") {
    CHECK(refused("not json"));
    CHECK(refused(R"({"environments": [], "tls": {}})"));
    CHECK(refused(R"({"environments": [{"id": "Bad", "displayName": "x", "host": "h",
        "port": 1, "subjectPrefix": "p"}], "tls": {}})"));
    CHECK(refused(R"({"environments": [{"id": "a", "displayName": "x", "host": "h",
        "port": 70000, "subjectPrefix": "p"}], "tls": {}})"));
    CHECK(refused(R"({"environments": [{"id": "a", "displayName": "x", "host": "h",
        "port": 1, "subjectPrefix": "p"}, {"id": "a", "displayName": "y", "host": "h",
        "port": 1, "subjectPrefix": "p"}], "tls": {}})"));
    CHECK(refused(R"({"active": "b", "environments": [{"id": "a", "displayName": "x",
        "host": "h", "port": 1, "subjectPrefix": "p"}], "tls": {}})"));
}

TEST_CASE("site_configuration_chooses_the_environment_in_the_bff_order", "[site]") {
    const auto path = site_file(two_environments);
    CHECK(load_site(path, "", variables({})).served.id == "local");
    CHECK(load_site(path, "prod", variables({{"ORES_WEB_ENV", "local"}})).served.id == "prod");
    CHECK(load_site(path, "", variables({{"ORES_WEB_ENV", "prod"}, {"ORES_ENV_NAME", "local"}}))
              .served.id == "prod");
    CHECK(load_site(path, "", variables({{"ORES_ENV_NAME", "prod"}})).served.id == "prod");
    CHECK(load_site("", "", variables({{"ORES_WEB_SITE_CONFIG", path}})).source ==
          std::filesystem::absolute(path).string());
    CHECK_THROWS_AS(load_site(path, "staging", variables({})), site_configuration_error);
    CHECK_THROWS_AS(load_site("", "", variables({})), site_configuration_error);
}

TEST_CASE("site_configuration_lets_the_deployment_override_the_developer_surface", "[site]") {
    const auto path = site_file(two_environments);
    CHECK(load_site(path, "", variables({})).configuration.developerTools);
    CHECK_FALSE(load_site(path, "", variables({{"ORES_WEB_DEVELOPER_TOOLS", "0"}}))
                    .configuration.developerTools);
    CHECK(load_site(path, "", variables({{"ORES_WEB_DEVELOPER_TOOLS", "TRUE"}}))
              .configuration.developerTools);
}

TEST_CASE("site_configuration_reads_the_file_ores_web_ships", "[site]") {
    const auto path =
        ores::testing::project_root::resolve("projects/ores.web/config/environments.json");
    const auto site = load_site(path.string(), "", variables({}));
    CHECK_FALSE(site.served.id.empty());
}

TEST_CASE("site_routes_state_offers_test_accounts_only_with_the_developer_surface", "[site]") {
    const auto path = site_file(two_environments);
    const auto open = site_routes::site_state(load_site(path, "prod", variables({})));
    CHECK(open.body.find(R"("appName":"ORE Studio")") != std::string::npos);
    CHECK(
        open.body.find(
            R"("environment":{"id":"prod","displayName":"Production","description":"Live","nonProduction":false})") !=
        std::string::npos);
    CHECK(open.body.find(R"("username":"alice")") != std::string::npos);
    const auto closed = site_routes::site_state(
        load_site(path, "", variables({{"ORES_WEB_DEVELOPER_TOOLS", "0"}})));
    CHECK(closed.body.find(R"("developerAccounts":[])") != std::string::npos);
}

TEST_CASE("site_routes_deployment_is_absent_without_the_developer_surface", "[site]") {
    const auto path = site_file(two_environments);
    const auto absent = site_routes::deployment(
        load_site(path, "", variables({{"ORES_WEB_DEVELOPER_TOOLS", "0"}})));
    CHECK(absent.status == http_status::not_found);
    CHECK(absent.body.find(R"("code":"invalid-request")") != std::string::npos);
    const auto shown = site_routes::deployment(load_site(path, "local", variables({})));
    CHECK(shown.status == http_status::ok);
    CHECK(shown.body.find(R"("host":"localhost")") != std::string::npos);
    CHECK(shown.body.find(R"("tls":{})") != std::string::npos);
    CHECK(
        shown.body.find(
            R"("available":[{"id":"local","displayName":"Local","nonProduction":true},{"id":"prod","displayName":"Production","nonProduction":false}])") !=
        std::string::npos);
}

TEST_CASE("site_routes_client_log_takes_only_the_browser_trail", "[site]") {
    CHECK(site_routes::read_client_log(entry("ores.web.client")));
    CHECK(site_routes::read_client_log(
        entry("ores.web.client.blotter", R"(,"fields":{"rows":3,"ok":true,"tab":"fx"})")));
    CHECK_FALSE(site_routes::read_client_log(entry("ores.iam.service")));
    CHECK_FALSE(site_routes::read_client_log(entry("ores.web.client", R"(,"fields":{"Bad":1})")));
    CHECK_FALSE(site_routes::read_client_log(entry("ores.web.client", R"(,"fields":{"k":[1]})")));
    CHECK_FALSE(site_routes::read_client_log(
        R"({"entries":[{"at":"x","level":"fatal","logger":"ores.web.client","message":"m"}]})"));
    CHECK_FALSE(site_routes::read_client_log("{}"));
    std::string many = R"({"entries":[)";
    for (int i = 0; i < 101; ++i)
        many += std::string(i ? "," : "") +
                R"({"at":"x","level":"info","logger":"ores.web.client","message":"m"})";
    CHECK_FALSE(site_routes::read_client_log(many + "]}"));
}

TEST_CASE("site_routes_client_log_answers_as_the_bff_does", "[site]") {
    const auto ok = site_routes::handle_client_log(with_body(entry("ores.web.client")));
    CHECK(ok.status == http_status::ok);
    CHECK(ok.body == R"({"ok":true})");
    const auto bad = site_routes::handle_client_log(with_body("{}"));
    CHECK(bad.status == http_status::bad_request);
}

TEST_CASE("site_routes_register_the_liveness_answer_without_a_site", "[site]") {
    auto router = std::make_shared<ores::http::net::router>();
    site_routes::register_routes(
        router, std::make_shared<ores::http::openapi::endpoint_registry>(), std::nullopt);
    const auto& routes = router->routes();
    const auto has = [&](const std::string& pattern, bool auth) {
        return std::ranges::any_of(
            routes, [&](const auto& r) { return r.pattern == pattern && r.requires_auth == auth; });
    };
    CHECK(has("/api/health", false));
    CHECK(has("/api/client-log", true));
    CHECK_FALSE(has("/api/site", false));
}

TEST_CASE("site_routes_client_log_keeps_each_entry_on_one_journal_line", "[site]") {
    const auto entries = site_routes::read_client_log(
        R"({"entries":[{"at":"t\r","level":"info","logger":"ores.web.client",)"
        R"("message":"a\nlevel=error forged","fields":{"note":"x\ny","big":18446744073709551615}}]})");
    REQUIRE(entries);
    const auto& entry = entries->front();
    CHECK(entry.message == R"(a\nlevel=error forged)");
    CHECK(entry.at == R"(t\r)");
    CHECK(entry.fields[0].second == R"(x\ny)");
    CHECK(entry.fields[1].second == "18446744073709551615");
}
