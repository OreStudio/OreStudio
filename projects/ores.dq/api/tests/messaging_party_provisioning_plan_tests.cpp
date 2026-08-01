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
#include "ores.dq.api/messaging/party_provisioning_plan.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <vector>

namespace {

const std::string tags("[messaging]");

using namespace ores::dq::messaging;

const std::vector<party_bundle_publish_step> three_step_plan{
    {"bundle_a", "Bundle A"},
    {"bundle_b", "Bundle B"},
    {"bundle_c", "Bundle C"},
};

std::optional<publish_bundle_response> make_response() {
    publish_bundle_response resp;
    resp.success = true;
    resp.instance_id = "instance";
    resp.datasets_dispatched = 1;
    return resp;
}

}

TEST_CASE("publish_party_provisioning_plan_calls_on_step_once_per_step_in_order", tags) {
    std::vector<std::string> observed;
    publish_party_provisioning_plan(
        three_step_plan,
        boost::uuids::random_generator()(),
        [](const std::string&, const std::string&) { return make_response(); },
        [](const std::string&, std::size_t) { return true; },
        [&](const party_bundle_publish_step& step) { observed.push_back(step.bundle_code); });

    CHECK(observed == std::vector<std::string>{"bundle_a", "bundle_b", "bundle_c"});
}

TEST_CASE("publish_party_provisioning_plan_returns_true_when_every_step_succeeds", tags) {
    bool ok = publish_party_provisioning_plan(
        three_step_plan,
        boost::uuids::random_generator()(),
        [](const std::string&, const std::string&) { return make_response(); },
        [](const std::string&, std::size_t) { return true; });

    CHECK(ok);
}

TEST_CASE("publish_party_provisioning_plan_stops_and_returns_false_on_publish_failure", tags) {
    std::vector<std::string> published;
    bool ok = publish_party_provisioning_plan(
        three_step_plan,
        boost::uuids::random_generator()(),
        [&](const std::string& bundle_code,
            const std::string&) -> std::optional<publish_bundle_response> {
            published.push_back(bundle_code);
            if (bundle_code == "bundle_b")
                return std::nullopt;
            return make_response();
        },
        [](const std::string&, std::size_t) { return true; });

    CHECK(!ok);
    CHECK(published == std::vector<std::string>{"bundle_a", "bundle_b"});
}

TEST_CASE("publish_party_provisioning_plan_stops_and_returns_false_on_wait_failure", tags) {
    std::vector<std::string> waited;
    bool ok = publish_party_provisioning_plan(
        three_step_plan,
        boost::uuids::random_generator()(),
        [](const std::string&, const std::string&) { return make_response(); },
        [&](const std::string&, std::size_t) {
            waited.push_back("wait");
            return waited.size() != 1;
        });

    CHECK(!ok);
    CHECK(waited.size() == 1);
}
