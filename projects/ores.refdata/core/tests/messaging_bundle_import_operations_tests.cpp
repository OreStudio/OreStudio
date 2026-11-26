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
#include "ores.logging/logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.refdata.core/messaging/bundle_import_operations_registrar.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>
#include <vector>

// Holds the registration of the three bundle import operations: the subjects
// are declared with responses and called by the ORE run configuration import,
// and a caller that finds nobody home only learns it by timing out.
//
// The case reads what the registrar returns rather than sending a request, so
// it holds whether the subjects are subscribed whether or not the fleet is up.
// What each operation decides is service::bundle_import_operations_service,
// which bundle_import_commit_tests.cpp drives without NATS; that a live request
// is answered is held by build/scripts/check_subject_coverage.py.

namespace {

inline auto& lg() {
    static auto instance = ores::logging::make_logger("ores.refdata.core.tests");
    return instance;
}

struct bundle_import_operations_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    std::vector<ores::nats::service::subscription> subs;

    bundle_import_operations_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs = ores::refdata::messaging::register_bundle_import_operations_handlers(
            nats, db.context(), std::nullopt);
    }

    [[nodiscard]] std::vector<std::string> subjects() const {
        std::vector<std::string> subscribed;
        subscribed.reserve(subs.size());
        for (const auto& sub : subs)
            subscribed.push_back(sub.subject());
        return subscribed;
    }
};

}

TEST_CASE("every_bundle_import_operation_is_subscribed", "[bundle_import][messaging]") {
    bundle_import_operations_fixture f;
    const auto subscribed = f.subjects();
    ORES_LOG_SEV(lg(), info) << "subscribed " << subscribed.size() << " subject(s)";

    const std::vector<std::string_view> wanted{
        ores::refdata::messaging::begin_bundle_import_request::nats_subject,
        ores::refdata::messaging::stage_bundle_family_request::nats_subject,
        ores::refdata::messaging::commit_bundle_import_request::nats_subject,
    };
    for (const auto subject : wanted)
        CHECK(std::ranges::find(subscribed, f.nats.make_subject(subject)) != subscribed.end());
    CHECK(subscribed.size() == wanted.size());
}
