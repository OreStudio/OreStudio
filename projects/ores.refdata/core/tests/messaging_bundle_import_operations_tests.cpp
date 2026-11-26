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
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.refdata.core/messaging/bundle_import_operations_registrar.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

// Proves the three bundle import operations are reachable: a request on each of
// them is answered over real NATS. The subjects were declared, generated and
// called by the ORE run configuration import for two months while no registrar
// subscribed them, and a caller could only find out by timing out.
//
// The answer here is a refusal, because the request carries no token: that is
// the point. Any answer means a handler is subscribed and ran its prelude. What
// each operation decides is service::bundle_import_operations_service, which
// bundle_import_commit_tests.cpp drives without NATS.

namespace {

using namespace ores::refdata::messaging;

struct bundle_import_operations_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    std::vector<ores::nats::service::subscription> subs;

    bundle_import_operations_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs = register_bundle_import_operations_handlers(nats, db.context(), std::nullopt);
    }

    /// Whether @p req on @p subject is answered with the error a caller with no
    /// token earns. A subject nobody subscribes throws before it returns.
    template <typename Req>
    bool refused(std::string_view subject, const Req& req) {
        const std::unordered_map<std::string, std::string> no_credentials;
        const auto reply = nats.request_sync(
            subject, ores::nats::default_wire_codec().encode(req), no_credentials);
        return reply.headers.contains(std::string(ores::nats::headers::x_error));
    }
};

}

TEST_CASE("every_bundle_import_operation_answers_on_its_subject", "[bundle_import][messaging]") {
    bundle_import_operations_fixture f;

    CHECK(f.refused(begin_bundle_import_request::nats_subject, begin_bundle_import_request{}));
    CHECK(f.refused(stage_bundle_family_request::nats_subject, stage_bundle_family_request{}));
    CHECK(f.refused(commit_bundle_import_request::nats_subject, commit_bundle_import_request{}));
}
