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
#include "ores.compute.api/generators/app_generator.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.core/repository/app_repository.hpp"
#include "ores.compute.core/service/app_version_service.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <string>
#include <vector>

/*
 * An app version is keyed by its id. Two versions of one app that share a wrapper
 * version, such as two engine builds wrapped by the same wrapper, must each read
 * their own row when a write states the version it read. An earlier key, the
 * wrapper version, made the second version read the first one's row and refuse
 * every republish with a version conflict.
 */

namespace {

const std::string tags("[service][app_version]");

using ores::compute::messaging::put_app_version_request;
using ores::compute::service::app_version_service;
using ores::testing::database_helper;
using ores::utility::domain::outcome;
using ores::utility::domain::precondition_kind;

put_app_version_request request_for(const boost::uuids::uuid& id,
                                    const boost::uuids::uuid& app_id,
                                    const std::string& engine_version,
                                    const std::string& release_note) {
    put_app_version_request r;
    r.change.write.id = id;
    r.change.write.app_id = app_id;
    r.change.write.wrapper_version = "1.0.0";
    r.change.write.engine_version = engine_version;
    r.change.write.min_ram_mb = 0;
    r.change.write.release_note = release_note;
    r.intent.reason_code = "system.test";
    return r;
}

/// Writes the row, creating it when no version is stated and matching the version otherwise.
outcome write(app_version_service& service, put_app_version_request r, std::uint32_t version) {
    if (version == 0) {
        r.change.precondition.kind = precondition_kind::must_not_exist;
    } else {
        r.change.precondition.kind = precondition_kind::must_match_version;
        r.change.precondition.version = version;
    }
    return service.put_app_version(r).result.outcome;
}

} // namespace

TEST_CASE("two versions that share a wrapper version republish each on its own row", tags) {
    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto app = ores::compute::generators::generate_synthetic_app(gen_ctx);
    app.change_reason_code = "system.test";
    ores::compute::repository::app_repository{}.write(h.context(), app);

    app_version_service service(h.context());
    const auto first = boost::uuids::random_generator()();
    const auto second = boost::uuids::random_generator()();

    REQUIRE(write(service, request_for(first, app.id, "1.8.16.0", ""), 0) == outcome::ok);
    REQUIRE(write(service, request_for(second, app.id, "1.8.17.0", ""), 0) == outcome::ok);

    CHECK(write(service, request_for(first, app.id, "1.8.16.0", "first"), 1) == outcome::ok);
    CHECK(write(service, request_for(second, app.id, "1.8.17.0", "second"), 1) == outcome::ok);
    CHECK(write(service, request_for(first, app.id, "1.8.16.0", "first again"), 2) == outcome::ok);
    CHECK(write(service, request_for(second, app.id, "1.8.17.0", "second again"), 2) ==
          outcome::ok);
}

TEST_CASE("a stale version is still refused for the row it names", tags) {
    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto app = ores::compute::generators::generate_synthetic_app(gen_ctx);
    app.change_reason_code = "system.test";
    ores::compute::repository::app_repository{}.write(h.context(), app);

    app_version_service service(h.context());
    const auto id = boost::uuids::random_generator()();
    REQUIRE(write(service, request_for(id, app.id, "1.8.16.0", ""), 0) == outcome::ok);
    REQUIRE(write(service, request_for(id, app.id, "1.8.16.0", "note"), 1) == outcome::ok);

    CHECK(write(service, request_for(id, app.id, "1.8.16.0", "stale"), 1) == outcome::conflict);
}

TEST_CASE("the versions of an app are listed without the versions of another app", tags) {
    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto first_app = ores::compute::generators::generate_synthetic_app(gen_ctx);
    auto second_app = ores::compute::generators::generate_synthetic_app(gen_ctx);
    first_app.change_reason_code = "system.test";
    second_app.change_reason_code = "system.test";
    ores::compute::repository::app_repository{}.write(h.context(), first_app);
    ores::compute::repository::app_repository{}.write(h.context(), second_app);

    app_version_service service(h.context());
    const auto a1 = boost::uuids::random_generator()();
    const auto a2 = boost::uuids::random_generator()();
    const auto b1 = boost::uuids::random_generator()();
    REQUIRE(write(service, request_for(a1, first_app.id, "1.0", ""), 0) == outcome::ok);
    REQUIRE(write(service, request_for(a2, first_app.id, "2.0", ""), 0) == outcome::ok);
    REQUIRE(write(service, request_for(b1, second_app.id, "9.0", ""), 0) == outcome::ok);

    ores::compute::messaging::list_by_app_id_app_versions_request request;
    request.app_id = first_app.id;
    const auto reply = service.list_by_app_id_app_versions(request);

    REQUIRE(reply.result.outcome == outcome::ok);
    CHECK(reply.total == 2);
    std::vector<std::string> engines;
    for (const auto& v : reply.app_versions)
        engines.push_back(v.engine_version);
    std::ranges::sort(engines);
    CHECK(engines == std::vector<std::string>{"1.0", "2.0"});
}

TEST_CASE("the list of the versions of an app refuses a subtree and an order it cannot serve",
          tags) {
    database_helper h;
    app_version_service service(h.context());

    ores::compute::messaging::list_by_app_id_app_versions_request subtree;
    subtree.app_id = boost::uuids::random_generator()();
    subtree.scope = ores::utility::domain::scope::subtree;
    CHECK(service.list_by_app_id_app_versions(subtree).result.outcome == outcome::invalid);

    ores::compute::messaging::list_by_app_id_app_versions_request unsortable;
    unsortable.app_id = boost::uuids::random_generator()();
    unsortable.order.field = "wrapper_version";
    CHECK(service.list_by_app_id_app_versions(unsortable).result.outcome == outcome::invalid);
}
