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
#include "curve_publish_fixture.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.core/repository/approval_kind_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/messaging/publish_due_curves_handler.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>

constexpr const char* curve_republish_tags = "[curve_republish]";

namespace {

using ores::inbox::service::approval_lifecycle;
using ores::marketdata::service::app::curve_publish_kind_code;
using ores::marketdata::service::tests::curve_fixture;
using ores::marketdata::service::tests::seed_person;

/**
 * @brief The seeded curve publish kind, approving every raise for one test.
 *
 * A kind states through its =auto_approval_code= which raises close without a
 * person. The row is put back as it was read when the test ends, even when the
 * test fails, because the suites that read the kind expect the seeded value.
 */
class zero_approval_curve_kind {
public:
    explicit zero_approval_curve_kind(ores::testing::scoped_database_helper& h)
        : ctx_(h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user())) {
        approval_lifecycle lifecycle(ctx_);
        const auto found = lifecycle.kind(std::string(curve_publish_kind_code));
        if (!found)
            throw std::runtime_error("The curve publish kind is not seeded.");
        // An aborted run can leave the zero-approval rule open in the installation's
        // tenant, which would approve every tenant's publish. Put the seeded rule
        // back before reading it, so this fixture never starts from a test rule.
        if (found->change_reason_code == "system.test" &&
            found->auto_approval_code != "system_tenant") {
            auto healed = *found;
            healed.auto_approval_code = "system_tenant";
            healed.change_reason_code = "system.test";
            healed.change_commentary = "Restored after an aborted zero-approval test";
            ores::inbox::repository::approval_kind_repository repo;
            repo.write(ctx_, healed);
        }
        const auto seeded = lifecycle.kind(std::string(curve_publish_kind_code));
        if (!seeded)
            throw std::runtime_error("The curve publish kind is not seeded.");
        was_ = *seeded;
        auto zeroed = *found;
        zeroed.auto_approval_code = "every_request";
        zeroed.change_reason_code = "system.test";
        zeroed.change_commentary = "Every raise approved, for the curve sweep test";
        ores::inbox::repository::approval_kind_repository repo;
        repo.write(ctx_, zeroed);
    }

    ~zero_approval_curve_kind() {
        try {
            ores::inbox::repository::approval_kind_repository repo;
            was_.change_reason_code = "system.test";
            was_.change_commentary = "Restored after the curve sweep test";
            repo.write(ctx_, was_);
        } catch (...) {
            // The seeded count is what the suite came for; a failure here must
            // not fail a test that has already asserted it.
        }
    }

    zero_approval_curve_kind(const zero_approval_curve_kind&) = delete;
    zero_approval_curve_kind& operator=(const zero_approval_curve_kind&) = delete;

private:
    ores::database::context ctx_;
    ores::inbox::domain::approval_kind was_;
};

}

using namespace ores::marketdata::messaging;

TEST_CASE("the_sweep_publishes_each_curve_and_names_the_one_that_cannot_build",
          curve_republish_tags) {
    curve_fixture f;
    zero_approval_curve_kind zeroed(f.h);
    const auto maker = seed_person(f.h);
    // A second config with no pillars: its bootstrap cannot run, so the sweep
    // must name it and leave the first curve's publish in place.
    f.seed_second_curve();

    const auto ctx =
        f.h.context().with_party(f.h.tenant_id(), f.party_id, {f.party_id}, maker.account.username);
    const auto resp = publish_due_curves_handler::publish_all(ctx);

    // One curve cannot build, so the firing did not do everything it was asked
    // even though the other curve published.
    CHECK_FALSE(resp.success);
    REQUIRE(resp.curves.size() == 2);

    const auto first = std::ranges::find_if(resp.curves, [&](const auto& one) {
        return one.bootstrap_config_id == boost::uuids::to_string(f.config_id);
    });
    REQUIRE(first != resp.curves.end());
    CHECK(first->published);
    CHECK(first->duration_ms >= 0);
    CHECK(f.output_point_count() == 2);

    const auto second = std::ranges::find_if(resp.curves, [&](const auto& one) {
        return one.bootstrap_config_id != boost::uuids::to_string(f.config_id);
    });
    REQUIRE(second != resp.curves.end());
    CHECK_FALSE(second->published);
    CHECK_FALSE(second->message.empty());
}

TEST_CASE("each_firing_supersedes_the_publish_the_one_before_it_made", curve_republish_tags) {
    curve_fixture f;
    zero_approval_curve_kind zeroed(f.h);
    const auto maker = seed_person(f.h);
    const auto ctx =
        f.h.context().with_party(f.h.tenant_id(), f.party_id, {f.party_id}, maker.account.username);

    // Two past instants, so the reader that serves the curve sees both sets.
    const auto first_at = std::chrono::system_clock::now() - std::chrono::hours(2);
    REQUIRE(publish_due_curves_handler::publish_all(ctx, first_at).success);
    REQUIRE(publish_due_curves_handler::publish_all(ctx, first_at + std::chrono::hours(1)).success);

    auto lg = ores::logging::make_logger("test.curve_republish");
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        "select (count(*) = 2)::text, (count(*) filter (where superseded_at is null) = 1)::text, "
        "(max(as_of) > min(as_of))::text "
        "from ores_marketdata_curve_publishes_tbl where output_series_id = $1::uuid "
        "and valid_to = ores_utility_infinity_timestamp_fn()",
        {boost::uuids::to_string(f.output_series_id)},
        lg,
        "reading the curve publishes of two firings");

    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().size() == 3);
    // Two sets were written, exactly one is live, and the live one is the later.
    CHECK(rows.front()[0].value_or("") == "true");
    CHECK(rows.front()[1].value_or("") == "true");
    CHECK(rows.front()[2].value_or("") == "true");
}
