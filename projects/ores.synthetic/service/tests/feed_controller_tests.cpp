/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.
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
#include "../src/feed_controller.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <string>
#include <vector>

namespace {

const std::string tags("[feed_controller]");

using ores::marketdata::domain::IFeed;
using ores::synthetic::domain::binding_mode;
using ores::synthetic::feed::producer_subject;
using ores::synthetic::service::feed_binding_store;
using ores::synthetic::service::feed_controller;
using ores::synthetic::service::feeds_conflict;

const std::string test_bearer("test-session-token");

// A feed whose only job is to carry an identity into the controller: it
// publishes nothing and its start() returns at once, so a controller can be
// driven without a process, a NATS subject, or a tick thread that outlives
// the test.
class stub_feed final : public IFeed {
public:
    stub_feed(std::string kind, std::string source, std::string qualifier, std::string role)
        : kind_(std::move(kind))
        , source_(std::move(source))
        , qualifier_(std::move(qualifier))
        , role_(std::move(role)) {}

    const std::string& source_name() const override {
        return source_;
    }
    const std::string& qualifier() const override {
        return qualifier_;
    }
    const std::string& role() const override {
        return role_;
    }
    const std::string& nats_subject() const override {
        return subject_;
    }
    std::string_view kind() const override {
        return kind_;
    }
    std::string conflict_key() const override {
        return ores::marketdata::domain::feed_conflict_key(qualifier_, role_);
    }
    void start() override {}
    void stop() override {}
    std::uint64_t publish_count() const override {
        return 0;
    }

private:
    std::string kind_;
    std::string source_;
    std::string qualifier_;
    std::string role_;
    std::string subject_;
};

// In-memory feed_binding store. Mirrors the real store's one rule -- bind a
// source unless it is already bound -- so a second start of the same source
// is observable as a single recorded row rather than a second write.
class fake_binding_store final : public feed_binding_store {
public:
    std::expected<void, std::string>
    save_if_absent(const std::string& source_name, const std::string&) override {
        for (const auto& b : bindings_)
            if (b.source_name == source_name)
                return {};
        ores::marketdata::domain::feed_binding b;
        b.source_name = source_name;
        b.enabled = true;
        b.change_reason_code = "system.new_record";
        b.change_commentary = "Auto-created by feed_controller on feed start.";
        bindings_.push_back(b);
        return {};
    }

    std::vector<ores::marketdata::domain::feed_binding> bindings_;
};

// A controller wired to a fake store. The raw client is never connected: a
// stub feed publishes nothing, and the fake store never reaches NATS.
struct controller_fixture {
    ores::nats::service::client nats;
    ores::nats::service::nats_client auth_nats;
    std::shared_ptr<fake_binding_store> store = std::make_shared<fake_binding_store>();
    std::shared_ptr<feed_controller> ctrl;

    controller_fixture()
        : nats(ores::testing::make_nats_options())
        , auth_nats(nats, [](bool) { return std::string(); }) {
        ctrl = std::make_shared<feed_controller>(nats, auth_nats, store);
    }

    std::shared_ptr<IFeed> feed(const std::string& kind,
                                const std::string& source,
                                const std::string& qualifier,
                                const std::string& role = {}) {
        return std::make_shared<stub_feed>(kind, source, qualifier, role);
    }
};

// One producer kind's identity: an FX feed carries no role, an IR curve feed
// carries one. Everything the controller does with them is identical, which
// is the point of the two rows.
struct producer_kinds {
    std::string fx_kind = "fx_spot";
    std::string fx_source = "EUR_USD_GBM_1";
    std::string fx_qualifier = "EUR/USD";
    std::string fx_role;

    std::string ir_kind = "ir_curve";
    std::string ir_source = "usd.sofr";
    std::string ir_qualifier = "USD/SOFR";
    std::string ir_role = "self_discounting";
};

}

TEST_CASE("producer_subject: bound publishes on the source's tick subject, for every kind", tags) {
    CHECK(producer_subject("EUR_USD_GBM", binding_mode::bound) == "synthetic.v1.tick.EUR_USD_GBM");
    CHECK(producer_subject("usd.sofr", binding_mode::bound) == "synthetic.v1.tick.usd.sofr");
}

TEST_CASE("producer_subject: sandboxed publishes on a distinct subject the "
          "marketdata ingest loop never subscribes to, for every kind",
          tags) {
    const auto bound_subject = producer_subject("EUR_USD_GBM", binding_mode::bound);
    const auto sandboxed_subject = producer_subject("EUR_USD_GBM", binding_mode::sandboxed);

    CHECK(sandboxed_subject == "synthetic.v1.sandbox.tick.EUR_USD_GBM");
    CHECK(sandboxed_subject != bound_subject);
    CHECK(producer_subject("usd.sofr", binding_mode::sandboxed) ==
          "synthetic.v1.sandbox.tick.usd.sofr");
    // The ingest loop's one wildcard is "synthetic.v1.tick.>" (see
    // feed_ingest_loop.cpp) -- the sandboxed subject must not collide with
    // that prefix under any source_name, or the exclusion isn't real.
    CHECK(sandboxed_subject.starts_with("synthetic.v1.sandbox.tick."));
    CHECK_FALSE(bound_subject.starts_with("synthetic.v1.sandbox.tick."));
}

TEST_CASE("producer_subject: same source_name never collides across binding modes, "
          "for a variety of source names",
          tags) {
    const std::vector<std::string> sources{"eur.usd", "eur-usd", "EUR_USD_2", "weird name!*>"};
    for (const auto& source : sources) {
        const auto bound = producer_subject(source, binding_mode::bound);
        const auto sandboxed = producer_subject(source, binding_mode::sandboxed);
        CHECK(bound != sandboxed);
    }
}

TEST_CASE("producer_subject: unsafe characters are still replaced under sandboxed "
          "binding mode, matching bound's sanitisation",
          tags) {
    CHECK(producer_subject("weird name!*>", binding_mode::sandboxed) ==
          "synthetic.v1.sandbox.tick.weird_name___");
}

TEST_CASE("starting a bound feed creates its feed_binding, for FX and IR alike", tags) {
    producer_kinds kinds;
    controller_fixture f;

    const std::vector<std::shared_ptr<IFeed>> feeds{
        f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_qualifier, kinds.fx_role),
        f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_qualifier, kinds.ir_role)};
    REQUIRE(f.ctrl->start(feeds[0], binding_mode::bound, test_bearer) ==
            feed_controller::start_result::started);
    REQUIRE(f.ctrl->start(feeds[1], binding_mode::bound, test_bearer) ==
            feed_controller::start_result::started);

    REQUIRE(f.store->bindings_.size() == 2);
    CHECK(f.store->bindings_[0].source_name == kinds.fx_source);
    CHECK(f.store->bindings_[0].enabled == true);
    CHECK(f.store->bindings_[1].source_name == kinds.ir_source);
    CHECK(f.store->bindings_[1].enabled == true);
}

TEST_CASE("starting a sandboxed feed never creates a feed_binding, for FX and IR alike", tags) {
    producer_kinds kinds;
    controller_fixture f;

    REQUIRE(f.ctrl->start(f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_qualifier, kinds.fx_role),
                          binding_mode::sandboxed,
                          test_bearer) == feed_controller::start_result::started);
    REQUIRE(f.ctrl->start(f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_qualifier, kinds.ir_role),
                          binding_mode::sandboxed,
                          test_bearer) == feed_controller::start_result::started);

    CHECK(f.store->bindings_.empty());
    CHECK(f.ctrl->running_count() == 2);
}

TEST_CASE("starting a bound feed twice does not duplicate its feed_binding, for FX and IR alike",
          tags) {
    producer_kinds kinds;
    controller_fixture f;

    const std::vector<std::shared_ptr<IFeed>> feeds{
        f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_qualifier, kinds.fx_role),
        f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_qualifier, kinds.ir_role)};
    for (const auto& feed : feeds) {
        REQUIRE(f.ctrl->start(feed, binding_mode::bound, test_bearer) ==
                feed_controller::start_result::started);
        REQUIRE(f.ctrl->start(feed, binding_mode::bound, test_bearer) ==
                feed_controller::start_result::already_running);
    }

    REQUIRE(f.store->bindings_.size() == 2);
    CHECK(f.store->bindings_[0].source_name == kinds.fx_source);
    CHECK(f.store->bindings_[1].source_name == kinds.ir_source);
}

TEST_CASE("an already-running sandboxed feed is not bound by a re-start, whatever mode it requests",
          tags) {
    producer_kinds kinds;
    controller_fixture f;

    const auto feed = f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_qualifier, kinds.fx_role);
    REQUIRE(f.ctrl->start(feed, binding_mode::sandboxed, test_bearer) ==
            feed_controller::start_result::started);
    REQUIRE(f.ctrl->start(feed, binding_mode::bound, test_bearer) ==
            feed_controller::start_result::already_running);

    CHECK(f.store->bindings_.empty());
}

TEST_CASE("add() binds a feed of every kind through the same gate start() uses", tags) {
    producer_kinds kinds;
    controller_fixture f;

    CHECK(f.ctrl->add(f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_qualifier, kinds.fx_role),
                      binding_mode::bound,
                      test_bearer));
    CHECK(f.ctrl->add(f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_qualifier, kinds.ir_role),
                      binding_mode::sandboxed,
                      test_bearer));

    REQUIRE(f.store->bindings_.size() == 1);
    CHECK(f.store->bindings_[0].source_name == kinds.fx_source);
}

TEST_CASE("feeds_conflict: same qualifier and same role is a conflict — a second feed "
          "would publish into the same observation series",
          tags) {
    CHECK(feeds_conflict("USD-SOFR", "self_discounting", "USD-SOFR", "self_discounting"));
}

TEST_CASE("feeds_conflict: same qualifier with a different role is not a conflict — a "
          "discount feed and a projection feed for the same qualifier coexist",
          tags) {
    CHECK_FALSE(feeds_conflict("USD-SOFR", "self_discounting", "USD-SOFR", "projection"));
}

TEST_CASE("feeds_conflict: different qualifiers never conflict, regardless of role", tags) {
    CHECK_FALSE(feeds_conflict("USD-SOFR", "self_discounting", "EUR-EURIBOR", "self_discounting"));
    CHECK_FALSE(feeds_conflict("USD-SOFR", "self_discounting", "EUR-EURIBOR", "projection"));
}

TEST_CASE("feeds_conflict: an empty qualifier (an unparseable ORE key) has no published key "
          "to protect and conflicts with nothing",
          tags) {
    CHECK_FALSE(feeds_conflict("", "self_discounting", "", "self_discounting"));
    CHECK_FALSE(feeds_conflict("", "", "", ""));
    CHECK_FALSE(feeds_conflict("USD-SOFR", "self_discounting", "", "self_discounting"));
    CHECK_FALSE(feeds_conflict("", "self_discounting", "USD-SOFR", "self_discounting"));
}

TEST_CASE("feeds_conflict: FX feeds carry an empty role, making the comparison "
          "qualifier-only — two FX feeds on the same pair conflict",
          tags) {
    CHECK(feeds_conflict("FX/RATE/EUR/USD", "", "FX/RATE/EUR/USD", ""));
    CHECK_FALSE(feeds_conflict("FX/RATE/EUR/USD", "", "FX/RATE/GBP/USD", ""));
}
