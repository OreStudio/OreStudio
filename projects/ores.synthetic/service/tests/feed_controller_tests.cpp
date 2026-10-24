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

using ores::marketdata::domain::i_feed;
using ores::synthetic::feed::producer_subject;
using ores::synthetic::service::feed_binding_store;
using ores::synthetic::service::feed_controller;
using ores::synthetic::service::feeds_conflict;
using ores::synthetic::service::marketdata_binding_store;

const std::string test_bearer("test-session-token");

// A feed whose only job is to carry an identity into the controller: it
// publishes nothing and its start() returns at once, so a controller can be
// driven without a process, a NATS subject, or a tick thread that outlives
// the test.
class stub_feed final : public i_feed {
public:
    stub_feed(std::string kind, std::string source, std::string oresmd_uri, std::string role)
        : kind_(std::move(kind))
        , source_(std::move(source))
        , series_uris_(oresmd_uri.empty() ? std::vector<std::string>{} :
                                            std::vector<std::string>{std::move(oresmd_uri)})
        , role_(std::move(role)) {}

    const std::string& source_name() const override {
        return source_;
    }
    const std::vector<std::string>& series_uris() const override {
        return series_uris_;
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
    void start() override {}
    void stop() override {}
    std::uint64_t publish_count() const override {
        return 0;
    }

private:
    std::string kind_;
    std::string source_;
    std::vector<std::string> series_uris_;
    std::string role_;
    std::string subject_;
};

// In-memory feed_binding store. Mirrors the real store's one rule -- bind a
// source unless it is already bound -- so a second start of the same source
// is observable as a single recorded row rather than a second write. It
// records the production store's own binding, so the kinds the controller's
// bindings carry are asserted rather than restated here.
class fake_binding_store final : public feed_binding_store {
public:
    std::expected<void, std::string> save_if_absent(const std::string& source_name,
                                                    const std::string&) override {
        for (const auto& b : bindings_)
            if (b.source_name == source_name)
                return {};
        bindings_.push_back(marketdata_binding_store::binding_for(source_name));
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

    std::shared_ptr<i_feed> feed(const std::string& kind,
                                 const std::string& source,
                                 const std::string& oresmd_uri,
                                 const std::string& role = {}) {
        return std::make_shared<stub_feed>(kind, source, oresmd_uri, role);
    }
};

// One producer kind's identity: an FX feed carries no role, an IR curve feed
// carries one. Everything the controller does with them is identical, which
// is the point of the two rows.
struct producer_kinds {
    std::string fx_kind = "fx_spot";
    std::string fx_source = "EUR_USD_GBM_1";
    std::string fx_oresmd_uri = "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD";
    std::string fx_role;

    std::string ir_kind = "ir_curve";
    std::string ir_source = "usd.sofr";
    std::string ir_oresmd_uri =
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D";
    std::string ir_role = "self_discounting";
};

}

TEST_CASE("producer_subject: a feed publishes on the source's tick subject, for every kind", tags) {
    CHECK(producer_subject("EUR_USD_GBM") == "synthetic.v1.ops.tick.EUR_USD_GBM");
    CHECK(producer_subject("usd.sofr") == "synthetic.v1.ops.tick.usd.sofr");
}

TEST_CASE("producer_subject: unsafe characters are replaced", tags) {
    CHECK(producer_subject("weird name!*>") == "synthetic.v1.ops.tick.weird_name___");
}

TEST_CASE("starting a feed creates its feed_binding, for FX and IR alike", tags) {
    producer_kinds kinds;
    controller_fixture f;

    const std::vector<std::shared_ptr<i_feed>> feeds{
        f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role),
        f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_oresmd_uri, kinds.ir_role)};
    REQUIRE(f.ctrl->start(feeds[0], test_bearer) == feed_controller::start_result::started);
    REQUIRE(f.ctrl->start(feeds[1], test_bearer) == feed_controller::start_result::started);

    REQUIRE(f.store->bindings_.size() == 2);
    CHECK(f.store->bindings_[0].source_name == kinds.fx_source);
    CHECK(f.store->bindings_[0].enabled == true);
    CHECK(f.store->bindings_[0].producer_kind == "SYNTHETIC");
    CHECK(f.store->bindings_[1].source_name == kinds.ir_source);
    CHECK(f.store->bindings_[1].enabled == true);
    CHECK(f.store->bindings_[1].producer_kind == "SYNTHETIC");
}

TEST_CASE("the controller names the running feed that holds a series", tags) {
    producer_kinds kinds;
    controller_fixture f;

    const auto fx = f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role);
    REQUIRE(f.ctrl->start(fx, test_bearer) == feed_controller::start_result::started);

    const auto held = f.ctrl->running_source_name_for_series({kinds.fx_oresmd_uri});
    REQUIRE(held.has_value());
    CHECK(*held == kinds.fx_source);

    CHECK_FALSE(f.ctrl->running_source_name_for_series({"oresmd://fx/nobody"}).has_value());
    CHECK_FALSE(f.ctrl->running_source_name_for_series({}).has_value());
}

TEST_CASE("the binding a synthetic store creates names a generated producer, for FX and IR "
          "alike",
          tags) {
    CHECK(marketdata_binding_store::binding_for("eur.usd").producer_kind == "SYNTHETIC");
    CHECK(marketdata_binding_store::binding_for("usd.sofr").producer_kind == "SYNTHETIC");
}

TEST_CASE("starting a feed twice does not duplicate its feed_binding, for FX and IR alike", tags) {
    producer_kinds kinds;
    controller_fixture f;

    const std::vector<std::shared_ptr<i_feed>> feeds{
        f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role),
        f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_oresmd_uri, kinds.ir_role)};
    for (const auto& feed : feeds) {
        REQUIRE(f.ctrl->start(feed, test_bearer) == feed_controller::start_result::started);
        REQUIRE(f.ctrl->start(feed, test_bearer) == feed_controller::start_result::already_running);
    }

    REQUIRE(f.store->bindings_.size() == 2);
    CHECK(f.store->bindings_[0].source_name == kinds.fx_source);
    CHECK(f.store->bindings_[1].source_name == kinds.ir_source);
}

TEST_CASE("add() binds a feed of every kind as start() does", tags) {
    producer_kinds kinds;
    controller_fixture f;

    CHECK(f.ctrl->add(f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role),
                      test_bearer));
    CHECK(f.ctrl->add(f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_oresmd_uri, kinds.ir_role),
                      test_bearer));

    REQUIRE(f.store->bindings_.size() == 2);
    CHECK(f.store->bindings_[0].source_name == kinds.fx_source);
    CHECK(f.store->bindings_[1].source_name == kinds.ir_source);
}

TEST_CASE("feeds_conflict: feeds that share a series conflict — both would write points "
          "into it",
          tags) {
    const std::vector<std::string> sofr{
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D"};
    CHECK(feeds_conflict(sofr, sofr));
}

TEST_CASE("feeds_conflict: one shared series among several is enough", tags) {
    const std::vector<std::string> spot{
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D"};
    const std::vector<std::string> spot_and_dated{
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D",
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=20260128&tenor=1D"};
    CHECK(feeds_conflict(spot, spot_and_dated));
    CHECK(feeds_conflict(spot_and_dated, spot));
}

TEST_CASE("feeds_conflict: feeds of different series never conflict, whatever their role", tags) {
    const std::vector<std::string> usd{
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D"};
    const std::vector<std::string> eur_6m{
        "oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M"};
    const std::vector<std::string> eur_3m{
        "oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M"};
    CHECK_FALSE(feeds_conflict(usd, eur_6m));
    CHECK_FALSE(feeds_conflict(eur_6m, eur_3m));
}

TEST_CASE("feeds_conflict: a feed that names no series has nothing to protect and conflicts "
          "with nothing",
          tags) {
    const std::vector<std::string> none;
    const std::vector<std::string> empty_name{""};
    const std::vector<std::string> usd{
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D"};
    CHECK_FALSE(feeds_conflict(none, none));
    CHECK_FALSE(feeds_conflict(empty_name, empty_name));
    CHECK_FALSE(feeds_conflict(usd, none));
    CHECK_FALSE(feeds_conflict(none, usd));
}

TEST_CASE("feeds_conflict: two FX feeds on the same pair conflict, and on different pairs do not",
          tags) {
    const std::vector<std::string> eur{
        "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD"};
    const std::vector<std::string> gbp{
        "oresmd://fx/GBP?type=series&instrument=fx_spot&quote=rate&ccy=USD"};
    CHECK(feeds_conflict(eur, eur));
    CHECK_FALSE(feeds_conflict(eur, gbp));
}
