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
using ores::synthetic::feed::producer_subject;
using ores::synthetic::service::feed_controller;
using ores::synthetic::service::feeds_conflict;


// A feed whose only job is to carry an identity into the controller: it
// publishes nothing and its start() returns at once, so a controller can be
// driven without a process, a NATS subject, or a tick thread that outlives
// the test.
class stub_feed final : public IFeed {
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

// A controller over a client that is never connected: a stub feed publishes
// nothing, so no NATS round-trip is needed.
struct controller_fixture {
    ores::nats::service::client nats;
    std::shared_ptr<feed_controller> ctrl;

    controller_fixture()
        : nats(ores::testing::make_nats_options()) {
        ctrl = std::make_shared<feed_controller>(nats);
    }

    std::shared_ptr<IFeed> feed(const std::string& kind,
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

TEST_CASE("the controller names the running feed that holds a series", tags) {
    producer_kinds kinds;
    controller_fixture f;

    const auto fx = f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role);
    REQUIRE(f.ctrl->start(fx) == feed_controller::start_result::started);

    const auto held = f.ctrl->running_source_name_for_series({kinds.fx_oresmd_uri});
    REQUIRE(held.has_value());
    CHECK(*held == kinds.fx_source);

    CHECK_FALSE(f.ctrl->running_source_name_for_series({"oresmd://fx/nobody"}).has_value());
    CHECK_FALSE(f.ctrl->running_source_name_for_series({}).has_value());
}

TEST_CASE("starting a feed twice reports already_running, for FX and IR alike", tags) {
    producer_kinds kinds;
    controller_fixture f;

    const std::vector<std::shared_ptr<IFeed>> feeds{
        f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role),
        f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_oresmd_uri, kinds.ir_role)};
    for (const auto& feed : feeds) {
        REQUIRE(f.ctrl->start(feed) == feed_controller::start_result::started);
        REQUIRE(f.ctrl->start(feed) == feed_controller::start_result::already_running);
    }
}

TEST_CASE("add() starts a feed of every kind as start() does", tags) {
    producer_kinds kinds;
    controller_fixture f;

    CHECK(f.ctrl->add(f.feed(kinds.fx_kind, kinds.fx_source, kinds.fx_oresmd_uri, kinds.fx_role)));
    CHECK(f.ctrl->add(f.feed(kinds.ir_kind, kinds.ir_source, kinds.ir_oresmd_uri, kinds.ir_role)));
    CHECK(f.ctrl->running_count() == 2);
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
