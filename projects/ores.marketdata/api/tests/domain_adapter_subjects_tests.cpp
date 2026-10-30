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
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::marketdata::domain;

TEST_CASE("the_control_subjects_name_the_adapter", "[domain][adapter_subjects]") {
    CHECK(adapter_announce_subject() == "marketdata.v1.adapters.announce");
    CHECK(adapter_demand_subject("bloomberg-sim") == "marketdata.v1.adapters.bloomberg-sim.demand");
    CHECK(adapter_heartbeat_subject("bloomberg-sim") ==
          "marketdata.v1.adapters.bloomberg-sim.heartbeat");
}

TEST_CASE("the_control_subjects_encode_an_awkward_adapter_id", "[domain][adapter_subjects]") {
    CHECK(adapter_demand_subject("ore sim.1") == "marketdata.v1.adapters.ore%20sim%2E1.demand");
    CHECK(adapter_heartbeat_subject("ore sim.1") ==
          "marketdata.v1.adapters.ore%20sim%2E1.heartbeat");
}

TEST_CASE("an_ingress_subject_holds_one_token_for_the_adapter_and_one_for_the_id",
          "[domain][adapter_subjects]") {
    const auto subject = adapter_ingress_subject("bloomberg-sim", "EURUSD Curncy");
    CHECK(subject == "marketdata.v1.ingress.bloomberg-sim.EURUSD%20Curncy");
}

TEST_CASE("an_ingress_subject_encodes_spaces_equals_signs_and_dots", "[domain][adapter_subjects]") {
    const auto bloomberg = adapter_ingress_subject("a", "EURUSD Curncy");
    const auto lseg = adapter_ingress_subject("a", "EUR=");
    const auto dotted = adapter_ingress_subject("a", "GBP.USD");
    CHECK(lseg == "marketdata.v1.ingress.a.EUR%3D");
    for (const auto& subject : {bloomberg, lseg, dotted}) {
        CAPTURE(subject);
        const auto prefix = std::string("marketdata.v1.ingress.a.");
        const auto token = subject.substr(prefix.size());
        CHECK(token.find(' ') == std::string::npos);
        CHECK(token.find('=') == std::string::npos);
        CHECK(token.find('.') == std::string::npos);
    }
}

TEST_CASE("an_ingress_subject_gives_its_adapter_and_id_back", "[domain][adapter_subjects]") {
    for (const auto& id : {std::string("EURUSD Curncy"),
                           std::string("EUR="),
                           std::string("GBP.USD"),
                           std::string("FX/RATE/EUR/USD"),
                           std::string("a.b c*d>e%f")}) {
        CAPTURE(id);
        const auto parts = adapter_ingress_subject_parts(adapter_ingress_subject("ore sim.1", id));
        REQUIRE(parts.has_value());
        CHECK(parts->adapter_id == "ore sim.1");
        CHECK(parts->external_id == id);
    }
}

TEST_CASE("a_subject_that_is_not_an_ingress_subject_gives_nothing_back",
          "[domain][adapter_subjects]") {
    CHECK_FALSE(adapter_ingress_subject_parts("marketdata.v1.adapters.announce").has_value());
    CHECK_FALSE(adapter_ingress_subject_parts("marketdata.v1.ingress.only").has_value());
    CHECK_FALSE(adapter_ingress_subject_parts("marketdata.v1.ingress.a.b.c").has_value());
    CHECK_FALSE(adapter_ingress_subject_parts("marketdata.v1.ingress.a.EUR=").has_value());
    CHECK_FALSE(adapter_ingress_subject_parts("marketdata.v1.ingress.a.").has_value());
    CHECK_FALSE(adapter_ingress_subject_parts("marketdata.v1.ingress..x").has_value());
}

TEST_CASE("the_wildcards_cover_every_adapter_or_one", "[domain][adapter_subjects]") {
    CHECK(adapter_ingress_subject_all() == "marketdata.v1.ingress.>");
    CHECK(adapter_ingress_subject_for("ore-sim") == "marketdata.v1.ingress.ore-sim.>");
}

TEST_CASE("no_adapter_subject_carries_a_mark_of_the_source_kind", "[domain][adapter_subjects]") {
    for (const auto& subject : {adapter_announce_subject(),
                                adapter_demand_subject("a"),
                                adapter_heartbeat_subject("a"),
                                adapter_ingress_subject("a", "x"),
                                adapter_ingress_subject_all()}) {
        CAPTURE(subject);
        for (const auto* mark : {"synthetic", "simulated", "live", "replay"})
            CHECK(subject.find(mark) == std::string::npos);
    }
}

TEST_CASE("a_heartbeat_subject_gives_its_adapter_back", "[domain][adapter_subjects]") {
    for (const auto& id :
         {std::string("bloomberg-sim"), std::string("ore sim.1"), std::string("a=b")}) {
        CAPTURE(id);
        const auto adapter = adapter_heartbeat_subject_adapter(adapter_heartbeat_subject(id));
        REQUIRE(adapter.has_value());
        CHECK(*adapter == id);
    }
    CHECK(adapter_heartbeat_subject_all() == "marketdata.v1.adapters.*.heartbeat");
}

TEST_CASE("a_subject_that_is_not_a_heartbeat_gives_no_adapter", "[domain][adapter_subjects]") {
    CHECK_FALSE(adapter_heartbeat_subject_adapter("marketdata.v1.adapters.announce").has_value());
    CHECK_FALSE(adapter_heartbeat_subject_adapter("marketdata.v1.adapters.a.demand").has_value());
    CHECK_FALSE(adapter_heartbeat_subject_adapter("marketdata.v1.adapters..heartbeat").has_value());
    CHECK_FALSE(
        adapter_heartbeat_subject_adapter("marketdata.v1.adapters.a b.heartbeat").has_value());
}

TEST_CASE("the_ingest_consumes_an_adapters_tick_and_a_producers_tick",
          "[domain][adapter_subjects][ingest]") {
    using namespace ores::marketdata::domain;
    // The two families a publisher may use: the ingress of an adapter, and the producers that
    // name no adapter.
    CHECK(ingest_consumes(adapter_ingress_subject("bloomberg-a", "EURUSD Curncy")));
    CHECK(ingest_consumes(synthetic_tick_subject_for("synthetic.bloomberg.usdsofr")));

    // A control message, another family, and the environment-prefixed form are none of them a
    // subject the loop reads.
    CHECK_FALSE(ingest_consumes(adapter_announce_subject()));
    CHECK_FALSE(ingest_consumes(adapter_demand_subject("bloomberg-a")));
    CHECK_FALSE(ingest_consumes(feed_display_subject_for("bloomberg-a")));
    CHECK_FALSE(ingest_consumes("synthetic.v1.ops.tick"));
}

TEST_CASE("the_ingest_subscribes_to_exactly_its_two_subject_families",
          "[domain][adapter_subjects][ingest]") {
    using namespace ores::marketdata::domain;
    const auto ingested = ingest_subject_wildcards();
    CHECK(ingested.producers == "synthetic.v1.ops.tick.>");
    CHECK(ingested.adapters == adapter_ingress_subject_all());
    CHECK(ingested.all() ==
          std::vector<std::string>{"synthetic.v1.ops.tick.>", "marketdata.v1.ingress.>"});
}

TEST_CASE("a_wildcard_covers_one_token_or_more_and_a_star_covers_exactly_one",
          "[domain][adapter_subjects][ingest]") {
    using namespace ores::marketdata::domain;
    CHECK(subject_matches("a.>", "a.b"));
    CHECK(subject_matches("a.>", "a.b.c"));
    CHECK_FALSE(subject_matches("a.>", "a"));
    CHECK_FALSE(subject_matches("a.>", "a."));
    CHECK(subject_matches("a.*.c", "a.b.c"));
    CHECK_FALSE(subject_matches("a.*.c", "a.b.b.c"));
    CHECK(subject_matches("a.b", "a.b"));
    CHECK_FALSE(subject_matches("a.b", "a.b.c"));
}
