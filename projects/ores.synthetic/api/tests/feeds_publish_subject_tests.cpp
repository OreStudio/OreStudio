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
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

/*
 * The market data ingest listens on two subject families, and a feed that publishes outside them
 * stores nothing while its log still reports a publish. These cases pin the synthetic feeds'
 * subjects to those families, so a change to either side fails here rather than being found in a
 * running system with an empty tick store.
 */

TEST_CASE("a_feed_without_an_adapter_publishes_where_the_ingest_listens",
          "[feeds][subjects][ingest]") {
    using namespace ores::marketdata::domain;
    for (const auto& source :
         {std::string("synthetic.bloomberg.usdsofr"), std::string("synthetic.demo.eur"), std::string("a")}) {
        CAPTURE(source);
        CHECK(ingest_consumes(ores::synthetic::feed::producer_subject(source)));
    }
}

TEST_CASE("a_feed_that_serves_an_adapter_publishes_where_the_ingest_listens",
          "[feeds][subjects][ingest]") {
    using namespace ores::marketdata::domain;
    // The id of a pillar, which the publisher writes in the provider's notation.
    for (const auto& id :
         {std::string("USD SOFR DEPO SPOT 3M Curncy"), std::string("EUR="), std::string("EUR/USD")}) {
        CAPTURE(id);
        CHECK(ingest_consumes(adapter_ingress_subject("bloomberg-a", id)));
    }
}

TEST_CASE("a_source_name_keeps_its_dots_and_stays_inside_the_producer_family",
          "[feeds][subjects][ingest]") {
    using namespace ores::marketdata::domain;
    // The builder keeps the dots of a source name, so the producer subject is one level deeper
    // under the ingest's wildcard and not a family of its own.
    const auto producer = ores::synthetic::feed::producer_subject("synthetic.bloomberg.usdsofr");
    CHECK(producer == "synthetic.v1.ops.tick.synthetic.bloomberg.usdsofr");
    CHECK(subject_matches(ingest_producer_subject_all(), producer));
    CHECK(ingest_subject_wildcards().size() == 2);
}
