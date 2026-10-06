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
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string tags("[app][feed_ingest_loop]");

using ores::marketdata::domain::feed_binding;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::repository::feed_binding_repository;
using ores::marketdata::repository::market_observation_repository;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::service::app::feed_ingest_loop;
using ores::marketdata::service::app::tick_drop;
using ores::refdata::repository::party_repository;
using ores::testing::scoped_database_helper;

const std::string bound_prefix = "synthetic.bound.";
const std::string other_prefix = "synthetic.other.";
const std::string tick_value = "1.0845";

/**
 * A tag unique to this run. The suite reuses its tenant, so a source name or a
 * series identity that repeated would let an earlier run's binding answer for
 * this one; the identity under test is therefore drawn per run.
 */
std::string run_tag() {
    static boost::uuids::random_generator uuid;
    auto hex = boost::uuids::to_string(uuid());
    hex.erase(std::remove(hex.begin(), hex.end(), '-'), hex.end());
    return hex.substr(0, 8);
}

/**
 * The quote URI of one run's own FX spot datum, so every read below is scoped
 * to the series this run creates rather than to whatever else the suite's
 * tenant happens to hold.
 */
std::string eur_usd_spot() {
    const auto tag = run_tag();
    const auto code = [&](std::size_t offset) {
        return std::string{"AAAAA"} + tag[offset] + "A";
    };
    return "oresmd://fx/" + code(0) + "?type=quote&instrument=fx_spot&quote=rate&ccy=" + code(1);
}

/**
 * The series URI of @p quote_uri's datum: the same key with the series quote
 * type. A spot's key carries the currency pair the point does, so only the
 * type field differs.
 */
std::string series_uri(const std::string& quote_uri) {
    auto uri = quote_uri;
    const std::string quote_type = "type=quote";
    uri.replace(uri.find(quote_type), quote_type.size(), "type=series");
    return uri;
}

/**
 * One test tenant's fixture: a party of its own, a helper that writes the
 * binding for a source under it, and the NATS client the loop is built around.
 * The binding's producer kind is what the ingest loop stamps a series it
 * creates with, so the cases state it.
 */
struct fixture {
    scoped_database_helper h;
    ores::utility::generation::generation_context gen = ores::testing::make_generation_context(h);
    ores::nats::service::client nats{ores::testing::make_nats_options()};
    boost::uuids::uuid party_id;

    fixture() {
        auto party = ores::refdata::generators::generate_synthetic_party(gen);
        const auto existing = party_repository{}.read_latest(h.context());
        if (!existing.empty())
            party.parent_party_id = existing.front().id;
        party_repository{}.write(h.context(), party);
        party_id = party.id;
    }

    ~fixture() {
        nats.disconnect();
    }

    /**
     * The subject the service republishes a stored tick on is covered by the
     * stream the service provisions at startup, so a test that stores through
     * the loop provisions it the same way.
     */
    void connect() {
        nats.connect();
        nats.make_admin().ensure_stream(nats.make_stream_name("marketdata_ticks"),
                                        {nats.make_subject(market_tick::nats_subject)});
        REQUIRE(nats.is_connected());
    }

    void bind(const std::string& source, const std::string& producer_kind) {
        feed_binding b;
        b.tenant_id = h.tenant_id();
        b.id = boost::uuids::random_generator{}();
        b.party_id = party_id;
        b.source_name = source;
        b.producer_kind = producer_kind;
        b.enabled = true;
        b.modified_by = h.db_user();
        b.performed_by = h.db_user();
        b.change_reason_code = "system.test";
        b.change_commentary = "Bound source fixture";
        feed_binding_repository{}.write(h.context(), b);
    }

    std::vector<ores::marketdata::domain::market_series> series_for(const std::string& quote_uri) {
        return market_series_repository{}.read_latest_by_uri(
            h.context(), series_uri(quote_uri), boost::uuids::to_string(party_id));
    }
};

market_tick tick_from(const std::string& source, const std::string& oresmd_uri) {
    market_tick t;
    t.oresmd_uri = oresmd_uri;
    t.value = tick_value;
    t.source = source;
    t.observation_time = std::chrono::system_clock::now();
    return t;
}

}

TEST_CASE("a_bound_synthetic_tick_writes_the_series_and_the_observation", tags) {
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    // The loop is built but never started: no subscription is opened, so
    // handle_tick is the store path under test rather than the wire. refresh()
    // builds the bindings cache from the row above, exactly as start() does.
    feed_ingest_loop loop(f.nats, f.h.context());
    loop.refresh();

    const auto outcome = loop.handle_tick(tick_from(bound_source, quote_uri));
    REQUIRE_FALSE(outcome.has_value());

    const auto series = f.series_for(quote_uri);
    REQUIRE(series.size() == 1);
    CHECK(series.front().oresmd_uri == series_uri(quote_uri));
    CHECK(series.front().producer_kind == "SYNTHETIC");
    CHECK(series.front().series_subclass == "spot");
    CHECK(series.front().tenant_id.to_string() == f.h.tenant_id().to_string());
    CHECK(boost::uuids::to_string(series.front().party_id) == boost::uuids::to_string(f.party_id));

    market_observation_repository obs_repo;
    const auto observations = obs_repo.read_latest_for_series(f.h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().series_id == series.front().id);
    CHECK(observations.front().oresmd_uri == quote_uri);
    CHECK(observations.front().source == bound_source);
    CHECK(observations.front().value == tick_value);
    CHECK(boost::uuids::to_string(observations.front().party_id) ==
          boost::uuids::to_string(f.party_id));
}

TEST_CASE("a_tick_from_a_source_with_no_enabled_binding_is_dropped_and_writes_nothing", tags) {
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto other_source = other_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(other_source, "SYNTHETIC");
    f.connect();

    feed_ingest_loop loop(f.nats, f.h.context());
    loop.refresh();

    const auto outcome = loop.handle_tick(tick_from(bound_source, quote_uri));
    REQUIRE(outcome.has_value());
    CHECK(*outcome == tick_drop::unbound_source);

    CHECK(f.series_for(quote_uri).empty());
}
