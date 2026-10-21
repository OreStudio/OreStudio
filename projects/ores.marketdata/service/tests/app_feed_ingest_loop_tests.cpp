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
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.marketdata.service/app/tick_writer.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdlib>
#include <libpq-fe.h>
#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace {

const std::string tags("[app][feed_ingest_loop]");

using ores::marketdata::domain::feed_binding;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::repository::feed_binding_repository;
using ores::marketdata::repository::market_observation_repository;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::service::app::feed_ingest_loop;
using ores::marketdata::service::app::observation_dates;
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

    void connect() {
        nats.connect();
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

TEST_CASE("a_tick_is_recorded_under_its_own_day_and_the_last_business_day_that_closed",
          "[app][feed_ingest_loop][observation_dates]") {
    using namespace std::chrono;

    // Wednesday 2026-10-14 16:30 and Monday 2026-10-19 09:00, both business days, so the
    // close is the day before the first and the Friday before the second.
    const auto wednesday = sys_days{year{2026} / 10 / 14} + hours{16} + minutes{30};
    const auto monday = sys_days{year{2026} / 10 / 19} + hours{9};

    const auto midweek = observation_dates(system_clock::time_point{wednesday});
    REQUIRE(midweek.size() == 2);
    CHECK(midweek.front() == system_clock::time_point{wednesday});
    CHECK(midweek.back() ==
          system_clock::time_point{sys_days{year{2026} / 10 / 13} + hours{16} + minutes{30}});

    const auto after_the_weekend = observation_dates(system_clock::time_point{monday});
    REQUIRE(after_the_weekend.size() == 2);
    CHECK(after_the_weekend.back() ==
          system_clock::time_point{sys_days{year{2026} / 10 / 16} + hours{9}});
}

TEST_CASE("a_weekend_tick_records_only_its_own_day", "[app][feed_ingest_loop][observation_dates]") {
    using namespace std::chrono;

    // Saturday 2026-10-17: the last business day closed on the Friday, whose ticks already
    // recorded it, so a weekend observation adds no close of its own.
    const auto saturday = sys_days{year{2026} / 10 / 17} + hours{11};
    const auto dates = observation_dates(system_clock::time_point{saturday});

    REQUIRE(dates.size() == 1);
    CHECK(dates.front() == system_clock::time_point{saturday});
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

    const auto tick = tick_from(bound_source, quote_uri);
    const auto outcome = loop.handle_tick(tick);
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
    auto recorded_at = observation_dates(tick.observation_time);
    REQUIRE(observations.size() == recorded_at.size());

    // The store returns the rows oldest first, which is the order the design
    // asks for: a reader that takes the last row takes the newest close. The
    // dates the loop records come newest first, so the two are compared in one
    // order rather than by position.
    auto expected = recorded_at;
    std::sort(expected.begin(), expected.end());
    for (std::size_t i = 0; i < observations.size(); ++i) {
        // to_iso8601_utc writes six digits of microseconds, which is the
        // precision the column keeps, so the round trip loses nothing a
        // time_point held below the microsecond.
        const auto delta = observations[i].observation_datetime - expected[i];
        CHECK(std::chrono::abs(delta) < std::chrono::milliseconds(1));
        CHECK(observations[i].series_id == series.front().id);
        CHECK(observations[i].oresmd_uri == quote_uri);
        CHECK(observations[i].source == bound_source);
        CHECK(observations[i].value == tick_value);
        CHECK(boost::uuids::to_string(observations[i].party_id) ==
              boost::uuids::to_string(f.party_id));
    }
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

TEST_CASE("a_tick_published_on_the_wire_reaches_the_store", tags) {
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    // Started, the loop subscribes as the service does, so the tick travels the
    // wire: a subscription on the wrong subject drops every tick unseen.
    feed_ingest_loop loop(f.nats, f.h.context());
    loop.start();

    const auto subject = ores::marketdata::domain::synthetic_tick_subject_for(bound_source);
    const auto payload =
        ores::nats::default_wire_codec().encode(tick_from(bound_source, quote_uri));
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(15);
    while (f.series_for(quote_uri).empty() && std::chrono::steady_clock::now() < deadline) {
        f.nats.publish(subject, payload);
        std::this_thread::sleep_for(std::chrono::milliseconds(250));
    }

    REQUIRE(f.series_for(quote_uri).size() == 1);
}

TEST_CASE("a_batch_of_ticks_stores_every_datum_and_keeps_the_last_of_a_repeated_key", tags) {
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto first_uri = eur_usd_spot();
    const auto second_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    feed_ingest_loop loop(f.nats, f.h.context());
    loop.refresh();

    // Two ticks of one datum with one observation time make one key. The second is the
    // one the store keeps.
    auto repeated = tick_from(bound_source, first_uri);
    auto latest = repeated;
    latest.value = "1.0999";
    const auto other = tick_from(bound_source, second_uri);

    const auto outcomes = loop.handle_batch({repeated, latest, other});
    REQUIRE(outcomes.size() == 3);
    for (const auto& outcome : outcomes)
        CHECK_FALSE(outcome.has_value());

    market_observation_repository obs_repo;
    const auto first_series = f.series_for(first_uri);
    REQUIRE(first_series.size() == 1);
    const auto first_rows = obs_repo.read_latest_for_series(f.h.context(), first_series.front().id);
    REQUIRE(first_rows.size() == observation_dates(repeated.observation_time).size());
    for (const auto& row : first_rows)
        CHECK(row.value == "1.0999");

    const auto second_series = f.series_for(second_uri);
    REQUIRE(second_series.size() == 1);
    CHECK(obs_repo.read_latest_for_series(f.h.context(), second_series.front().id).size() ==
          observation_dates(other.observation_time).size());
}

TEST_CASE("a_started_loop_can_be_destroyed_while_ticks_arrive", tags) {
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    auto loop = std::make_unique<feed_ingest_loop>(f.nats, f.h.context());
    loop->start();

    const auto subject = ores::marketdata::domain::synthetic_tick_subject_for(bound_source);
    std::atomic<bool> stop{false};
    std::thread publisher([&] {
        while (!stop.load()) {
            f.nats.publish(
                subject,
                ores::nats::default_wire_codec().encode(tick_from(bound_source, quote_uri)));
            std::this_thread::sleep_for(std::chrono::milliseconds(2));
        }
    });

    std::this_thread::sleep_for(std::chrono::milliseconds(300));
    // The loop goes while the publisher still sends. A callback that ran after the queue
    // was gone would crash here.
    loop.reset();
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    stop.store(true);
    publisher.join();
    SUCCEED();
}

namespace {

/// The number of rows of @p series in the tick table, read as the marketdata service, which
/// has no BYPASSRLS, under the tenant and party of the test. This read proves what the
/// policies let a reader see.
std::string
tick_scalar(const std::string& tenant, const std::string& party, const std::string& sql_text) {
    const auto env = [](const char* name) {
        const auto* v = std::getenv(name);
        return v != nullptr ? std::string(v) : std::string();
    };
    const auto info = "host=" + env("ORES_TEST_DB_HOST") +
                      " dbname=" + env("ORES_TEST_DB_DATABASE") +
                      " user=" + env("ORES_MARKETDATA_SERVICE_DB_USER") +
                      " password=" + env("ORES_MARKETDATA_SERVICE_DB_PASSWORD");
    std::unique_ptr<PGconn, decltype(&PQfinish)> conn(PQconnectdb(info.c_str()), &PQfinish);
    REQUIRE(PQstatus(conn.get()) == CONNECTION_OK);
    const auto run = [&](const std::string& sql) {
        std::unique_ptr<PGresult, decltype(&PQclear)> r(PQexec(conn.get(), sql.c_str()), &PQclear);
        REQUIRE((PQresultStatus(r.get()) == PGRES_COMMAND_OK ||
                 PQresultStatus(r.get()) == PGRES_TUPLES_OK));
        return PQntuples(r.get()) > 0 ? std::string(PQgetvalue(r.get(), 0, 0)) : std::string();
    };
    run("set app.current_tenant_id = '" + tenant + "'");
    run("set app.visible_party_ids = '{" + party + "}'");
    return run(sql_text);
}

std::string
tick_rows_of(const std::string& tenant, const std::string& party, const std::string& series) {
    return tick_scalar(tenant,
                       party,
                       "select count(*) from ores_marketdata_ticks_tbl where series_id = '" +
                           series + "'");
}

}

TEST_CASE("a_stored_tick_is_copied_to_the_tick_table_under_the_owner_of_its_series", tags) {
    const auto* writer_user = std::getenv("ORES_MARKETDATA_SERVICE_WRITER_DB_USER");
    const auto* writer_password = std::getenv("ORES_MARKETDATA_SERVICE_WRITER_DB_PASSWORD");
    if (writer_user == nullptr || writer_password == nullptr ||
        std::getenv("ORES_MARKETDATA_SERVICE_DB_USER") == nullptr)
        SKIP("The tick writer role is not configured");

    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    auto writer = std::make_shared<ores::marketdata::service::app::tick_writer>(
        ores::marketdata::service::app::tick_writer_options{
            .user = writer_user,
            .password = writer_password,
            .host = std::getenv("ORES_TEST_DB_HOST"),
            .database = std::getenv("ORES_TEST_DB_DATABASE")});
    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.refresh();

    REQUIRE_FALSE(loop.handle_tick(tick_from(bound_source, quote_uri)).has_value());

    const auto series = f.series_for(quote_uri);
    REQUIRE(series.size() == 1);
    const auto series_text = boost::uuids::to_string(series.front().id);
    const auto tenant = f.h.tenant_id().to_string();
    const auto party = boost::uuids::to_string(f.party_id);

    CHECK(tick_rows_of(tenant, party, series_text) == "1");
    CHECK(tick_rows_of(tenant,
                       boost::uuids::to_string(boost::uuids::random_generator{}()),
                       series_text) == "0");
}

TEST_CASE("the_ticks_of_a_datum_with_a_coordinate_are_copied_under_its_coordinate_number", tags) {
    const auto* writer_user = std::getenv("ORES_MARKETDATA_SERVICE_WRITER_DB_USER");
    const auto* writer_password = std::getenv("ORES_MARKETDATA_SERVICE_WRITER_DB_PASSWORD");
    if (writer_user == nullptr || writer_password == nullptr ||
        std::getenv("ORES_MARKETDATA_SERVICE_DB_USER") == nullptr)
        SKIP("The tick writer role is not configured");

    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto name = std::string{"AAAAA"} + run_tag()[0] + "A";
    const auto cds_series = "oresmd://credit/" + name +
                            "?type=quote&instrument=cds&quote=credit_spread&seniority=SNRFOR&"
                            "ccy=USD&doc_clause=XR14";
    const auto cds = [&](const std::string& term) {
        return cds_series + "&term=" + term;
    };
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    auto writer = std::make_shared<ores::marketdata::service::app::tick_writer>(
        ores::marketdata::service::app::tick_writer_options{
            .user = writer_user,
            .password = writer_password,
            .host = std::getenv("ORES_TEST_DB_HOST"),
            .database = std::getenv("ORES_TEST_DB_DATABASE")});
    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.refresh();

    REQUIRE_FALSE(loop.handle_tick(tick_from(bound_source, cds("5Y"))).has_value());
    REQUIRE_FALSE(loop.handle_tick(tick_from(bound_source, cds("10Y"))).has_value());
    REQUIRE_FALSE(loop.handle_tick(tick_from(bound_source, cds("5Y"))).has_value());

    const auto series = f.series_for(cds_series);
    REQUIRE(series.size() == 1);
    const auto series_text = boost::uuids::to_string(series.front().id);
    const auto tenant = f.h.tenant_id().to_string();
    const auto party = boost::uuids::to_string(f.party_id);

    CHECK(tick_rows_of(tenant, party, series_text) == "3");
    CHECK(tick_scalar(tenant,
                      party,
                      "select count(distinct coord) from ores_marketdata_ticks_tbl "
                      "where coord > 0 and series_id = '" +
                          series_text + "'") == "2");
    CHECK(tick_scalar(tenant,
                      party,
                      "select count(*) from ores_marketdata_tick_coords_tbl where series_id = '" +
                          series_text + "'") == "2");
}
