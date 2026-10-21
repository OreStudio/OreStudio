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
#include "ores.marketdata.core/repository/feed_binding_activity_repository.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.marketdata.service/app/tick_writer.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
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
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <libpq-fe.h>
#include <memory>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace {

const std::string tags("[app][feed_ingest_loop]");

using ores::marketdata::domain::feed_binding;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::repository::feed_binding_activity_repository;
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

/// The options of the writer of the tick table as the environment configures them, or nothing
/// when the role is not set.
std::optional<ores::marketdata::service::app::tick_writer_options> writer_options() {
    const auto* user = std::getenv("ORES_MARKETDATA_SERVICE_WRITER_DB_USER");
    const auto* password = std::getenv("ORES_MARKETDATA_SERVICE_WRITER_DB_PASSWORD");
    if (user == nullptr || password == nullptr ||
        std::getenv("ORES_MARKETDATA_SERVICE_DB_USER") == nullptr)
        return std::nullopt;
    return ores::marketdata::service::app::tick_writer_options{
        .user = user,
        .password = password,
        .host = std::getenv("ORES_TEST_DB_HOST"),
        .database = std::getenv("ORES_TEST_DB_DATABASE")};
}

/// The writer of the tick table as the environment configures it, or nothing when the role is
/// not set. The loop stores nothing without it.
std::shared_ptr<ores::marketdata::service::app::tick_writer> make_writer() {
    const auto options = writer_options();
    if (!options)
        return nullptr;
    return std::make_shared<ores::marketdata::service::app::tick_writer>(*options);
}

}

TEST_CASE("a_bound_synthetic_tick_writes_the_series_and_the_tick", tags) {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    // The loop is built but never started: no subscription is opened, so
    // handle_tick is the store path under test rather than the wire. refresh()
    // builds the bindings cache from the row above, exactly as start() does.
    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
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

    // One row, not one for each date: the close of the last business day is a question for
    // the reader, and no row answers it.
    const auto tenant = f.h.tenant_id().to_string();
    const auto party = boost::uuids::to_string(f.party_id);
    const auto series_text = boost::uuids::to_string(series.front().id);
    CHECK(tick_rows_of(tenant, party, series_text) == "1");

    // The reader finds the tick under the datum URI the feed sent.
    const auto owner =
        f.h.context().with_party(f.h.tenant_id(), f.party_id, {f.party_id}, f.h.db_user());
    const auto points = market_observation_repository{}.read_as_of(
        owner, series.front().id, tick.observation_time + std::chrono::minutes(1));
    REQUIRE(points.size() == 1);
    CHECK(points.front().oresmd_uri == quote_uri);
    CHECK(points.front().value == tick_value);
    CHECK(std::chrono::abs(points.front().observation_datetime - tick.observation_time) <
          std::chrono::milliseconds(1));
    CHECK(boost::uuids::to_string(points.front().party_id) == party);

    // The binding says what it last delivered, since the tick has no source of its own.
    const auto recorded = feed_binding_activity_repository{}.read(owner, f.party_id, bound_source);
    REQUIRE(recorded.has_value());
    CHECK(recorded->series_id == series.front().id);
    CHECK(std::chrono::abs(recorded->last_tick_at - tick.observation_time) <
          std::chrono::milliseconds(1));
}

TEST_CASE("the_activity_of_a_binding_is_recorded_once_in_the_interval", tags) {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.refresh();

    const auto first = tick_from(bound_source, quote_uri);
    auto second = first;
    second.observation_time += std::chrono::seconds(2);
    REQUIRE_FALSE(loop.handle_tick(first).has_value());
    REQUIRE_FALSE(loop.handle_tick(second).has_value());

    const auto owner =
        f.h.context().with_party(f.h.tenant_id(), f.party_id, {f.party_id}, f.h.db_user());
    const auto recorded = feed_binding_activity_repository{}.read(owner, f.party_id, bound_source);
    REQUIRE(recorded.has_value());
    CHECK(std::chrono::abs(recorded->last_tick_at - first.observation_time) <
          std::chrono::milliseconds(1));

    // A source that delivered nothing has no row.
    CHECK_FALSE(feed_binding_activity_repository{}
                    .read(owner, f.party_id, bound_source + ".none")
                    .has_value());
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
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto first_uri = eur_usd_spot();
    const auto second_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.refresh();

    // Two ticks of one datum with one observation time make one row. The second is the one
    // the store keeps.
    auto repeated = tick_from(bound_source, first_uri);
    auto latest = repeated;
    latest.value = "1.0999";
    const auto other = tick_from(bound_source, second_uri);

    const auto outcomes = loop.handle_batch({repeated, latest, other});
    REQUIRE(outcomes.size() == 3);
    for (const auto& outcome : outcomes)
        CHECK_FALSE(outcome.has_value());

    const auto tenant = f.h.tenant_id().to_string();
    const auto party = boost::uuids::to_string(f.party_id);
    const auto owner =
        f.h.context().with_party(f.h.tenant_id(), f.party_id, {f.party_id}, f.h.db_user());
    const auto first_series = f.series_for(first_uri);
    REQUIRE(first_series.size() == 1);
    CHECK(tick_rows_of(tenant, party, boost::uuids::to_string(first_series.front().id)) == "1");
    const auto first_points = market_observation_repository{}.read_as_of(
        owner, first_series.front().id, repeated.observation_time + std::chrono::minutes(1));
    REQUIRE(first_points.size() == 1);
    CHECK(first_points.front().value == "1.0999");

    const auto second_series = f.series_for(second_uri);
    REQUIRE(second_series.size() == 1);
    CHECK(tick_rows_of(tenant, party, boost::uuids::to_string(second_series.front().id)) == "1");
}

TEST_CASE("a_tick_is_accepted_and_not_stored_when_the_store_does_not_answer", tags) {
    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

    auto unreachable = std::make_shared<ores::marketdata::service::app::tick_writer>(
        ores::marketdata::service::app::tick_writer_options{.user = "nobody",
                                                            .password = "none",
                                                            .host = "127.0.0.1",
                                                            .database = "none",
                                                            .port = 1});
    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, unreachable);
    loop.refresh();

    CHECK_FALSE(loop.handle_tick(tick_from(bound_source, quote_uri)).has_value());
    CHECK_FALSE(loop.handle_tick(tick_from(bound_source, quote_uri)).has_value());

    const auto series = f.series_for(quote_uri);
    REQUIRE(series.size() == 1);
    CHECK(tick_rows_of(f.h.tenant_id().to_string(),
                       boost::uuids::to_string(f.party_id),
                       boost::uuids::to_string(series.front().id)) == "0");
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

TEST_CASE("a_stored_tick_is_copied_to_the_tick_table_under_the_owner_of_its_series", tags) {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");

    fixture f;
    const auto bound_source = bound_prefix + run_tag();
    const auto quote_uri = eur_usd_spot();
    f.bind(bound_source, "SYNTHETIC");
    f.connect();

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
    auto writer = make_writer();
    if (!writer)
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

// The burst measurements below are hidden: they write about a million rows to the database of
// the environment, and they delete them again. Run them by their tag:
//   ores.marketdata.service.tests "[burst]"
// Each prints RESULT lines. A burst of 60,000 ticks fits the 65,536 places of the queue, so no
// tick is dropped for lack of room, and the one tick of each datum that follows a warm-up
// finds its series, its coordinate number and its binding in the caches.

namespace {

constexpr std::size_t burst_datums = 200;

/// A setting of the environment, or @p fallback when it is not set. A profiler run stays short
/// with fewer ticks and one repetition.
std::size_t burst_setting(const char* name, std::size_t fallback) {
    const auto* v = std::getenv(name);
    return v != nullptr ? static_cast<std::size_t>(std::stoull(v)) : fallback;
}

const std::size_t burst_ticks = burst_setting("ORES_BURST_TICKS", 60000);
const int burst_repetitions = static_cast<int>(burst_setting("ORES_BURST_REPETITIONS", 5));

using burst_clock = std::chrono::steady_clock;

std::vector<std::string> burst_uris() {
    const std::string hex = "0123456789abcdef";
    const auto code = [&](std::size_t i) {
        return std::string{"AAAAA"} + hex[i] + "A";
    };
    std::vector<std::string> uris;
    for (std::size_t unit = 0; unit < 16 && uris.size() < burst_datums; ++unit)
        for (std::size_t ccy = 0; ccy < 16 && uris.size() < burst_datums; ++ccy)
            if (unit != ccy)
                uris.push_back("oresmd://fx/" + code(unit) +
                               "?type=quote&instrument=fx_spot&quote=rate&ccy=" + code(ccy));
    return uris;
}

/// A connection of the marketdata service role, which the policies apply to, with the tenant
/// and the party of the test set. It answers the one question the measurements ask.
class burst_reader {
public:
    burst_reader(const std::string& tenant, const std::string& party)
        : conn_(PQconnectdb(connection_info().c_str()), &PQfinish) {
        REQUIRE(PQstatus(conn_.get()) == CONNECTION_OK);
        run("set app.current_tenant_id = '" + tenant + "'");
        run("set app.visible_party_ids = '{" + party + "}'");
    }

    /// Whether the tick of @p series at @p ts is in the table. One index probe.
    bool has(const std::string& series, std::chrono::system_clock::time_point ts) {
        return run("select 1 from ores_marketdata_ticks_tbl where series_id = '" + series +
                   "' and ts = '" + ores::platform::time::datetime::to_iso8601_utc(ts) + "'") ==
               "1";
    }

    std::size_t count_since(std::chrono::system_clock::time_point ts) {
        return std::stoull(run("select count(*) from ores_marketdata_ticks_tbl where ts >= '" +
                               ores::platform::time::datetime::to_iso8601_utc(ts) + "'"));
    }

private:
    static std::string env(const char* name) {
        const auto* v = std::getenv(name);
        return v != nullptr ? std::string(v) : std::string();
    }
    static std::string connection_info() {
        return "host=" + env("ORES_TEST_DB_HOST") + " dbname=" + env("ORES_TEST_DB_DATABASE") +
               " user=" + env("ORES_MARKETDATA_SERVICE_DB_USER") +
               " password=" + env("ORES_MARKETDATA_SERVICE_DB_PASSWORD");
    }
    std::string run(const std::string& sql) {
        std::unique_ptr<PGresult, decltype(&PQclear)> r(PQexec(conn_.get(), sql.c_str()), &PQclear);
        REQUIRE((PQresultStatus(r.get()) == PGRES_COMMAND_OK ||
                 PQresultStatus(r.get()) == PGRES_TUPLES_OK));
        return PQntuples(r.get()) > 0 ? std::string(PQgetvalue(r.get(), 0, 0)) : std::string();
    }

    std::unique_ptr<PGconn, decltype(&PQfinish)> conn_;
};

/// Deletes the rows a measurement wrote for its tenant, as the DDL user, who owns the tables
/// and is not subject to the policies.
struct burst_cleanup {
    std::string tenant;
    ~burst_cleanup() {
        const auto* user = std::getenv("ORES_DB_DDL_USER");
        const auto* password = std::getenv("ORES_DB_DDL_PASSWORD");
        if (user == nullptr || password == nullptr) {
            std::cout << "RESULT cleanup skipped: no DDL user, rows of tenant " << tenant
                      << " remain" << std::endl;
            return;
        }
        const std::string info = std::string("host=") + std::getenv("ORES_TEST_DB_HOST") +
                                 " dbname=" + std::getenv("ORES_TEST_DB_DATABASE") +
                                 " user=" + user + " password=" + password;
        std::unique_ptr<PGconn, decltype(&PQfinish)> conn(PQconnectdb(info.c_str()), &PQfinish);
        if (PQstatus(conn.get()) != CONNECTION_OK)
            return;
        for (const auto* table : {"ores_marketdata_ticks_tbl",
                                  "ores_marketdata_tick_coords_tbl",
                                  "ores_marketdata_feed_binding_activity_tbl"}) {
            std::unique_ptr<PGresult, decltype(&PQclear)> r(
                PQexec(conn.get(),
                       (std::string("delete from ") + table + " where tenant_id = '" + tenant + "'")
                           .c_str()),
                &PQclear);
        }
    }
};

/// One datum of the burst, with the series id the loop gave it.
struct burst_set {
    std::vector<std::string> uris;
    std::vector<std::string> series;
};

/// Sends one tick for each datum and waits until every series exists and holds its tick, so that
/// the caches of the loop are warm. The ticks of the burst follow later.
burst_set warm_up(fixture& f,
                  const std::string& source,
                  burst_reader& reader,
                  const std::function<void(const std::vector<market_tick>&)>& send) {
    burst_set set;
    set.uris = burst_uris();
    std::vector<market_tick> warm;
    for (const auto& uri : set.uris)
        warm.push_back(tick_from(source, uri));
    send(warm);

    const auto deadline = burst_clock::now() + std::chrono::seconds(120);
    for (std::size_t i = 0; i < set.uris.size(); ++i) {
        for (;;) {
            const auto found = f.series_for(set.uris[i]);
            if (!found.empty() &&
                reader.has(boost::uuids::to_string(found.front().id), warm[i].observation_time)) {
                set.series.push_back(boost::uuids::to_string(found.front().id));
                break;
            }
            REQUIRE(burst_clock::now() < deadline);
            std::this_thread::sleep_for(std::chrono::milliseconds(20));
        }
    }
    return set;
}

void report(const std::string& what, int rep, std::size_t rows, double seconds) {
    std::cout << "RESULT " << what << " run " << rep << ": " << rows << " rows in " << std::fixed
              << std::setprecision(3) << seconds << " s = " << std::setprecision(0)
              << static_cast<double>(rows) / seconds << " rows per second" << std::endl;
}

}

TEST_CASE("burst_a_bare_copy_of_the_tick_writer", "[.][burst]") {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    burst_cleanup cleanup{f.h.tenant_id().to_string()};
    burst_reader reader(f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id));

    const auto series = boost::uuids::to_string(boost::uuids::random_generator{}());
    const auto tenant = f.h.tenant_id().to_string();
    const auto party = boost::uuids::to_string(f.party_id);
    for (int rep = 1; rep <= burst_repetitions; ++rep) {
        const auto base = std::chrono::system_clock::now();
        std::vector<ores::marketdata::service::app::tick_row> rows(burst_ticks);
        for (std::size_t i = 0; i < burst_ticks; ++i) {
            rows[i].tenant_id = tenant;
            rows[i].party_id = party;
            rows[i].series_id = series;
            rows[i].ts = base + std::chrono::microseconds(i);
            rows[i].coord = 1;
            rows[i].value = 999.0;
        }
        const auto start = burst_clock::now();
        writer->write(rows);
        const auto seconds = std::chrono::duration<double>(burst_clock::now() - start).count();
        CHECK(reader.count_since(base) == burst_ticks);
        report("copy of the tick writer", rep, burst_ticks, seconds);
    }
}

TEST_CASE("burst_the_store_path_of_the_loop_without_nats", "[.][burst]") {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    burst_cleanup cleanup{f.h.tenant_id().to_string()};
    const auto source = bound_prefix + run_tag();
    f.bind(source, "SYNTHETIC");
    f.connect();
    burst_reader reader(f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id));

    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.refresh();
    const auto set = warm_up(f, source, reader, [&](const std::vector<market_tick>& ticks) {
        loop.handle_batch(ticks);
    });

    // The loop takes at most 20,000 ticks in a batch, so the burst goes in chunks of that size.
    constexpr std::size_t chunk = 20000;
    for (int rep = 1; rep <= burst_repetitions; ++rep) {
        const auto base = std::chrono::system_clock::now();
        std::vector<market_tick> ticks;
        ticks.reserve(burst_ticks);
        for (std::size_t i = 0; i < burst_ticks; ++i) {
            auto tick = tick_from(source, set.uris[i % set.uris.size()]);
            tick.observation_time = base + std::chrono::microseconds(i);
            ticks.push_back(std::move(tick));
        }
        const auto start = burst_clock::now();
        for (std::size_t from = 0; from < burst_ticks; from += chunk)
            loop.handle_batch(std::vector<market_tick>(
                ticks.begin() + from, ticks.begin() + std::min(from + chunk, burst_ticks)));
        const auto seconds = std::chrono::duration<double>(burst_clock::now() - start).count();
        CHECK(reader.count_since(base) == burst_ticks);
        report("store path of the loop", rep, burst_ticks, seconds);
    }
}

TEST_CASE("burst_through_nats_and_the_worker_of_the_loop", "[.][burst]") {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    burst_cleanup cleanup{f.h.tenant_id().to_string()};
    const auto source = bound_prefix + run_tag();
    f.bind(source, "SYNTHETIC");
    f.connect();
    burst_reader reader(f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id));

    // Started, the loop subscribes as the service does, and its worker thread takes the batches.
    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.start();
    const auto subject = ores::marketdata::domain::synthetic_tick_subject_for(source);
    const auto publish = [&](const market_tick& tick) {
        f.nats.publish(subject, ores::nats::default_wire_codec().encode(tick));
    };
    const auto set = warm_up(f, source, reader, [&](const std::vector<market_tick>& ticks) {
        for (const auto& tick : ticks)
            publish(tick);
    });

    for (int rep = 1; rep <= burst_repetitions; ++rep) {
        const auto base = std::chrono::system_clock::now();
        const auto start = burst_clock::now();
        std::chrono::system_clock::time_point last_ts;
        std::size_t last_datum = 0;
        for (std::size_t i = 0; i < burst_ticks; ++i) {
            auto tick = tick_from(source, set.uris[i % set.uris.size()]);
            publish(tick);
            last_ts = tick.observation_time;
            last_datum = i % set.uris.size();
        }
        const auto published = std::chrono::duration<double>(burst_clock::now() - start).count();

        // One worker stores the batches in order, so the last tick is the last row to appear.
        const auto deadline = start + std::chrono::seconds(60);
        bool done = false;
        while (burst_clock::now() < deadline &&
               !(done = reader.has(set.series[last_datum], last_ts)))
            std::this_thread::sleep_for(std::chrono::milliseconds(2));
        const auto seconds = std::chrono::duration<double>(burst_clock::now() - start).count();
        std::cout << "RESULT hand-over to the nats client run " << rep << ": " << burst_ticks
                  << " ticks in " << std::fixed << std::setprecision(3) << published << " s"
                  << std::endl;
        REQUIRE(done);
        const auto stored = reader.count_since(base);
        CHECK(stored == burst_ticks);
        report("drain through nats and the worker", rep, stored, seconds);
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
}

TEST_CASE("burst_serial_against_pipelined_through_nats", "[.][burst]") {
    auto writer = make_writer();
    if (!writer)
        SKIP("The tick writer role is not configured");
    fixture f;
    burst_cleanup cleanup{f.h.tenant_id().to_string()};
    const auto source = bound_prefix + run_tag();
    f.bind(source, "SYNTHETIC");
    f.connect();
    burst_reader reader(f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id));
    const auto subject = ores::marketdata::domain::synthetic_tick_subject_for(source);
    const auto publish = [&](const market_tick& tick) {
        f.nats.publish(subject, ores::nats::default_wire_codec().encode(tick));
    };

    // One loop and one burst for each sample, the two modes in turn, so that both see the load
    // of the machine as it changes.
    const auto sample = [&](bool pipelined) {
        feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer, pipelined);
        loop.start();
        const auto set = warm_up(f, source, reader, [&](const std::vector<market_tick>& ticks) {
            for (const auto& tick : ticks)
                publish(tick);
        });
        const auto base = std::chrono::system_clock::now();
        const auto start = burst_clock::now();
        std::chrono::system_clock::time_point last_ts;
        std::size_t last_datum = 0;
        for (std::size_t i = 0; i < burst_ticks; ++i) {
            auto tick = tick_from(source, set.uris[i % set.uris.size()]);
            publish(tick);
            last_ts = tick.observation_time;
            last_datum = i % set.uris.size();
        }
        const auto deadline = start + std::chrono::seconds(60);
        bool done = false;
        while (burst_clock::now() < deadline &&
               !(done = reader.has(set.series[last_datum], last_ts)))
            std::this_thread::sleep_for(std::chrono::milliseconds(2));
        const auto seconds = std::chrono::duration<double>(burst_clock::now() - start).count();
        REQUIRE(done);
        CHECK(reader.count_since(base) == burst_ticks);
        return static_cast<double>(burst_ticks) / seconds;
    };

    for (int rep = 1; rep <= burst_repetitions; ++rep) {
        const auto serial = sample(false);
        const auto pipelined = sample(true);
        std::cout << "RESULT serial against pipelined run " << rep << ": serial " << std::fixed
                  << std::setprecision(0) << serial << ", pipelined " << pipelined
                  << " rows per second, load " << std::setprecision(1) <<
            []() {
                double load = 0;
                std::ifstream("/proc/loadavg") >> load;
                return load;
            }() << std::endl;
    }
}

namespace {

/// The writer of the environment with a copy that fails once, when it is armed. It counts the
/// rows that its copies wrote.
class flaky_writer final : public ores::marketdata::service::app::tick_writer {
public:
    using tick_writer::tick_writer;

    void arm() {
        armed_ = true;
    }

    std::size_t written() const {
        return written_;
    }

    void write(std::span<const ores::marketdata::service::app::tick_row> rows) override {
        if (armed_.exchange(false))
            throw std::runtime_error("the copy fails on purpose");
        tick_writer::write(rows);
        written_ += rows.size();
    }

private:
    std::atomic<bool> armed_{false};
    std::atomic<std::size_t> written_{0};
};

}

TEST_CASE("a_failed_copy_loses_no_other_tick_and_stores_none_twice_while_the_next_batch_waits",
          tags) {
    const auto options = writer_options();
    if (!options)
        SKIP("The tick writer role is not configured");
    auto writer = std::make_shared<flaky_writer>(*options);

    fixture f;
    burst_cleanup cleanup{f.h.tenant_id().to_string()};
    const auto source = bound_prefix + run_tag();
    f.bind(source, "SYNTHETIC");
    f.connect();
    const auto tenant = f.h.tenant_id().to_string();
    const auto party = boost::uuids::to_string(f.party_id);
    burst_reader reader(tenant, party);

    // Every tick is republished once, stored or not, so the count of the republish says that
    // none was lost, and the rows of the table say that none was stored twice.
    std::atomic<std::size_t> republished{0};
    const auto sub =
        f.nats.subscribe(ores::marketdata::domain::market_tick_subject_all(tenant, party),
                         [&](ores::nats::message) { ++republished; });

    feed_ingest_loop loop(f.nats, f.h.context(), nullptr, writer);
    loop.start();
    const auto subject = ores::marketdata::domain::synthetic_tick_subject_for(source);
    const auto uris = burst_uris();
    std::size_t sent = 0;
    std::chrono::system_clock::time_point last_ts;
    std::size_t last_datum = 0;
    const auto send = [&](std::size_t count) {
        for (std::size_t i = 0; i < count; ++i) {
            auto tick = tick_from(source, uris[i % uris.size()]);
            f.nats.publish(subject, ores::nats::default_wire_codec().encode(tick));
            last_ts = tick.observation_time;
            last_datum = i % uris.size();
            ++sent;
        }
    };
    const auto wait_until = [](const std::function<bool()>& done) {
        const auto deadline = burst_clock::now() + std::chrono::seconds(60);
        while (!done() && burst_clock::now() < deadline)
            std::this_thread::sleep_for(std::chrono::milliseconds(5));
        return done();
    };

    const auto base = std::chrono::system_clock::now();
    std::vector<std::string> series;
    {
        std::vector<market_tick> warm;
        for (const auto& uri : uris)
            warm.push_back(tick_from(source, uri));
        for (const auto& tick : warm)
            f.nats.publish(subject, ores::nats::default_wire_codec().encode(tick));
        sent += warm.size();
        for (std::size_t i = 0; i < uris.size(); ++i) {
            std::string id;
            REQUIRE(wait_until([&] {
                const auto found = f.series_for(uris[i]);
                if (found.empty())
                    return false;
                id = boost::uuids::to_string(found.front().id);
                return reader.has(id, warm[i].observation_time);
            }));
            series.push_back(id);
        }
    }

    // A burst that the table takes.
    constexpr std::size_t part = 6000;
    send(part);
    REQUIRE(wait_until([&] { return reader.has(series[last_datum], last_ts); }));
    const auto stored_before = reader.count_since(base);
    CHECK(stored_before == uris.size() + part);

    // A burst whose first copy fails. The batches behind it are prepared while it fails, and the
    // wait that follows the failure leaves them unstored.
    writer->arm();
    send(20000);
    REQUIRE(wait_until([&] { return republished.load() == sent; }));
    CHECK(reader.count_since(base) == stored_before);

    // After the wait, the store is tried again and takes the next burst.
    std::this_thread::sleep_for(std::chrono::milliseconds(1500));
    send(part);
    REQUIRE(wait_until([&] { return reader.has(series[last_datum], last_ts); }));
    REQUIRE(wait_until([&] { return republished.load() == sent; }));

    CHECK(reader.count_since(base) == stored_before + part);
    CHECK(writer->written() == stored_before + part);
    CHECK(republished.load() == sent);
}
