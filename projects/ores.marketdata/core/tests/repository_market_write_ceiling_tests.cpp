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
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdlib>
#include <format>
#include <iostream>
#include <libpq-fe.h>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

/*
 * The write ceiling of the tick store, measured from the application.
 *
 * Both cases are hidden: they run only when named, because they write many rows. Run
 * them with the test database settings of the environment, for example
 *
 *   ores.marketdata.core.tests "[.write_ceiling_repository]"
 *   ores.marketdata.core.tests "[.write_ceiling_copy]"
 *
 * The COPY case creates scratch tables in the schema write_ceiling with the DDL user of the
 * test database. It drops the schema at the end, also after a failure, unless
 * WRITE_CEILING_KEEP is set, which lets a script fill the tables with history first.
 * These settings change the run:
 *
 *   WRITE_CEILING_KEEP         keep the scratch schema.
 *   WRITE_CEILING_ONLY_CREATE  create the scratch tables and stop.
 *   WRITE_CEILING_SHAPES       run only the shapes whose name contains this text, for example
 *                              append_only.
 *   ORES_DB_DDL_USER and ORES_DB_DDL_PASSWORD
 *                              the role that writes the tick_owner shape. It has BYPASSRLS
 *                              and does not own the table.
 *
 * The shape tick_owner is the append-only shape with a tenant and a party on each row, the two
 * policies of the other tables in the scalar subquery form, and FORCE row-level security.
 *
 * The shape observation_with_trigger is a scratch copy of the observation table with a copy of
 * its insert trigger. COPY cannot write the real table, because PostgreSQL refuses COPY FROM
 * on a table with row-level security. The repository case writes the real table and leaves
 * its rows there, because the delete trigger only closes rows. Run it against a test
 * database that the gate rebuilds.
 *
 * @see The tick ingest pipeline (doc/knowledge/architecture/tick_ingest_pipeline.org)
 */
namespace {

using stopwatch = std::chrono::steady_clock;

const std::string repository_tags("[.write_ceiling_repository]");
const std::string copy_tags("[.write_ceiling_copy]");

const std::string uri = "oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY";

/// A setting of the process environment, read through the platform provider.
///
/// Reaching the CRT's getenv directly is deprecated on Windows and the build
/// treats that as an error, and the provider is the one place that knows how
/// each platform is asked.
std::string setting(const std::string& name) {
    return ores::platform::environment::environment::get_value_or_throw(name);
}

struct connection_closer {
    void operator()(PGconn* c) const {
        PQfinish(c);
    }
};
using connection = std::unique_ptr<PGconn, connection_closer>;

connection connect_as(const char* user_setting, const char* password_setting) {
    const auto host = setting("ORES_TEST_DB_HOST");
    const auto database = setting("ORES_TEST_DB_DATABASE");
    const auto user = setting(user_setting);
    const auto password = setting(password_setting);
    const char* keys[] = {"host", "dbname", "user", "password", nullptr};
    const char* values[] = {
        host.c_str(), database.c_str(), user.c_str(), password.c_str(), nullptr};
    connection c(PQconnectdbParams(keys, values, 0));
    if (PQstatus(c.get()) != CONNECTION_OK)
        throw std::runtime_error(std::string("Cannot connect: ") + PQerrorMessage(c.get()));
    return c;
}

void run(PGconn* c, const std::string& sql) {
    std::unique_ptr<PGresult, void (*)(PGresult*)> r(PQexec(c, sql.c_str()), PQclear);
    const auto status = PQresultStatus(r.get());
    if (status != PGRES_COMMAND_OK && status != PGRES_TUPLES_OK)
        throw std::runtime_error(std::string("Statement failed: ") + PQerrorMessage(c) + " in " +
                                 sql);
}

std::string scalar(PGconn* c, const std::string& sql) {
    std::unique_ptr<PGresult, void (*)(PGresult*)> r(PQexec(c, sql.c_str()), PQclear);
    if (PQresultStatus(r.get()) != PGRES_TUPLES_OK)
        throw std::runtime_error(std::string("Query failed: ") + PQerrorMessage(c) + " in " + sql);
    if (PQntuples(r.get()) == 0 || PQgetisnull(r.get(), 0, 0))
        return {};
    return PQgetvalue(r.get(), 0, 0);
}

/// Drops the scratch schema when the case ends, whether it passes or fails.
class scratch_schema_guard {
public:
    scratch_schema_guard(PGconn* c, bool keep)
        : conn_(c)
        , keep_(keep) {}
    scratch_schema_guard(const scratch_schema_guard&) = delete;
    scratch_schema_guard& operator=(const scratch_schema_guard&) = delete;
    ~scratch_schema_guard() {
        if (keep_)
            return;
        try {
            run(conn_, "drop schema if exists write_ceiling cascade");
        } catch (...) {
        }
    }

private:
    PGconn* conn_;
    bool keep_;
};

void copy_text(PGconn* c, const std::string& copy_sql, const std::string& text) {
    std::unique_ptr<PGresult, void (*)(PGresult*)> start(PQexec(c, copy_sql.c_str()), PQclear);
    if (PQresultStatus(start.get()) != PGRES_COPY_IN)
        throw std::runtime_error(std::string("COPY did not start: ") + PQerrorMessage(c));
    if (PQputCopyData(c, text.data(), static_cast<int>(text.size())) != 1 ||
        PQputCopyEnd(c, nullptr) != 1)
        throw std::runtime_error(std::string("COPY failed: ") + PQerrorMessage(c));
    std::unique_ptr<PGresult, void (*)(PGresult*)> done(PQgetResult(c), PQclear);
    if (PQresultStatus(done.get()) != PGRES_COMMAND_OK)
        throw std::runtime_error(std::string("COPY ended badly: ") + PQerrorMessage(c));
}

/// The start of this run, to the microsecond. Rows are stamped from here on, so they
/// come after any history that a script loaded first.
std::chrono::sys_time<std::chrono::microseconds> start_of_run() {
    static const auto start =
        std::chrono::floor<std::chrono::microseconds>(std::chrono::system_clock::now());
    return start;
}

double seconds_since(stopwatch::time_point start) {
    return std::chrono::duration<double>(stopwatch::now() - start).count();
}

void repository_market_write_ceiling_report(const std::string& shape,
                                            const std::string& writer,
                                            std::size_t batch,
                                            std::size_t groups,
                                            std::size_t rows,
                                            double secs) {
    std::cout << std::format("RESULT shape={} writer={} batch={} transactions_per_batch={} rows={} "
                             "secs={:.2f} rows_per_s={:.0f}\n",
                             shape,
                             writer,
                             batch,
                             groups,
                             rows,
                             secs,
                             static_cast<double>(rows) / secs)
              << std::flush;
}

/// The text of @p count rows of the observation shape, for COPY. The times run on from
/// @p first so that no two rows of a batch share a key.
std::string observation_text(std::size_t count,
                             std::size_t first,
                             const std::vector<std::string>& series,
                             const std::string& tenant,
                             const std::string& party,
                             boost::uuids::random_generator& gen) {
    using namespace std::chrono;
    const auto base = start_of_run();
    std::string out;
    out.reserve(count * 260);
    for (std::size_t i = 0; i < count; ++i) {
        const auto at = base + microseconds{static_cast<std::int64_t>((first + i) * 10)};
        const auto stamp = std::format("{:%Y-%m-%d %H:%M:%S}+00", at);
        out += boost::uuids::to_string(gen());
        out += '\t' + tenant + '\t' + party + '\t' + series[(first + i) % series.size()] + '\t';
        out += stamp + '\t' + uri + "\t43.98966624067043\tsynthetic.bloomberg.usdtry\t" + stamp +
               "\tinfinity\n";
    }
    return out;
}

/// The text of @p count rows of the owner shape, for COPY. Every row belongs to the tenant and
/// the party of @p group, and uses one of the 20 series of that group.
std::string owner_text(std::size_t count,
                       std::size_t first,
                       std::size_t group,
                       const std::vector<std::string>& series,
                       const std::vector<std::string>& tenants,
                       const std::vector<std::string>& parties) {
    using namespace std::chrono;
    const auto base = start_of_run();
    std::string out;
    out.reserve(count * 140);
    for (std::size_t i = 0; i < count; ++i) {
        const auto at = base + microseconds{static_cast<std::int64_t>((first + i) * 10)};
        out += tenants[group] + '\t' + parties[group] + '\t' + series[group * 20 + (i % 20)] +
               '\t' + std::format("{:%Y-%m-%d %H:%M:%S}+00", at) + "\t0\t43.98966624067043\n";
    }
    return out;
}

/// The text of @p count rows of the append-only shape, for COPY.
std::string
tick_text(std::size_t count, std::size_t first, const std::vector<std::string>& series) {
    using namespace std::chrono;
    const auto base = start_of_run();
    std::string out;
    out.reserve(count * 80);
    for (std::size_t i = 0; i < count; ++i) {
        const auto at = base + microseconds{static_cast<std::int64_t>((first + i) * 10)};
        out += series[(first + i) % series.size()] + '\t' +
               std::format("{:%Y-%m-%d %H:%M:%S}+00", at) + "\t0\t43.98966624067043\n";
    }
    return out;
}

}

TEST_CASE("write_ceiling_of_the_repository_insert", repository_tags) {
    ores::testing::database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    ores::marketdata::repository::market_series_repository series_repo;
    std::vector<boost::uuids::uuid> pool;
    for (int i = 0; i < 200; ++i) {
        auto s = ores::marketdata::generators::generate_synthetic_market_series(ctx);
        series_repo.write(h.context(), s);
        pool.push_back(s.id);
    }

    auto templ = ores::marketdata::generators::generate_synthetic_market_observation(ctx);
    boost::uuids::random_generator gen;
    ores::marketdata::repository::market_observation_repository obs_repo;

    using namespace std::chrono;
    const auto base = system_clock::time_point{microseconds{1'700'000'000'000'000}};
    std::size_t next = 0;

    for (const std::size_t batch : {100, 1000, 5000, 20000}) {
        const std::size_t batches = std::max<std::size_t>(3, 40000 / batch);
        const auto started = stopwatch::now();
        for (std::size_t b = 0; b < batches; ++b) {
            std::vector<ores::marketdata::domain::market_observation> rows;
            rows.reserve(batch);
            for (std::size_t i = 0; i < batch; ++i, ++next) {
                auto o = templ;
                o.id = gen();
                o.series_id = pool[next % pool.size()];
                o.observation_datetime = base + microseconds{static_cast<std::int64_t>(next * 10)};
                o.oresmd_uri = uri;
                o.value = "43.98966624067043";
                rows.push_back(std::move(o));
            }
            obs_repo.insert(h.context(), rows);
        }
        repository_market_write_ceiling_report(
            "observation", "repository", batch, 1, batch * batches, seconds_since(started));
    }
    SUCCEED();
}

TEST_CASE("write_ceiling_of_copy_through_libpq", copy_tags) {
    const bool keep = ores::platform::environment::environment::get_value("WRITE_CEILING_KEEP").has_value();
    auto conn = connect_as("ORES_TEST_DB_DDL_USER", "ORES_TEST_DB_DDL_PASSWORD");
    scratch_schema_guard guard(conn.get(), keep);

    run(conn.get(), "create schema if not exists write_ceiling");
    run(conn.get(),
        "create table if not exists write_ceiling.obs_a1 (like "
        "public.ores_marketdata_market_observations_tbl including all)");
    run(conn.get(),
        "select create_hypertable('write_ceiling.obs_a1', 'observation_datetime', "
        "chunk_time_interval => interval '30 days', if_not_exists => true)");
    run(conn.get(),
        "create table if not exists write_ceiling.obs_trg (like "
        "public.ores_marketdata_market_observations_tbl including all)");
    run(conn.get(),
        "select create_hypertable('write_ceiling.obs_trg', 'observation_datetime', "
        "chunk_time_interval => interval '30 days', if_not_exists => true)");
    // The body of ores_marketdata_market_observations_insert_fn, aimed at the scratch table.
    run(conn.get(),
        "create or replace function write_ceiling.obs_trg_insert_fn() returns trigger as $$ "
        "begin "
        "new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id); "
        "update write_ceiling.obs_trg set valid_to = current_timestamp "
        "where tenant_id = new.tenant_id and series_id = new.series_id "
        "and observation_datetime = new.observation_datetime and oresmd_uri = new.oresmd_uri "
        "and valid_to = ores_utility_infinity_timestamp_fn() and valid_from < current_timestamp; "
        "new.valid_from := current_timestamp; "
        "new.valid_to := ores_utility_infinity_timestamp_fn(); "
        "return new; end; $$ language plpgsql set search_path = public, pg_temp");
    run(conn.get(),
        "create or replace trigger obs_trg_insert_trg before insert on write_ceiling.obs_trg "
        "for each row execute function write_ceiling.obs_trg_insert_fn()");
    run(conn.get(),
        "create table if not exists write_ceiling.tick_append (series_id uuid not null, "
        "ts timestamptz not null, coord smallint not null default 0, value double precision not "
        "null)");
    run(conn.get(),
        "create index if not exists tick_append_series_ts_idx on write_ceiling.tick_append "
        "(series_id, ts desc)");
    run(conn.get(),
        "select create_hypertable('write_ceiling.tick_append', 'ts', chunk_time_interval => "
        "interval '1 day', if_not_exists => true)");

    // The owner shape is the append-only shape with the tenant and the party on each row, and
    // the two policies of the other tables. FORCE makes the owner subject to them, so that this
    // connection can play the reader. The writer is a second role with BYPASSRLS that does not
    // own the table.
    const std::string writer_role =
        ores::platform::environment::environment::get_value_or_default("ORES_DB_DDL_USER", "");
    run(conn.get(),
        "create table if not exists write_ceiling.tick_owner (tenant_id uuid not null, party_id "
        "uuid not null, series_id uuid not null, ts timestamptz not null, coord smallint not null "
        "default 0, value double precision not null)");
    run(conn.get(),
        "create index if not exists tick_owner_series_ts_idx on write_ceiling.tick_owner "
        "(series_id, ts desc)");
    run(conn.get(),
        "select create_hypertable('write_ceiling.tick_owner', 'ts', chunk_time_interval => "
        "interval '1 day', if_not_exists => true)");
    run(conn.get(), "alter table write_ceiling.tick_owner enable row level security");
    run(conn.get(), "alter table write_ceiling.tick_owner force row level security");
    run(conn.get(), "drop policy if exists tenant_policy on write_ceiling.tick_owner");
    run(conn.get(),
        "create policy tenant_policy on write_ceiling.tick_owner for all using (tenant_id = "
        "(select nullif(current_setting('app.current_tenant_id', true), '')::uuid)) with check "
        "(tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid))");
    run(conn.get(), "drop policy if exists party_policy on write_ceiling.tick_owner");
    run(conn.get(),
        "create policy party_policy on write_ceiling.tick_owner as restrictive for select using "
        "(party_id in (select unnest(nullif(current_setting('app.visible_party_ids', true), "
        "'')::uuid[])))");
    if (!writer_role.empty()) {
        run(conn.get(), "grant usage on schema write_ceiling to \"" + writer_role + "\"");
        run(conn.get(), "grant insert on write_ceiling.tick_owner to \"" + writer_role + "\"");
    }

    if (ores::platform::environment::environment::get_value("WRITE_CEILING_ONLY_CREATE")
            .has_value()) {
        SUCCEED();
        return;
    }
    const std::string only_shape =
        ores::platform::environment::environment::get_value_or_default("WRITE_CEILING_SHAPES", "");

    boost::uuids::random_generator gen;
    std::vector<std::string> series;
    for (int i = 0; i < 2000; ++i)
        series.push_back(boost::uuids::to_string(gen()));
    // The trigger shape needs a tenant that exists, so it takes the first one of the database.
    std::string tenant = scalar(conn.get(),
                                "select id from public.ores_iam_tenants_tbl "
                                "where status in ('active', 'bootstrapping') and valid_to > now() "
                                "and id <> 'ffffffff-ffff-ffff-ffff-ffffffffffff' "
                                "order by valid_from limit 1");
    if (tenant.empty())
        tenant = "540caae1-5ccc-43c9-82a8-5a49c5136d58";
    const std::string party = "e93025eb-c4bf-4df0-ba80-11574a8f3d67";
    std::vector<std::string> tenants;
    std::vector<std::string> parties;
    for (int i = 0; i < 100; ++i) {
        tenants.push_back(boost::uuids::to_string(gen()));
        parties.push_back(boost::uuids::to_string(gen()));
    }
    connection writer;
    if (!writer_role.empty() &&
        (only_shape.empty() || std::string("tick_owner").find(only_shape) != std::string::npos))
        writer = connect_as("ORES_DB_DDL_USER", "ORES_DB_DDL_PASSWORD");

    // The rows of one batch are split across this many transactions, one for each tenant.
    std::size_t next = 0;
    for (const std::string shape :
         {"observation_with_trigger", "observation_no_trigger", "append_only", "tick_owner"}) {
        for (const std::size_t batch : {100, 1000, 10000, 50000}) {
            for (const std::size_t groups : {1, 10, 100}) {
                // A transaction of fewer than 100 rows is commit-bound, which the first run
                // showed, and it takes many minutes for no new information.
                if (batch / groups < 100)
                    continue;
                if (!only_shape.empty() && shape.find(only_shape) == std::string::npos)
                    continue;
                if (shape == "tick_owner" && !writer)
                    continue;
                const std::size_t total =
                    shape == "append_only" || shape == "tick_owner" ? 300000 : 100000;
                const std::size_t batches = std::max<std::size_t>(2, total / batch);
                const auto started = stopwatch::now();
                for (std::size_t b = 0; b < batches; ++b) {
                    const std::size_t per_group = batch / groups;
                    for (std::size_t g = 0; g < groups; ++g, next += per_group) {
                        if (shape == "tick_owner")
                            copy_text(writer.get(),
                                      "copy write_ceiling.tick_owner (tenant_id, party_id, "
                                      "series_id, ts, coord, value) from stdin",
                                      owner_text(per_group, next, g, series, tenants, parties));
                        else if (shape == "append_only")
                            copy_text(conn.get(),
                                      "copy write_ceiling.tick_append (series_id, ts, coord, "
                                      "value) from stdin",
                                      tick_text(per_group, next, series));
                        else if (shape == "observation_with_trigger")
                            copy_text(
                                conn.get(),
                                "copy write_ceiling.obs_trg (id, tenant_id, party_id, "
                                "series_id, observation_datetime, oresmd_uri, value, "
                                "source, valid_from, valid_to) from stdin",
                                observation_text(per_group, next, series, tenant, party, gen));
                        else
                            copy_text(
                                conn.get(),
                                "copy write_ceiling.obs_a1 (id, tenant_id, party_id, "
                                "series_id, observation_datetime, oresmd_uri, value, "
                                "source, valid_from, valid_to) from stdin",
                                observation_text(per_group, next, series, tenant, party, gen));
                    }
                }
                repository_market_write_ceiling_report(shape,
                                                       "copy",
                                                       batch,
                                                       groups,
                                                       batches * (batch / groups) * groups,
                                                       seconds_since(started));
            }
        }
    }

    SUCCEED();
}
