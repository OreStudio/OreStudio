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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/pq/transaction.hpp"
#include "ores.database/pq/store_error.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <unordered_set>

namespace ores::database::pq {

namespace {

const statement<std::tuple<>, std::string, std::string, std::string, std::string, std::string>
    set_context{"ores_pq_set_context",
                "select set_config('app.current_tenant_id', $1, true),"
                " set_config('app.current_party_id', $2, true),"
                " set_config('app.visible_party_ids', $3, true),"
                " set_config('app.current_actor', $4, true),"
                " set_config('app.current_service', $5, true),"
                " set_config('TimeZone', 'UTC', true)"};

std::string visible_text(const std::vector<boost::uuids::uuid>& ids) {
    if (ids.empty())
        return {};
    std::string out = "{";
    for (std::size_t i = 0; i < ids.size(); ++i) {
        if (i > 0)
            out += ',';
        out += boost::uuids::to_string(ids[i]);
    }
    out += '}';
    return out;
}

enum class sent_kind { begin, prepare, execute };

struct sent_command {
    sent_kind kind;
    const sent_statement* statement;
};

[[noreturn]] void lost(const connection& c, std::string_view what) {
    const bool broken = !c.healthy();
    throw store_error(std::string(what) + ": " + PQerrorMessage(c.native()),
                      broken ? "08006" : "",
                      broken ? error_kind::retryable : error_kind::refused);
}

constexpr std::size_t copy_chunk = 1 << 20;

}

transaction::transaction(connection& c, session_context context)
    : connection_(c)
    , context_(std::move(context)) {}


void transaction::round_trip(std::span<const sent_statement> statements) {
    if (done_)
        throw store_error("The transaction has ended", "25000", error_kind::refused);

    PGconn* c = connection_.native();
    if (PQenterPipelineMode(c) != 1)
        lost(connection_, "Could not enter pipeline mode");

    const auto context_params = set_context.encode(
        context_.tenant.to_string(),
        context_.party ? boost::uuids::to_string(*context_.party) : std::string(),
        visible_text(context_.visible_parties),
        context_.actor,
        context_.service);
    const sent_statement context_statement{set_context.name(),
                                           set_context.sql(),
                                           set_context.param_oids,
                                           context_params,
                                           &set_context.counters(),
                                           nullptr};

    std::vector<sent_command> sent;
    std::unordered_set<std::string> preparing;
    const auto send = [&](const sent_statement& s) {
        std::vector<const char*> values;
        std::vector<int> lengths;
        std::vector<int> formats;
        for (const auto& p : s.params) {
            values.push_back(p.null ? nullptr : p.bytes.data());
            lengths.push_back(static_cast<int>(p.bytes.size()));
            formats.push_back(p.text ? 0 : 1);
        }
        if (s.name.empty()) {
            const std::string sql(s.sql);
            if (PQsendQueryParams(c,
                                  sql.c_str(),
                                  static_cast<int>(s.params.size()),
                                  s.oids.data(),
                                  values.data(),
                                  lengths.data(),
                                  formats.data(),
                                  s.result_format) != 1)
                lost(connection_, "Could not send a statement");
            sent.push_back({sent_kind::execute, &s});
            return;
        }
        const std::string name(s.name);
        if (!connection_.is_prepared(name) && preparing.insert(name).second) {
            const std::string sql(s.sql);
            if (PQsendPrepare(
                    c, name.c_str(), sql.c_str(), static_cast<int>(s.oids.size()), s.oids.data()) !=
                1)
                lost(connection_, "Could not send a prepare");
            sent.push_back({sent_kind::prepare, &s});
        }
        if (PQsendQueryPrepared(c,
                                name.c_str(),
                                static_cast<int>(s.params.size()),
                                values.data(),
                                lengths.data(),
                                formats.data(),
                                s.result_format) != 1)
            lost(connection_, "Could not send a statement");
        sent.push_back({sent_kind::execute, &s});
    };

    const bool beginning = !begun_;
    if (beginning) {
        if (PQsendQueryParams(c, "begin", 0, nullptr, nullptr, nullptr, nullptr, 0) != 1)
            lost(connection_, "Could not send begin");
        sent.push_back({sent_kind::begin, nullptr});
        send(context_statement);
    }
    for (const auto& s : statements)
        send(s);
    if (PQpipelineSync(c) != 1)
        lost(connection_, "Could not sync the pipeline");

    std::optional<store_error> failure;
    for (const auto& command : sent) {
        result r(PQgetResult(c));
        if (!r)
            lost(connection_, "The connection ended a pipeline early");
        if (r.status() != PGRES_PIPELINE_ABORTED) {
            try {
                const std::string_view what = !command.statement ? "begin" :
                                              command.statement->name.empty() ?
                                                                   "statement" :
                                                                   command.statement->name;
                check(connection_, r, what);
                if (command.kind == sent_kind::begin) {
                    begun_ = true;
                } else if (command.kind == sent_kind::prepare) {
                    connection_.mark_prepared(command.statement->name);
                    command.statement->counters->prepares.fetch_add(1);
                } else {
                    if (command.statement->counters)
                        command.statement->counters->executions.fetch_add(1);
                    if (command.statement->out)
                        *command.statement->out = std::move(r);
                }
            } catch (store_error& e) {
                if (!failure)
                    failure = std::move(e);
            }
        }
        while (result(PQgetResult(c))) {
        }
    }
    const result sync(PQgetResult(c));
    if (!sync || sync.status() != PGRES_PIPELINE_SYNC)
        lost(connection_, "The pipeline ended without its sync");
    if (PQexitPipelineMode(c) != 1)
        lost(connection_, "Could not leave pipeline mode");
    if (failure)
        throw *failure;
}

std::vector<std::uint64_t> transaction::run(const batch& b) {
    std::vector<result> results(b.entries_.size());
    std::vector<sent_statement> statements;
    statements.reserve(b.entries_.size());
    for (std::size_t i = 0; i < b.entries_.size(); ++i) {
        const auto& e = b.entries_[i];
        statements.push_back({e.name, e.sql, e.oids, e.params, e.counters, &results[i]});
    }
    round_trip(statements);
    std::vector<std::uint64_t> affected;
    affected.reserve(results.size());
    for (const auto& r : results)
        affected.push_back(r.affected());
    return affected;
}

std::string transaction::copy_header() {
    std::string header("PGCOPY\n\377\r\n\0", 11);
    wire::put_u32(header, 0);
    wire::put_u32(header, 0);
    return header;
}

std::uint64_t
transaction::copy_data(std::string_view table, std::string_view columns, const std::string& data) {
    if (!begun_)
        round_trip({});
    PGconn* c = connection_.native();
    std::string sql = "copy ";
    sql += table;
    sql += " (";
    sql += columns;
    sql += ") from stdin (format binary)";
    const result start(PQexec(c, sql.c_str()));
    if (start.status() != PGRES_COPY_IN) {
        check(connection_, start, "copy");
        lost(connection_, "Copy did not start");
    }
    for (std::size_t at = 0; at < data.size(); at += copy_chunk) {
        const auto n = std::min(copy_chunk, data.size() - at);
        if (PQputCopyData(c, data.data() + at, static_cast<int>(n)) != 1)
            lost(connection_, "Could not send the copy data");
    }
    if (PQputCopyEnd(c, nullptr) != 1)
        lost(connection_, "Could not end the copy");
    const result end(PQgetResult(c));
    check(connection_, end, "copy");
    const auto written = end.affected();
    while (result(PQgetResult(c))) {
    }
    return written;
}

void transaction::commit() {
    if (done_)
        return;
    done_ = true;
    if (!begun_)
        return;
    if (PQtransactionStatus(connection_.native()) == PQTRANS_INERROR) {
        const result r(PQexec(connection_.native(), "rollback"));
        throw store_error(
            "The transaction was aborted by a failed statement, so it was rolled back",
            "25P02",
            error_kind::refused);
    }
    const result r(PQexec(connection_.native(), "commit"));
    check(connection_, r, "commit");
}

void transaction::rollback() noexcept {
    if (done_)
        return;
    done_ = true;
    if (!begun_ || !connection_.healthy())
        return;
    const result r(PQexec(connection_.native(), "rollback"));
}

}
