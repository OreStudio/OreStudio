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
#ifndef ORES_DATABASE_PQ_TRANSACTION_HPP
#define ORES_DATABASE_PQ_TRANSACTION_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/statement.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace ores::database::pq {

/**
 * @brief Who a transaction acts for. The policies of row-level security read
 * these settings, and the audit columns name the actor and the service.
 */
struct session_context final {
    utility::uuid::tenant_id tenant = utility::uuid::tenant_id::system();
    std::optional<boost::uuids::uuid> party;
    std::vector<boost::uuids::uuid> visible_parties;
    std::string actor;
    std::string service;
};

/**
 * @brief One statement of a round trip: what to prepare, what to send, and
 * where its result goes.
 */
struct sent_statement final {
    std::string_view name;
    std::string_view sql;
    std::span<const Oid> oids;
    std::span<const param> params;
    const statement_counters* counters = nullptr;
    result* out = nullptr;
    /// 1 for binary results, 0 for text.
    int result_format = 1;
};

/**
 * @brief One open transaction on one connection.
 *
 * Nothing is sent until the first statement. That statement goes out in one
 * pipeline with =begin= and the =set_config(..., true)= of the session
 * context, so the context costs no round trip of its own, and it ends with
 * the transaction. A statement not yet prepared on the connection is prepared
 * in the same pipeline.
 *
 * Only a unit of work makes, commits and rolls back a transaction: see
 * transact().
 */
class ORES_DATABASE_EXPORT transaction final {
public:
    transaction(connection& c, session_context context);

    /**
     * @brief Joins the transaction that sqlgen holds open on @p c.
     *
     * Nothing begins and nothing is set: the connection is already in the
     * transaction, with the context sqlgen set on it. Commit and rollback are
     * left to sqlgen. A borrowed connection keeps no record of what is
     * prepared on it, so every statement runs unnamed.
     */
    [[nodiscard]] static transaction join(connection& c);

    transaction(const transaction&) = delete;
    transaction& operator=(const transaction&) = delete;

    /**
     * @brief Runs a statement and reads its rows.
     */
    template <typename... Columns, typename... Params, typename... Args>
    std::vector<std::tuple<Columns...>> query(const statement<std::tuple<Columns...>, Params...>& s,
                                              const Args&... args) {
        const auto params = s.encode(args...);
        result r;
        run_one(s, params, r);
        return s.rows(r);
    }

    /**
     * @brief Runs a statement and returns the rows it touched.
     */
    template <typename... Columns, typename... Params, typename... Args>
    std::uint64_t execute(const statement<std::tuple<Columns...>, Params...>& s,
                          const Args&... args) {
        const auto params = s.encode(args...);
        result r;
        run_one(s, params, r);
        return r.affected();
    }

    /**
     * @brief Runs a statement built at run time and reads its rows into the
     * given column types.
     */
    template <typename... Columns>
    std::vector<std::tuple<Columns...>> query(const dynamic_statement& s) {
        result r;
        run_dynamic(s, r);
        return read_rows<Columns...>(r);
    }

    /**
     * @brief Runs a statement built at run time and returns the rows it
     * touched.
     */
    std::uint64_t execute(const dynamic_statement& s) {
        result r;
        run_dynamic(s, r);
        return r.affected();
    }

    /**
     * @brief Runs a text statement and returns its result, in text.
     */
    result run(const text_statement& s, std::span<const param> params) {
        result r;
        const sent_statement one{s.name(), s.sql(), {}, params, &s.counters(), &r, 0};
        round_trip(std::span(&one, 1));
        return r;
    }

    /**
     * @brief Runs a statement built at run time and returns its result, in text.
     */
    result run_text(const dynamic_statement& s) {
        result r;
        const sent_statement one{{}, s.sql(), s.oids(), s.params(), nullptr, &r, 0};
        round_trip(std::span(&one, 1));
        return r;
    }

    /**
     * @brief Statements sent together in one pipeline, with one sync.
     */
    class batch final {
    public:
        template <typename... Columns, typename... Params, typename... Args>
        batch& add(const statement<std::tuple<Columns...>, Params...>& s, const Args&... args) {
            const auto encoded = s.encode(args...);
            entry e{
                s.name(), s.sql(), s.param_oids, {encoded.begin(), encoded.end()}, &s.counters()};
            entries_.push_back(std::move(e));
            return *this;
        }

        batch& add(const text_statement& s, std::vector<param> params) {
            entries_.push_back(entry{s.name(), s.sql(), {}, std::move(params), &s.counters()});
            return *this;
        }

        [[nodiscard]] std::size_t size() const noexcept {
            return entries_.size();
        }

    private:
        friend class transaction;
        struct entry {
            std::string_view name;
            std::string_view sql;
            std::span<const Oid> oids;
            std::vector<param> params;
            const statement_counters* counters;
        };
        std::vector<entry> entries_;
    };

    /**
     * @brief Sends a batch in one pipeline and returns the rows each statement
     * touched, in order.
     */
    std::vector<std::uint64_t> run(const batch& b);

    /**
     * @brief Writes rows with binary COPY and returns how many it wrote.
     *
     * The table and the columns are code, never input. Each row is a tuple
     * whose element types have codecs, in the order of the columns.
     */
    template <typename... Columns>
    std::uint64_t copy_in(std::string_view table,
                          std::string_view columns,
                          const std::vector<std::tuple<Columns...>>& rows) {
        std::string data = copy_header();
        for (const auto& row : rows) {
            wire::put_u16(data, static_cast<std::uint16_t>(sizeof...(Columns)));
            std::apply([&](const auto&... v) { (append_field(data, v), ...); }, row);
        }
        wire::put_u16(data, 0xFFFF);
        return copy_data(table, columns, data);
    }

    /**
     * @brief Commits. A transaction that ran nothing has nothing to commit.
     */
    void commit();

    /**
     * @brief Rolls back, and never throws: it runs on the error path.
     */
    void rollback() noexcept;

private:
    template <typename S, std::size_t N>
    void run_one(const S& s, const std::array<param, N>& params, result& out) {
        const sent_statement one{s.name(), s.sql(), s.param_oids, params, &s.counters(), &out};
        round_trip(std::span(&one, 1));
    }

    void run_dynamic(const dynamic_statement& s, result& out) {
        const sent_statement one{{}, s.sql(), s.oids(), s.params(), nullptr, &out};
        round_trip(std::span(&one, 1));
    }

    template <typename T>
    static void append_field(std::string& data, const T& v) {
        const auto p = encode_param<T>(v);
        if (p.null) {
            wire::put_u32(data, 0xFFFFFFFF);
            return;
        }
        wire::put_u32(data, static_cast<std::uint32_t>(p.bytes.size()));
        data += p.bytes;
    }

    static std::string copy_header();
    std::uint64_t
    copy_data(std::string_view table, std::string_view columns, const std::string& data);
    void round_trip(std::span<const sent_statement> statements);

    struct joined_tag {};
    transaction(connection& c, joined_tag);

    connection& connection_;
    session_context context_;
    bool begun_ = false;
    bool done_ = false;
    bool joined_ = false;
};

}

#endif
