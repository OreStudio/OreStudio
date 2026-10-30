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
#ifndef ORES_HTTP_API_NET_SESSION_BYTES_ACCUMULATOR_HPP
#define ORES_HTTP_API_NET_SESSION_BYTES_ACCUMULATOR_HPP

#include "ores.http.api/export.hpp"
#include "ores.http.api/net/http_session.hpp"
#include "ores.http.api/net/worker_pool.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <chrono>
#include <cstdint>
#include <mutex>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::http::net {

/**
 * @brief Holds a session's byte totals and writes them on a timer.
 *
 * The column these numbers feed is the total a session has sent and received,
 * but the gateway only knows each request's size. Writing that size per request
 * both blocks the connection's thread on the database and stores the size of the
 * last request rather than the total. This class keeps the running total in
 * memory and writes it once per interval, from a worker thread.
 *
 * The write is skipped for a session that saw no traffic since the last flush,
 * so an idle browser costs nothing. A session's entry is dropped once it has been
 * quiet for the retain window, which bounds the memory a long-lived process
 * holds; a session that comes back after that window starts its total again from
 * that point.
 *
 * A failed write costs at most one interval of counts: the totals stay in memory
 * and the next flush tries again.
 *
 * @see doc/plans/2026-10-28-single-http-server-design.org, increment A1.
 */
class ORES_HTTP_API_EXPORT session_bytes_accumulator final {
public:
    using sink_fn = session_bytes_callback;

    /**
     * @param io         The server's context, on which the timer runs.
     * @param sink       Where a session's totals go. Called from a worker.
     * @param workers    The pool the write runs on, so the I/O thread never
     *                   blocks on the database.
     * @param interval   How often the totals are written.
     * @param retain     How long a quiet session keeps its total in memory.
     */
    session_bytes_accumulator(boost::asio::io_context& io,
                              sink_fn sink,
                              worker_pool& workers,
                              std::chrono::milliseconds interval,
                              std::chrono::milliseconds retain)
        : io_(io)
        , sink_(std::move(sink))
        , workers_(workers)
        , interval_(interval)
        , retain_(retain)
        , timer_(io) {}

    ~session_bytes_accumulator() {
        stopped_ = true;
        cancel_timer();
    }

    session_bytes_accumulator(const session_bytes_accumulator&) = delete;
    session_bytes_accumulator& operator=(const session_bytes_accumulator&) = delete;

    /// Starts the timer. Safe to call once.
    void start() {
        schedule();
    }

    /**
     * @brief Adds one request's sizes to a session's running total.
     *
     * Called after every authenticated request. It books the numbers and
     * returns: the database is not touched here.
     */
    void add(const std::string& session_id,
             std::chrono::system_clock::time_point start_time,
             std::size_t bytes_sent,
             std::size_t bytes_received) {
        const auto now = std::chrono::system_clock::now();
        std::lock_guard lock(mu_);
        auto& entry = entries_[session_id];
        if (entry.sent == 0 && entry.received == 0)
            entry.start_time = start_time;
        entry.sent += bytes_sent;
        entry.received += bytes_received;
        entry.last_activity = now;
    }

    /// Stops the timer and writes whatever is pending, on the calling thread.
    void stop() {
        stopped_ = true;
        cancel_timer();
        write_pending();
    }

    /// Writes the totals that changed since the last write. Thread-safe.
    void flush() {
        write_pending();
    }

    /// The sessions currently holding a total. For a test and for a log line.
    [[nodiscard]] std::size_t tracked_sessions() const {
        std::lock_guard lock(mu_);
        return entries_.size();
    }

private:
    struct totals final {
        std::chrono::system_clock::time_point start_time{};
        std::uint64_t sent{0};
        std::uint64_t received{0};
        std::uint64_t written_sent{0};
        std::uint64_t written_received{0};
        std::chrono::system_clock::time_point last_activity{};
    };

    /// Cancels the timer. A cancelled or already expired timer is not an error.
    void cancel_timer() noexcept {
        try {
            timer_.cancel();
        } catch (...) {
        }
    }

    void schedule() {
        if (stopped_)
            return;
        timer_.expires_after(interval_);
        timer_.async_wait([this](const boost::system::error_code& ec) {
            if (ec || stopped_)
                return;
            /*
             * The write belongs to a worker, not to this context: a database
             * round trip on the I/O thread is the defect this class exists to
             * remove. The next flush is armed when this one finishes, so two
             * flushes never overlap.
             */
            boost::asio::co_spawn(
                io_,
                [this]() -> boost::asio::awaitable<void> {
                    co_await workers_.run([this]() -> boost::asio::awaitable<void> {
                        write_pending();
                        co_return;
                    });
                    schedule();
                },
                boost::asio::detached);
        });
    }

    void write_pending() {
        std::vector<std::pair<std::string, totals>> owing;
        const auto now = std::chrono::system_clock::now();
        {
            std::lock_guard lock(mu_);
            owing.reserve(entries_.size());
            for (auto it = entries_.begin(); it != entries_.end();) {
                const auto& entry = it->second;
                if (entry.sent != entry.written_sent ||
                    entry.received != entry.written_received)
                    owing.emplace_back(it->first, entry);
                if (now - entry.last_activity > retain_)
                    it = entries_.erase(it);
                else
                    ++it;
            }
        }

        for (const auto& [session_id, snapshot] : owing) {
            try {
                sink_(session_id, snapshot.start_time, snapshot.sent, snapshot.received);
            } catch (const std::exception&) {
                /*
                 * A counter is best effort: the entry is left unwritten, so the
                 * next flush offers the same total again. A database hiccup then
                 * costs a delay and not a gap in the numbers.
                 */
                continue;
            }
            std::lock_guard lock(mu_);
            const auto it = entries_.find(session_id);
            if (it == entries_.end())
                continue;
            it->second.written_sent = snapshot.sent;
            it->second.written_received = snapshot.received;
        }
    }

    boost::asio::io_context& io_;
    sink_fn sink_;
    worker_pool& workers_;
    std::chrono::milliseconds interval_;
    std::chrono::milliseconds retain_;
    boost::asio::steady_timer timer_;
    mutable std::mutex mu_;
    std::unordered_map<std::string, totals> entries_;
    bool stopped_{false};
};

}

#endif
