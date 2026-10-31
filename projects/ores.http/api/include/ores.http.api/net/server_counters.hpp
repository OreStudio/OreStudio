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
#ifndef ORES_HTTP_NET_SERVER_COUNTERS_HPP
#define ORES_HTTP_NET_SERVER_COUNTERS_HPP

#include "ores.http.api/export.hpp"
#include <atomic>
#include <cstdint>
#include <string>

namespace ores::http::net {

/**
 * @brief What the server knows about its own traffic.
 *
 * Every field is a counter one request increments once, so a session never waits
 * on another and the numbers cannot deadlock. Metrics are the thing to report, not
 * the thing to compute: nothing here derives anything at request time.
 */
struct ORES_HTTP_API_EXPORT server_counters final {
    /// Connections that are open now.
    std::atomic<std::uint64_t> connections_open{0};

    /// Requests read in full, whether or not a route answered them.
    std::atomic<std::uint64_t> requests{0};

    /// Answers by status class: 2xx, 3xx, 4xx and 5xx.
    std::atomic<std::uint64_t> responses_2xx{0};
    std::atomic<std::uint64_t> responses_3xx{0};
    std::atomic<std::uint64_t> responses_4xx{0};
    std::atomic<std::uint64_t> responses_5xx{0};

    /// Requests refused for their size, and requests refused because the pool was full.
    std::atomic<std::uint64_t> refused_too_large{0};
    std::atomic<std::uint64_t> refused_saturated{0};

    /// Bytes a request body carried, and bytes an answer wrote.
    std::atomic<std::uint64_t> request_bytes{0};
    std::atomic<std::uint64_t> response_bytes{0};

    /**
     * @brief Counts an answer by the class of its status.
     */
    void record_response(unsigned status) {
        if (status >= 500)
            responses_5xx.fetch_add(1, std::memory_order_relaxed);
        else if (status >= 400)
            responses_4xx.fetch_add(1, std::memory_order_relaxed);
        else if (status >= 300)
            responses_3xx.fetch_add(1, std::memory_order_relaxed);
        else
            responses_2xx.fetch_add(1, std::memory_order_relaxed);
    }

    /**
     * @brief The counters in the text format a scraper reads.
     */
    [[nodiscard]] std::string to_text() const;
};

}

#endif
