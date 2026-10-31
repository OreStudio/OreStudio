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
#include "ores.http.api/net/server_counters.hpp"
#include <sstream>
#include <string_view>

namespace ores::http::net {

namespace {

void add_counter(std::ostringstream& out,
                 std::string_view name,
                 std::string_view help,
                 std::uint64_t value) {
    out << "# HELP " << name << ' ' << help << '\n'
        << "# TYPE " << name << " counter\n"
        << name << ' ' << value << '\n';
}

void add_gauge(std::ostringstream& out,
               std::string_view name,
               std::string_view help,
               std::uint64_t value) {
    out << "# HELP " << name << ' ' << help << '\n'
        << "# TYPE " << name << " gauge\n"
        << name << ' ' << value << '\n';
}

}

std::string server_counters::to_text() const {
    std::ostringstream out;

    add_gauge(out,
              "ores_http_connections_open",
              "Connections open now",
              connections_open.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_requests_total",
                "Requests read in full",
                requests.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_responses_2xx_total",
                "Answers with a 2xx status",
                responses_2xx.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_responses_3xx_total",
                "Answers with a 3xx status",
                responses_3xx.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_responses_4xx_total",
                "Answers with a 4xx status",
                responses_4xx.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_responses_5xx_total",
                "Answers with a 5xx status",
                responses_5xx.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_refused_too_large_total",
                "Bodies refused for their size",
                refused_too_large.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_refused_saturated_total",
                "Requests refused because the pool was full",
                refused_saturated.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_request_bytes_total",
                "Bytes carried by request bodies",
                request_bytes.load(std::memory_order_relaxed));
    add_counter(out,
                "ores_http_response_bytes_total",
                "Bytes written by answers",
                response_bytes.load(std::memory_order_relaxed));

    return out.str();
}

}
