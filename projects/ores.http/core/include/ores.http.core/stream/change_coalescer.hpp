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
#ifndef ORES_HTTP_CORE_STREAM_CHANGE_COALESCER_HPP
#define ORES_HTTP_CORE_STREAM_CHANGE_COALESCER_HPP

#include "ores.http.core/export.hpp"
#include <cstddef>
#include <map>
#include <mutex>
#include <set>
#include <string>
#include <tuple>
#include <vector>

namespace ores::http::stream {

/**
 * @brief One change, as a service announces it.
 *
 * A change names the tenant and party whose row moved, the entity it belongs to and
 * the row itself. The party is optional because some entities belong to the tenant
 * rather than to one of its parties.
 */
struct change_event final {
    std::string tenant;
    std::string party;
    std::string entity;
    std::string id;
};

/**
 * @brief What one tick has to say about one entity.
 *
 * The identifiers are sorted and unique, so two ticks over the same changes produce
 * the same frame and a client can compare them.
 */
struct coalesced_frame final {
    std::string tenant;
    std::string party;
    std::string entity;
    std::vector<std::string> ids;
};

/**
 * @brief Gathers changes into one frame per entity per tick.
 *
 * A bulk import writes thousands of rows, and a stream that sends one frame per row
 * is one a screen cannot use: it redraws a list per row and falls behind the writer.
 * The coalescer keeps a dirty set per tenant, party and entity, and a tick drains it
 * into one frame that says what changed rather than how often.
 *
 * A change that arrives for another tenant or party is a different frame, because a
 * frame is addressed at one party's data and never at a mixture.
 */
class ORES_HTTP_CORE_EXPORT change_coalescer final {
public:
    /// Records one change in the window that is open.
    void record(const change_event& event);

    /// Takes everything recorded since the last drain, one frame per entity.
    std::vector<coalesced_frame> drain();

    /// How many rows are waiting, so a caller can decide against draining.
    [[nodiscard]] std::size_t pending() const;

private:
    using key = std::tuple<std::string, std::string, std::string>;

    mutable std::mutex mu_;
    std::map<key, std::set<std::string>> dirty_;
};

}

#endif
