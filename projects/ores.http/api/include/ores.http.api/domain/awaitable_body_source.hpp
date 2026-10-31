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
#ifndef ORES_HTTP_DOMAIN_AWAITABLE_BODY_SOURCE_HPP
#define ORES_HTTP_DOMAIN_AWAITABLE_BODY_SOURCE_HPP

#include "ores.http.api/export.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>

namespace ores::http::domain {

/**
 * @brief A body that may not have its next piece yet.
 *
 * The synchronous =body_source= suits a file, which always has more to give. A
 * source that waits for an event does not: read synchronously on the connection's
 * thread, it stops every other connection that thread serves. This one waits by
 * suspending the response instead, so the thread carries other work until the next
 * piece is ready.
 *
 * The length has the same meaning as on the synchronous form: a source that knows
 * it sets =Content-Length=, and one that does not is written with chunked framing.
 */
class ORES_HTTP_API_EXPORT awaitable_body_source {
public:
    awaitable_body_source() = default;
    awaitable_body_source(const awaitable_body_source&) = delete;
    awaitable_body_source& operator=(const awaitable_body_source&) = delete;
    virtual ~awaitable_body_source() = default;

    /**
     * @brief The whole length, when the source knows it before it is read.
     */
    [[nodiscard]] virtual std::optional<std::uint64_t> size() const {
        return std::nullopt;
    }

    /**
     * @brief Fills what it can, waiting for the next piece, and says how much.
     *
     * Zero means the body has ended. The headers are already on the wire by the
     * time this runs, so a failure cannot become a status: a source that cannot
     * produce throws, and the session closes the connection rather than send a
     * body shorter than the length it promised.
     *
     * The span is the session's buffer and outlives the wait. A source that
     * suspends for longer than the response lives must copy what it has.
     *
     * @throws std::runtime_error when the source cannot produce what it holds.
     */
    virtual boost::asio::awaitable<std::size_t> read(std::span<char> into) = 0;
};

}

#endif
