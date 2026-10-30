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
#ifndef ORES_HTTP_DOMAIN_BODY_SINK_HPP
#define ORES_HTTP_DOMAIN_BODY_SINK_HPP

#include "ores.http.api/export.hpp"
#include <string_view>

namespace ores::http::domain {

/**
 * @brief Where a streamed request body goes.
 *
 * The session reads the body in chunks and hands each one to the sink on the
 * connection's own executor, so a large body costs a chunk and not itself, and
 * the socket stays on the one executor that owns it. The handler runs after the
 * last chunk with the sink in hand and decides whether the bytes are published:
 * commit() keeps them, abort() discards them, and a sink destroyed without a
 * commit discards them too.
 *
 * A sink is created per streamed request, because it carries the state of one
 * transfer. A buffered route never has one.
 */
class ORES_HTTP_API_EXPORT body_sink {
public:
    body_sink() = default;
    body_sink(const body_sink&) = delete;
    body_sink& operator=(const body_sink&) = delete;
    virtual ~body_sink() = default;

    /**
     * @brief Takes the next chunk.
     *
     * @returns false when the destination cannot take it, which ends the request
     * with a 500 rather than reading a body that has nowhere to go.
     */
    virtual bool consume(std::string_view chunk) = 0;

    /**
     * @brief Called once the whole body has arrived.
     *
     * @returns false when the bytes taken so far cannot be completed.
     */
    virtual bool finish() = 0;

    /**
     * @brief Keeps what was written.
     *
     * Called by the handler once it has decided the request may have the bytes.
     *
     * @returns false when publishing them fails.
     */
    virtual bool commit() = 0;

    /**
     * @brief Discards what was written.
     *
     * Safe to call more than once, and safe after commit().
     */
    virtual void abort() = 0;
};

}

#endif
