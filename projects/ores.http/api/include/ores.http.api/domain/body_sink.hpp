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
 * connection's own executor, so a large body costs a chunk and not itself.
 *
 * The handler runs after the last chunk with the sink in hand and decides
 * whether the bytes are kept: publish() keeps them and abort() discards them.
 * The session holds a guard over the sink and aborts it on every path that did
 * not publish, so a refused, failed or abandoned request leaves nothing behind
 * and no error path has to remember to say so.
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
    bool consume(std::string_view chunk) {
        if (published_ || discarded_)
            return false;
        return consume_chunk(chunk);
    }

    /**
     * @brief Called once the whole body has arrived.
     *
     * @returns false when the bytes taken so far cannot be completed.
     */
    bool finish() {
        if (published_ || discarded_)
            return false;
        finished_ = finish_body();
        return finished_;
    }

    /**
     * @brief Keeps what was written, and stops the destructor discarding it.
     *
     * Called by the handler once it has decided the request may have the bytes,
     * and only after finish() has said they all arrived.
     *
     * @returns false when publishing them fails.
     */
    bool publish() {
        if (published_)
            return true;
        if (discarded_ || !finished_)
            return false;
        published_ = publish_chunk();
        return published_;
    }

    /**
     * @brief Discards what was written.
     *
     * The bytes are discarded once and no more, and a published sink is left
     * alone, so a caller may abort on every path without counting them.
     */
    void abort() noexcept {
        if (published_ || discarded_)
            return;
        discarded_ = true;
        discard();
    }

    /**
     * @brief Whether the bytes were kept.
     *
     * A streamed handler that answers success without publishing has thrown the
     * body away, and the session says so in the log.
     */
    [[nodiscard]] bool published() const {
        return published_;
    }

protected:
    /// Takes one chunk. Only called before publish().
    virtual bool consume_chunk(std::string_view chunk) = 0;

    /// Completes the bytes taken so far.
    virtual bool finish_body() = 0;

    /// Keeps the bytes.
    virtual bool publish_chunk() = 0;

    /// Discards the bytes. Called at most once, by abort(), unless they were kept.
    virtual void discard() noexcept = 0;

private:
    bool finished_ = false;
    bool published_ = false;
    bool discarded_ = false;
};

}

#endif
