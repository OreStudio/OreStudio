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
#ifndef ORES_HTTP_DOMAIN_BODY_SOURCE_HPP
#define ORES_HTTP_DOMAIN_BODY_SOURCE_HPP

#include "ores.http.api/export.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>

namespace ores::http::domain {

/**
 * @brief The body of a response, read as it is written.
 *
 * A response that names a source is written in chunks: the session pulls from the
 * source on the connection's own executor and hands each piece to the socket, so a
 * file or a stream costs a buffer and not itself.
 *
 * The length is optional. A file knows it, and knowing it lets the response carry
 * =Content-Length=, which a browser shows progress from and which lets the
 * connection be reused without chunked framing. A stream does not know it.
 */
class ORES_HTTP_API_EXPORT body_source {
public:
    body_source() = default;
    body_source(const body_source&) = delete;
    body_source& operator=(const body_source&) = delete;
    virtual ~body_source() = default;

    /**
     * @brief The whole length, when the source knows it before it is read.
     */
    [[nodiscard]] virtual std::optional<std::uint64_t> size() const = 0;

    /**
     * @brief Fills what it can and says how much it filled.
     *
     * Zero means the body has ended. The headers are already on the wire by the
     * time this runs, so a failure cannot become a status: a source that cannot
     * read throws, and the session closes the connection rather than send a body
     * shorter than the length it promised.
     *
     * @throws std::runtime_error when the source cannot produce what it holds.
     */
    virtual std::size_t read(std::span<char> into) = 0;
};

}

#endif
