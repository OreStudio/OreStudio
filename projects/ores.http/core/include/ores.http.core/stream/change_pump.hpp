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
#ifndef ORES_HTTP_CORE_STREAM_CHANGE_PUMP_HPP
#define ORES_HTTP_CORE_STREAM_CHANGE_PUMP_HPP

#include "ores.http.core/export.hpp"
#include "ores.http.core/stream/change_coalescer.hpp"
#include "ores.http.core/stream/change_hub.hpp"
#include <cstddef>
#include <string>
#include <string_view>

namespace ores::http::stream {

/**
 * @brief One coalesced frame as an event-stream message.
 *
 * The event name is the component and the collection, so a client can listen for the
 * one collection it draws and read nothing else. The payload is JSON with the tenant,
 * the party and the same two names, written here rather than by a codec because it is
 * the envelope the stream states and not a domain object.
 */
ORES_HTTP_CORE_EXPORT std::string frame_text(const coalesced_frame& frame);

/**
 * @brief Takes what a window gathered and gives it to every open stream.
 *
 * The coalescer and the hub are two halves of one path and neither knows the other:
 * this is the join, and it is a function rather than a thread so a case can pump one
 * window and read what a subscriber got.
 *
 * @return the number of frames published, which a caller uses to decide whether the
 *         tick has anything to do.
 */
class ORES_HTTP_CORE_EXPORT change_pump final {
public:
    change_pump(change_coalescer& coalescer, change_hub& hub)
        : coalescer_(coalescer)
        , hub_(hub) {}

    std::size_t pump();

private:
    change_coalescer& coalescer_;
    change_hub& hub_;
};

}

#endif
