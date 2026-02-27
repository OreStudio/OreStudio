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
#include "ores.mq/generators/queue_message_event_generator.hpp"

#include <sstream>
#include <boost/uuid/uuid_io.hpp>

namespace ores::mq::generators {

using namespace ores::utility::generation;

domain::queue_message_event generate_synthetic_queue_message_event(
    const std::string& queue_name,
    generation_context& ctx) {

    domain::queue_message_event ev;
    ev.queue_name = queue_name.empty()
        ? "pgmq.q_" + ctx.alphanumeric(8)
        : queue_name;
    ev.msg_id = ctx.random_int(1, 1000000);
    ev.timestamp = ctx.past_timepoint(1);
    ev.encoding = domain::payload_encoding::json;
    ev.tenant_id = boost::uuids::to_string(ctx.generate_uuid());

    // Generate a small synthetic JSON payload
    std::ostringstream body;
    body << R"({"id":")" << boost::uuids::to_string(ctx.generate_uuid())
         << R"(","value":)" << ctx.random_int(0, 9999) << "}";
    const auto str = body.str();
    ev.payload.reserve(str.size());
    for (const char c : str) {
        ev.payload.push_back(static_cast<std::byte>(c));
    }

    return ev;
}

domain::queue_message_event generate_synthetic_json_queue_message_event(
    const std::string& queue_name,
    generation_context& ctx) {
    // For now identical to the base generator; both produce JSON payloads.
    return generate_synthetic_queue_message_event(queue_name, ctx);
}

}
